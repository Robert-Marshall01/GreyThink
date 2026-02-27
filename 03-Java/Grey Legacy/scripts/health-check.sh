#!/bin/bash
###############################################################################
# health-check.sh - Grey Legacy Application Health Check
#
# Performs comprehensive health checks and outputs a JSON status object
# suitable for monitoring systems (Nagios, Zabbix, Datadog, etc.).
#
# Usage: ./health-check.sh [--quiet] [--json-only]
# Cron:  */5 * * * * /opt/greylegacy/scripts/health-check.sh --json-only >> /var/log/greylegacy/health.jsonl
#
# Exit Codes:
#   0 - All checks passed
#   1 - One or more checks failed (WARNING)
#   2 - Critical failure
#
# Maintainer: Grey Legacy SRE Team
# Last Modified: 2025-08-19
###############################################################################
set -uo pipefail

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------
readonly APP_NAME="greylegacy"
readonly TOMCAT_HOME="${TOMCAT_HOME:-/opt/tomcat}"
readonly HEALTH_URL="http://localhost:8080/${APP_NAME}/health"
readonly APP_URL="http://localhost:8080/${APP_NAME}/"
readonly HTTP_TIMEOUT=10

readonly DB_HOST="${GL_DB_HOST:-db-prod.greylegacy.internal}"
readonly DB_PORT="${GL_DB_PORT:-5432}"
readonly DB_NAME="${GL_DB_NAME:-greylegacy}"
readonly DB_USER="${GL_DB_USER:-health_check}"
readonly PGPASSFILE="${HOME}/.pgpass"
export PGPASSFILE

readonly DISK_WARN_PERCENT=80
readonly DISK_CRIT_PERCENT=90
readonly MEM_WARN_PERCENT=85
readonly LOG_DIR="/var/log/greylegacy"
readonly CATALINA_LOG="${TOMCAT_HOME}/logs/catalina.out"
readonly ERROR_SCAN_MINUTES=5

readonly JMX_PORT="${JMX_PORT:-9010}"
readonly JMX_CLIENT="/opt/greylegacy/lib/cmdline-jmxclient-0.10.3.jar"

JSON_ONLY=false
QUIET=false

# ---------------------------------------------------------------------------
# Parse arguments
# ---------------------------------------------------------------------------
while [[ $# -gt 0 ]]; do
    case "$1" in
        --quiet)     QUIET=true;     shift ;;
        --json-only) JSON_ONLY=true; shift ;;
        *) shift ;;
    esac
done

# ---------------------------------------------------------------------------
# Status tracking
# ---------------------------------------------------------------------------
declare -A CHECK_RESULTS
OVERALL_STATUS="OK"
OVERALL_CODE=0

record_check() {
    local name="$1" status="$2" message="$3"
    CHECK_RESULTS["${name}"]="${status}|${message}"
    if [[ "${status}" == "CRITICAL" ]]; then
        OVERALL_STATUS="CRITICAL"
        OVERALL_CODE=2
    elif [[ "${status}" == "WARNING" && "${OVERALL_STATUS}" != "CRITICAL" ]]; then
        OVERALL_STATUS="WARNING"
        [[ "${OVERALL_CODE}" -lt 1 ]] && OVERALL_CODE=1
    fi
    if [[ "${QUIET}" == "false" && "${JSON_ONLY}" == "false" ]]; then
        printf "  %-20s [%-8s] %s\n" "${name}" "${status}" "${message}"
    fi
}

# ---------------------------------------------------------------------------
# Check: Tomcat process
# ---------------------------------------------------------------------------
check_tomcat_process() {
    local pid
    pid=$(pgrep -f "catalina" || true)
    if [[ -n "${pid}" ]]; then
        local uptime
        uptime=$(ps -o etime= -p "${pid}" 2>/dev/null | xargs || echo "unknown")
        record_check "tomcat_process" "OK" "PID ${pid}, uptime ${uptime}"
    else
        record_check "tomcat_process" "CRITICAL" "Tomcat process not found"
    fi
}

# ---------------------------------------------------------------------------
# Check: HTTP health endpoint
# ---------------------------------------------------------------------------
check_http_health() {
    local http_code response_time
    http_code=$(curl -s -o /dev/null -w "%{http_code}" --max-time "${HTTP_TIMEOUT}" "${HEALTH_URL}" 2>/dev/null || echo "000")
    response_time=$(curl -s -o /dev/null -w "%{time_total}" --max-time "${HTTP_TIMEOUT}" "${APP_URL}" 2>/dev/null || echo "0")

    if [[ "${http_code}" == "200" ]]; then
        record_check "http_health" "OK" "HTTP ${http_code}, ${response_time}s"
    elif [[ "${http_code}" == "000" ]]; then
        record_check "http_health" "CRITICAL" "Connection refused or timeout"
    else
        record_check "http_health" "WARNING" "HTTP ${http_code}, ${response_time}s"
    fi
}

# ---------------------------------------------------------------------------
# Check: Database connectivity
# ---------------------------------------------------------------------------
check_database() {
    local result
    result=$(psql -h "${DB_HOST}" -p "${DB_PORT}" -U "${DB_USER}" -d "${DB_NAME}" \
        -t -c "SELECT 'OK' AS status, COUNT(*) AS claim_count FROM claims WHERE status = 'OPEN';" 2>/dev/null || echo "FAIL")

    if echo "${result}" | grep -q "OK"; then
        local claim_count
        claim_count=$(echo "${result}" | awk -F'|' '{print $2}' | xargs)
        record_check "database" "OK" "Connected, ${claim_count} open claims"
    else
        record_check "database" "CRITICAL" "Cannot connect to ${DB_HOST}:${DB_PORT}/${DB_NAME}"
    fi
}

# ---------------------------------------------------------------------------
# Check: Disk space
# ---------------------------------------------------------------------------
check_disk_space() {
    local usage
    usage=$(df --output=pcent / | tail -1 | tr -d ' %')

    if [[ "${usage}" -ge "${DISK_CRIT_PERCENT}" ]]; then
        record_check "disk_space" "CRITICAL" "${usage}% used (threshold: ${DISK_CRIT_PERCENT}%)"
    elif [[ "${usage}" -ge "${DISK_WARN_PERCENT}" ]]; then
        record_check "disk_space" "WARNING" "${usage}% used (threshold: ${DISK_WARN_PERCENT}%)"
    else
        record_check "disk_space" "OK" "${usage}% used"
    fi

    # Also check Tomcat logs partition if different
    local log_usage
    log_usage=$(df --output=pcent "${LOG_DIR}" 2>/dev/null | tail -1 | tr -d ' %' || echo "0")
    if [[ "${log_usage}" -ge "${DISK_CRIT_PERCENT}" ]]; then
        record_check "disk_logs" "CRITICAL" "Log partition ${log_usage}% used"
    else
        record_check "disk_logs" "OK" "Log partition ${log_usage}% used"
    fi
}

# ---------------------------------------------------------------------------
# Check: Memory usage
# ---------------------------------------------------------------------------
check_memory() {
    local mem_total mem_avail pct_used
    mem_total=$(grep MemTotal /proc/meminfo | awk '{print $2}')
    mem_avail=$(grep MemAvailable /proc/meminfo | awk '{print $2}')
    pct_used=$(( (mem_total - mem_avail) * 100 / mem_total ))

    if [[ "${pct_used}" -ge "${MEM_WARN_PERCENT}" ]]; then
        record_check "memory" "WARNING" "${pct_used}% used (${mem_avail}kB available)"
    else
        record_check "memory" "OK" "${pct_used}% used (${mem_avail}kB available)"
    fi
}

# ---------------------------------------------------------------------------
# Check: Recent errors in logs
# ---------------------------------------------------------------------------
check_log_errors() {
    if [[ ! -f "${CATALINA_LOG}" ]]; then
        record_check "log_errors" "WARNING" "catalina.out not found"
        return
    fi

    local cutoff error_count oom_count
    cutoff=$(date -d "-${ERROR_SCAN_MINUTES} minutes" '+%Y-%m-%d %H:%M' 2>/dev/null || date '+%Y-%m-%d %H:%M')
    error_count=$(awk -v cutoff="${cutoff}" '$0 >= cutoff' "${CATALINA_LOG}" 2>/dev/null \
        | grep -ci "exception\|error\|fatal" || echo "0")
    oom_count=$(awk -v cutoff="${cutoff}" '$0 >= cutoff' "${CATALINA_LOG}" 2>/dev/null \
        | grep -ci "OutOfMemoryError" || echo "0")

    if [[ "${oom_count}" -gt 0 ]]; then
        record_check "log_errors" "CRITICAL" "${oom_count} OOM error(s) in last ${ERROR_SCAN_MINUTES}min"
    elif [[ "${error_count}" -gt 20 ]]; then
        record_check "log_errors" "WARNING" "${error_count} errors in last ${ERROR_SCAN_MINUTES}min"
    else
        record_check "log_errors" "OK" "${error_count} errors in last ${ERROR_SCAN_MINUTES}min"
    fi
}

# ---------------------------------------------------------------------------
# Check: JMX metrics (heap, threads)
# ---------------------------------------------------------------------------
check_jmx_metrics() {
    if [[ ! -f "${JMX_CLIENT}" ]]; then
        record_check "jmx_metrics" "WARNING" "JMX client not found"
        return
    fi

    local heap_used heap_max thread_count
    heap_used=$(java -jar "${JMX_CLIENT}" "localhost:${JMX_PORT}" \
        "java.lang:type=Memory" HeapMemoryUsage 2>/dev/null | grep "used" | awk '{print $NF}' || echo "0")
    heap_max=$(java -jar "${JMX_CLIENT}" "localhost:${JMX_PORT}" \
        "java.lang:type=Memory" HeapMemoryUsage 2>/dev/null | grep "max" | awk '{print $NF}' || echo "1")
    thread_count=$(java -jar "${JMX_CLIENT}" "localhost:${JMX_PORT}" \
        "java.lang:type=Threading" ThreadCount 2>/dev/null | tail -1 || echo "0")

    if [[ "${heap_max}" -gt 0 && "${heap_used}" -gt 0 ]]; then
        local heap_pct=$(( heap_used * 100 / heap_max ))
        local heap_mb=$(( heap_used / 1048576 ))
        local max_mb=$(( heap_max / 1048576 ))
        record_check "jmx_heap" "$([ "${heap_pct}" -gt 90 ] && echo WARNING || echo OK)" \
            "${heap_mb}MB/${max_mb}MB (${heap_pct}%), threads: ${thread_count}"
    else
        record_check "jmx_heap" "WARNING" "Could not retrieve JMX heap metrics"
    fi
}

# ---------------------------------------------------------------------------
# Output JSON
# ---------------------------------------------------------------------------
output_json() {
    local checks_json=""
    local first=true
    for name in "${!CHECK_RESULTS[@]}"; do
        local status msg
        status=$(echo "${CHECK_RESULTS[$name]}" | cut -d'|' -f1)
        msg=$(echo "${CHECK_RESULTS[$name]}" | cut -d'|' -f2-)
        [[ "${first}" == "true" ]] && first=false || checks_json+=","
        checks_json+="\"${name}\":{\"status\":\"${status}\",\"message\":\"${msg}\"}"
    done

    cat <<EOF
{"timestamp":"$(date -u '+%Y-%m-%dT%H:%M:%SZ')","hostname":"$(hostname)","application":"${APP_NAME}","overall_status":"${OVERALL_STATUS}","checks":{${checks_json}}}
EOF
}

# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------
main() {
    [[ "${JSON_ONLY}" == "false" ]] && echo "Grey Legacy Health Check - $(date '+%Y-%m-%d %H:%M:%S')"
    [[ "${JSON_ONLY}" == "false" ]] && echo "=========================================="

    check_tomcat_process
    check_http_health
    check_database
    check_disk_space
    check_memory
    check_log_errors
    check_jmx_metrics

    [[ "${JSON_ONLY}" == "false" ]] && echo "=========================================="
    [[ "${JSON_ONLY}" == "false" ]] && echo "Overall: ${OVERALL_STATUS}"

    output_json

    exit "${OVERALL_CODE}"
}

main "$@"

{{/*
=============================================================================
Grey Distributed — Helm Template Helpers
=============================================================================

Common template functions used across all Grey chart templates.

=============================================================================
*/}}

{{/*
Expand the name of the chart.
*/}}
{{- define "grey.name" -}}
{{- default .Chart.Name .Values.global.nameOverride | trunc 63 | trimSuffix "-" }}
{{- end }}

{{/*
Create a default fully qualified app name.
Truncate at 63 chars because Kubernetes names are limited.
*/}}
{{- define "grey.fullname" -}}
{{- if .Values.global.fullnameOverride }}
{{- .Values.global.fullnameOverride | trunc 63 | trimSuffix "-" }}
{{- else }}
{{- $name := default .Chart.Name .Values.global.nameOverride }}
{{- if contains $name .Release.Name }}
{{- .Release.Name | trunc 63 | trimSuffix "-" }}
{{- else }}
{{- printf "%s-%s" .Release.Name $name | trunc 63 | trimSuffix "-" }}
{{- end }}
{{- end }}
{{- end }}

{{/*
Create chart name and version for chart label.
*/}}
{{- define "grey.chart" -}}
{{- printf "%s-%s" .Chart.Name .Chart.Version | replace "+" "_" | trunc 63 | trimSuffix "-" }}
{{- end }}

{{/*
Common labels for all resources.
*/}}
{{- define "grey.labels" -}}
helm.sh/chart: {{ include "grey.chart" . }}
{{ include "grey.selectorLabels" . }}
{{- if .Chart.AppVersion }}
app.kubernetes.io/version: {{ .Chart.AppVersion | quote }}
{{- end }}
app.kubernetes.io/managed-by: {{ .Release.Service }}
app.kubernetes.io/part-of: grey-distributed
{{- end }}

{{/*
Selector labels (subset of common labels for matching).
*/}}
{{- define "grey.selectorLabels" -}}
app.kubernetes.io/name: {{ include "grey.name" . }}
app.kubernetes.io/instance: {{ .Release.Name }}
{{- end }}

{{/*
Coordinator-specific labels.
*/}}
{{- define "grey.coordinator.labels" -}}
{{ include "grey.labels" . }}
app.kubernetes.io/component: coordinator
grey.io/role: consensus
{{- end }}

{{- define "grey.coordinator.selectorLabels" -}}
{{ include "grey.selectorLabels" . }}
app.kubernetes.io/component: coordinator
{{- end }}

{{/*
Worker-specific labels.
*/}}
{{- define "grey.worker.labels" -}}
{{ include "grey.labels" . }}
app.kubernetes.io/component: worker
grey.io/role: executor
{{- end }}

{{- define "grey.worker.selectorLabels" -}}
{{ include "grey.selectorLabels" . }}
app.kubernetes.io/component: worker
{{- end }}

{{/*
Gateway-specific labels.
*/}}
{{- define "grey.gateway.labels" -}}
{{ include "grey.labels" . }}
app.kubernetes.io/component: gateway
grey.io/role: ingress
{{- end }}

{{- define "grey.gateway.selectorLabels" -}}
{{ include "grey.selectorLabels" . }}
app.kubernetes.io/component: gateway
{{- end }}

{{/*
Create image reference.
*/}}
{{- define "grey.image" -}}
{{- $registry := .Values.global.image.registry -}}
{{- $repository := .Values.global.image.repository -}}
{{- $tag := .Values.global.image.tag | default .Chart.AppVersion -}}
{{- printf "%s/%s:%s" $registry $repository $tag -}}
{{- end }}

{{/*
Create component-specific image (with override support).
Usage: {{ include "grey.componentImage" (dict "global" .Values.global "component" .Values.coordinator "Chart" .Chart) }}
*/}}
{{- define "grey.componentImage" -}}
{{- $registry := .global.image.registry -}}
{{- $repository := .component.image.repository | default .global.image.repository -}}
{{- $tag := .component.image.tag | default .global.image.tag | default .Chart.AppVersion -}}
{{- printf "%s/%s:%s" $registry $repository $tag -}}
{{- end }}

{{/*
Create service account name for a component.
*/}}
{{- define "grey.coordinator.serviceAccountName" -}}
{{- if .Values.coordinator.serviceAccount.create }}
{{- default (printf "%s-coordinator" (include "grey.fullname" .)) .Values.coordinator.serviceAccount.name }}
{{- else }}
{{- default "default" .Values.coordinator.serviceAccount.name }}
{{- end }}
{{- end }}

{{- define "grey.worker.serviceAccountName" -}}
{{- if .Values.worker.serviceAccount.create }}
{{- default (printf "%s-worker" (include "grey.fullname" .)) .Values.worker.serviceAccount.name }}
{{- else }}
{{- default "default" .Values.worker.serviceAccount.name }}
{{- end }}
{{- end }}

{{- define "grey.gateway.serviceAccountName" -}}
{{- if .Values.gateway.serviceAccount.create }}
{{- default (printf "%s-gateway" (include "grey.fullname" .)) .Values.gateway.serviceAccount.name }}
{{- else }}
{{- default "default" .Values.gateway.serviceAccount.name }}
{{- end }}
{{- end }}

{{/*
Coordinator headless service name.
*/}}
{{- define "grey.coordinator.headlessServiceName" -}}
{{- printf "%s-coordinator-headless" (include "grey.fullname" .) -}}
{{- end }}

{{/*
ConfigMap checksum for pod annotation (triggers rollout on config change).
*/}}
{{- define "grey.configChecksum" -}}
{{- include (print .Template.BasePath "/configmap.yaml") . | sha256sum | trunc 63 }}
{{- end }}

{{/*
Calculate minimum available for PDB.
For n replicas, need n/2 + 1 for quorum.
*/}}
{{- define "grey.coordinator.pdbMinAvailable" -}}
{{- if .Values.coordinator.pdb.minAvailable }}
{{- .Values.coordinator.pdb.minAvailable }}
{{- else }}
{{- $replicas := int .Values.coordinator.replicas }}
{{- add (div $replicas 2) 1 }}
{{- end }}
{{- end }}

{{/*
Pod anti-affinity configuration.
*/}}
{{- define "grey.antiAffinity" -}}
{{- if eq .antiAffinityPreset "hard" }}
podAntiAffinity:
  requiredDuringSchedulingIgnoredDuringExecution:
    - labelSelector:
        matchLabels:
          {{- .selectorLabels | nindent 10 }}
      topologyKey: kubernetes.io/hostname
{{- else if eq .antiAffinityPreset "soft" }}
podAntiAffinity:
  preferredDuringSchedulingIgnoredDuringExecution:
    - weight: 100
      podAffinityTerm:
        labelSelector:
          matchLabels:
            {{- .selectorLabels | nindent 12 }}
        topologyKey: kubernetes.io/hostname
{{- end }}
{{- end }}

{{/*
Common environment variables for all Grey pods.
*/}}
{{- define "grey.commonEnv" -}}
- name: POD_NAME
  valueFrom:
    fieldRef:
      fieldPath: metadata.name
- name: POD_NAMESPACE
  valueFrom:
    fieldRef:
      fieldPath: metadata.namespace
- name: POD_IP
  valueFrom:
    fieldRef:
      fieldPath: status.podIP
- name: GOMEMLIMIT
  valueFrom:
    resourceFieldRef:
      resource: limits.memory
- name: GOMAXPROCS
  valueFrom:
    resourceFieldRef:
      resource: limits.cpu
{{- end }}

{{/*
Common volume mounts.
*/}}
{{- define "grey.commonVolumeMounts" -}}
- name: config
  mountPath: /etc/grey
  readOnly: true
- name: tls-certs
  mountPath: /etc/grey/certs
  readOnly: true
- name: tmp
  mountPath: /tmp
{{- end }}

{{/*
Common volumes.
*/}}
{{- define "grey.commonVolumes" -}}
- name: config
  configMap:
    name: {{ include "grey.fullname" . }}-config
- name: tls-certs
  secret:
    secretName: {{ include "grey.fullname" . }}-tls
- name: tmp
  emptyDir:
    medium: Memory
    sizeLimit: 100Mi
{{- end }}

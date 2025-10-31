/*
 * SPDX-License-Identifier: MIT
 * Copyright (C) 2025 Msh project contributors
 *
 * This file is part of Msh. See LICENSES.md for full license and third-party attribution details.
 */

// VERY IMPORTANT: GitHub Copilot AI generated, then modified by me.

// IMPORTANT: This shell is NOT comprehensive and does NOT cover all edge cases.
// IMPORTANT: This shell does NOT implement advanced features like piping.
// IMPORTANT: User manipulation is not implemented.
// IMPORTANT: This shell is for educational purposes and does NOT use any process injection or suspicious API calls.
// IMPORTANT: Advanced programmer features like bash scripting, environment variables, and job control are NOT implemented.
// IMPORTANT: This program is for educational purposes and does NOT use any process injection or suspicious API calls.
// IMPORTANT: It simply reads user input and executes commands using the standard C library.
// LANGUAGE VERSION: C99 (compatible with GCC)

// Marshall Shell: Simple command shell for Windows

// DOCUMENTATION:
// Marshall Shell Custom Commands:
// You can also run any executable in your PATH, e.g. notepad, calc, etc.

/*
To run the program:
1. Install the C/C++ extension in VS Code.
2. Install GCC compiler from https://jmeubank.github.io/tdm-gcc/
3. Test installation with `gcc --version` in the terminal.
4. Restart VS Code and your terminal after installation.
5. Open the terminal in VS Code.
6. Navigate to the directory where this file is located.
7. Compile and run the program with:
	powershell -NoProfile -ExecutionPolicy Bypass -File .\marshall_shell_executor.ps1
*/
/*
DOCUMENTATION:
cd - Change directory.
Usage: cd [directory]
Changes the current working directory to the specified directory.

mv - Move/Rename file or directory.
Usage: mv [source] [destination]
Moves or renames the specified file or directory.

touch - Create an empty file.
Usage: touch [file]
Creates an empty file with the specified name.

rm - Remove file or directory.
Usage: rm [file/directory]
Removes the specified file or directory.

cp - Copy file or directory.
Usage: cp [source] [destination]
Copies the specified file or directory to the destination.

ls - List directory contents.
Usage: ls [directory]
Lists the contents of the specified directory.

pwd - Print working directory.
Usage: pwd
Prints the current working directory.

grep - Search for a pattern in a file.
Usage: grep [pattern] [file]
Searches for the specified pattern in the given file.

echo - Display a line of text.
Usage: echo [text]
Displays the specified text in the terminal.
	echo [text] > [file] - Overwrite file with text.
	echo [text] >> [file] - Append text to file.

cat - Display file contents.
Usage: cat [file]
Displays the contents of the specified file.

head - Display the first 10 lines of a file.
Usage: head [file]
Displays the first 10 lines of the specified file.

tail - Display the last 10 lines of a file.
Usage: tail [file]
Displays the last 10 lines of the specified file.

mkdir - Create a new directory.
Usage: mkdir [directory]
Creates a new directory with the specified name.

rmdir - Remove an empty directory.
Usage: rmdir [directory]
Removes the specified empty directory.

exit - Exit the shell
Usage: exit
Exits the shell program.

clear - Clear the terminal screen
Usage: clear
Clears the terminal screen.

ln - create a hard link
Usage: ln [source] [linkname]
Creates a hard link from the source to the linkname.


*/
#include <stdio.h>
#include <string.h>
// Prototypes for helpers
#include <stdlib.h>
#include <windows.h>
#include <io.h>
#include <direct.h>
#include <fcntl.h>
#include "jobs.h"

char *extract_pattern_arg(const char *cmd, int skiplen);
char *extract_filename_arg(const char *cmd, int skiplen);
void strip_quotes(char *s);
static int wildcard_match(const char *pattern, const char *str);
void run_builtin_command(const char *cmd, FILE *in, FILE *out);

int is_builtin(const char *cmd);

// New helpers for semicolon splitting and single-line processing
static int split_semicolon(const char *line, char segments[][1024], int maxseg);
static int handle_command_line(char *line, int in_noninteractive);
// Forward declarations for wrapper handlers
static int run_ls_handler(char *line, FILE *out);
static int run_ps_handler(char *line, FILE *out);

// Runtime control: -1 = unset (read from env), 0 = disabled, 1 = enabled
static int MSH_forward_enabled = -1;

// Updated run_builtin_command to support grep with input stream
// ...existing code...

// run_builtin_command is implemented in a separate source file now (run_builtin_command.c)
// to avoid pulling a large header into this translation unit.

// Extracts a single pattern argument from a command string (after cmd)
// Handles quotes, skips leading/trailing spaces, ignores extra args
char *extract_pattern_arg(const char *cmd, int skiplen) {
	static char patbuf[256];
	const char *src = cmd + skiplen;
	while (*src == ' ' || *src == '\t') src++;
	int i = 0;
	if (*src == '"' || *src == '\'') {
		char quote = *src++;
		while (*src && *src != quote && i < (int)sizeof(patbuf)-1) {
			patbuf[i++] = *src++;
		}
		if (*src == quote) src++;
	} else {
		while (*src && *src != ' ' && *src != '\t' && i < (int)sizeof(patbuf)-1) {
			patbuf[i++] = *src++;
		}
	}
	patbuf[i] = '\0';
	// Trim leading and trailing whitespace
	char *start = patbuf;
	while (*start == ' ' || *start == '\t') start++;
	char *end = patbuf + strlen(patbuf) - 1;
	while (end >= start && (*end == ' ' || *end == '\t')) *end-- = '\0';
	if (!start || *start == '\0') return NULL;
	strncpy(patbuf, start, sizeof(patbuf)-1);
	patbuf[sizeof(patbuf)-1] = '\0';
	return patbuf;
}
#ifndef MAX_PATH
#define MAX_PATH 260
#endif
// Utility: strip surrounding quotes from a string in-place
void strip_quotes(char *s) {
	size_t len = strlen(s);
	if (len >= 2 && ((s[0] == '"' && s[len-1] == '"') || (s[0] == '\'' && s[len-1] == '\''))) {
		s[len-1] = 0;
		memmove(s, s+1, len-1);
	}
}

// Extracts a single filename argument from a command string (after cmd)
// Handles quotes, skips leading/trailing spaces, ignores extra args
char *extract_filename_arg(const char *cmd, int skiplen) {
	static char filebuf[512];
	const char *src = cmd + skiplen;
	while (*src == ' ' || *src == '\t') src++;
	int i = 0;
	if (*src == '"' || *src == '\'') {
		char quote = *src++;
		while (*src && *src != quote && i < (int)sizeof(filebuf)-1) {
			filebuf[i++] = *src++;
		}
		if (*src == quote) src++;
	} else {
		while (*src && *src != ' ' && *src != '\t' && i < (int)sizeof(filebuf)-1) {
			filebuf[i++] = *src++;
		}
	}
	filebuf[i] = '\0';
	// Remove trailing spaces
	i--;
	while (i >= 0 && (filebuf[i] == ' ' || filebuf[i] == '\t')) filebuf[i--] = '\0';
	strip_quotes(filebuf);
	// Remove leading spaces again after quotes
	char *file = filebuf;
	while (*file == ' ' || *file == '\t') file++;
	if (!file || *file == '\0') return NULL;
	return file;
}

// --- Thread helpers for built-in | built-in pipe ---
typedef struct { char *cmd; FILE *out; } builtin_left_args;
typedef struct { char *cmd; FILE *in; } builtin_right_args;


// Helper for recursive directory copy (for cp -r)
int copy_dir(const char *src_dir, const char *dst_dir) {
	CreateDirectoryA(dst_dir, NULL);
	char search[MAX_PATH*2];
	snprintf(search, sizeof(search), "%s\\*", src_dir);
	WIN32_FIND_DATAA fd;
	HANDLE h = FindFirstFileA(search, &fd);
	if (h == INVALID_HANDLE_VALUE) return 0;
	do {
		if (strcmp(fd.cFileName, ".") == 0 || strcmp(fd.cFileName, "..") == 0) continue;
		char src_path[MAX_PATH*2], dst_path[MAX_PATH*2];
		snprintf(src_path, sizeof(src_path), "%s\\%s", src_dir, fd.cFileName);
		snprintf(dst_path, sizeof(dst_path), "%s\\%s", dst_dir, fd.cFileName);
		if (fd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) {
			copy_dir(src_path, dst_path);
		} else {
			CopyFileA(src_path, dst_path, FALSE);
		}
	} while (FindNextFileA(h, &fd));
	FindClose(h);
	return 1;
}

// Robust recursive directory removal using Windows API
int remove_directory_recursive(const char *path) {
	char search_path[MAX_PATH];
	snprintf(search_path, sizeof(search_path), "%s\\*", path);
	WIN32_FIND_DATAA findData;
	HANDLE hFind = FindFirstFileA(search_path, &findData);
	if (hFind == INVALID_HANDLE_VALUE) {
		// Directory is empty or not found
		DWORD attr = GetFileAttributesA(path);
		if (attr != INVALID_FILE_ATTRIBUTES && (attr & FILE_ATTRIBUTE_READONLY)) {
			SetFileAttributesA(path, attr & ~FILE_ATTRIBUTE_READONLY);
		}
		if (RemoveDirectoryA(path)) return 1;
		else {
			if (attr != INVALID_FILE_ATTRIBUTES) perror(path);
			return 0;
		}
	}
	int ok = 1;
	do {
		if (strcmp(findData.cFileName, ".") == 0 || strcmp(findData.cFileName, "..") == 0) continue;
		char full_path[MAX_PATH];
		snprintf(full_path, sizeof(full_path), "%s\\%s", path, findData.cFileName);
		DWORD attr = GetFileAttributesA(full_path);
		if (attr & FILE_ATTRIBUTE_DIRECTORY) {
			if (!remove_directory_recursive(full_path)) {
				fprintf(stderr, "Failed to remove directory: %s\n", full_path);
				ok = 0;
			}
		} else {
			// Remove read-only attribute if set
			if (attr != INVALID_FILE_ATTRIBUTES && (attr & FILE_ATTRIBUTE_READONLY)) {
				SetFileAttributesA(full_path, attr & ~FILE_ATTRIBUTE_READONLY);
			}
			if (DeleteFileA(full_path) == 0) {
				perror(full_path);
				ok = 0;
			}
		}
	} while (FindNextFileA(hFind, &findData));
	FindClose(hFind);
	if (ok) {
		DWORD attr = GetFileAttributesA(path);
		if (attr != INVALID_FILE_ATTRIBUTES && (attr & FILE_ATTRIBUTE_READONLY)) {
			SetFileAttributesA(path, attr & ~FILE_ATTRIBUTE_READONLY);
		}
		if (RemoveDirectoryA(path)) return 1;
		else {
			DWORD err = GetLastError();
			fprintf(stderr, "rmdir: Failed to remove directory: %s (Windows error code: %lu)\n", path, err);
			return 0;
		}
	}
	return 0;
}

int is_builtin(const char *cmd) {
    while (*cmd == ' ') cmd++;
    return (
		strncmp(cmd, "touch", 5) == 0 ||
        strncmp(cmd, "medit", 5) == 0 ||
        strncmp(cmd, "ifconfig", 8) == 0 ||
		strncmp(cmd, "find", 4) == 0 ||
        	strncmp(cmd, "adduser", 7) == 0 ||
			strncmp(cmd, "passwd", 6) == 0 ||
			strncmp(cmd, "deluser", 7) == 0 ||
			strncmp(cmd, "groupadd", 8) == 0 ||
			strncmp(cmd, "addgroup", 8) == 0 ||
			strncmp(cmd, "groupdel", 8) == 0 ||
			strncmp(cmd, "delgroup", 8) == 0 ||
			strncmp(cmd, "chown", 5) == 0 ||
			strncmp(cmd, "chgrp", 5) == 0 ||
	strncmp(cmd, "grep", 4) == 0 ||
		strncmp(cmd, "sed", 3) == 0 ||
	strncmp(cmd, "awk", 3) == 0 ||
		strncmp(cmd, "useradd", 7) == 0 ||
		strncmp(cmd, "userdel", 7) == 0 ||
		strncmp(cmd, "chmod", 5) == 0 ||
		strncmp(cmd, "apt", 3) == 0 ||
		strncmp(cmd, "apt-get", 7) == 0 ||
		strncmp(cmd, "kill", 4) == 0 ||
		strncmp(cmd, "wc", 2) == 0 ||
        strncmp(cmd, "ls", 2) == 0 ||
        strncmp(cmd, "pwd", 3) == 0 ||
		strncmp(cmd, "hostname", 8) == 0 ||
        strncmp(cmd, "cat", 3) == 0 ||
        strncmp(cmd, "echo", 4) == 0 ||
        strncmp(cmd, "head", 4) == 0 ||
		strncmp(cmd, "tail", 4) == 0 ||
		strncmp(cmd, "wget", 4) == 0
		|| strncmp(cmd, "jobs", 4) == 0
		|| strncmp(cmd, "bg", 2) == 0
		|| strncmp(cmd, "fg", 2) == 0
			|| strncmp(cmd, "msh", 3) == 0
    );
}

// Trim leading/trailing spaces in-place
static void trim_inplace(char *s) {
	// trim leading
	char *start = s;
	while (*start == ' ' || *start == '\t') start++;
	if (start != s) memmove(s, start, strlen(start)+1);
	// trim trailing
	size_t len = strlen(s);
	while (len > 0 && (s[len-1] == ' ' || s[len-1] == '\t')) s[--len] = '\0';
}

// Piping behavior removed: this shell does not implement pipelines.
// Simple pipeline implementation (sequential stage execution using temp files)
// This implementation executes pipeline stages left-to-right. For built-in
// commands we call run_builtin_command(in, out). For external commands we
// execute them with shell redirection using temporary files for stdin/stdout.
// This is not a fully concurrent implementation like bash, but it provides
// the common pipe behavior for most use-cases.

// Trim and split a command line into pipeline segments (handles quotes)
static int split_pipeline(const char *line, char segments[][1024], int maxseg) {
	int seg = 0;
	int pos = 0;
	char quote = 0;
	for (int i = 0; line[i] != '\0'; i++) {
		char c = line[i];
		if ((c == '"' || c == '\'') ) {
			if (quote == 0) quote = c;
			else if (quote == c) quote = 0;
		}
		if (c == '|' && quote == 0) {
			// end current segment
			segments[seg][pos] = '\0';
			trim_inplace(segments[seg]);
			seg++;
			if (seg >= maxseg) return seg;
			pos = 0;
			continue;
		}
		if (pos < 1023) segments[seg][pos++] = c;
	}
	segments[seg][pos] = '\0';
	trim_inplace(segments[seg]);
	return seg + 1;
}

// Copy contents from FILE* src to FILE* dst
static void copy_file_fp(FILE *src, FILE *dst) {
	char buf[4096];
	size_t n;
	while ((n = fread(buf, 1, sizeof(buf), src)) > 0) {
		fwrite(buf, 1, n, dst);
	}
}

// Execute an external command, optionally with input from 'in' FILE*, and
// write its stdout into the provided 'out' FILE*.
// Uses temporary filenames and shell redirection. Returns system() return code.
static int run_external_with_streams(const char *cmd, FILE *in, FILE *out) {
	char inname[MAX_PATH];
	char outname[MAX_PATH];
	inname[0] = '\0'; outname[0] = '\0';
	char syscmd[4096];
	// prepare input temp file if needed
	if (in && in != stdin) {
		char tempPath[MAX_PATH];
		if (GetTempPathA(MAX_PATH, tempPath) == 0 || GetTempFileNameA(tempPath, "msh", 0, inname) == 0) inname[0] = '\0';
		FILE *f = fopen(inname, "wb");
		if (!f) return -1;
		rewind(in);
		copy_file_fp(in, f);
		fclose(f);
	}
	{
		char tempPath[MAX_PATH];
		if (GetTempPathA(MAX_PATH, tempPath) == 0 || GetTempFileNameA(tempPath, "msh", 0, outname) == 0) outname[0] = '\0';
	}
	// Build shell command. On Windows system() uses cmd.exe so use < and >
	if (inname[0]) {
		snprintf(syscmd, sizeof(syscmd), "%s < \"%s\" > \"%s\"", cmd, inname, outname);
	} else {
		snprintf(syscmd, sizeof(syscmd), "%s > \"%s\"", cmd, outname);
	}
	int rc = system(syscmd);
	// Read output file into out
	FILE *rf = fopen(outname, "rb");
	if (rf) {
		copy_file_fp(rf, out);
		fclose(rf);
	}
	// cleanup temp files
	if (inname[0]) remove(inname);
	if (outname[0]) remove(outname);
	rewind(out);
	return rc;
}

// Run a pipeline (e.g. "cmd1 | cmd2 | cmd3"). Returns 0 on success.
static int run_pipeline(const char *line) {
	char segments[32][1024];
	int n = split_pipeline(line, segments, 32);
	if (n <= 0) return -1;
	FILE *in = NULL; // input for current stage
	FILE *stage_out = NULL;
	char tmpnames[32][L_tmpnam];
	for (int ti = 0; ti < 32; ti++) tmpnames[ti][0] = '\0';
	int prev_tmp_index = -1;
	for (int i = 0; i < n; i++) {
		// prepare an output temp FILE for this stage using tmpnam + fopen
		// create a temp file name using Win32 API
		char tempPath[MAX_PATH];
		if (GetTempPathA(MAX_PATH, tempPath) == 0 || GetTempFileNameA(tempPath, "msh", 0, tmpnames[i]) == 0) {
			fprintf(stderr, "pipeline: failed to create temporary name\n");
			if (in && in != stdin) fclose(in);
			for (int k = 0; k < i; k++) if (tmpnames[k][0]) remove(tmpnames[k]);
			return -1;
		}
		stage_out = fopen(tmpnames[i], "w+b");
		if (!stage_out) {
			fprintf(stderr, "pipeline: failed to open temporary file '%s'\n", tmpnames[i]);
			if (in && in != stdin) fclose(in);
			for (int k = 0; k <= i; k++) if (tmpnames[k][0]) remove(tmpnames[k]);
			return -1;
		}
		// if builtin
		if (is_builtin(segments[i])) {
			// run_builtin_command expects FILE* in/out; provide stdin if NULL
			FILE *usein = (in && in != stdin) ? in : stdin;
			// special-case ls and ps so they can write to the provided FILE*
			if (strncmp(segments[i], "ls", 2) == 0) {
				run_ls_handler(segments[i], stage_out);
				fflush(stage_out); rewind(stage_out);
			} else if (strncmp(segments[i], "ps", 2) == 0) {
				run_ps_handler(segments[i], stage_out);
				fflush(stage_out); rewind(stage_out);
			} else {
				run_builtin_command(segments[i], usein, stage_out);
				fflush(stage_out);
				rewind(stage_out);
			}
		} else {
			// external command: execute and capture output
			int rc = run_external_with_streams(segments[i], in, stage_out);
			(void)rc; // ignore for now
		}
		// close previous input if it was a temp file and remove its name
		if (in && in != stdin) {
			fclose(in);
			if (prev_tmp_index >= 0 && tmpnames[prev_tmp_index][0]) remove(tmpnames[prev_tmp_index]);
		}
		// next stage will read from stage_out
		in = stage_out;
		prev_tmp_index = i;
		stage_out = NULL;
	}
	// final output: copy 'in' to stdout
	if (in) {
		rewind(in);
		copy_file_fp(in, stdout);
		if (in != stdin) {
			fclose(in);
			if (prev_tmp_index >= 0 && tmpnames[prev_tmp_index][0]) remove(tmpnames[prev_tmp_index]);
		}
	}
	return 0;
}

int main(int argc, char **argv) {
	// Note: -c handling removed; this shell no longer supports running a single command via -c.
	char line[1024];
	int is_root = 0;
	// Check if running as administrator at startup
	{
		BOOL isAdmin = FALSE;
		HANDLE token = NULL;
		if (OpenProcessToken(GetCurrentProcess(), TOKEN_QUERY, &token)) {
			TOKEN_ELEVATION elevation;
			DWORD size;
			if (GetTokenInformation(token, TokenElevation, &elevation, sizeof(elevation), &size)) {
				isAdmin = elevation.TokenIsElevated;
			}
			CloseHandle(token);
		}
		if (isAdmin) is_root = 1;
	}
	char username[256];
	DWORD size = sizeof(username);
	GetUserNameA(username, &size);
	char computername[256];
	DWORD csize = sizeof(computername);
	GetComputerNameA(computername, &csize);

	printf("Marshall Shell - Type 'exit' to quit.\n");
	printf("Documentation: visit https://www.gnu.org/software/bash/manual/ for command manuals.\n");
	printf("Attribution: Some command documentation and design inspired by GNU Bash manual.\n");
	printf("GNU Bash manual © Free Software Foundation, Inc., licensed under the GNU Free Documentation License (GFDL).\n");
	printf("License details: https://www.gnu.org/licenses/fdl.html\n\n");
	/* Support running a single command when invoked elevated via --run-cmd
	 * Usage (internal): Msh.exe --run-cmd "command string"
	 */
	if (argc > 1 && strcmp(argv[1], "--run-cmd") == 0) {
		/* Reconstruct the command from argv[2..] */
		char cmdline[4096]; cmdline[0] = '\0';
		for (int i = 2; i < argc; ++i) {
			if (i > 2) strncat(cmdline, " ", sizeof(cmdline)-strlen(cmdline)-1);
			strncat(cmdline, argv[i], sizeof(cmdline)-strlen(cmdline)-1);
		}
		if (cmdline[0]) {
			/* Execute the command string once and exit */
			char segments[64][1024];
			int nseg = split_semicolon(cmdline, segments, 64);
			for (int si = 0; si < nseg; si++) {
				if (segments[si][0] == '\0') continue;
				int should_exit = handle_command_line(segments[si], 1);
				if (should_exit) break;
			}
		}
		return 0;
	}

	while (1) {
		char cwd[1024];
		GetCurrentDirectoryA(sizeof(cwd), cwd);
		char *prompt_dir = cwd;
		// Convert backslashes to slashes for Bash-like look
		for (char *p = prompt_dir; *p; ++p) if (*p == '\\') *p = '/';
		printf("%s@%s:%s%s ", username, computername, prompt_dir, is_root ? "#" : "$" );

	if (!fgets(line, sizeof(line), stdin)) break;
	// Immediately print raw line to debug what we received (includes CR/LF)
	fprintf(stderr, "[msh-debug] raw input (with newline): %s", line);
	// remove trailing '\n' and optional '\r' (handle CRLF inputs)
	size_t _li = strcspn(line, "\n");
	if (_li < sizeof(line)) line[_li] = '\0';
	// remove trailing '\r' left from CRLF
	size_t llen = strlen(line);
	while (llen > 0 && line[llen-1] == '\r') { line[--llen] = '\0'; }
	if (llen == 0) continue;

	// Support top-level semicolon-separated commands (ignore semicolons inside quotes)
	// Use the centralized handle_command_line so behavior matches the inline handlers.
	{
		char segments[64][1024];
		int nseg = split_semicolon(line, segments, 64);
		int should_exit = 0;
		for (int si = 0; si < nseg; ++si) {
			if (segments[si][0] == '\0') continue;
			int r = handle_command_line(segments[si], 0);
			if (r) { should_exit = 1; break; }
		}
		if (should_exit) goto msh_exit;
		continue; /* processed this input line via handler(s) */
	}

		// Detect background '&' early so builtins can be backgrounded too
		char line_noamp[1024]; strncpy(line_noamp, line, sizeof(line_noamp)-1); line_noamp[sizeof(line_noamp)-1] = '\0';
		int bg_requested = 0;
		int _llen = (int)strlen(line_noamp);
		while (_llen > 0 && (line_noamp[_llen-1] == ' ' || line_noamp[_llen-1] == '\t')) line_noamp[--_llen] = '\0';
		if (_llen > 0 && line_noamp[_llen-1] == '&') {
			bg_requested = 1; line_noamp[--_llen] = '\0';
			while (_llen > 0 && (line_noamp[_llen-1] == ' ' || line_noamp[_llen-1] == '\t')) line_noamp[--_llen] = '\0';
		}

		// If background requested and the (trimmed) command is a builtin, launch it as a background job
		if (bg_requested && is_builtin(line_noamp)) {
			STARTUPINFOA si; PROCESS_INFORMATION pi; memset(&si, 0, sizeof(si)); si.cb = sizeof(si);
			char cmdcopy[1200]; snprintf(cmdcopy, sizeof(cmdcopy), "cmd.exe /C %s", line_noamp);
			BOOL ok = CreateProcessA(NULL, cmdcopy, NULL, NULL, FALSE, CREATE_NEW_PROCESS_GROUP, NULL, NULL, &si, &pi);
			if (!ok) {
				fprintf(stderr, "Failed to launch background builtin job.\n");
			} else {
				int jid = msh_add_job(pi.hProcess, pi.hThread, pi.dwProcessId, line_noamp);
				if (jid < 0) { fprintf(stderr, "Job list full.\n"); CloseHandle(pi.hProcess); CloseHandle(pi.hThread); }
				else printf("[%d] %lu\n", jid, (unsigned long)pi.dwProcessId);
			}
			continue;
		}

		// If the command contains a pipe, handle the pipeline
		if (strchr(line, '|')) {
			run_pipeline(line);
			continue;
		}

		// For non-pipeline commands, fall back to the existing inline handling by reusing system() for externals and builtins handled below.
		// We'll execute the same large set of builtin handlers inline (preserved from original code) to keep behavior intact.

		// sudo
		if (strncmp(line, "sudo", 4) == 0 && (line[4] == ' ' || line[4] == '\t')) {
			char *cmd = (char *)line + 4; while (*cmd == ' ' || *cmd == '\t') cmd++;
			if (!*cmd) { fprintf(stderr, "Usage: sudo <command>\n"); continue; }
			// Check if already running as administrator
			BOOL isAdmin = FALSE;
			HANDLE token = NULL;
			if (OpenProcessToken(GetCurrentProcess(), TOKEN_QUERY, &token)) {
				TOKEN_ELEVATION elevation;
				DWORD size;
				if (GetTokenInformation(token, TokenElevation, &elevation, sizeof(elevation), &size)) {
					isAdmin = elevation.TokenIsElevated;
				}
				CloseHandle(token);
			}
			if (isAdmin) {
				/* Already elevated: execute the command string directly. Use run_pipeline
				 * which handles builtins/pipelines inline. */
				run_pipeline(cmd);
				continue;
			}
			// Not elevated: if stdin is not a console (non-interactive), don't attempt UAC elevation
			// because it won't be visible in non-interactive contexts; instead run the command locally.
			DWORD consoleMode = 0;
			BOOL hasConsole = GetConsoleMode(GetStdHandle(STD_INPUT_HANDLE), &consoleMode);
			if (!hasConsole) {
				printf("Not running in interactive console; running command locally (no elevation).\n");
				run_pipeline(cmd);
				continue;
			}
			// Otherwise attempt elevation
			char exePath[MAX_PATH]; GetModuleFileNameA(NULL, exePath, MAX_PATH);
			// Build parameter string: --run-cmd "<cmd>"
			char params[4096]; snprintf(params, sizeof(params), "--run-cmd %s", cmd);
			SHELLEXECUTEINFOA sei = {0}; sei.cbSize = sizeof(sei);
			sei.fMask = SEE_MASK_NOCLOSEPROCESS; sei.hwnd = NULL; sei.lpVerb = "runas";
			sei.lpFile = exePath; sei.lpParameters = params; sei.lpDirectory = NULL; sei.nShow = SW_SHOWNORMAL;
			if (ShellExecuteExA(&sei)) {
				printf("Elevation requested. Exiting current shell.\n"); exit(0);
			} else {
				printf("Failed to elevate. User may have cancelled or denied UAC prompt. Falling back to local execution.\n");
				/* Attempt to execute the command locally (handles builtins) */
				run_pipeline(cmd);
			}
			continue;
		}
		// exit
		if (strcmp(line, "exit") == 0) break;

			// forward-wsl [on|off|status] - runtime toggle for forwarding Linux commands to WSL
			if (strncmp(line, "forward-wsl", 11) == 0) {
				char *arg = line + 11;
				while (*arg == ' ') arg++;
				if (*arg == '\0' || strncmp(arg, "status", 6) == 0) {
					int val = MSH_forward_enabled;
					if (val == -1) {
						const char *env = getenv("MSH_FORWARD_LINUX_CMDS");
						if (!env) printf("forward-wsl: status = (env default) enabled\n");
						else printf("forward-wsl: status = env(%s)\n", env);
					} else if (val == 0) printf("forward-wsl: disabled\n"); else printf("forward-wsl: enabled\n");
					continue;
				} else if (strncmp(arg, "on", 2) == 0) {
					MSH_forward_enabled = 1; printf("forward-wsl: enabled\n"); continue;
				} else if (strncmp(arg, "off", 3) == 0) {
					MSH_forward_enabled = 0; printf("forward-wsl: disabled\n"); continue;
				} else {
					printf("Usage: forward-wsl [on|off|status]\n"); continue;
				}
			}

		// cd
		if (strncmp(line, "cd ", 3) == 0) {
			char *dir = line + 3;
			while (*dir == ' ') dir++;
			if (SetCurrentDirectoryA(dir)) {
				// success
			} else {
				perror("cd");
			}
			continue;
		}

		// mv
		if (strncmp(line, "mv ", 3) == 0) {
			char *rest = line + 3;
			while (*rest == ' ') rest++;
			char *src = rest;
			char *dst = strchr(rest, ' ');
			if (!dst) { fprintf(stderr, "Usage: mv [source] [destination]\n"); continue; }
			*dst = '\0'; dst++;
			while (*dst == ' ') dst++;
			DWORD attr = GetFileAttributesA(dst);
			char final_dst[MAX_PATH];
			if (attr != INVALID_FILE_ATTRIBUTES && (attr & FILE_ATTRIBUTE_DIRECTORY)) {
				// Destination is a directory, append filename
				const char *filename = strrchr(src, '\\');
				if (!filename) filename = src; else filename++;
				snprintf(final_dst, sizeof(final_dst), "%s\\%s", dst, filename);
			} else {
				strncpy(final_dst, dst, sizeof(final_dst));
				final_dst[sizeof(final_dst)-1] = '\0';
			}
			if (MoveFileA(src, final_dst)) {
				// success
			} else {
				perror("mv");
			}
			continue;
		}

		// touch
		if (strncmp(line, "touch ", 6) == 0) {
			char *file = line + 6;
			while (*file == ' ') file++;
			FILE *fp = fopen(file, "a");
			if (fp) fclose(fp);
			else perror("touch");
			continue;
		}

		// rm
		if (strncmp(line, "rm ", 3) == 0) {
			char *target = line + 3;
			while (*target == ' ') target++;
			int recursive = 0;
			if (*target == '-' && (*(target+1) == 'r' || *(target+1) == 'R')) {
				recursive = 1;
				target += 2;
				while (*target == ' ') target++;
			}
			DWORD attr = GetFileAttributesA(target);
			if (attr != INVALID_FILE_ATTRIBUTES && (attr & FILE_ATTRIBUTE_DIRECTORY)) {
				if (recursive) {
					if (!remove_directory_recursive(target)) {
						perror("rm");
					}
				} else {
					if (!RemoveDirectoryA(target)) {
						perror("rm");
					}
				}
			} else {
				if (!DeleteFileA(target)) {
					perror("rm");
				}
			}
			continue;
		}

		// cp
		if (strncmp(line, "cp ", 3) == 0) {
			char *rest = line + 3;
			while (*rest == ' ') rest++;
			int recursive = 0;
			if (*rest == '-' && (*(rest+1) == 'r' || *(rest+1) == 'R')) {
				recursive = 1;
				rest += 2;
				while (*rest == ' ') rest++;
			}
			char *src = rest;
			char *dst = strchr(rest, ' ');
			if (!dst) { fprintf(stderr, "Usage: cp [-r] [source] [destination]\n"); continue; }
			*dst = '\0'; dst++;
			while (*dst == ' ') dst++;
			DWORD attr = GetFileAttributesA(src);
			DWORD dst_attr = GetFileAttributesA(dst);
			char final_dst[MAX_PATH];
			if (dst_attr != INVALID_FILE_ATTRIBUTES && (dst_attr & FILE_ATTRIBUTE_DIRECTORY)) {
				// Destination is a directory, append filename
				const char *filename = strrchr(src, '\\');
				if (!filename) filename = src; else filename++;
				snprintf(final_dst, sizeof(final_dst), "%s\\%s", dst, filename);
			} else {
				strncpy(final_dst, dst, sizeof(final_dst));
				final_dst[sizeof(final_dst)-1] = '\0';
			}
			if ((attr != INVALID_FILE_ATTRIBUTES) && (attr & FILE_ATTRIBUTE_DIRECTORY)) {
				if (!recursive) {
					fprintf(stderr, "cp: omitting directory '%s' (use -r to copy directories)\n", src);
					continue;
				}
				if (!copy_dir(src, final_dst)) {
					perror("cp");
				}
			} else {
				if (CopyFileA(src, final_dst, FALSE)) {
					// success
				} else {
					perror("cp");
				}
			}
			continue;
		}

		// chmod (forward to builtin implementation)
		if (strncmp(line, "chmod", 5) == 0 && (line[5] == ' ' || line[5] == '\t')) {
			// reuse the builtin implementation which accepts FILE* streams
			run_builtin_command(line, stdin, stdout);
			continue;
		}

		// useradd (forward to builtin implementation on Windows)
		if (strncmp(line, "useradd", 7) == 0 && (line[7] == ' ' || line[7] == '\t' || line[7] == '\0')) {
			run_builtin_command(line, stdin, stdout);
			continue;
		}

		// wc (forward to builtin implementation)
		if (strncmp(line, "wc", 2) == 0 && (line[2] == ' ' || line[2] == '\t' || line[2] == '\0')) {
			run_builtin_command(line, stdin, stdout);
			continue;
		}

		// jobs, bg, fg (forward to builtin implementation)
		if (strncmp(line, "jobs", 4) == 0 && (line[4] == '\0' || line[4] == ' ' || line[4] == '\t')) {
			run_builtin_command(line, stdin, stdout);
			continue;
		}
		if (strncmp(line, "bg", 2) == 0 && (line[2] == ' ' || line[2] == '\t')) {
			run_builtin_command(line, stdin, stdout);
			continue;
		}
		if (strncmp(line, "fg", 2) == 0 && (line[2] == ' ' || line[2] == '\t')) {
			run_builtin_command(line, stdin, stdout);
			continue;
		}

		// ls
		if (strncmp(line, "ls", 2) == 0) {
			fprintf(stderr, "[msh-debug] entering ls handler\n");
			// Custom implementation: ls [-l] [-a] [-h] [-t] [-S] [-i] [dir]
			char *args = line + 2;
			while (*args == ' ') args++;
			int show_all = 0, long_list = 0, human = 0, sort_time = 0, sort_size = 0, show_index = 0;
			char *dir = NULL;
			if (*args == '-') {
				args++;
				while (*args && *args != ' ') {
					if (*args == 'a') show_all = 1;
					if (*args == 'l') long_list = 1;
					if (*args == 'h') human = 1;
					if (*args == 't') sort_time = 1;
					if (*args == 'S') sort_size = 1;
					if (*args == 'i') show_index = 1;
					args++;
				}
				while (*args == ' ') args++;
			}
			if (*args) dir = args;
			char path[MAX_PATH];
			if (dir && strlen(dir) > 0) {
				strncpy(path, dir, MAX_PATH-1);
				path[MAX_PATH-1] = '\0';
			} else {
				GetCurrentDirectoryA(MAX_PATH, path);
			}
			// Collect files
			WIN32_FIND_DATAA findData;
			char search_path[MAX_PATH];
			snprintf(search_path, sizeof(search_path), "%s\\*", path);
			HANDLE hFind = FindFirstFileA(search_path, &findData);
			if (hFind == INVALID_HANDLE_VALUE) {
				perror("ls");
				continue;
			}
			typedef struct {
				char name[MAX_PATH];
				ULONGLONG size;
				FILETIME mtime;
				DWORD attr;
				ULONGLONG fileid;
			} FileEntry;
			FileEntry files[2048];
			int count = 0;
			do {
				if (!show_all && (findData.cFileName[0] == '.')) continue;
				strncpy(files[count].name, findData.cFileName, MAX_PATH-1);
				files[count].name[MAX_PATH-1] = '\0';
				files[count].size = ((ULONGLONG)findData.nFileSizeHigh << 32) | findData.nFileSizeLow;
				files[count].mtime = findData.ftLastWriteTime;
				files[count].attr = findData.dwFileAttributes;
				// Get file index (similar to inode)
				char fullpath[MAX_PATH*2];
				snprintf(fullpath, sizeof(fullpath), "%s\\%s", path, findData.cFileName);
				HANDLE fh = CreateFileA(fullpath, 0, FILE_SHARE_READ, NULL, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, NULL);
				if (fh != INVALID_HANDLE_VALUE) {
					BY_HANDLE_FILE_INFORMATION info;
					if (GetFileInformationByHandle(fh, &info)) {
						files[count].fileid = ((ULONGLONG)info.nFileIndexHigh << 32) | info.nFileIndexLow;
					} else {
						files[count].fileid = 0;
					}
					CloseHandle(fh);
				} else {
					files[count].fileid = 0;
				}
				count++;
			} while (FindNextFileA(hFind, &findData) && count < 2048);
			FindClose(hFind);
			// Sorting
			if (sort_time) {
				for (int i = 0; i < count-1; i++) {
					for (int j = i+1; j < count; j++) {
						if (CompareFileTime(&files[i].mtime, &files[j].mtime) < 0) {
							FileEntry tmp = files[i]; files[i] = files[j]; files[j] = tmp;
						}
					}
				}
			} else if (sort_size) {
				for (int i = 0; i < count-1; i++) {
					for (int j = i+1; j < count; j++) {
						if (files[i].size < files[j].size) {
							FileEntry tmp = files[i]; files[i] = files[j]; files[j] = tmp;
						}
					}
				}
			}
			// Print
			for (int i = 0; i < count; i++) {
				if (long_list) {
					// Attributes
					printf("%c%c%c%c%c ",
						(files[i].attr & FILE_ATTRIBUTE_DIRECTORY) ? 'd' : '-',
						(files[i].attr & FILE_ATTRIBUTE_READONLY) ? 'r' : '-',
						(files[i].attr & FILE_ATTRIBUTE_ARCHIVE) ? 'a' : '-',
						(files[i].attr & FILE_ATTRIBUTE_HIDDEN) ? 'h' : '-',
						(files[i].attr & FILE_ATTRIBUTE_SYSTEM) ? 's' : '-');
				}
				if (show_index) {
					printf("%10llu ", files[i].fileid);
				}
				if (long_list) {
					// Size
					if (human) {
						double sz = (double)files[i].size;
						const char *unit = "B";
						if (sz > 1024) { sz /= 1024; unit = "K"; }
						if (sz > 1024) { sz /= 1024; unit = "M"; }
						if (sz > 1024) { sz /= 1024; unit = "G"; }
						printf("%8.1f%s ", sz, unit);
					} else {
						printf("%10llu ", files[i].size);
					}
					// Date
					SYSTEMTIME st;
					FileTimeToSystemTime(&files[i].mtime, &st);
					printf("%04d-%02d-%02d %02d:%02d ", st.wYear, st.wMonth, st.wDay, st.wHour, st.wMinute);
				}
				printf("%s\n", files[i].name);
			}
			continue;
		}

		// find - basic implementation
		if (strncmp(line, "find", 4) == 0 && (line[4] == ' ' || line[4] == '\t' || line[4] == '\0')) {
			// supports: find [path] -name pattern
			char *rest = line + 4;
			while (*rest == ' ' || *rest == '\t') rest++;
			char pathbuf[MAX_PATH] = {0};
			char namepat[512] = {0};
			// default path is current directory
			if (!*rest) {
				GetCurrentDirectoryA(MAX_PATH, pathbuf);
				strcat(pathbuf, "\\."); // ensure search wildcard later
			} else {
				// parse optional path
				if (*rest && *rest != '-') {
					int i = 0;
					if (*rest == '"' || *rest == '\'') {
						char q = *rest++;
						while (*rest && *rest != q && i < MAX_PATH-2) pathbuf[i++] = *rest++;
						if (*rest == q) rest++;
					} else {
						while (*rest && *rest != ' ' && *rest != '\t' && i < MAX_PATH-2) pathbuf[i++] = *rest++;
					}
					pathbuf[i] = '\0';
					// If user provided a directory, keep it; we'll append wildcard later
				}
				// skip spaces
				while (*rest == ' ' || *rest == '\t') rest++;
			}
			// parse -name option
			if (strncmp(rest, "-name", 5) == 0) {
				rest += 5;
				while (*rest == ' ' || *rest == '\t') rest++;
				int j = 0;
				if (*rest == '\"' || *rest == '\'') {
					char q = *rest++;
					while (*rest && *rest != q && j < (int)sizeof(namepat)-1) namepat[j++] = *rest++;
					if (*rest == q) rest++;
				} else {
					while (*rest && *rest != ' ' && *rest != '\t' && j < (int)sizeof(namepat)-1) namepat[j++] = *rest++;
				}
				namepat[j] = '\0';
			}
			// If no explicit path, set to current dir
			char startpath[MAX_PATH];
			if (!pathbuf[0]) {
				GetCurrentDirectoryA(MAX_PATH, startpath);
			} else {
				// if path ends with slash or backslash, remove trailing
				strncpy(startpath, pathbuf, MAX_PATH-1);
				startpath[MAX_PATH-1] = '\0';
				int lp = strlen(startpath);
				if (lp > 0 && (startpath[lp-1] == '/' || startpath[lp-1] == '\\')) startpath[lp-1] = '\0';
			}

			// Use PathMatchSpecA for wildcard matching if available
			int use_pathmatch = 1;
			#ifdef _WIN32
			// PathMatchSpecA is in shlwapi; try to call it dynamically to avoid link issues
			HMODULE hShlw = LoadLibraryA("shlwapi.dll");
			int (__stdcall *pPathMatchSpecA)(LPCSTR, LPCSTR) = NULL;
			if (hShlw) pPathMatchSpecA = (void*)GetProcAddress(hShlw, "PathMatchSpecA");
			if (!pPathMatchSpecA) use_pathmatch = 0;
			#else
			use_pathmatch = 0;
			#endif

			// simple recursive traversal using a dynamic stack to avoid large stack frames
			typedef struct { char *full; } StackItem;
			StackItem *stack = NULL;
			int sp = 0, stackCap = 0;
			// push startpath
			char *startcopy = _strdup(startpath);
			if (!startcopy) startcopy = strdup(startpath);
			if (!startcopy) { fprintf(stderr, "find: memory allocation failed\n"); continue; }
			// push onto stack
			stackCap = 16;
			stack = (StackItem*)malloc(sizeof(StackItem) * stackCap);
			if (!stack) { free(startcopy); fprintf(stderr, "find: memory allocation failed\n"); continue; }
			stack[sp++].full = startcopy;
			char searchpath[MAX_PATH*2];
			while (sp > 0) {
				StackItem it = stack[--sp];
				// enumerate entries
				snprintf(searchpath, sizeof(searchpath), "%s\\*", it.full);
				WIN32_FIND_DATAA fd;
				HANDLE h = FindFirstFileA(searchpath, &fd);
				if (h == INVALID_HANDLE_VALUE) continue;
				do {
					if (strcmp(fd.cFileName, ".") == 0 || strcmp(fd.cFileName, "..") == 0) continue;
					char itempath[MAX_PATH*2];
					snprintf(itempath, sizeof(itempath), "%s\\%s", it.full, fd.cFileName);
					int match = 1;
					if (namepat[0]) {
						if (use_pathmatch && pPathMatchSpecA) {
							match = pPathMatchSpecA(fd.cFileName, namepat);
						} else {
							// use a small recursive matcher for '*' and '?'
							match = wildcard_match(namepat, fd.cFileName);
						}
					}
					if (match) printf("%s\n", itempath);
					if (fd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) {
						// push directory
						if (sp + 1 > stackCap) {
							int newCap = stackCap * 2;
							StackItem *ns = (StackItem*)realloc(stack, sizeof(StackItem) * newCap);
							if (!ns) {
								// allocation failed, skip pushing further directories
							} else {
								stack = ns; stackCap = newCap;
							}
						}
						if (sp < stackCap) {
							stack[sp++].full = _strdup(itempath);
						}
					}
				} while (FindNextFileA(h, &fd));
				FindClose(h);
				free(it.full);
			}
			if (hShlw) FreeLibrary(hShlw);
			// free remaining stack storage
			for (int i = 0; i < sp; i++) if (stack[i].full) free(stack[i].full);
			free(stack);
			continue;
		}

		// pwd
		if (strcmp(line, "pwd") == 0) {
			char cwd[1024];
			if (GetCurrentDirectoryA(sizeof(cwd), cwd))
				printf("%s\n", cwd);
			else
				perror("pwd");
			continue;
		}

		// grep (flexible: supports 'grep pattern', 'grep pattern file', and pipe input)
		if (strncmp(line, "grep", 4) == 0 && (line[4] == ' ' || line[4] == '\t')) {
			char *rest = line + 4;
			while (*rest == ' ' || *rest == '\t') rest++;
			if (*rest == '\0') { fprintf(stderr, "Usage: grep [pattern] [file]\n"); continue; }
			// Extract pattern
			char pattern[256] = {0};
			int i = 0;
			if (*rest == '"' || *rest == '\'') {
				char quote = *rest++;
				while (*rest && *rest != quote && i < 255) pattern[i++] = *rest++;
				if (*rest == quote) rest++;
			} else {
				while (*rest && *rest != ' ' && *rest != '\t' && i < 255) pattern[i++] = *rest++;
			}
			pattern[i] = '\0';
			while (*rest == ' ' || *rest == '\t') rest++;
			FILE *fp = NULL;
			if (*rest) {
				// Extract filename
				char filename[512] = {0};
				int j = 0;
				if (*rest == '"' || *rest == '\'') {
					char quote = *rest++;
					while (*rest && *rest != quote && j < 511) filename[j++] = *rest++;
					if (*rest == quote) rest++;
				} else {
					while (*rest && *rest != ' ' && *rest != '\t' && j < 511) filename[j++] = *rest++;
				}
				filename[j] = '\0';
				if (filename[0]) fp = fopen(filename, "r");
			}
			if (!pattern[0]) { fprintf(stderr, "grep: Invalid pattern\n"); continue; }
			char buf[512];
			if (fp) {
				while (fgets(buf, sizeof(buf), fp)) {
					// Remove trailing newline
					size_t len = strlen(buf);
					if (len > 0 && buf[len-1] == '\n') buf[len-1] = '\0';
					if (strstr(buf, pattern)) printf("%s\n", buf);
				}
				fclose(fp);
			}
			continue;
		}

		// echo
		if (strncmp(line, "echo ", 5) == 0) {
			char *text = line + 5;
			while (*text == ' ') text++;

			// Look for > or >>
			char *redir = strstr(text, ">>");
			int append = 0;
			if (!redir) {
				redir = strchr(text, '>');
			} else {
				append = 1;
			}
			if (redir) {
				// Separate text and filename
				char *file = redir + (append ? 2 : 1);
				while (*file == ' ') file++;
				// Remove > or >> and trailing spaces from text
				int len = (int)(redir - text);
				while (len > 0 && (text[len-1] == ' ')) len--;
				char saved = text[len];
				text[len] = '\0';
				FILE *fp = fopen(file, append ? "a" : "w");
				if (!fp) {
					perror("echo");
				} else {
					fprintf(fp, "%s\n", text);
					fclose(fp);
				}
				text[len] = saved;
			} else {
				printf("%s\n", text);
			}
			continue;
		}

		// cat (supports piping: 'cat file', 'cat' for stdin)
		if (strncmp(line, "cat", 3) == 0 && (line[3] == ' ' || line[3] == '\t' || line[3] == '\0')) {
			char *rest = line + 3;
			while (*rest == ' ' || *rest == '\t') rest++;
			char file[512] = {0};
			int i = 0;
			if (*rest == '"' || *rest == '\'') {
				char quote = *rest++;
				while (*rest && *rest != quote && i < 511) file[i++] = *rest++;
				if (*rest == quote) rest++;
			} else {
				while (*rest && *rest != ' ' && *rest != '\t' && i < 511) file[i++] = *rest++;
			}
			file[i] = '\0';
			if (!file[0]) {
				fprintf(stderr, "cat: Invalid argument\n");
			} else {
				FILE *fp = fopen(file, "r");
				if (!fp) {
					perror("cat");
				} else {
					char buf[512];
					while (fgets(buf, sizeof(buf), fp)) {
						printf("%s", buf);
					}
					fclose(fp);
				}
			}
			continue;
		}

		// head
		if (strncmp(line, "head ", 5) == 0) {
			char *file = line + 5;
			while (*file == ' ') file++;
			strip_quotes(file);
			FILE *fp = fopen(file, "r");
			if (!fp) {
				perror("head");
			} else {
				char buf[512];
				int count = 0;
				while (fgets(buf, sizeof(buf), fp) && count < 10) {
					printf("%s", buf);
					count++;
				}
				fclose(fp);
			}
			continue;
		}

		// tail
		if (strncmp(line, "tail ", 5) == 0) {
			char *file = line + 5;
			while (*file == ' ') file++;
			strip_quotes(file);
			FILE *fp = fopen(file, "r");
			if (!fp) {
				perror("tail");
			} else {
				char *lines[10];
				int count = 0;
				char buf[512];
				while (fgets(buf, sizeof(buf), fp)) {
					if (count < 10) {
						lines[count] = _strdup(buf);
						count++;
					} else {
						// shift lines left, free oldest
						free(lines[0]);
						for (int j = 1; j < 10; j++) lines[j-1] = lines[j];
						lines[9] = _strdup(buf);
					}
				}
				for (int i = 0; i < count; i++) {
					printf("%s", lines[i]);
					free(lines[i]);
				}
				fclose(fp);
			}
			continue;
		}

		// mkdir
		if (strncmp(line, "mkdir ", 6) == 0) {
			char *dir = line + 6;
			while (*dir == ' ') dir++;
			if (CreateDirectoryA(dir, NULL)) {
				// success
			} else {
				perror("mkdir");
			}
			continue;
		}

		// rmdir (recursive)
		if (strncmp(line, "rmdir ", 6) == 0) {
			char *dir = line + 6;
			while (*dir == ' ') dir++;
			char absdir[MAX_PATH];
			if (!_fullpath(absdir, dir, MAX_PATH)) {
				fprintf(stderr, "rmdir: Invalid path: %s\n", dir);
				continue;
			}
			DWORD attr = GetFileAttributesA(absdir);
			if (attr == INVALID_FILE_ATTRIBUTES || !(attr & FILE_ATTRIBUTE_DIRECTORY)) {
				fprintf(stderr, "rmdir: No such directory: %s\n", absdir);
				continue;
			}
			if (remove_directory_recursive(absdir)) {
				// success
			} else {
				attr = GetFileAttributesA(absdir);
				if (attr != INVALID_FILE_ATTRIBUTES) perror("rmdir");
			}
			continue;
		}

		// clear
		if (strcmp(line, "clear") == 0) {
			system("cls");
			continue;
		}

		// ln (hard link)
		if (strncmp(line, "ln ", 3) == 0) {
			char *rest = line + 3;
			while (*rest == ' ') rest++;
			char *src = rest;
			char *linkname = strchr(rest, ' ');
			if (!linkname) {
				fprintf(stderr, "Usage: ln [source] [linkname]\n");
				continue;
			}
			*linkname = '\0'; linkname++;
			while (*linkname == ' ') linkname++;
			if (CreateHardLinkA(linkname, src, NULL)) {
				// success
			} else {
				DWORD err = GetLastError();
				fprintf(stderr, "ln: Failed to create hard link: %s -> %s (Windows error code: %lu)\n", src, linkname, err);
			}
			continue;
		}

		// medit (Marshall edit) launch
		if (strncmp(line, "medit ", 6) == 0) {
			char *file = line + 6;
			while (*file == ' ') file++;
			if (!*file) {
				fprintf(stderr, "medit: No filename provided.\n");
				continue;
			}
			// Launch Medit.exe from the msh directory
			system("cls");
			char msh_path[MAX_PATH];
			GetModuleFileNameA(NULL, msh_path, MAX_PATH);
			char *last_sep = strrchr(msh_path, '\\');
			if (last_sep) *(last_sep+1) = '\0';
			char medit_path[MAX_PATH+64];
			snprintf(medit_path, sizeof(medit_path), "%sMedit.exe", msh_path);
			printf("[DEBUG] Medit.exe path: %s\n", medit_path);
			FILE *medit_fp = fopen(medit_path, "r");
			if (!medit_fp) {
				fprintf(stderr, "medit: Medit.exe not found at %s\n", medit_path);
				continue;
			}
			fclose(medit_fp);
			printf("[DEBUG] Attempting ShellExecuteA: file='%s', medit_path='%s'\n", file, medit_path);
			HINSTANCE result = ShellExecuteA(NULL, "open", medit_path, file, NULL, SW_SHOWNORMAL);
			if ((INT_PTR)result <= 32) {
				fprintf(stderr, "medit: ShellExecuteA failed to launch Medit.exe (error code %ld).\n", (long)(INT_PTR)result);
			}
			system("cls");
			continue;
		}

		// whoami
		if (strcmp(line, "whoami") == 0) {
			char username[256];
			DWORD size = sizeof(username);
			if (GetUserNameA(username, &size)) {
				printf("%s\n", username);
			} else {
				perror("whoami");
			}
			continue;
		}

		// zip <zip file> <file>
		if (strncmp(line, "zip ", 4) == 0) {
			char *rest = line + 4;
			while (*rest == ' ') rest++;
			char *zipfile = rest;
			char *file = strchr(rest, ' ');
			if (!file) {
				fprintf(stderr, "Usage: zip <zip file> <file>\n");
				continue;
			}
			*file = '\0'; file++;
			while (*file == ' ') file++;
			char cmd[1024];
			snprintf(cmd, sizeof(cmd), "powershell -Command \"Compress-Archive -Path '%s' -DestinationPath '%s' -Force\"", file, zipfile);
			int result = system(cmd);
			if (result != 0) {
				fprintf(stderr, "zip: Failed to create archive.\n");
			}
			continue;
		}
		// unzip <zip file>
		if (strncmp(line, "unzip ", 6) == 0) {
			char *zipfile = line + 6;
			while (*zipfile == ' ') zipfile++;
			if (strlen(zipfile) == 0) {
				fprintf(stderr, "Usage: unzip <zip file>\n");
				continue;
			}
			char cmd[1024];
			snprintf(cmd, sizeof(cmd), "powershell -Command \"Expand-Archive -Path '%s' -DestinationPath '.' -Force\"", zipfile);
			int result = system(cmd);
			if (result != 0) {
				fprintf(stderr, "unzip: Failed to extract archive.\n");
			}
			continue;
		}
		// ifconfig
		if (strcmp(line, "ifconfig") == 0) {
			char cmd[1024];
			snprintf(cmd, sizeof(cmd),
				"powershell -Command \"Get-NetIPConfiguration | ForEach-Object { $mac = $_.InterfaceDescription; $adapter = Get-NetAdapter | Where-Object { $_.InterfaceDescription -eq $mac }; $stats = $null; try { $stats = $adapter | Get-NetAdapterStatistics 2>$null } catch {} $ipv4 = $_.IPv4Address.IPAddress; $ipv6 = $_.IPv6Address.IPAddress; $ether = $adapter.MacAddress; Write-Host $_.InterfaceAlias; Write-Host '  inet  : ' $ipv4; Write-Host '  inet6 : ' $ipv6; Write-Host '  ether : ' $ether; if ($stats) { Write-Host '  RX    : ' $($stats.ReceivedBytes); Write-Host '  TX    : ' $($stats.SentBytes); } else { Write-Host '  RX    : N/A'; Write-Host '  TX    : N/A'; } Write-Host '' }\""
			);
			int result = system(cmd);
			if (result != 0) {
				fprintf(stderr, "ifconfig: Failed to retrieve network configuration.\n");
			}
			continue;
		}
		// ping <host>
		if (strncmp(line, "ping ", 5) == 0) {
			// Support background flag: use line_noamp when background requested
			const char *use_line = bg_requested ? line_noamp : line;
			char *host = (char*)(use_line + 5);
			while (*host == ' ') host++;
			if (strlen(host) == 0) {
				fprintf(stderr, "Usage: ping <host>\n");
				continue;
			}
			char cmd[1024];
			snprintf(cmd, sizeof(cmd), "ping %s", host);
			if (!bg_requested) {
				int result = system(cmd);
				if (result != 0) {
					fprintf(stderr, "ping: Failed to reach host.\n");
				}
			} else {
				// launch in background and add to job table
				STARTUPINFOA si; PROCESS_INFORMATION pi; memset(&si, 0, sizeof(si)); si.cb = sizeof(si);
				char cmdcopy[1200]; snprintf(cmdcopy, sizeof(cmdcopy), "cmd.exe /C %s", cmd);
				BOOL ok = CreateProcessA(NULL, cmdcopy, NULL, NULL, FALSE, CREATE_NEW_PROCESS_GROUP, NULL, NULL, &si, &pi);
				if (!ok) {
					fprintf(stderr, "Failed to launch background ping.\n");
				} else {
					int jid = msh_add_job(pi.hProcess, pi.hThread, pi.dwProcessId, cmd);
					if (jid < 0) { fprintf(stderr, "Job list full.\n"); CloseHandle(pi.hProcess); CloseHandle(pi.hThread); }
					else printf("[%d] %lu\n", jid, (unsigned long)pi.dwProcessId);
				}
			}
			continue;
		}
		// traceroute <host>
		if (strncmp(line, "traceroute ", 11) == 0) {
			char *host = line + 11;
			while (*host == ' ') host++;
			if (strlen(host) == 0) {
				fprintf(stderr, "Usage: traceroute <host>\n");
				continue;
			}
			char cmd[1024];
			snprintf(cmd, sizeof(cmd), "tracert %s", host);
			int result = system(cmd);
			if (result != 0) {
				fprintf(stderr, "traceroute: Failed to trace route to host.\n");
			}
			continue;
		}

		// wget <url> [-O filename]
		// NOTE: This implements a lightweight builtin wrapper for downloading a URL.
		// It will first attempt to run the external `wget` program. If that fails it
		// falls back to `curl -L` with the appropriate options. On Windows, as a
		// last resort it will call PowerShell's Invoke-WebRequest. This keeps the
		// shell usable on systems without wget installed.
		// The implementation is intentionally simple and does not implement all
		// wget flags (only supports -O to specify output filename).
		if (strncmp(line, "wget", 4) == 0 && (line[4] == ' ' || line[4] == '\t')) {
			char *rest = line + 4;
			while (*rest == ' ' || *rest == '\t') rest++;
			if (*rest == '\0') { fprintf(stderr, "Usage: wget <url> [-O filename]\n"); continue; }
			// Tokenize arguments (respect quotes)
			char *tokens[32];
			int tcount = 0;
			char tokbuf[2048];
			char *p = rest;
			while (*p && tcount < (int)(sizeof(tokens)/sizeof(tokens[0]))) {
				while (*p == ' ' || *p == '\t') p++;
				if (!*p) break;
				char *start = p;
				int tb = 0;
				if (*p == '"' || *p == '\'') {
					char q = *p++;
					while (*p && *p != q && tb < (int)sizeof(tokbuf)-1) tokbuf[tb++] = *p++;
					if (*p == q) p++;
				} else {
					while (*p && *p != ' ' && *p != '\t' && tb < (int)sizeof(tokbuf)-1) tokbuf[tb++] = *p++;
				}
				tokbuf[tb] = '\0';
				// allocate a small copy for token
				tokens[tcount] = _strdup(tokbuf);
				tcount++;
			}
			if (tcount == 0) { fprintf(stderr, "Usage: wget <url> [-O filename]\n"); continue; }
			char url[1024] = {0};
			char outfile[1024] = {0};
			int save_to_downloads = 0;
			// Helper to detect URL-like token
			#define IS_URL(s) ((s) && (strncmp((s), "http://", 7) == 0 || strncmp((s), "https://", 8) == 0))
			for (int ti = 0; ti < tcount; ti++) {
				char *tk = tokens[ti];
				if (strcmp(tk, "--to-downloads") == 0 || strcmp(tk, "--downloads") == 0) {
					save_to_downloads = 1;
					continue;
				}
				if (strcmp(tk, "-O") == 0 || strcmp(tk, "-o") == 0) {
					// next token should be filename; but if next looks like URL, assume user
					// mistakenly wrote `-o <url>` and treat that token as the URL (warn)
					if (ti+1 < tcount) {
						if (IS_URL(tokens[ti+1])) {
							if (!url[0]) {
								strncpy(url, tokens[ti+1], sizeof(url)-1);
								url[sizeof(url)-1] = '\0';
								// warn but continue
								fprintf(stderr, "wget: warning: treating '%s' as URL (missing -O filename?)\n", tokens[ti+1]);
							}
							ti++;
						} else {
							strncpy(outfile, tokens[ti+1], sizeof(outfile)-1);
							outfile[sizeof(outfile)-1] = '\0';
							ti++;
						}
					} else {
						fprintf(stderr, "wget: option '%s' requires an argument\n", tk);
					}
				} else if ((tk[0] == '-' && (tk[1] == 'O' || tk[1] == 'o')) && tk[2] != '\0') {
					// joined form -Ofilename
					strncpy(outfile, tk+2, sizeof(outfile)-1);
					outfile[sizeof(outfile)-1] = '\0';
				} else {
					// treat first non-option as URL; subsequent non-options become outfile if not set
					if (!url[0] && IS_URL(tk)) {
						strncpy(url, tk, sizeof(url)-1);
						url[sizeof(url)-1] = '\0';
					} else if (!url[0]) {
						// if it doesn't look like a URL but url not set, still accept it
						strncpy(url, tk, sizeof(url)-1);
						url[sizeof(url)-1] = '\0';
					} else if (!outfile[0]) {
						strncpy(outfile, tk, sizeof(outfile)-1);
						outfile[sizeof(outfile)-1] = '\0';
					}
				}
			}
			// free tokens
			for (int ti = 0; ti < tcount; ti++) free(tokens[ti]);
			#undef IS_URL
			// If requested, and no explicit outfile, set outfile to Downloads/<basename>
			if (save_to_downloads && !outfile[0]) {
				const char *last_slash = strrchr(url, '/');
				char bnbuf[256] = {0};
				const char *bn = NULL;
				if (!last_slash) bn = url;
				else {
					bn = last_slash + 1;
					if (*bn == '\0') { strncpy(bnbuf, "index.html", sizeof(bnbuf)-1); bnbuf[sizeof(bnbuf)-1] = '\0'; bn = bnbuf; }
				}
				char downloads_path[1024] = {0};
#ifdef _WIN32
				const char *up = getenv("USERPROFILE");
				if (up && *up) snprintf(downloads_path, sizeof(downloads_path), "%s\\Downloads\\%s", up, bn);
				else snprintf(downloads_path, sizeof(downloads_path), "%s", bn);
				// Ensure Downloads directory exists
				char dirpath[1024]; strncpy(dirpath, downloads_path, sizeof(dirpath)-1); dirpath[sizeof(dirpath)-1] = '\0';
				char *sep = strrchr(dirpath, '\\');
				if (sep) { *sep = '\0'; CreateDirectoryA(dirpath, NULL); }
#else
				const char *home = getenv("HOME");
				if (home && *home) snprintf(downloads_path, sizeof(downloads_path), "%s/Downloads/%s", home, bn);
				else snprintf(downloads_path, sizeof(downloads_path), "%s", bn);
				// Ensure Downloads directory exists
				char dirpath[1024]; strncpy(dirpath, downloads_path, sizeof(dirpath)-1); dirpath[sizeof(dirpath)-1] = '\0';
				char *sep = strrchr(dirpath, '/');
				if (sep) { *sep = '\0'; char mkcmd[1200]; snprintf(mkcmd, sizeof(mkcmd), "mkdir -p \"%s\"", dirpath); system(mkcmd); }
#endif
				strncpy(outfile, downloads_path, sizeof(outfile)-1);
				outfile[sizeof(outfile)-1] = '\0';
			}
			// Build command: prefer wget, then curl -O, then PowerShell Invoke-WebRequest on Windows
			char cmd[2048];
			int rc = -1;
			// Try wget first. If outfile is empty, call wget without -O so it
			// behaves like Linux wget and saves to the current directory.
			if (outfile[0]) {
				snprintf(cmd, sizeof(cmd), "wget -O \"%s\" \"%s\"", outfile, url);
			} else {
				snprintf(cmd, sizeof(cmd), "wget \"%s\"", url);
			}
			if (strlen(cmd) + strlen(url) + 2 < sizeof(cmd)) {
				strncat(cmd, " ", sizeof(cmd)-strlen(cmd)-1);
				strncat(cmd, "\"", sizeof(cmd)-strlen(cmd)-1);
				strncat(cmd, url, sizeof(cmd)-strlen(cmd)-1);
				strncat(cmd, "\"", sizeof(cmd)-strlen(cmd)-1);
			}
			rc = system(cmd);
			if (rc != 0) {
				// Fallback to curl: use -o when outfile provided, otherwise -O to
				// mimic wget's default (save to current directory with remote name).
				if (outfile[0]) snprintf(cmd, sizeof(cmd), "curl -L -o \"%s\" \"%s\"", outfile, url);
				else snprintf(cmd, sizeof(cmd), "curl -L -O \"%s\"", url);
				rc = system(cmd);
			}
			if (rc != 0) {
				// On Windows, try PowerShell's Invoke-WebRequest as last resort
				#ifdef _WIN32
				if (outfile[0]) {
					snprintf(cmd, sizeof(cmd), "powershell -Command \"Invoke-WebRequest -Uri '%s' -OutFile '%s' -UseBasicParsing\"", url, outfile);
				} else {
					snprintf(cmd, sizeof(cmd), "powershell -Command \"Invoke-WebRequest -Uri '%s' -OutFile (Split-Path -Leaf '%s') -UseBasicParsing\"", url, url);
				}
				rc = system(cmd);
				#endif
			}
			if (rc != 0) fprintf(stderr, "wget: download failed for %s\n", url);
			continue;
		}
		// netstat
		if (strcmp(line, "netstat") == 0) {
			int result = system("netstat -ano");
			if (result != 0) {
				fprintf(stderr, "netstat: Failed to display network statistics.\n");
			}
			continue;
		}
		// ss
		if (strcmp(line, "ss") == 0) {
			int result = system("netstat -ano");
			if (result != 0) {
				fprintf(stderr, "ss: Failed to display socket statistics.\n");
			}
			continue;
		}
		// uname
		if (strcmp(line, "uname") == 0) {
			OSVERSIONINFOEXA osvi;
			memset(&osvi, 0, sizeof(osvi));
			osvi.dwOSVersionInfoSize = sizeof(osvi);
			if (GetVersionExA((OSVERSIONINFOA*)&osvi)) {
				printf("Windows %lu.%lu Build %lu %s\n", osvi.dwMajorVersion, osvi.dwMinorVersion, osvi.dwBuildNumber, osvi.szCSDVersion);
			} else {
				printf("Windows\n");
			}
			continue;
		}
		// cal
		if (strcmp(line, "cal") == 0) {
			char cmd[1024];
			snprintf(cmd, sizeof(cmd), "powershell -Command \"Get-Culture | ForEach-Object { $now=Get-Date; $first=Get-Date -Day 1 -Month $now.Month -Year $now.Year; $last=Get-Date -Day 1 -Month ($now.Month+1) -Year $now.Year; $last=$last.AddDays(-1); $days=@('Su','Mo','Tu','We','Th','Fr','Sa'); Write-Host ('     ' + $now.ToString('MMMM yyyy')); Write-Host ($days -join ' '); $pad='   '*$first.DayOfWeek.value__; $out=$pad; for ($d=1; $d -le $last.Day; $d++) { $out+=[string]::Format('{0,2} ', $d); if ((($d+$first.DayOfWeek.value__) % 7) -eq 0) { Write-Host $out.TrimEnd(); $out=''; } } if ($out.Trim() -ne '') { Write-Host $out.TrimEnd(); } }\"");
			int result = system(cmd);
			if (result != 0) {
				fprintf(stderr, "cal: Failed to display calendar.\n");
			}
			continue;
		}
		// free
		if (strcmp(line, "free") == 0) {
			char cmd[1024];
			snprintf(cmd, sizeof(cmd), "powershell -Command \"Get-CimInstance Win32_OperatingSystem | ForEach-Object { $total=$_.TotalVisibleMemorySize; $free=$_.FreePhysicalMemory; $used=$total-$free; Write-Host ('             total        used        free'); Write-Host (('Mem:'.PadRight(12)) + ('{0,10:N0}' -f $total) + ('{0,12:N0}' -f $used) + ('{0,12:N0}' -f $free)); }\"");
			int result = system(cmd);
			if (result != 0) {
				fprintf(stderr, "free: Failed to display memory usage.\n");
			}
			continue;
		}
		// df
		if (strcmp(line, "df") == 0) {
			char cmd[1024];
			snprintf(cmd, sizeof(cmd), "powershell -Command \"Get-CimInstance Win32_LogicalDisk | Where-Object { $_.DriveType -eq 3 } | ForEach-Object { $used=$_.Size-$_.FreeSpace; Write-Host ('Filesystem'.PadRight(12) + '   1K-blocks      Used      Available  Mounted on'); Write-Host ($_.DeviceID.PadRight(12) + ('{0,12:N0}' -f ($_.Size/1KB)) + ('{0,12:N0}' -f ($used/1KB)) + ('{0,12:N0}' -f ($_.FreeSpace/1KB)) + '  ' + $_.ProviderName); }\"");
			int result = system(cmd);
			if (result != 0) {
				fprintf(stderr, "df: Failed to display disk usage.\n");
			}
			continue;
		}
		// ps with flags
		if (strncmp(line, "ps", 2) == 0) {
			char *args = line + 2;
			while (*args == ' ') args++;
			int flag_e = 0, flag_l = 0, flag_f = 0, flag_a = 0, flag_u = 0, flag_x = 0;
			if (*args == '-') {
				args++;
				while (*args && *args != ' ') {
					if (*args == 'e') flag_e = 1;
					if (*args == 'l') flag_l = 1;
					if (*args == 'f') flag_f = 1;
					if (*args == 'a') flag_a = 1;
					if (*args == 'u') flag_u = 1;
					if (*args == 'x') flag_x = 1;
					args++;
				}
			}
			// Simulate Linux ps flag behavior
			// e/a/x: all processes (default on Windows)
			// l: long format, f: full format, u: user format
			char select[256] = "Id,ProcessName";
			if (flag_l) {
				strcpy(select, "Id,ProcessName,CPU,WS,StartTime,Path,Description");
			}
			if (flag_f) {
				strcpy(select, "Id,ProcessName,CPU,WS,StartTime,Path,Description,SessionId,Handles,Threads");
			}
			if (flag_u) {
				strcpy(select, "Id,ProcessName,CPU,WS,StartTime,Path,Description,SessionId,Handles,Threads,UserName");
			}
			// If multiple format flags, prefer most verbose
			if (flag_u && flag_f) {
				strcpy(select, "Id,ProcessName,CPU,WS,StartTime,Path,Description,SessionId,Handles,Threads,UserName");
			} else if (flag_f) {
				strcpy(select, "Id,ProcessName,CPU,WS,StartTime,Path,Description,SessionId,Handles,Threads");
			} else if (flag_l) {
				strcpy(select, "Id,ProcessName,CPU,WS,StartTime,Path,Description");
			}
			char cmd[1024];
			snprintf(cmd, sizeof(cmd), "powershell -Command \"Get-Process | Select-Object %s | Format-Table -AutoSize\"", select);
			int result = system(cmd);
			if (result != 0) {
				fprintf(stderr, "ps: Failed to display process list.\n");
			}
			continue;
		}

		// fallback: try running as external command
		// Detect background execution with trailing '&'
		char cmdline[1024]; strncpy(cmdline, line, sizeof(cmdline)-1); cmdline[sizeof(cmdline)-1] = '\0';
		int background = 0;
		// trim trailing spaces
		int len = (int)strlen(cmdline);
		while (len > 0 && (cmdline[len-1] == ' ' || cmdline[len-1] == '\t')) cmdline[--len] = '\0';
		if (len > 0 && cmdline[len-1] == '&') {
			background = 1; cmdline[--len] = '\0';
			while (len > 0 && (cmdline[len-1] == ' ' || cmdline[len-1] == '\t')) cmdline[--len] = '\0';
		}

	fprintf(stderr, "[msh-debug] parsed cmdline='%s' background=%d len=%d lastchar='%c'\n", cmdline, background, len, (len>0?cmdline[len-1]:' '));
	if (!background) {
		/* If the command is a builtin, run it via run_builtin_command so builtins
		 * like userdel/useradd/groupadd are handled by our implementations. */
		char cmdcpy_builtin[1200]; strncpy(cmdcpy_builtin, cmdline, sizeof(cmdcpy_builtin)-1); cmdcpy_builtin[sizeof(cmdcpy_builtin)-1] = '\0';
		if (is_builtin(cmdcpy_builtin)) {
			run_builtin_command(cmdline, stdin, stdout);
			continue;
		}
		// If on Windows, optionally forward known Linux package manager commands to WSL
#ifdef _WIN32
		int do_forward = 0;
		if (MSH_forward_enabled == 1) do_forward = 1;
		else if (MSH_forward_enabled == 0) do_forward = 0;
		else {
			const char *fwd_env = getenv("MSH_FORWARD_LINUX_CMDS");
			if (!fwd_env || strcmp(fwd_env, "1") == 0 || strcmp(fwd_env, "true") == 0 || strcmp(fwd_env, "yes") == 0) do_forward = 1;
		}
		if (do_forward) {
			// Detect wsl on PATH with `where` (uses cmd.exe under the hood)
			if (system("where wsl >nul 2>&1") == 0) {
				// Simple check for commands that should be forwarded to WSL
				// We forward: apt, apt-get, dpkg, apt-key
				// Also forward user/group management commands so users can run them in WSL from Msh
				// Forwarded: useradd, adduser, userdel, deluser, groupadd, addgroup, groupdel, delgroup
				char cmdcpy[1200]; strncpy(cmdcpy, cmdline, sizeof(cmdcpy)-1); cmdcpy[sizeof(cmdcpy)-1] = '\0';
				char firsttok[256] = {0};
				sscanf(cmdcpy, "%255s", firsttok);
				if (strcmp(firsttok, "apt") == 0 || strcmp(firsttok, "apt-get") == 0 || strcmp(firsttok, "dpkg") == 0 || strcmp(firsttok, "apt-key") == 0) {
					// Build wsl invocation: optionally include distro from MSH_WSL_DISTRO
					const char *distro = getenv("MSH_WSL_DISTRO");
					char wslcmd[1600];
					if (distro && distro[0]) snprintf(wslcmd, sizeof(wslcmd), "wsl -d %s -- %s", distro, cmdline);
					else snprintf(wslcmd, sizeof(wslcmd), "wsl -- %s", cmdline);
					printf("[msh] forwarding to WSL: %s\n", wslcmd);
					int ret = system(wslcmd);
					if (ret == -1) fprintf(stderr, "Command failed to execute.\n");
					continue;
				}
			}
		}
#endif
		int ret = system(cmdline);
		if (ret == -1) fprintf(stderr, "Command failed to execute.\n");
	} else {
			// Launch process with CreateProcess and track as a job
			fprintf(stderr, "[msh-debug] background requested: '%s'\n", cmdline);
			STARTUPINFOA si; PROCESS_INFORMATION pi; memset(&si, 0, sizeof(si)); si.cb = sizeof(si);
			si.dwFlags = 0;
			char cmdcopy[1400];
			// If forwarding enabled and wsl exists and this is an apt-like command, run wsl under cmd.exe /C
#ifdef _WIN32
			int do_forward_bg = 0;
			if (MSH_forward_enabled == 1) do_forward_bg = 1;
			else if (MSH_forward_enabled == 0) do_forward_bg = 0;
			else {
				const char *fwd_env_bg = getenv("MSH_FORWARD_LINUX_CMDS");
				if (!fwd_env_bg || strcmp(fwd_env_bg, "1") == 0 || strcmp(fwd_env_bg, "true") == 0 || strcmp(fwd_env_bg, "yes") == 0) do_forward_bg = 1;
			}
			if (do_forward_bg && system("where wsl >nul 2>&1") == 0) {
				char cmdcpy2[1200]; strncpy(cmdcpy2, cmdline, sizeof(cmdcpy2)-1); cmdcpy2[sizeof(cmdcpy2)-1] = '\0';
				char firsttok2[256] = {0}; sscanf(cmdcpy2, "%255s", firsttok2);
				if (strcmp(firsttok2, "apt") == 0 || strcmp(firsttok2, "apt-get") == 0 || strcmp(firsttok2, "dpkg") == 0 || strcmp(firsttok2, "apt-key") == 0) {
					const char *distro = getenv("MSH_WSL_DISTRO");
					if (distro && distro[0]) snprintf(cmdcopy, sizeof(cmdcopy), "cmd.exe /C wsl -d %s -- %s", distro, cmdline);
					else snprintf(cmdcopy, sizeof(cmdcopy), "cmd.exe /C wsl -- %s", cmdline);
				} else {
					snprintf(cmdcopy, sizeof(cmdcopy), "cmd.exe /C %s", cmdline);
				}
			} else
#endif
			{
				snprintf(cmdcopy, sizeof(cmdcopy), "cmd.exe /C %s", cmdline);
			}
			BOOL ok = CreateProcessA(NULL, cmdcopy, NULL, NULL, FALSE, CREATE_NEW_PROCESS_GROUP, NULL, NULL, &si, &pi);
			if (!ok) {
				fprintf(stderr, "Failed to launch background job.\n");
			} else {
				int jid = msh_add_job(pi.hProcess, pi.hThread, pi.dwProcessId, cmdline);
				fprintf(stderr, "[msh-debug] CreateProcess pid=%lu, handles: hProc=%p hThread=%p jid=%d\n", (unsigned long)pi.dwProcessId, (void*)pi.hProcess, (void*)pi.hThread, jid);
				if (jid < 0) {
					fprintf(stderr, "Job list full.\n");
					CloseHandle(pi.hProcess); CloseHandle(pi.hThread);
				} else {
					printf("[%d] %lu\n", jid, (unsigned long)pi.dwProcessId);
					// don't close handles here; jobs.c will close when reaped/fg
				}
			}
		}
	}
msh_exit:
	printf("Exiting Msh.\n");
}

// Simple iterative wildcard matcher supporting '*' and '?' (no charset)
static int wildcard_match(const char *pattern, const char *str) {
	const char *s = NULL;
	const char *p = NULL;
	while (*str) {
		if (*pattern == '*') {
			// skip consecutive '*'
			while (*pattern == '*') pattern++;
			if (!*pattern) return 1; // trailing * matches rest
			p = pattern;
			s = str;
			// try to match remaining pattern at each position
			while (*s) {
				if (wildcard_match(p, s)) return 1;
				s++;
			}
			return 0;
		} else if (*pattern == '?' || *pattern == *str) {
			pattern++; str++;
		} else {
			return 0;
		}
	}
	// consume trailing * in pattern
	while (*pattern == '*') pattern++;
	return *pattern == '\0';
}

// Split a line into segments on top-level semicolons (ignoring semicolons inside quotes)
static int split_semicolon(const char *line, char segments[][1024], int maxseg) {
	int seg = 0;
	int pos = 0;
	char quote = 0;
	for (int i = 0; line[i] != '\0'; i++) {
		char c = line[i];
		if ((c == '"' || c == '\'') ) {
			if (quote == 0) quote = c;
			else if (quote == c) quote = 0;
		}
		if (c == ';' && quote == 0) {
			segments[seg][pos] = '\0';
			trim_inplace(segments[seg]);
			seg++;
			if (seg >= maxseg) return seg;
			pos = 0;
			continue;
		}
		if (pos < 1023) segments[seg][pos++] = c;
	}
	segments[seg][pos] = '\0';
	trim_inplace(segments[seg]);
	return seg + 1;
}

// Process a single (already-trimmed) command line. Returns 1 when caller should exit the shell.
// handle_command_line: centralizes per-line dispatch (interactive or non-interactive)
static int handle_command_line(char *line, int in_noninteractive) {
	// This code mirrors the original inline handlers in main and provides the same behavior.
	// Detect background '&' early so builtins can be backgrounded too
	char line_noamp[1024]; strncpy(line_noamp, line, sizeof(line_noamp)-1); line_noamp[sizeof(line_noamp)-1] = '\0';
	int bg_requested = 0;
	int _llen = (int)strlen(line_noamp);
	while (_llen > 0 && (line_noamp[_llen-1] == ' ' || line_noamp[_llen-1] == '\t')) line_noamp[--_llen] = '\0';
	if (_llen > 0 && line_noamp[_llen-1] == '&') {
		bg_requested = 1; line_noamp[--_llen] = '\0';
		while (_llen > 0 && (line_noamp[_llen-1] == ' ' || line_noamp[_llen-1] == '\t')) line_noamp[--_llen] = '\0';
	}

	// If background requested and the (trimmed) command is a builtin, launch it as a background job
	if (bg_requested && is_builtin(line_noamp)) {
		STARTUPINFOA si; PROCESS_INFORMATION pi; memset(&si, 0, sizeof(si)); si.cb = sizeof(si);
		char cmdcopy[1200]; snprintf(cmdcopy, sizeof(cmdcopy), "cmd.exe /C %s", line_noamp);
		BOOL ok = CreateProcessA(NULL, cmdcopy, NULL, NULL, FALSE, CREATE_NEW_PROCESS_GROUP, NULL, NULL, &si, &pi);
		if (!ok) {
			fprintf(stderr, "Failed to launch background builtin job.\n");
		} else {
			int jid = msh_add_job(pi.hProcess, pi.hThread, pi.dwProcessId, line_noamp);
			if (jid < 0) { fprintf(stderr, "Job list full.\n"); CloseHandle(pi.hProcess); CloseHandle(pi.hThread); }
			else printf("[%d] %lu\n", jid, (unsigned long)pi.dwProcessId);
		}
		return 0;
	}

	// If the command contains a pipe, handle the pipeline
	if (strchr(line, '|')) {
		run_pipeline(line);
		return 0;
	}

	// sudo
	if (strncmp(line, "sudo", 4) == 0 && (line[4] == ' ' || line[4] == '\t')) {
		char *cmd = (char *)line + 4; while (*cmd == ' ' || *cmd == '\t') cmd++;
		if (!*cmd) { fprintf(stderr, "Usage: sudo <command>\n"); return 0; }
		// Check if already running as administrator
		BOOL isAdmin = FALSE;
		HANDLE token = NULL;
		if (OpenProcessToken(GetCurrentProcess(), TOKEN_QUERY, &token)) {
			TOKEN_ELEVATION elevation;
			DWORD size;
			if (GetTokenInformation(token, TokenElevation, &elevation, sizeof(elevation), &size)) {
				isAdmin = elevation.TokenIsElevated;
			}
			CloseHandle(token);
		}
		if (isAdmin) {
			run_pipeline(cmd);
			return 0;
		}
		DWORD consoleMode = 0;
		BOOL hasConsole = GetConsoleMode(GetStdHandle(STD_INPUT_HANDLE), &consoleMode);
		if (!hasConsole) {
			printf("Not running in interactive console; running command locally (no elevation).\n");
			run_pipeline(cmd);
			return 0;
		}
		char exePath[MAX_PATH]; GetModuleFileNameA(NULL, exePath, MAX_PATH);
		char params[4096]; snprintf(params, sizeof(params), "--run-cmd %s", cmd);
		SHELLEXECUTEINFOA sei = {0}; sei.cbSize = sizeof(sei);
		sei.fMask = SEE_MASK_NOCLOSEPROCESS; sei.hwnd = NULL; sei.lpVerb = "runas";
		sei.lpFile = exePath; sei.lpParameters = params; sei.lpDirectory = NULL; sei.nShow = SW_SHOWNORMAL;
		if (ShellExecuteExA(&sei)) {
			printf("Elevation requested. Exiting current shell.\n"); exit(0);
		} else {
			printf("Failed to elevate. User may have cancelled or denied UAC prompt. Falling back to local execution.\n");
			run_pipeline(cmd);
		}
		return 0;
	}

	// exit
	if (strcmp(line, "exit") == 0) return 1;

	// forward-wsl [on|off|status]
	if (strncmp(line, "forward-wsl", 11) == 0) {
		char *arg = line + 11;
		while (*arg == ' ') arg++;
		if (*arg == '\0' || strncmp(arg, "status", 6) == 0) {
			int val = MSH_forward_enabled;
			if (val == -1) {
				const char *env = getenv("MSH_FORWARD_LINUX_CMDS");
				if (!env) printf("forward-wsl: status = (env default) enabled\n");
				else printf("forward-wsl: status = env(%s)\n", env);
			} else if (val == 0) printf("forward-wsl: disabled\n"); else printf("forward-wsl: enabled\n");
			return 0;
		} else if (strncmp(arg, "on", 2) == 0) { MSH_forward_enabled = 1; printf("forward-wsl: enabled\n"); return 0; }
		else if (strncmp(arg, "off", 3) == 0) { MSH_forward_enabled = 0; printf("forward-wsl: disabled\n"); return 0; }
		else { printf("Usage: forward-wsl [on|off|status]\n"); return 0; }
	}

	// cd
	if (strncmp(line, "cd ", 3) == 0) {
		char *dir = line + 3;
		while (*dir == ' ') dir++;
		if (SetCurrentDirectoryA(dir)) {
			// success
		} else {
			perror("cd");
		}
		return 0;
	}

	// If the command matches a builtin, run via run_builtin_command
	// Some commands (ls, ps) were originally implemented inline in main; call their wrappers first
	// medit (Marshall edit) launch - support when running via handle_command_line
	if (strncmp(line, "medit", 5) == 0) {
		char *file = line + 5;
		while (*file == ' ' || *file == '\t') file++;
		if (!*file) { fprintf(stderr, "medit: No filename provided. Usage: medit <file>\n"); return 0; }
		/* Launch Medit.exe from the msh directory */
		system("cls");
		char msh_path[MAX_PATH];
		GetModuleFileNameA(NULL, msh_path, MAX_PATH);
		char *last_sep = strrchr(msh_path, '\\');
		if (last_sep) *(last_sep+1) = '\0';
		char medit_path[MAX_PATH+64];
		snprintf(medit_path, sizeof(medit_path), "%sMedit.exe", msh_path);
		FILE *medit_fp = fopen(medit_path, "r");
		if (!medit_fp) {
			fprintf(stderr, "medit: Medit.exe not found at %s\n", medit_path);
			return 0;
		}
		fclose(medit_fp);
		HINSTANCE result = ShellExecuteA(NULL, "open", medit_path, file, NULL, SW_SHOWNORMAL);
		if ((INT_PTR)result <= 32) {
			fprintf(stderr, "medit: ShellExecuteA failed to launch Medit.exe (error code %ld).\n", (long)(INT_PTR)result);
		}
		system("cls");
		return 0;
	}
	if (strncmp(line, "ls", 2) == 0) {
		run_ls_handler(line, stdout);
		return 0;
	}
	if (strncmp(line, "ps", 2) == 0) {
		run_ps_handler(line, stdout);
		return 0;
	}
	// Otherwise, if the command matches a builtin, run via run_builtin_command
	if (is_builtin(line)) {
		run_builtin_command(line, stdin, stdout);
		return 0;
	}

	// Otherwise fallback to external execution (respect background)
	char cmdline[1024]; strncpy(cmdline, line, sizeof(cmdline)-1); cmdline[sizeof(cmdline)-1] = '\0';
	int background = 0;
	int len = (int)strlen(cmdline);
	while (len > 0 && (cmdline[len-1] == ' ' || cmdline[len-1] == '\t')) cmdline[--len] = '\0';
	if (len > 0 && cmdline[len-1] == '&') {
		background = 1; cmdline[--len] = '\0';
		while (len > 0 && (cmdline[len-1] == ' ' || cmdline[len-1] == '\t')) cmdline[--len] = '\0';
	}

	if (!background) {
#ifdef _WIN32
		int do_forward = 0;
		if (MSH_forward_enabled == 1) do_forward = 1;
		else if (MSH_forward_enabled == 0) do_forward = 0;
		else {
			const char *fwd_env = getenv("MSH_FORWARD_LINUX_CMDS");
			if (!fwd_env || strcmp(fwd_env, "1") == 0 || strcmp(fwd_env, "true") == 0 || strcmp(fwd_env, "yes") == 0) do_forward = 1;
		}
		if (do_forward) {
			if (system("where wsl >nul 2>&1") == 0) {
				char cmdcpy[1200]; strncpy(cmdcpy, cmdline, sizeof(cmdcpy)-1); cmdcpy[sizeof(cmdcpy)-1] = '\0';
				char firsttok[256] = {0}; sscanf(cmdcpy, "%255s", firsttok);
				if (strcmp(firsttok, "apt") == 0 || strcmp(firsttok, "apt-get") == 0 || strcmp(firsttok, "dpkg") == 0 || strcmp(firsttok, "apt-key") == 0) {
					const char *distro = getenv("MSH_WSL_DISTRO");
					char wslcmd[1600];
					if (distro && distro[0]) snprintf(wslcmd, sizeof(wslcmd), "wsl -d %s -- %s", distro, cmdline);
					else snprintf(wslcmd, sizeof(wslcmd), "wsl -- %s", cmdline);
					printf("[msh] forwarding to WSL: %s\n", wslcmd);
					int ret = system(wslcmd);
					if (ret == -1) fprintf(stderr, "Command failed to execute.\n");
					return 0;
				}
			}
		}
#endif
		int ret = system(cmdline);
		if (ret == -1) fprintf(stderr, "Command failed to execute.\n");
	} else {
		// background execution
		STARTUPINFOA si; PROCESS_INFORMATION pi; memset(&si, 0, sizeof(si)); si.cb = sizeof(si);
		si.dwFlags = 0;
		char cmdcopy[1400];
#ifdef _WIN32
		int do_forward_bg = 0;
		if (MSH_forward_enabled == 1) do_forward_bg = 1;
		else if (MSH_forward_enabled == 0) do_forward_bg = 0;
		else {
			const char *fwd_env_bg = getenv("MSH_FORWARD_LINUX_CMDS");
			if (!fwd_env_bg || strcmp(fwd_env_bg, "1") == 0 || strcmp(fwd_env_bg, "true") == 0 || strcmp(fwd_env_bg, "yes") == 0) do_forward_bg = 1;
		}
		if (do_forward_bg && system("where wsl >nul 2>&1") == 0) {
			char cmdcpy2[1200]; strncpy(cmdcpy2, cmdline, sizeof(cmdcpy2)-1); cmdcpy2[sizeof(cmdcpy2)-1] = '\0';
			char firsttok2[256] = {0}; sscanf(cmdcpy2, "%255s", firsttok2);
			if (strcmp(firsttok2, "apt") == 0 || strcmp(firsttok2, "apt-get") == 0 || strcmp(firsttok2, "dpkg") == 0 || strcmp(firsttok2, "apt-key") == 0) {
				const char *distro = getenv("MSH_WSL_DISTRO");
				if (distro && distro[0]) snprintf(cmdcopy, sizeof(cmdcopy), "cmd.exe /C wsl -d %s -- %s", distro, cmdline);
				else snprintf(cmdcopy, sizeof(cmdcopy), "cmd.exe /C wsl -- %s", cmdline);
			} else {
				snprintf(cmdcopy, sizeof(cmdcopy), "cmd.exe /C %s", cmdline);
			}
		} else
#endif
		{
			snprintf(cmdcopy, sizeof(cmdcopy), "cmd.exe /C %s", cmdline);
		}
		BOOL ok = CreateProcessA(NULL, cmdcopy, NULL, NULL, FALSE, CREATE_NEW_PROCESS_GROUP, NULL, NULL, &si, &pi);
		if (!ok) {
			fprintf(stderr, "Failed to launch background job.\n");
		} else {
			int jid = msh_add_job(pi.hProcess, pi.hThread, pi.dwProcessId, cmdline);
			if (jid < 0) {
				fprintf(stderr, "Job list full.\n");
				CloseHandle(pi.hProcess); CloseHandle(pi.hThread);
			} else {
				printf("[%d] %lu\n", jid, (unsigned long)pi.dwProcessId);
			}
		}
	}
	return 0;
}

// Extracted handlers for commands that were implemented inline in main.
// We expose small wrapper functions so handle_command_line can call them.
static int run_ls_handler(char *line, FILE *out) {
	// reuse the ls implementation from main, but write to provided FILE* 'out'
	fprintf(stderr, "[msh-debug] entering ls handler (wrapper)\n");
	char *args = line + 2;
	while (*args == ' ') args++;
	int show_all = 0, long_list = 0, human = 0, sort_time = 0, sort_size = 0, show_index = 0;
	char *dir = NULL;
	if (*args == '-') {
		args++;
		while (*args && *args != ' ') {
			if (*args == 'a') show_all = 1;
			if (*args == 'l') long_list = 1;
			if (*args == 'h') human = 1;
			if (*args == 't') sort_time = 1;
			if (*args == 'S') sort_size = 1;
			if (*args == 'i') show_index = 1;
			args++;
		}
		while (*args == ' ') args++;
	}
	if (*args) dir = args;
	char path[MAX_PATH];
	if (dir && strlen(dir) > 0) {
		strncpy(path, dir, MAX_PATH-1);
		path[MAX_PATH-1] = '\0';
	} else {
		GetCurrentDirectoryA(MAX_PATH, path);
	}
	WIN32_FIND_DATAA findData;
	char search_path[MAX_PATH];
	snprintf(search_path, sizeof(search_path), "%s\\*", path);
	HANDLE hFind = FindFirstFileA(search_path, &findData);
	if (hFind == INVALID_HANDLE_VALUE) {
		perror("ls");
		return 0;
	}
	typedef struct {
		char name[MAX_PATH];
		ULONGLONG size;
		FILETIME mtime;
		DWORD attr;
		ULONGLONG fileid;
	} FileEntry;
	FileEntry files[2048];
	int count = 0;
	do {
		if (!show_all && (findData.cFileName[0] == '.')) continue;
		strncpy(files[count].name, findData.cFileName, MAX_PATH-1);
		files[count].name[MAX_PATH-1] = '\0';
		files[count].size = ((ULONGLONG)findData.nFileSizeHigh << 32) | findData.nFileSizeLow;
		files[count].mtime = findData.ftLastWriteTime;
		files[count].attr = findData.dwFileAttributes;
		char fullpath[MAX_PATH*2];
		snprintf(fullpath, sizeof(fullpath), "%s\\%s", path, findData.cFileName);
		HANDLE fh = CreateFileA(fullpath, 0, FILE_SHARE_READ, NULL, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, NULL);
		if (fh != INVALID_HANDLE_VALUE) {
			BY_HANDLE_FILE_INFORMATION info;
			if (GetFileInformationByHandle(fh, &info)) {
				files[count].fileid = ((ULONGLONG)info.nFileIndexHigh << 32) | info.nFileIndexLow;
			} else {
				files[count].fileid = 0;
			}
			CloseHandle(fh);
		} else {
			files[count].fileid = 0;
		}
		count++;
	} while (FindNextFileA(hFind, &findData) && count < 2048);
	FindClose(hFind);
	if (sort_time) {
		for (int i = 0; i < count-1; i++) {
			for (int j = i+1; j < count; j++) {
				if (CompareFileTime(&files[i].mtime, &files[j].mtime) < 0) {
					FileEntry tmp = files[i]; files[i] = files[j]; files[j] = tmp;
				}
			}
		}
	} else if (sort_size) {
		for (int i = 0; i < count-1; i++) {
			for (int j = i+1; j < count; j++) {
				if (files[i].size < files[j].size) {
					FileEntry tmp = files[i]; files[i] = files[j]; files[j] = tmp;
				}
			}
		}
	}
	for (int i = 0; i < count; i++) {
		if (long_list) {
			fprintf(out, "%c%c%c%c%c ",
				(files[i].attr & FILE_ATTRIBUTE_DIRECTORY) ? 'd' : '-',
				(files[i].attr & FILE_ATTRIBUTE_READONLY) ? 'r' : '-',
				(files[i].attr & FILE_ATTRIBUTE_ARCHIVE) ? 'a' : '-',
				(files[i].attr & FILE_ATTRIBUTE_HIDDEN) ? 'h' : '-',
				(files[i].attr & FILE_ATTRIBUTE_SYSTEM) ? 's' : '-');
		}
		if (show_index) {
			fprintf(out, "%10llu ", files[i].fileid);
		}
		if (long_list) {
			if (human) {
				double sz = (double)files[i].size;
				const char *unit = "B";
				if (sz > 1024) { sz /= 1024; unit = "K"; }
				if (sz > 1024) { sz /= 1024; unit = "M"; }
				if (sz > 1024) { sz /= 1024; unit = "G"; }
				fprintf(out, "%8.1f%s ", sz, unit);
			} else {
				fprintf(out, "%10llu ", files[i].size);
			}
			SYSTEMTIME st;
			FileTimeToSystemTime(&files[i].mtime, &st);
			fprintf(out, "%04d-%02d-%02d %02d:%02d ", st.wYear, st.wMonth, st.wDay, st.wHour, st.wMinute);
		}
		fprintf(out, "%s\n", files[i].name);
	}
	return 0;
}

static int run_ps_handler(char *line, FILE *out) {
	char *args = line + 2;
	while (*args == ' ') args++;
	int flag_e = 0, flag_l = 0, flag_f = 0, flag_a = 0, flag_u = 0, flag_x = 0;
	if (*args == '-') {
		args++;
		while (*args && *args != ' ') {
			if (*args == 'e') flag_e = 1;
			if (*args == 'l') flag_l = 1;
			if (*args == 'f') flag_f = 1;
			if (*args == 'a') flag_a = 1;
			if (*args == 'u') flag_u = 1;
			if (*args == 'x') flag_x = 1;
			args++;
		}
	}
	char select[256] = "Id,ProcessName";
	if (flag_l) strcpy(select, "Id,ProcessName,CPU,WS,StartTime,Path,Description");
	if (flag_f) strcpy(select, "Id,ProcessName,CPU,WS,StartTime,Path,Description,SessionId,Handles,Threads");
	if (flag_u) strcpy(select, "Id,ProcessName,CPU,WS,StartTime,Path,Description,SessionId,Handles,Threads,UserName");
	if (flag_u && flag_f) strcpy(select, "Id,ProcessName,CPU,WS,StartTime,Path,Description,SessionId,Handles,Threads,UserName");
	else if (flag_f) strcpy(select, "Id,ProcessName,CPU,WS,StartTime,Path,Description,SessionId,Handles,Threads");
	else if (flag_l) strcpy(select, "Id,ProcessName,CPU,WS,StartTime,Path,Description");
	char cmd[1024];
	snprintf(cmd, sizeof(cmd), "powershell -Command \"Get-Process | Select-Object %s | Format-Table -AutoSize\"", select);
	/* Use _popen to capture the output and write it into 'out' so pipelines receive it */
	FILE *pf = _popen(cmd, "r");
	if (!pf) {
		fprintf(stderr, "ps: failed to run command\n");
		return 0;
	}
	char buf[4096];
	while (fgets(buf, sizeof(buf), pf)) {
		fputs(buf, out);
	}
	_pclose(pf);
	return 0;
}
//VERY IMPORTANT: GitHub Copilot AI generated, then modified by me.
//to compile: gcc Medit.c -o Medit.exe
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <windows.h>
#include <conio.h>


#define MAX_LINE_LENGTH 256
char **lines = NULL;
int max_lines = 0;
int num_lines = 0;
int cursor_x = 0, cursor_y = 0;
int insert_mode = 0;
int top_line = 0; // kept for compatibility (unused by visual mapping)
int top_visual = 0; // first visual row displayed (accounts for wrapped rows)
// Number of lines reserved at the top for the header (filename + mode)
#define HEADER_LINES 2
// Number of logical file lines to always show at top of content (sticky file header)
#define FILE_HEADER_LINES 2
static CONSOLE_CURSOR_INFO saved_cci;
static int cursor_info_saved = 0;

void hide_console_cursor() {
    HANDLE h = GetStdHandle(STD_OUTPUT_HANDLE);
    CONSOLE_CURSOR_INFO cci;
    if (GetConsoleCursorInfo(h, &cci)) {
        // Only save the original cursor info the first time we hide it.
        if (!cursor_info_saved) {
            saved_cci = cci;
            cursor_info_saved = 1;
        }
        // Always request the cursor be hidden for drawing, but don't
        // overwrite the saved original information.
        cci.bVisible = FALSE;
        SetConsoleCursorInfo(h, &cci);
    }
}

void restore_console_cursor() {
    if (!cursor_info_saved) return;
    HANDLE h = GetStdHandle(STD_OUTPUT_HANDLE);
    SetConsoleCursorInfo(h, &saved_cci);
    // Clear saved flag so subsequent hide/restore cycles re-capture the
    // current console cursor state (prevents restoring an already-hidden
    // state saved from a previous hide call).
    cursor_info_saved = 0;
}

// Ensure we have space for at least `min_lines` entries in `lines`.
void ensure_capacity(int min_lines) {
    if (min_lines <= max_lines) return;
    int old_max = max_lines;
    if (old_max < 1) old_max = 0;
    // Grow by doubling or at least min_lines + 10 to reduce realloc churn
    if (max_lines < 1) max_lines = min_lines + 10;
    while (max_lines < min_lines) max_lines *= 2;
    if (max_lines < min_lines) max_lines = min_lines + 10;
    lines = realloc(lines, max_lines * sizeof(char*));
    for (int i = old_max; i < max_lines; i++) lines[i] = calloc(MAX_LINE_LENGTH, 1);
}

void clear_screen() {
    system("cls");
}

void load_file(const char *filename) {
    FILE *fp = fopen(filename, "r");
    num_lines = 0;
    int file_lines = 0;
    char temp[MAX_LINE_LENGTH];
    if (!fp) {
        max_lines = 10;
        lines = malloc(max_lines * sizeof(char*));
        for (int i = 0; i < max_lines; i++) {
            lines[i] = calloc(MAX_LINE_LENGTH, 1);
        }
        return;
    }
    // Count lines
    while (fgets(temp, MAX_LINE_LENGTH, fp)) file_lines++;
    rewind(fp);
    num_lines = 0;
    max_lines = file_lines + 10;
    lines = malloc(max_lines * sizeof(char*));
    for (int i = 0; i < max_lines; i++) lines[i] = calloc(MAX_LINE_LENGTH, 1);
    while (num_lines < file_lines && fgets(lines[num_lines], MAX_LINE_LENGTH, fp)) {
        lines[num_lines][strcspn(lines[num_lines], "\r\n")] = 0;
        num_lines++;
    }
    fclose(fp);
}

void save_file(const char *filename) {
    FILE *fp = fopen(filename, "w");
    if (!fp) return;
    for (int i = 0; i < num_lines; i++) {
        fprintf(fp, "%s\n", lines[i]);
    }
    fclose(fp);
}

void draw_editor(const char *filename) {
    // Get console size to determine how many lines to display
    CONSOLE_SCREEN_BUFFER_INFO csbi;
    HANDLE h = GetStdHandle(STD_OUTPUT_HANDLE);
    int win_rows = 25; // default
    if (GetConsoleScreenBufferInfo(h, &csbi)) {
        win_rows = csbi.srWindow.Bottom - csbi.srWindow.Top + 1;
    }
    hide_console_cursor();
    clear_screen();
    // Anchor header to the top of the visible window using an explicit cursor position
    COORD win_top_left = {0,0};
    if (GetConsoleScreenBufferInfo(h, &csbi)) {
        win_top_left.X = csbi.srWindow.Left;
        win_top_left.Y = csbi.srWindow.Top;
    }
    // Print the program header at absolute window top rows so they remain visible
    COORD pos = { win_top_left.X, win_top_left.Y };
    SetConsoleCursorPosition(h, pos);
    printf("Medit - %s", filename);
    pos.Y = win_top_left.Y + 1;
    SetConsoleCursorPosition(h, pos);
    printf("-- %s --", insert_mode ? "INSERT" : "COMMAND");

    // Number of lines available for file content (subtract header lines)
    int content_rows = win_rows - HEADER_LINES; // header lines used above
    if (content_rows < 1) content_rows = 1;

    // Determine console width to avoid printing past window and confusing cursor position
    int win_cols = 80;
    if (GetConsoleScreenBufferInfo(h, &csbi)) {
        win_cols = csbi.srWindow.Right - csbi.srWindow.Left + 1;
    }
    // Determine printable width (columns available for file text)
    int visible_cols = win_cols - 4;
    if (visible_cols < 1) visible_cols = 1;

    // Compute cursor's visual row (how many wrapped rows before the cursor)
    // Compute visual rows occupied by the sticky file header (first FILE_HEADER_LINES)
    int header_visual_rows = 0;
    for (int i = 0; i < FILE_HEADER_LINES && i < num_lines; i++) {
        int llen = (int)strlen(lines[i]);
        header_visual_rows += (llen > 0) ? ((llen + visible_cols - 1) / visible_cols) : 1;
    }

    // Remaining rows available for scrollable content after the sticky file header
    int remaining_rows = content_rows - header_visual_rows;
    if (remaining_rows < 1) remaining_rows = 1;

    // Compute cursor visual row relative to the area after the sticky file header.
    // If the cursor is within the sticky header, we treat it specially so it stays
    // visible in that fixed region.
    int cursor_in_header = 0;
    int cursor_visual_row_after = 0; // visual row index within the scrollable area
    int cursor_visual_row_in_header = 0; // visual row index within header area
    if (cursor_y < FILE_HEADER_LINES) {
        cursor_in_header = 1;
        for (int i = 0; i < cursor_y; i++) {
            int llen = (int)strlen(lines[i]);
            int rows = (llen > 0) ? ((llen + visible_cols - 1) / visible_cols) : 1;
            cursor_visual_row_in_header += rows;
        }
        cursor_visual_row_in_header += (cursor_x) / visible_cols;
    } else {
        for (int i = FILE_HEADER_LINES; i < cursor_y; i++) {
            int llen = (int)strlen(lines[i]);
            int rows = (llen > 0) ? ((llen + visible_cols - 1) / visible_cols) : 1;
            cursor_visual_row_after += rows;
        }
        cursor_visual_row_after += (cursor_x) / visible_cols;
    }

    // Compute total visual rows for the scrollable area (excluding sticky header)
    int total_visual_rows_after = 0;
    for (int i = FILE_HEADER_LINES; i < num_lines; i++) {
        int llen = (int)strlen(lines[i]);
        total_visual_rows_after += (llen > 0) ? ((llen + visible_cols - 1) / visible_cols) : 1;
    }

    // Center the cursor vertically in the remaining (scrollable) area with smooth step
    int max_top = total_visual_rows_after - remaining_rows;
    if (max_top < 0) max_top = 0;
    int target_visual_top = cursor_in_header ? 0 : (cursor_visual_row_after - remaining_rows / 2);
    if (target_visual_top < 0) target_visual_top = 0;
    if (target_visual_top > max_top) target_visual_top = max_top;
    if (top_visual < target_visual_top) top_visual++;
    else if (top_visual > target_visual_top) top_visual--;

    // Draw sticky file header (first FILE_HEADER_LINES) at the top of the content area
    // Print these unconditionally so the file header always appears.
    int printed = 0;
    // top-left window again for content line placement
    SHORT left = csbi.srWindow.Left;
    SHORT top = csbi.srWindow.Top;
    for (int li = 0; li < FILE_HEADER_LINES && li < num_lines; li++) {
        int llen = (int)strlen(lines[li]);
        int rows = (llen > 0) ? ((llen + visible_cols - 1) / visible_cols) : 1;
        for (int sub = 0; sub < rows; sub++) {
            // place cursor at absolute row for this content line
            COORD line_pos = { (SHORT)(left + 2), (SHORT)(top + HEADER_LINES + printed) };
            SetConsoleCursorPosition(h, line_pos);
            if (li == cursor_y) printf("> "); else printf("  ");
            int start_col = sub * visible_cols;
            int remain = llen - start_col;
            int toprint = remain > 0 ? (remain < visible_cols ? remain : visible_cols) : 0;
            if (li == cursor_y) {
                int cursor_sub = cursor_x / visible_cols;
                int cursor_in_this_row = (sub == cursor_sub);
                HANDLE hOut = GetStdHandle(STD_OUTPUT_HANDLE);
                CONSOLE_SCREEN_BUFFER_INFO local_csbi;
                GetConsoleScreenBufferInfo(hOut, &local_csbi);
                WORD normal_attr = local_csbi.wAttributes;
                WORD invert_attr = ((normal_attr & 0x0F) << 4) | ((normal_attr & 0xF0) >> 4);
                for (int j = 0; j < toprint; j++) {
                    int src_idx = start_col + j;
                    if (cursor_in_this_row && !insert_mode && (src_idx == cursor_x)) {
                        SetConsoleTextAttribute(hOut, invert_attr);
                        putchar(lines[li][src_idx] ? lines[li][src_idx] : ' ');
                        SetConsoleTextAttribute(hOut, normal_attr);
                    } else {
                        putchar(lines[li][src_idx]);
                    }
                }
                if (!insert_mode && sub == (cursor_x / visible_cols) && cursor_x >= llen) {
                    HANDLE hOut = GetStdHandle(STD_OUTPUT_HANDLE);
                    CONSOLE_SCREEN_BUFFER_INFO local_csbi;
                    GetConsoleScreenBufferInfo(hOut, &local_csbi);
                    WORD normal_attr = local_csbi.wAttributes;
                    WORD invert_attr = ((normal_attr & 0x0F) << 4) | ((normal_attr & 0xF0) >> 4);
                    SetConsoleTextAttribute(hOut, invert_attr);
                    putchar(' ');
                    SetConsoleTextAttribute(hOut, normal_attr);
                }
                // line complete (next iteration will set the position explicitly)
                ;
            } else {
                if (toprint > 0) {
                    for (int j = 0; j < toprint; j++) putchar(lines[li][start_col + j]);
                }
                ;
            }
            printed++;
        }
    }

    // Draw the scrollable content area (lines after the sticky header), honoring top_visual
    int vis_index = 0; // visual row index within the scrollable area (after header)
    for (int li = FILE_HEADER_LINES; li < num_lines && printed < content_rows; li++) {
        int llen = (int)strlen(lines[li]);
        int rows = (llen > 0) ? ((llen + visible_cols - 1) / visible_cols) : 1;
        for (int sub = 0; sub < rows && printed < content_rows; sub++) {
            if (vis_index >= top_visual) {
                // position each printed line explicitly to avoid accidental scrolling
                COORD line_pos = { (SHORT)(left + 2), (SHORT)(top + HEADER_LINES + printed) };
                SetConsoleCursorPosition(h, line_pos);
                if (li == cursor_y) printf("> "); else printf("  ");
                int start_col = sub * visible_cols;
                int remain = llen - start_col;
                int toprint = remain > 0 ? (remain < visible_cols ? remain : visible_cols) : 0;
                if (li == cursor_y) {
                    int cursor_sub = cursor_x / visible_cols;
                    int cursor_in_this_row = (sub == cursor_sub);
                    HANDLE hOut = GetStdHandle(STD_OUTPUT_HANDLE);
                    CONSOLE_SCREEN_BUFFER_INFO local_csbi;
                    GetConsoleScreenBufferInfo(hOut, &local_csbi);
                    WORD normal_attr = local_csbi.wAttributes;
                    WORD invert_attr = ((normal_attr & 0x0F) << 4) | ((normal_attr & 0xF0) >> 4);
                    for (int j = 0; j < toprint; j++) {
                        int src_idx = start_col + j;
                        if (cursor_in_this_row && !insert_mode && (src_idx == cursor_x)) {
                            SetConsoleTextAttribute(hOut, invert_attr);
                            putchar(lines[li][src_idx] ? lines[li][src_idx] : ' ');
                            SetConsoleTextAttribute(hOut, normal_attr);
                        } else {
                            putchar(lines[li][src_idx]);
                        }
                    }
                    if (!insert_mode && sub == (cursor_x / visible_cols) && cursor_x >= llen) {
                        HANDLE hOut = GetStdHandle(STD_OUTPUT_HANDLE);
                        CONSOLE_SCREEN_BUFFER_INFO local_csbi;
                        GetConsoleScreenBufferInfo(hOut, &local_csbi);
                        WORD normal_attr = local_csbi.wAttributes;
                        WORD invert_attr = ((normal_attr & 0x0F) << 4) | ((normal_attr & 0xF0) >> 4);
                        SetConsoleTextAttribute(hOut, invert_attr);
                        putchar(' ');
                        SetConsoleTextAttribute(hOut, normal_attr);
                    }
                    ;
                } else {
                    if (toprint > 0) {
                        for (int j = 0; j < toprint; j++) putchar(lines[li][start_col + j]);
                    }
                    ;
                }
                printed++;
            }
            vis_index++;
        }
    }
    // If no more lines but still space, print ~ lines (positioned)
    while (printed < content_rows) {
        COORD line_pos = { (SHORT)(left + 2), (SHORT)(top + HEADER_LINES + printed) };
        SetConsoleCursorPosition(h, line_pos);
        printf("  ~");
        printed++;
    }
    // Status/help line at the bottom of the content area
    COORD status_pos = { (SHORT)(left), (SHORT)(top + HEADER_LINES + content_rows) };
    SetConsoleCursorPosition(h, status_pos);
    printf(": (vi: dd yy p P x X u w b 0 $ gg G, :w :q :wq)");
    fflush(stdout);
}

void move_console_cursor(int x, int y) {
    HANDLE h = GetStdHandle(STD_OUTPUT_HANDLE);
    CONSOLE_SCREEN_BUFFER_INFO csbi;
    // Map logical cursor (x,y) into visual coordinates accounting for wrapping and top_visual
    if (GetConsoleScreenBufferInfo(h, &csbi)) {
        SHORT left = csbi.srWindow.Left;
        SHORT top = csbi.srWindow.Top;
        SHORT right = csbi.srWindow.Right;
        SHORT bottom = csbi.srWindow.Bottom;
        int win_rows = csbi.srWindow.Bottom - csbi.srWindow.Top + 1;
        int content_rows = win_rows - HEADER_LINES;
        if (content_rows < 1) content_rows = 1;
        int win_cols = csbi.srWindow.Right - csbi.srWindow.Left + 1;
        int visible_cols = win_cols - 4;
        if (visible_cols < 1) visible_cols = 1;

        // Compute visual rows occupied by the sticky file header (first FILE_HEADER_LINES)
        int header_visual_rows = 0;
        for (int i = 0; i < FILE_HEADER_LINES && i < num_lines; i++) {
            int llen = (int)strlen(lines[i]);
            header_visual_rows += (llen > 0) ? ((llen + visible_cols - 1) / visible_cols) : 1;
        }
        int remaining_rows = content_rows - header_visual_rows;
        if (remaining_rows < 1) remaining_rows = 1;

        // Compute visual row for cursor (counting wrapped subrows) and whether it's in header
        int cursor_visual_row_after = 0;
        int cursor_visual_row_in_header = 0;
        int cursor_in_header = 0;
        if (y < FILE_HEADER_LINES) {
            cursor_in_header = 1;
            for (int i = 0; i < y && i < num_lines; i++) {
                int llen = (int)strlen(lines[i]);
                int rows = (llen > 0) ? ((llen + visible_cols - 1) / visible_cols) : 1;
                cursor_visual_row_in_header += rows;
            }
            cursor_visual_row_in_header += (x) / visible_cols;
        } else {
            for (int i = FILE_HEADER_LINES; i < y && i < num_lines; i++) {
                int llen = (int)strlen(lines[i]);
                int rows = (llen > 0) ? ((llen + visible_cols - 1) / visible_cols) : 1;
                cursor_visual_row_after += rows;
            }
            cursor_visual_row_after += (x) / visible_cols;
        }

        // Map to a visual index inside the content area (0..content_rows-1). The
        // content area starts at console row (top + HEADER_LINES). The sticky file
        // header occupies the first header_visual_rows of that content area.
        int visual_index = 0;
        if (cursor_in_header) {
            visual_index = cursor_visual_row_in_header;
        } else {
            visual_index = header_visual_rows + (cursor_visual_row_after - top_visual);
        }
        if (visual_index < 0) visual_index = 0;
        if (visual_index > content_rows - 1) visual_index = content_rows - 1;

        int vis_x = x % visible_cols;
        if (vis_x < 0) vis_x = 0;
        if (vis_x > visible_cols - 1) vis_x = visible_cols - 1;
    // Position cursor: map visual coordinates into absolute buffer coordinates.
    // NOTE: the visible window top (csbi.srWindow.Top) already represents the
    // window offset; use top + vis_y.
    // The printed lines start at column (left + 2) where we print the 2-char
    // prefix ("> " or "  "), and then the text begins at left + 4. Place the
    // cursor at that text column plus vis_x, and at the content top (top + HEADER_LINES)
    // plus the visual index we computed.
    COORD pos = { (SHORT)(left + 4 + vis_x), (SHORT)(top + HEADER_LINES + visual_index) };
        // Clamp to window bounds
        if (pos.X < left) pos.X = left;
        if (pos.X > right) pos.X = right;
        if (pos.Y < top) pos.Y = top;
        if (pos.Y > bottom) pos.Y = bottom;
        SetConsoleCursorPosition(h, pos);
    } else {
        // Fallback: basic mapping (no double-counting of header lines)
        int vis_y = y - top_line;
        if (vis_y < 0) vis_y = 0;
        COORD pos = { (SHORT)(x + 2), (SHORT)(vis_y) };
        SetConsoleCursorPosition(h, pos);
    }
}

// Ensure cursor_x is within the bounds of the current logical line.
void clamp_cursor() {
    if (num_lines <= 0) {
        cursor_y = 0;
        cursor_x = 0;
        return;
    }
    if (cursor_y < 0) cursor_y = 0;
    if (cursor_y >= num_lines) cursor_y = num_lines - 1;
    int len = (int)strlen(lines[cursor_y]);
    if (cursor_x > len) cursor_x = len;
    if (cursor_x < 0) cursor_x = 0;
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        printf("Usage: medit <file>\n");
        return 1;
    }
    const char *filename = argv[1];
    load_file(filename);
    // Enable ANSI escape sequence processing (virtual terminal) so colors and cursor
    // highlights work correctly when running inside PowerShell / Windows terminals.
    {
        HANDLE hOut = GetStdHandle(STD_OUTPUT_HANDLE);
        DWORD dwMode = 0;
        if (GetConsoleMode(hOut, &dwMode)) {
            dwMode |= ENABLE_VIRTUAL_TERMINAL_PROCESSING;
            SetConsoleMode(hOut, dwMode);
        }
    }
    insert_mode = 0;
    int running = 1;
    char command[64];
    char yank[MAX_LINE_LENGTH] = "";
    int last_deleted = -1;
    char undo[MAX_LINE_LENGTH] = "";
    int undo_line = -1;
    // Search state
    char search_str[MAX_LINE_LENGTH] = "";
    int search_len = 0;
    int last_search_line = -1, last_search_pos = -1;
    int last_search_dir = 1; // 1: forward, -1: backward
    // Vim repeat (.) and open line (o/O/a) state
    enum {CHANGE_NONE, CHANGE_A, CHANGE_O, CHANGE_O_UP, CHANGE_SUBSTITUTE} last_change_type = CHANGE_NONE;
    int last_change_line = -1, last_change_x = -1;
    char last_sub_search[MAX_LINE_LENGTH] = "", last_sub_replace[MAX_LINE_LENGTH] = "";
    int last_sub_global = 0;
    while (running) {
    clamp_cursor();
    draw_editor(filename);
    if (!insert_mode) { move_console_cursor(cursor_x, cursor_y); restore_console_cursor(); }
        if (insert_mode) {
            int ch;
            int line_len = strlen(lines[cursor_y]);
            if (cursor_x > line_len) cursor_x = line_len;
            move_console_cursor(cursor_x, cursor_y); restore_console_cursor();
            while (1) {
                ch = _getch();
                if (ch == 27) { // ESC key
                    insert_mode = 0;
                    break;
                } else if (ch == '\r' || ch == '\n') { // Enter key
                    if (num_lines + 10 > max_lines) {
                        int old_max = max_lines;
                        max_lines = num_lines + 10;
                        lines = realloc(lines, max_lines * sizeof(char*));
                        for (int i = old_max; i < max_lines; i++) lines[i] = calloc(MAX_LINE_LENGTH, 1);
                    }
                    ensure_capacity(num_lines + 2);
                    strcpy(lines[cursor_y + 1], &lines[cursor_y][cursor_x]);
                    lines[cursor_y][cursor_x] = '\0';
                    cursor_y++;
                    cursor_x = 0;
                    num_lines++;
                    draw_editor(filename);
                    move_console_cursor(cursor_x, cursor_y); restore_console_cursor();
                } else if (ch == 8) { // Backspace
                    if (cursor_x > 0) {
                        memmove(&lines[cursor_y][cursor_x-1], &lines[cursor_y][cursor_x], strlen(&lines[cursor_y][cursor_x]) + 1);
                        cursor_x--;
                    } else if (cursor_y > 0) {
                        int prev_len = strlen(lines[cursor_y-1]);
                        if (prev_len + strlen(lines[cursor_y]) < MAX_LINE_LENGTH) {
                            strcat(lines[cursor_y-1], lines[cursor_y]);
                            for (int i = cursor_y; i < num_lines-1; i++) strcpy(lines[i], lines[i+1]);
                            num_lines--;
                            cursor_y--;
                            cursor_x = prev_len;
                        }
                    }
                    draw_editor(filename);
                    move_console_cursor(cursor_x, cursor_y); restore_console_cursor();
                } else if (ch == 224) { // Arrow keys
                    int arrow = _getch();
                    if (arrow == 72 && cursor_y > 0) cursor_y--; // up
                    else if (arrow == 80 && cursor_y < num_lines-1) cursor_y++; // down
                    else if (arrow == 75 && cursor_x > 0) cursor_x--; // left
                    else if (arrow == 77 && cursor_x < strlen(lines[cursor_y])) cursor_x++; // right
                    draw_editor(filename);
                    move_console_cursor(cursor_x, cursor_y); restore_console_cursor();
                } else if (ch >= 32 && ch < 127) { // Printable
                    int len = strlen(lines[cursor_y]);
                    if (len < MAX_LINE_LENGTH-1) {
                        memmove(&lines[cursor_y][cursor_x+1], &lines[cursor_y][cursor_x], len-cursor_x+1);
                        lines[cursor_y][cursor_x] = (char)ch;
                        cursor_x++;
                    }
                    draw_editor(filename);
                    move_console_cursor(cursor_x, cursor_y);
                }
            }
        } else {
            // Real-time command mode: use _getch for VIM navigation
            int ch = _getch();
            int ch2 = 0;
            static int pending_G = 0;
            static int pending_G_val = 0;
            // Handle nG (go to line n)
            if (ch >= '0' && ch <= '9') {
                pending_G = 1;
                pending_G_val = ch - '0';
                while (1) {
                    int next_ch = _getch();
                    if (next_ch >= '0' && next_ch <= '9') {
                        pending_G_val = pending_G_val * 10 + (next_ch - '0');
                    } else if (next_ch == 'G') {
                        if (pending_G_val > 0 && pending_G_val <= num_lines) {
                            cursor_y = pending_G_val - 1;
                            if (cursor_y < 0) cursor_y = 0;
                            if (cursor_y >= num_lines) cursor_y = num_lines - 1;
                        }
                        pending_G = 0;
                        break;
                    } else {
                        // Not a G, process as normal key
                        ch = next_ch;
                        pending_G = 0;
                        break;
                    }
                }
                // If we handled nG, skip further processing
                if (!pending_G) continue;
            }
            if (ch == 224) ch2 = _getch();
            // Arrow keys
            if (ch == 224) {
                if (ch2 == 72 && cursor_y > 0) cursor_y--; // up
                else if (ch2 == 80 && cursor_y < num_lines - 1) cursor_y++; // down
                else if (ch2 == 75 && cursor_x > 0) cursor_x--; // left
                else if (ch2 == 77 && cursor_x < strlen(lines[cursor_y])) cursor_x++; // right
            } else if (ch == 'i') {
                insert_mode = 1;
            } else if (ch == 'a') {
                cursor_x++;
                insert_mode = 1;
                last_change_type = CHANGE_A;
                last_change_line = cursor_y;
                last_change_x = cursor_x;
            } else if (ch == 'o') {
                // Open new line below
                ensure_capacity(num_lines + 2);
                for (int i = num_lines; i > cursor_y + 1; i--) strcpy(lines[i], lines[i-1]);
                lines[cursor_y+1][0] = '\0';
                num_lines++;
                cursor_y++;
                cursor_x = 0;
                insert_mode = 1;
                last_change_type = CHANGE_O;
                last_change_line = cursor_y;
                last_change_x = cursor_x;
            } else if (ch == 'O') {
                // Open new line above
                ensure_capacity(num_lines + 2);
                for (int i = num_lines; i > cursor_y; i--) strcpy(lines[i], lines[i-1]);
                lines[cursor_y][0] = '\0';
                num_lines++;
                cursor_x = 0;
                insert_mode = 1;
                last_change_type = CHANGE_O_UP;
                last_change_line = cursor_y;
                last_change_x = cursor_x;
            } else if (ch == '.') {
                // Repeat last change
                if (last_change_type == CHANGE_A) {
                    cursor_y = last_change_line;
                    cursor_x = last_change_x;
                    insert_mode = 1;
                } else if (last_change_type == CHANGE_O) {
                    ensure_capacity(num_lines + 2);
                    for (int i = num_lines; i > last_change_line; i--) strcpy(lines[i], lines[i-1]);
                    lines[last_change_line][0] = '\0';
                    num_lines++;
                    cursor_y = last_change_line;
                    cursor_x = last_change_x;
                    insert_mode = 1;
                } else if (last_change_type == CHANGE_O_UP) {
                    ensure_capacity(num_lines + 2);
                    for (int i = num_lines; i > last_change_line; i--) strcpy(lines[i], lines[i-1]);
                    lines[last_change_line][0] = '\0';
                    num_lines++;
                    cursor_y = last_change_line;
                    cursor_x = last_change_x;
                    insert_mode = 1;
                } else if (last_change_type == CHANGE_SUBSTITUTE) {
                    // Repeat last substitute
                    for (int i = 0; i < num_lines; i++) {
                        char *line = lines[i];
                        char *found = NULL;
                        do {
                            found = strstr(line, last_sub_search);
                            if (found) {
                                int rep_len = strlen(last_sub_replace);
                                int search_len = strlen(last_sub_search);
                                char temp[MAX_LINE_LENGTH];
                                strcpy(temp, found + search_len);
                                strncpy(found, last_sub_replace, rep_len);
                                strcpy(found + rep_len, temp);
                                if (!last_sub_global) break;
                                line = found + rep_len;
                            }
                        } while (last_sub_global && found);
                    }
                }
            } else if (ch == 'j' && cursor_y < num_lines - 1) cursor_y++;
            else if (ch == 'k' && cursor_y > 0) cursor_y--;
            else if (ch == 'h' && cursor_x > 0) cursor_x--;
            else if (ch == 'l' && cursor_x < strlen(lines[cursor_y])) cursor_x++;
            else if (ch == '0') cursor_x = 0;
            else if (ch == '$') cursor_x = strlen(lines[cursor_y]);
            else if (ch == 'G') cursor_y = num_lines > 0 ? num_lines - 1 : 0;
            else if (ch == 'g') {
                // Wait for second 'g'
                int ch3 = _getch();
                if (ch3 == 'g') cursor_y = 0;
            }
            // / search
            else if (ch == '/') {
                draw_editor(filename);
                printf("/");
                memset(search_str, 0, sizeof(search_str));
                int pos = 0;
                while (1) {
                    int c = _getch();
                    if (c == '\r' || c == '\n') break;
                    if (c == 8 && pos > 0) { pos--; search_str[pos] = 0; printf("\b \b"); }
                    else if (c >= 32 && c < 127 && pos < MAX_LINE_LENGTH-1) { search_str[pos++] = (char)c; printf("%c", c); }
                }
                search_str[pos] = 0;
                search_len = strlen(search_str);
                last_search_dir = 1;
                // Search forward from current line
                int found = 0;
                for (int i = cursor_y; i < num_lines; i++) {
                    char *p = strstr(lines[i], search_str);
                    if (p) {
                        cursor_y = i;
                        cursor_x = p - lines[i];
                        last_search_line = i;
                        last_search_pos = cursor_x;
                        found = 1;
                        break;
                    }
                }
                if (!found) {
                    // Wrap around
                    for (int i = 0; i < cursor_y; i++) {
                        char *p = strstr(lines[i], search_str);
                        if (p) {
                            cursor_y = i;
                            cursor_x = p - lines[i];
                            last_search_line = i;
                            last_search_pos = cursor_x;
                            found = 1;
                            break;
                        }
                    }
                }
            }
            // n: next match
            else if (ch == 'n' && search_len > 0) {
                last_search_dir = 1;
                int found = 0;
                int start_line = cursor_y;
                int start_x = cursor_x + 1;
                // Search current line after cursor
                char *p = strstr(&lines[start_line][start_x], search_str);
                if (p) {
                    cursor_y = start_line;
                    cursor_x = p - lines[start_line];
                    last_search_line = cursor_y;
                    last_search_pos = cursor_x;
                    found = 1;
                } else {
                    // Search next lines
                    for (int i = cursor_y + 1; i < num_lines; i++) {
                        p = strstr(lines[i], search_str);
                        if (p) {
                            cursor_y = i;
                            cursor_x = p - lines[i];
                            last_search_line = cursor_y;
                            last_search_pos = cursor_x;
                            found = 1;
                            break;
                        }
                    }
                    // Wrap around
                    if (!found) {
                        for (int i = 0; i < cursor_y; i++) {
                            p = strstr(lines[i], search_str);
                            if (p) {
                                cursor_y = i;
                                cursor_x = p - lines[i];
                                last_search_line = cursor_y;
                                last_search_pos = cursor_x;
                                found = 1;
                                break;
                            }
                        }
                    }
                }
            }
            // N: previous match
            else if (ch == 'N' && search_len > 0) {
                last_search_dir = -1;
                int found = 0;
                int i = cursor_y;
                int x = cursor_x - 1;
                // Search current line before cursor
                if (x >= 0) {
                    for (int j = x; j >= 0; j--) {
                        if (strncmp(&lines[i][j], search_str, search_len) == 0) {
                            cursor_y = i;
                            cursor_x = j;
                            last_search_line = cursor_y;
                            last_search_pos = cursor_x;
                            found = 1;
                            break;
                        }
                    }
                }
                if (!found) {
                    // Search previous lines
                    for (i = cursor_y - 1; i >= 0; i--) {
                        char *p = NULL;
                        int last_pos = -1;
                        char *line = lines[i];
                        char *q = line;
                        while ((p = strstr(q, search_str))) {
                            last_pos = p - line;
                            q = p + 1;
                        }
                        if (last_pos >= 0) {
                            cursor_y = i;
                            cursor_x = last_pos;
                            last_search_line = cursor_y;
                            last_search_pos = cursor_x;
                            found = 1;
                            break;
                        }
                    }
                    // Wrap around
                    if (!found) {
                        for (i = num_lines - 1; i > cursor_y; i--) {
                            char *p = NULL;
                            int last_pos = -1;
                            char *line = lines[i];
                            char *q = line;
                            while ((p = strstr(q, search_str))) {
                                last_pos = p - line;
                                q = p + 1;
                            }
                            if (last_pos >= 0) {
                                cursor_y = i;
                                cursor_x = last_pos;
                                last_search_line = cursor_y;
                                last_search_pos = cursor_x;
                                found = 1;
                                break;
                            }
                        }
                    }
                }
            }
            else if (ch == 'w' && cursor_x < strlen(lines[cursor_y])) {
                int i = cursor_x;
                while (lines[cursor_y][i] && (lines[cursor_y][i] == ' ' || lines[cursor_y][i] == '\t')) i++;
                while (lines[cursor_y][i] && lines[cursor_y][i] != ' ' && lines[cursor_y][i] != '\t') i++;
                while (lines[cursor_y][i] && (lines[cursor_y][i] == ' ' || lines[cursor_y][i] == '\t')) i++;
                cursor_x = i;
            }
            else if (ch == 'b' && cursor_x > 0) {
                int i = cursor_x - 1;
                while (i > 0 && (lines[cursor_y][i] == ' ' || lines[cursor_y][i] == '\t')) i--;
                while (i > 0 && lines[cursor_y][i] != ' ' && lines[cursor_y][i] != '\t') i--;
                while (i > 0 && (lines[cursor_y][i] == ' ' || lines[cursor_y][i] == '\t')) i--;
                cursor_x = i < 0 ? 0 : i;
            }
            // vi editing (dd, yy, p, P, x, X, u)
            else if (ch == 'd') {
                int ch3 = _getch();
                if (ch3 == 'd' && num_lines > 0) {
                    strcpy(undo, lines[cursor_y]); undo_line = cursor_y;
                    strcpy(yank, lines[cursor_y]);
                    for (int i = cursor_y; i < num_lines - 1; i++) strcpy(lines[i], lines[i+1]);
                    lines[num_lines-1][0] = '\0';
                    num_lines--;
                    if (cursor_y >= num_lines) cursor_y = num_lines - 1;
                    last_deleted = undo_line;
                    // Shrink buffer only if it's significantly larger than needed
                    if (max_lines > num_lines + 50) {
                        for (int i = num_lines + 10; i < max_lines; i++) free(lines[i]);
                        max_lines = num_lines + 10;
                        lines = realloc(lines, max_lines * sizeof(char*));
                    }
                }
            }
            else if (ch == 'y') {
                int ch3 = _getch();
                if (ch3 == 'y' && num_lines > 0) {
                    strcpy(yank, lines[cursor_y]);
                }
            }
            else if (ch == 'p' && yank[0]) {
                ensure_capacity(num_lines + 2);
                for (int i = num_lines; i > cursor_y+1; i--) strcpy(lines[i], lines[i-1]);
                strcpy(lines[cursor_y+1], yank);
                num_lines++;
                cursor_y++;
            }
            else if (ch == 'P' && num_lines < max_lines && yank[0]) {
                ensure_capacity(num_lines + 2);
                for (int i = num_lines; i > cursor_y; i--) strcpy(lines[i], lines[i-1]);
                strcpy(lines[cursor_y], yank);
                num_lines++;
            }
            else if (ch == 'x' && strlen(lines[cursor_y]) > cursor_x) {
                strcpy(undo, lines[cursor_y]); undo_line = cursor_y;
                memmove(&lines[cursor_y][cursor_x], &lines[cursor_y][cursor_x+1], strlen(lines[cursor_y]) - cursor_x);
            }
            else if (ch == 'X' && cursor_x > 0 && strlen(lines[cursor_y]) > 0) {
                strcpy(undo, lines[cursor_y]); undo_line = cursor_y;
                memmove(&lines[cursor_y][cursor_x-1], &lines[cursor_y][cursor_x], strlen(lines[cursor_y]) - cursor_x + 1);
                cursor_x--;
            }
            else if (ch == 'u' && undo_line >= 0) {
                strcpy(lines[undo_line], undo);
                undo_line = -1;
            }
            // vi commands (colon)
            else if (ch == ':') {
                // Enter command line mode
                char cmdline[64] = {0};
                int pos = 0;
                draw_editor(filename);
                printf(":");
                while (1) {
                    int c = _getch();
                    if (c == '\r' || c == '\n') break;
                    if (c == 8 && pos > 0) { pos--; cmdline[pos] = 0; printf("\b \b"); }
                    else if (c >= 32 && c < 127 && pos < 63) { cmdline[pos++] = (char)c; printf("%c", c); }
                }
                cmdline[pos] = 0;
                // :w [filename] - save (and optionally rename)
                if (strncmp(cmdline, "w", 1) == 0 && (cmdline[1] == ' ' || cmdline[1] == '\0')) {
                    char *arg = NULL;
                    if (cmdline[1] == ' ') arg = cmdline + 2;
                    if (arg && strlen(arg) > 0) {
                        // If no extension, add original extension
                        const char *dot = strrchr(filename, '.');
                        char newname[260];
                        if (strchr(arg, '.') == NULL && dot) {
                            snprintf(newname, sizeof(newname), "%s%s", arg, dot);
                        } else {
                            snprintf(newname, sizeof(newname), "%s", arg);
                        }
                        save_file(newname);
                        // Rename file on disk and update filename
                        remove(newname); // Remove if exists to allow rename
                        rename(filename, newname);
                        strncpy((char*)filename, newname, 255); // Update filename variable
                        filename = (const char*)newname;
                    } else {
                        save_file(filename);
                    }
                }
                else if (strcmp(cmdline, "q") == 0) running = 0;
                else if (strcmp(cmdline, "wq") == 0) { save_file(filename); running = 0; }
                else if (strcmp(cmdline, "make") == 0) system("gcc Medit.c -o Medit.exe");
                // :%s/search/replace/ and :%s/search/replace/g
                else if (strncmp(cmdline, "%s/", 3) == 0) {
                    char *rest = cmdline + 3;
                    char *search = rest;
                    char *replace = strchr(rest, '/');
                    if (replace) {
                        *replace = '\0'; replace++;
                        char *end = strchr(replace, '/');
                        int global = 0;
                        if (end) {
                            *end = '\0';
                            if (strcmp(end + 1, "g") == 0) global = 1;
                        }
                        // Perform substitution
                        for (int i = 0; i < num_lines; i++) {
                            char *line = lines[i];
                            char *found = NULL;
                            do {
                                found = strstr(line, search);
                                if (found) {
                                    int rep_len = strlen(replace);
                                    int search_len = strlen(search);
                                    char temp[MAX_LINE_LENGTH];
                                    strcpy(temp, found + search_len);
                                    strncpy(found, replace, rep_len);
                                    strcpy(found + rep_len, temp);
                                    if (!global) break;
                                    line = found + rep_len;
                                }
                            } while (global && found);
                        }
                        // Save last substitute for .
                        strncpy(last_sub_search, search, MAX_LINE_LENGTH-1);
                        strncpy(last_sub_replace, replace, MAX_LINE_LENGTH-1);
                        last_sub_global = global;
                        last_change_type = CHANGE_SUBSTITUTE;
                    }
                }
            }
        }
    }
    // Free memory
    for (int i = 0; i < max_lines; i++) free(lines[i]);
    free(lines);
    return 0;
}

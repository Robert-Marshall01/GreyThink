/**
 * @file gui.h
 * @brief Grey Firmware - GTK3 GUI Interface
 *
 * Cross-platform graphical interface for the Grey Firmware demonstration.
 * Provides a dashboard view of all firmware modules, demo controls,
 * configuration, and a live log viewer.
 *
 * Requires GTK+ 3.0 or later.
 */

#ifndef GF_GUI_H
#define GF_GUI_H

#include <stdbool.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* GUI Configuration                                                          */
/*===========================================================================*/

#define GF_GUI_WINDOW_TITLE     "Grey Firmware v1.0.0"
#define GF_GUI_WINDOW_WIDTH     1024
#define GF_GUI_WINDOW_HEIGHT    700
#define GF_GUI_LOG_MAX_LINES    500
#define GF_GUI_REFRESH_MS       250

/*===========================================================================*/
/* GUI Status Codes                                                           */
/*===========================================================================*/

typedef enum {
    GF_GUI_OK = 0,
    GF_GUI_ERR_INIT,
    GF_GUI_ERR_ALREADY_RUNNING,
    GF_GUI_ERR_NOT_INIT
} gf_gui_result_t;

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

/**
 * @brief Initialize and run the GUI (blocks until window is closed)
 * @param argc Pointer to main argc
 * @param argv Pointer to main argv
 * @return 0 on success, non-zero on failure
 */
int gf_gui_run(int *argc, char ***argv);

/**
 * @brief Append a message to the GUI log viewer (thread-safe)
 * @param level Log level: "INFO", "WARN", "ERROR"
 * @param message The message text
 */
void gf_gui_log(const char *level, const char *message);

#ifdef __cplusplus
}
#endif

#endif /* GF_GUI_H */

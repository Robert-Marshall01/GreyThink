/**
 * @file gui_main.c
 * @brief Grey Firmware - GUI Entry Point
 *
 * Separate main() for the graphical interface build.
 * Launches the GTK3 dashboard instead of the CLI demo.
 */

#include "gui/gui.h"
#include <stdio.h>

int main(int argc, char *argv[]) {
    return gf_gui_run(&argc, &argv);
}

/**
 * @file gui.c
 * @brief Grey Firmware - GTK3 GUI Implementation
 *
 * Cross-platform graphical dashboard for the Grey Firmware demonstration.
 * Features:
 *   - Module status panel with live indicators
 *   - Demo mode selector and run controls
 *   - Configuration fields (MQTT broker, CAN baud, sensor rate)
 *   - Scrollable log viewer
 *   - About dialog with version and architecture summary
 */

#include "gui/gui.h"
#include "demo/demo.h"
#include "grey_firmware.h"

#include <gtk/gtk.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

/*===========================================================================*/
/* Private State                                                              */
/*===========================================================================*/

typedef struct {
    /* Top-level window */
    GtkWidget *window;

    /* Header bar */
    GtkWidget *header_bar;

    /* Status labels (module panel) */
    GtkWidget *lbl_scheduler;
    GtkWidget *lbl_msgbus;
    GtkWidget *lbl_can;
    GtkWidget *lbl_mqtt;
    GtkWidget *lbl_secboot;
    GtkWidget *lbl_sensors;

    /* Demo status labels */
    GtkWidget *lbl_running;
    GtkWidget *lbl_samples;
    GtkWidget *lbl_can_frames;
    GtkWidget *lbl_mqtt_msgs;
    GtkWidget *lbl_errors;
    GtkWidget *lbl_last_value;

    /* Controls */
    GtkWidget *combo_mode;
    GtkWidget *entry_broker;
    GtkWidget *spin_port;
    GtkWidget *entry_topic;
    GtkWidget *spin_sensor_hz;
    GtkWidget *btn_start;
    GtkWidget *btn_stop;

    /* Log text view */
    GtkWidget      *log_view;
    GtkTextBuffer  *log_buffer;

    /* Timer for periodic refresh */
    guint refresh_timer_id;

    /* Demo state */
    gf_demo_config_t  demo_cfg;
    gboolean          demo_running;
    gboolean          initialized;
} gui_state_t;

static gui_state_t g_gui;

/*===========================================================================*/
/* Helpers                                                                    */
/*===========================================================================*/

static void gui_append_log(const char *level, const char *msg) {
    if (!g_gui.log_buffer) return;

    time_t now = time(NULL);
    struct tm *t = localtime(&now);
    char line[512];
    snprintf(line, sizeof(line), "[%02d:%02d:%02d] [%s] %s\n",
             t->tm_hour, t->tm_min, t->tm_sec, level, msg);

    GtkTextIter end;
    gtk_text_buffer_get_end_iter(g_gui.log_buffer, &end);
    gtk_text_buffer_insert(g_gui.log_buffer, &end, line, -1);

    /* Auto-scroll to bottom */
    gtk_text_buffer_get_end_iter(g_gui.log_buffer, &end);
    GtkTextMark *mark = gtk_text_buffer_get_mark(g_gui.log_buffer, "end");
    if (!mark)
        mark = gtk_text_buffer_create_mark(g_gui.log_buffer, "end", &end, FALSE);
    else
        gtk_text_buffer_move_mark(g_gui.log_buffer, mark, &end);
    gtk_text_view_scroll_mark_onscreen(GTK_TEXT_VIEW(g_gui.log_view), mark);

    /* Trim very long logs */
    int line_count = gtk_text_buffer_get_line_count(g_gui.log_buffer);
    if (line_count > GF_GUI_LOG_MAX_LINES) {
        GtkTextIter start, trim_end;
        gtk_text_buffer_get_start_iter(g_gui.log_buffer, &start);
        gtk_text_buffer_get_iter_at_line(g_gui.log_buffer, &trim_end,
                                         line_count - GF_GUI_LOG_MAX_LINES);
        gtk_text_buffer_delete(g_gui.log_buffer, &start, &trim_end);
    }
}

static void set_label_markup(GtkWidget *label, const char *color,
                              const char *text) {
    char buf[256];
    snprintf(buf, sizeof(buf),
             "<span foreground='%s' weight='bold'>%s</span>", color, text);
    gtk_label_set_markup(GTK_LABEL(label), buf);
}

/*===========================================================================*/
/* Callbacks                                                                  */
/*===========================================================================*/

static void on_start_clicked(GtkWidget *widget, gpointer data) {
    (void)widget;
    (void)data;

    if (g_gui.demo_running) return;

    /* Read configuration from GUI fields */
    g_gui.demo_cfg.mode = (gf_demo_mode_t)gtk_combo_box_get_active(
        GTK_COMBO_BOX(g_gui.combo_mode));
    g_gui.demo_cfg.mqtt_broker = gtk_entry_get_text(
        GTK_ENTRY(g_gui.entry_broker));
    g_gui.demo_cfg.mqtt_port = (uint16_t)gtk_spin_button_get_value_as_int(
        GTK_SPIN_BUTTON(g_gui.spin_port));
    g_gui.demo_cfg.mqtt_topic = gtk_entry_get_text(
        GTK_ENTRY(g_gui.entry_topic));
    g_gui.demo_cfg.sensor_period_ms =
        (uint32_t)(1000.0 /
                   gtk_spin_button_get_value(GTK_SPIN_BUTTON(g_gui.spin_sensor_hz)));
    g_gui.demo_cfg.can_baud_rate = GF_CAN_BAUD_500KBIT;
    g_gui.demo_cfg.require_secure_boot = true;

    gui_append_log("INFO", "Initializing firmware modules...");

    int rc = gf_demo_init(&g_gui.demo_cfg);
    if (rc != 0) {
        char err[128];
        snprintf(err, sizeof(err), "Demo init failed (error %d)", rc);
        gui_append_log("ERROR", err);
        return;
    }

    rc = gf_demo_start();
    if (rc != 0) {
        char err[128];
        snprintf(err, sizeof(err), "Demo start failed (error %d)", rc);
        gui_append_log("ERROR", err);
        return;
    }

    g_gui.demo_running = TRUE;
    gtk_widget_set_sensitive(g_gui.btn_start, FALSE);
    gtk_widget_set_sensitive(g_gui.btn_stop, TRUE);

    set_label_markup(g_gui.lbl_scheduler, "#4CAF50", "● Active");
    set_label_markup(g_gui.lbl_msgbus,    "#4CAF50", "● Active");
    set_label_markup(g_gui.lbl_can,       "#4CAF50", "● Active");
    set_label_markup(g_gui.lbl_mqtt,      "#4CAF50", "● Active");
    set_label_markup(g_gui.lbl_secboot,   "#4CAF50", "● Verified");
    set_label_markup(g_gui.lbl_sensors,   "#4CAF50", "● Sampling");

    gui_append_log("INFO", "Demo started: Sensor → CAN → MQTT pipeline active");
}

static void on_stop_clicked(GtkWidget *widget, gpointer data) {
    (void)widget;
    (void)data;

    if (!g_gui.demo_running) return;

    gf_demo_stop();
    g_gui.demo_running = FALSE;
    gtk_widget_set_sensitive(g_gui.btn_start, TRUE);
    gtk_widget_set_sensitive(g_gui.btn_stop, FALSE);

    set_label_markup(g_gui.lbl_scheduler, "#9E9E9E", "● Idle");
    set_label_markup(g_gui.lbl_msgbus,    "#9E9E9E", "● Idle");
    set_label_markup(g_gui.lbl_can,       "#9E9E9E", "● Idle");
    set_label_markup(g_gui.lbl_mqtt,      "#9E9E9E", "● Idle");
    set_label_markup(g_gui.lbl_secboot,   "#9E9E9E", "● Idle");
    set_label_markup(g_gui.lbl_sensors,   "#9E9E9E", "● Idle");

    gui_append_log("INFO", "Demo stopped");
}

static void on_about_clicked(GtkWidget *widget, gpointer data) {
    (void)widget;
    (void)data;

    const char *authors[] = {"Grey Firmware Team", NULL};

    gtk_show_about_dialog(
        GTK_WINDOW(g_gui.window),
        "program-name", "Grey Firmware",
        "version", "1.0.0",
        "comments",
        "A modular firmware framework demonstration.\n\n"
        "Modules: IoT, Automotive, Consumer, Medical, Security\n"
        "Spotlights: CAN 2.0B Driver, Secure Bootloader",
        "authors", authors,
        "license-type", GTK_LICENSE_MIT_X11,
        NULL);
}

static void on_clear_log(GtkWidget *widget, gpointer data) {
    (void)widget;
    (void)data;
    GtkTextIter start, end;
    gtk_text_buffer_get_bounds(g_gui.log_buffer, &start, &end);
    gtk_text_buffer_delete(g_gui.log_buffer, &start, &end);
}

/* Periodic refresh: update status counters */
static gboolean on_refresh_timer(gpointer data) {
    (void)data;

    if (!g_gui.demo_running) return TRUE;

    /* Step simulation forward */
    gf_sched_yield();
    gf_msg_process(1);
    gf_can_process();
    gf_mqtt_process();
    gf_wdt_kick();

    gf_demo_status_t st;
    gf_demo_get_status(&st);

    char buf[64];
    snprintf(buf, sizeof(buf), "%u", st.samples_processed);
    gtk_label_set_text(GTK_LABEL(g_gui.lbl_samples), buf);

    snprintf(buf, sizeof(buf), "%u", st.can_frames_sent);
    gtk_label_set_text(GTK_LABEL(g_gui.lbl_can_frames), buf);

    snprintf(buf, sizeof(buf), "%u", st.mqtt_messages_sent);
    gtk_label_set_text(GTK_LABEL(g_gui.lbl_mqtt_msgs), buf);

    snprintf(buf, sizeof(buf), "%u", st.errors);
    gtk_label_set_text(GTK_LABEL(g_gui.lbl_errors), buf);

    snprintf(buf, sizeof(buf), "%.2f", st.last_sensor_value);
    gtk_label_set_text(GTK_LABEL(g_gui.lbl_last_value), buf);

    if (st.errors > 0)
        set_label_markup(g_gui.lbl_errors, "#F44336", buf);

    return TRUE;  /* keep timer running */
}

/*===========================================================================*/
/* CSS Theming                                                                */
/*===========================================================================*/

static void apply_css(void) {
    const char *css_data =
        "window { background-color: #1e1e2e; }\n"
        "headerbar { background: #313244; color: #cdd6f4; }\n"
        "label { color: #cdd6f4; }\n"
        "entry { background: #313244; color: #cdd6f4; border: 1px solid #585b70; }\n"
        "spinbutton { background: #313244; color: #cdd6f4; }\n"
        "combobox * { background: #313244; color: #cdd6f4; }\n"
        "button { background: #45475a; color: #cdd6f4; border-radius: 6px; "
        "         padding: 6px 14px; border: none; }\n"
        "button:hover { background: #585b70; }\n"
        "button.suggested-action { background: #89b4fa; color: #1e1e2e; }\n"
        "button.destructive-action { background: #f38ba8; color: #1e1e2e; }\n"
        "textview { background-color: #11111b; color: #a6e3a1; font-family: monospace; }\n"
        "textview text { background-color: #11111b; color: #a6e3a1; }\n"
        "frame { border: 1px solid #45475a; }\n"
        "frame > label { color: #89b4fa; font-weight: bold; }\n";

    GtkCssProvider *css = gtk_css_provider_new();
    gtk_css_provider_load_from_data(css, css_data, -1, NULL);
    gtk_style_context_add_provider_for_screen(
        gdk_screen_get_default(),
        GTK_STYLE_PROVIDER(css),
        GTK_STYLE_PROVIDER_PRIORITY_APPLICATION);
    g_object_unref(css);
}

/*===========================================================================*/
/* UI Construction                                                            */
/*===========================================================================*/

static GtkWidget *build_module_panel(void) {
    GtkWidget *frame = gtk_frame_new("Module Status");
    GtkWidget *grid  = gtk_grid_new();
    gtk_grid_set_row_spacing(GTK_GRID(grid), 6);
    gtk_grid_set_column_spacing(GTK_GRID(grid), 16);
    gtk_widget_set_margin_start(grid, 12);
    gtk_widget_set_margin_end(grid, 12);
    gtk_widget_set_margin_top(grid, 8);
    gtk_widget_set_margin_bottom(grid, 8);

    const char *names[] = {
        "Scheduler", "Message Bus", "CAN 2.0B",
        "MQTT", "Secure Boot", "Sensors"
    };
    GtkWidget **indicators[] = {
        &g_gui.lbl_scheduler, &g_gui.lbl_msgbus, &g_gui.lbl_can,
        &g_gui.lbl_mqtt, &g_gui.lbl_secboot, &g_gui.lbl_sensors
    };

    for (int i = 0; i < 6; i++) {
        GtkWidget *name_lbl = gtk_label_new(names[i]);
        gtk_widget_set_halign(name_lbl, GTK_ALIGN_START);
        gtk_grid_attach(GTK_GRID(grid), name_lbl, 0, i, 1, 1);

        *indicators[i] = gtk_label_new(NULL);
        set_label_markup(*indicators[i], "#9E9E9E", "● Idle");
        gtk_widget_set_halign(*indicators[i], GTK_ALIGN_END);
        gtk_grid_attach(GTK_GRID(grid), *indicators[i], 1, i, 1, 1);
    }

    gtk_container_add(GTK_CONTAINER(frame), grid);
    return frame;
}

static GtkWidget *build_stats_panel(void) {
    GtkWidget *frame = gtk_frame_new("Live Statistics");
    GtkWidget *grid  = gtk_grid_new();
    gtk_grid_set_row_spacing(GTK_GRID(grid), 6);
    gtk_grid_set_column_spacing(GTK_GRID(grid), 16);
    gtk_widget_set_margin_start(grid, 12);
    gtk_widget_set_margin_end(grid, 12);
    gtk_widget_set_margin_top(grid, 8);
    gtk_widget_set_margin_bottom(grid, 8);

    const char *stat_names[] = {
        "Running", "Samples", "CAN Frames",
        "MQTT Messages", "Errors", "Last Sensor Value"
    };
    GtkWidget **stat_labels[] = {
        &g_gui.lbl_running, &g_gui.lbl_samples, &g_gui.lbl_can_frames,
        &g_gui.lbl_mqtt_msgs, &g_gui.lbl_errors, &g_gui.lbl_last_value
    };
    const char *defaults[] = {"No", "0", "0", "0", "0", "0.00"};

    for (int i = 0; i < 6; i++) {
        GtkWidget *name_lbl = gtk_label_new(stat_names[i]);
        gtk_widget_set_halign(name_lbl, GTK_ALIGN_START);
        gtk_grid_attach(GTK_GRID(grid), name_lbl, 0, i, 1, 1);

        *stat_labels[i] = gtk_label_new(defaults[i]);
        gtk_widget_set_halign(*stat_labels[i], GTK_ALIGN_END);
        gtk_grid_attach(GTK_GRID(grid), *stat_labels[i], 1, i, 1, 1);
    }

    gtk_container_add(GTK_CONTAINER(frame), grid);
    return frame;
}

static GtkWidget *build_config_panel(void) {
    GtkWidget *frame = gtk_frame_new("Configuration");
    GtkWidget *grid  = gtk_grid_new();
    gtk_grid_set_row_spacing(GTK_GRID(grid), 6);
    gtk_grid_set_column_spacing(GTK_GRID(grid), 12);
    gtk_widget_set_margin_start(grid, 12);
    gtk_widget_set_margin_end(grid, 12);
    gtk_widget_set_margin_top(grid, 8);
    gtk_widget_set_margin_bottom(grid, 8);

    int row = 0;

    /* Demo mode combo */
    gtk_grid_attach(GTK_GRID(grid), gtk_label_new("Demo Mode"), 0, row, 1, 1);
    g_gui.combo_mode = gtk_combo_box_text_new();
    gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(g_gui.combo_mode),
                                   "Sensor → CAN → MQTT");
    gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(g_gui.combo_mode),
                                   "Sensor Only");
    gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(g_gui.combo_mode),
                                   "CAN Loopback");
    gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(g_gui.combo_mode),
                                   "MQTT Only");
    gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(g_gui.combo_mode),
                                   "Secure Boot Check");
    gtk_combo_box_set_active(GTK_COMBO_BOX(g_gui.combo_mode), 0);
    gtk_widget_set_hexpand(g_gui.combo_mode, TRUE);
    gtk_grid_attach(GTK_GRID(grid), g_gui.combo_mode, 1, row++, 1, 1);

    /* MQTT Broker */
    gtk_grid_attach(GTK_GRID(grid), gtk_label_new("MQTT Broker"), 0, row, 1, 1);
    g_gui.entry_broker = gtk_entry_new();
    gtk_entry_set_text(GTK_ENTRY(g_gui.entry_broker), "localhost");
    gtk_widget_set_hexpand(g_gui.entry_broker, TRUE);
    gtk_grid_attach(GTK_GRID(grid), g_gui.entry_broker, 1, row++, 1, 1);

    /* MQTT Port */
    gtk_grid_attach(GTK_GRID(grid), gtk_label_new("MQTT Port"), 0, row, 1, 1);
    g_gui.spin_port = gtk_spin_button_new_with_range(1, 65535, 1);
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(g_gui.spin_port), 1883);
    gtk_grid_attach(GTK_GRID(grid), g_gui.spin_port, 1, row++, 1, 1);

    /* MQTT Topic */
    gtk_grid_attach(GTK_GRID(grid), gtk_label_new("MQTT Topic"), 0, row, 1, 1);
    g_gui.entry_topic = gtk_entry_new();
    gtk_entry_set_text(GTK_ENTRY(g_gui.entry_topic),
                       "grey_firmware/sensor/data");
    gtk_widget_set_hexpand(g_gui.entry_topic, TRUE);
    gtk_grid_attach(GTK_GRID(grid), g_gui.entry_topic, 1, row++, 1, 1);

    /* Sensor rate */
    gtk_grid_attach(GTK_GRID(grid), gtk_label_new("Sensor Rate (Hz)"),
                    0, row, 1, 1);
    g_gui.spin_sensor_hz = gtk_spin_button_new_with_range(1, 1000, 1);
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(g_gui.spin_sensor_hz), 10);
    gtk_grid_attach(GTK_GRID(grid), g_gui.spin_sensor_hz, 1, row++, 1, 1);

    gtk_container_add(GTK_CONTAINER(frame), grid);
    return frame;
}

static GtkWidget *build_log_panel(void) {
    GtkWidget *frame = gtk_frame_new("Log Output");
    GtkWidget *vbox  = gtk_box_new(GTK_ORIENTATION_VERTICAL, 4);

    g_gui.log_view   = gtk_text_view_new();
    g_gui.log_buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(g_gui.log_view));
    gtk_text_view_set_editable(GTK_TEXT_VIEW(g_gui.log_view), FALSE);
    gtk_text_view_set_cursor_visible(GTK_TEXT_VIEW(g_gui.log_view), FALSE);
    gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(g_gui.log_view), GTK_WRAP_WORD_CHAR);
    gtk_text_view_set_left_margin(GTK_TEXT_VIEW(g_gui.log_view), 8);
    gtk_text_view_set_right_margin(GTK_TEXT_VIEW(g_gui.log_view), 8);

    GtkWidget *scroll = gtk_scrolled_window_new(NULL, NULL);
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scroll),
                                   GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
    gtk_widget_set_vexpand(scroll, TRUE);
    gtk_widget_set_hexpand(scroll, TRUE);
    gtk_container_add(GTK_CONTAINER(scroll), g_gui.log_view);

    GtkWidget *btn_clear = gtk_button_new_with_label("Clear Log");
    g_signal_connect(btn_clear, "clicked", G_CALLBACK(on_clear_log), NULL);

    gtk_box_pack_start(GTK_BOX(vbox), scroll, TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(vbox), btn_clear, FALSE, FALSE, 4);

    gtk_container_add(GTK_CONTAINER(frame), vbox);
    return frame;
}

/*===========================================================================*/
/* Main Window                                                                */
/*===========================================================================*/

static void build_window(GtkApplication *app) {
    g_gui.window = gtk_application_window_new(app);
    gtk_window_set_title(GTK_WINDOW(g_gui.window), GF_GUI_WINDOW_TITLE);
    gtk_window_set_default_size(GTK_WINDOW(g_gui.window),
                                GF_GUI_WINDOW_WIDTH, GF_GUI_WINDOW_HEIGHT);

    /* Header bar with About button */
    g_gui.header_bar = gtk_header_bar_new();
    gtk_header_bar_set_title(GTK_HEADER_BAR(g_gui.header_bar), "Grey Firmware");
    gtk_header_bar_set_subtitle(GTK_HEADER_BAR(g_gui.header_bar),
                                "Modular Firmware Framework");
    gtk_header_bar_set_show_close_button(GTK_HEADER_BAR(g_gui.header_bar), TRUE);

    GtkWidget *btn_about = gtk_button_new_with_label("About");
    g_signal_connect(btn_about, "clicked", G_CALLBACK(on_about_clicked), NULL);
    gtk_header_bar_pack_end(GTK_HEADER_BAR(g_gui.header_bar), btn_about);
    gtk_window_set_titlebar(GTK_WINDOW(g_gui.window), g_gui.header_bar);

    /* Root layout: horizontal pane */
    GtkWidget *hpaned = gtk_paned_new(GTK_ORIENTATION_HORIZONTAL);
    gtk_paned_set_position(GTK_PANED(hpaned), 340);

    /* Left column: panels stacked vertically */
    GtkWidget *left_box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 8);
    gtk_widget_set_margin_start(left_box, 8);
    gtk_widget_set_margin_top(left_box, 8);
    gtk_widget_set_margin_bottom(left_box, 8);

    gtk_box_pack_start(GTK_BOX(left_box), build_module_panel(), FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(left_box), build_stats_panel(), FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(left_box), build_config_panel(), TRUE, TRUE, 0);

    /* Control buttons */
    GtkWidget *btn_box = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 8);
    gtk_widget_set_margin_start(btn_box, 4);
    gtk_widget_set_margin_end(btn_box, 4);
    gtk_widget_set_margin_bottom(btn_box, 4);

    g_gui.btn_start = gtk_button_new_with_label("▶  Start Demo");
    gtk_style_context_add_class(gtk_widget_get_style_context(g_gui.btn_start),
                                "suggested-action");
    g_signal_connect(g_gui.btn_start, "clicked", G_CALLBACK(on_start_clicked), NULL);

    g_gui.btn_stop = gtk_button_new_with_label("■  Stop");
    gtk_style_context_add_class(gtk_widget_get_style_context(g_gui.btn_stop),
                                "destructive-action");
    gtk_widget_set_sensitive(g_gui.btn_stop, FALSE);
    g_signal_connect(g_gui.btn_stop, "clicked", G_CALLBACK(on_stop_clicked), NULL);

    gtk_box_pack_start(GTK_BOX(btn_box), g_gui.btn_start, TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(btn_box), g_gui.btn_stop, TRUE, TRUE, 0);
    gtk_box_pack_end(GTK_BOX(left_box), btn_box, FALSE, FALSE, 0);

    /* Right column: log panel */
    GtkWidget *right_box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
    gtk_widget_set_margin_end(right_box, 8);
    gtk_widget_set_margin_top(right_box, 8);
    gtk_widget_set_margin_bottom(right_box, 8);
    gtk_box_pack_start(GTK_BOX(right_box), build_log_panel(), TRUE, TRUE, 0);

    gtk_paned_pack1(GTK_PANED(hpaned), left_box, FALSE, FALSE);
    gtk_paned_pack2(GTK_PANED(hpaned), right_box, TRUE, TRUE);

    gtk_container_add(GTK_CONTAINER(g_gui.window), hpaned);

    /* Refresh timer */
    g_gui.refresh_timer_id =
        g_timeout_add(GF_GUI_REFRESH_MS, on_refresh_timer, NULL);

    /* Initial log entry */
    gui_append_log("INFO", "Grey Firmware GUI started");
    gui_append_log("INFO", "Select demo mode and press Start");

    gtk_widget_show_all(g_gui.window);
}

/*===========================================================================*/
/* Application Activate                                                       */
/*===========================================================================*/

static void on_activate(GtkApplication *app, gpointer data) {
    (void)data;
    apply_css();
    build_window(app);
}

/*===========================================================================*/
/* Public API                                                                 */
/*===========================================================================*/

int gf_gui_run(int *argc, char ***argv) {
    if (g_gui.initialized) return GF_GUI_ERR_ALREADY_RUNNING;
    g_gui.initialized = TRUE;

    memset(&g_gui.demo_cfg, 0, sizeof(g_gui.demo_cfg));

    GtkApplication *app = gtk_application_new("org.greyfirmware.gui",
                                               G_APPLICATION_DEFAULT_FLAGS);
    g_signal_connect(app, "activate", G_CALLBACK(on_activate), NULL);

    int status = g_application_run(G_APPLICATION(app), *argc, *argv);
    g_object_unref(app);

    if (g_gui.demo_running)
        gf_demo_stop();

    g_gui.initialized = FALSE;
    return status;
}

void gf_gui_log(const char *level, const char *message) {
    if (g_gui.log_buffer)
        gui_append_log(level, message);
}

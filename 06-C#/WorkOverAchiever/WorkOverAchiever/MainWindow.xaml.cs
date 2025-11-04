using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Runtime.InteropServices.WindowsRuntime;
using System.Globalization;
using System.Diagnostics;
using Windows.Storage;
using Microsoft.UI.Xaml;
using Microsoft.UI.Xaml.Controls;
using Microsoft.UI.Xaml.Controls.Primitives;
using Microsoft.UI.Xaml.Data;
using Microsoft.UI.Xaml.Input;
using Microsoft.UI.Xaml.Media;
using Windows.UI;
using System.Runtime.InteropServices; // for DllImport
using WinRT.Interop; // for WindowNative.GetWindowHandle

// To learn more about WinUI, the WinUI project structure,
// and more about our project templates, see: http://aka.ms/winui-project-info.

namespace WorkOverAchiever
{
    /// <summary>
    /// An empty window that can be used on its own or navigated to within a Frame.
    /// </summary>
    public sealed partial class MainWindow : Window
    {
        // Interop for setting native window icon
        [DllImport("user32.dll", CharSet = CharSet.Unicode, SetLastError = true)]
        private static extern IntPtr SendMessage(IntPtr hWnd, uint Msg, IntPtr wParam, IntPtr lParam);

        [DllImport("user32.dll", SetLastError = true)]
        private static extern bool DestroyIcon(IntPtr hIcon);

        private const uint WM_SETICON = 0x0080;
        private static readonly IntPtr ICON_SMALL = new IntPtr(0);
        private static readonly IntPtr ICON_BIG = new IntPtr(1);

        private IntPtr _hIcon = IntPtr.Zero;

        private const string ThemeSettingKey = "AppTheme"; // "Light" or "Dark" or "Default"

        public MainWindow()
        {
            try
            {
                InitializeComponent();

                // Try to set the native window icon from the packaged asset (if present)
                try { SetWindowIcon(); } catch { }
                // Ensure we destroy the HICON when window closes
                this.Closed += MainWindow_Closed;

                // Apply WindowBackgroundBrush from app resources if available (runtime fallback)
                try
                {
                    if (Application.Current?.Resources != null && Application.Current.Resources.ContainsKey("WindowBackgroundBrush"))
                    {
                        var brushObj = Application.Current.Resources["WindowBackgroundBrush"] as Brush;
                        if (brushObj != null && RootGrid != null)
                        {
                            RootGrid.Background = brushObj;
                        }
                    }
                }
                catch
                {
                    // ignore any fallback assignment errors
                }

                // Subscribe to the TabView Loaded event so we can hide template buttons like the Overflow (three-dot)
                MainTabView.Loaded += MainTabView_Loaded;

                // Restore saved theme
                RestoreThemeFromSettings();
            }
            catch (Exception ex)
            {
                try
                {
                    var path = global::System.IO.Path.Combine(global::System.IO.Path.GetTempPath(), "WorkOverAchiever_MainWindow_startup_error.txt");
                    global::System.IO.File.WriteAllText(path, ex.ToString());
                    Debug.WriteLine($"MainWindow startup error written to: {path}");
                }
                catch
                {
                    // ignore logging failures
                }

                // rethrow so Visual Studio breaks as before
                throw;
            }
        }

        private void MainWindow_Closed(object sender, WindowEventArgs e)
        {
            try
            {
                if (_hIcon != IntPtr.Zero)
                {
                    DestroyIcon(_hIcon);
                    _hIcon = IntPtr.Zero;
                }
            }
            catch { }
        }

        private void SetWindowIcon()
        {
            try
            {
                // Locate the packaged asset. AppContext.BaseDirectory points to the folder where the app is running.
                var path = Path.Combine(AppContext.BaseDirectory ?? string.Empty, "Assets", "WorkOverAchieverLogo.png");
                if (!File.Exists(path)) return;

                // Load bitmap and create an HICON using System.Drawing.Bitmap without adding a global using to avoid clashes
                using (var bmp = new System.Drawing.Bitmap(path))
                {
                    IntPtr hIcon = bmp.GetHicon();
                    if (hIcon != IntPtr.Zero)
                    {
                        var hwnd = WindowNative.GetWindowHandle(this);
                        // Set both small and large icons so the titlebar and taskbar use it where appropriate
                        SendMessage(hwnd, WM_SETICON, ICON_SMALL, hIcon);
                        SendMessage(hwnd, WM_SETICON, ICON_BIG, hIcon);
                        // Keep handle to destroy later when window closes
                        _hIcon = hIcon;
                    }
                }
            }
            catch (Exception ex)
            {
                Debug.WriteLine("SetWindowIcon failed: " + ex);
            }
        }

        private void MainTabView_Loaded(object sender, RoutedEventArgs e)
        {
            try
            {
                // Diagnostic dump to help identify any remaining 'See more' / overflow controls
#if DEBUG
                try
                {
                    LogPotentialOverflowElements(MainTabView);
                }
                catch { }
#endif

                // First attempt immediately
                TryHideTemplateButtons(MainTabView);

                // Also attempt specialized removal for any "See more" leftovers
                try { RemoveSeeMoreButtons(); } catch { }

                // Also attach a one-time LayoutUpdated handler to catch buttons that appear after template application
                void OnLayoutUpdated(object s, object args)
                {
                    try
                    {
                        TryHideTemplateButtons(MainTabView);
                        RemoveSeeMoreButtons();
                    }
                    catch
                    {
                        // ignore
                    }
                    finally
                    {
                        // detach after first run to avoid performance impact
                        try
                        {
                            MainTabView.LayoutUpdated -= OnLayoutUpdated;
                        }
                        catch
                        {
                            // ignore
                        }
                    }
                }

                MainTabView.LayoutUpdated += OnLayoutUpdated;
            }
            catch
            {
                // ignore
            }
        }

        // Stronger removal: find any element whose automation name, tooltip, text or content mentions 'see more'/'more'/ellipsis and collapse nearest clickable ancestor
        private void RemoveSeeMoreButtons()
        {
            try
            {
                DependencyObject root = this.Content as DependencyObject ?? MainTabView as DependencyObject;
                if (root == null) return;

                var matches = new List<FrameworkElement>();

                void Walk(DependencyObject node)
                {
                    if (node == null) return;
                    int cnt = VisualTreeHelper.GetChildrenCount(node);
                    for (int i = 0; i < cnt; i++)
                    {
                        var child = VisualTreeHelper.GetChild(node, i);
                        try
                        {
                            if (child is FrameworkElement fe)
                            {
                                string auto = string.Empty;
                                try { auto = fe.GetValue(Microsoft.UI.Xaml.Automation.AutomationProperties.NameProperty) as string ?? string.Empty; } catch { }
                                string tt = string.Empty;
                                try { tt = fe.GetValue(ToolTipService.ToolTipProperty)?.ToString() ?? string.Empty; } catch { }
                                string content = string.Empty;
                                try { content = (fe as Button)?.Content?.ToString() ?? string.Empty; } catch { }
                                string text = string.Empty;
                                try { text = (fe as TextBlock)?.Text ?? string.Empty; } catch { }

                                var combined = string.Join("|", new[] { auto, tt, content, text }).ToLowerInvariant();
                                if (!string.IsNullOrWhiteSpace(combined))
                                {
                                    if (combined.Contains("see more") || combined.Contains("see more actions") || combined.Contains("see more actions") || combined.Contains("more options") || combined.Contains("more") || combined.Contains("overflow") || combined.Contains("...") || combined.Contains("…"))
                                    {
                                        matches.Add(fe);
                                    }
                                }
                            }
                        }
                        catch { }

                        Walk(child);
                    }
                }

                Walk(root);

                foreach (var fe in matches)
                {
                    try
                    {
                        // find nearest clickable ancestor (Button, AppBarButton, Control with AppBar in type name)
                        DependencyObject cur = fe;
                        FrameworkElement toCollapse = null;
                        for (int depth = 0; depth < 6 && cur != null; depth++)
                        {
                            if (cur is Button b) { toCollapse = b; break; }
                            if (cur is FrameworkElement f && f.GetType().Name.IndexOf("AppBar", StringComparison.OrdinalIgnoreCase) >= 0) { toCollapse = f; break; }
                            if (cur is FrameworkElement f2 && f2.GetType().Name.IndexOf("Button", StringComparison.OrdinalIgnoreCase) >= 0) { toCollapse = f2; break; }
                            cur = VisualTreeHelper.GetParent(cur);
                        }

                        if (toCollapse == null)
                        {
                            // fallback: collapse the element itself
                            toCollapse = fe;
                        }

                        toCollapse.Visibility = Visibility.Collapsed;
                        Debug.WriteLine($"RemoveSeeMoreButtons: collapsed {toCollapse.GetType().Name} (Name='{toCollapse.Name}')");
                    }
                    catch { }
                }
            }
            catch
            {
                // ignore
            }
        }

        private void LogPotentialOverflowElements(DependencyObject root)
        {
            try
            {
                var results = new System.Text.StringBuilder();
                void Walk(DependencyObject node, string path)
                {
                    if (node == null) return;
                    int cnt = VisualTreeHelper.GetChildrenCount(node);
                    for (int i = 0; i < cnt; i++)
                    {
                        var child = VisualTreeHelper.GetChild(node, i);
                        var childPath = path + "/" + child.GetType().Name + (child is FrameworkElement fe && !string.IsNullOrEmpty(fe.Name) ? "[" + fe.Name + "]" : string.Empty);
                        try
                        {
                            if (child is FrameworkElement childFe)
                            {
                                string info = null;
                                try
                                {
                                    var name = childFe.Name ?? string.Empty;
                                    var auto = childFe.GetValue(Microsoft.UI.Xaml.Automation.AutomationProperties.NameProperty) as string ?? string.Empty;
                                    var tt = childFe.GetValue(ToolTipService.ToolTipProperty)?.ToString() ?? string.Empty;
                                    var content = (childFe as Button)?.Content?.ToString() ?? string.Empty;
                                    var text = (childFe as TextBlock)?.Text ?? string.Empty;

                                    var combined = string.Join(" | ", new[] { name, auto, tt, content, text }).Trim();
                                    if (!string.IsNullOrWhiteSpace(combined))
                                    {
                                        var cLower = combined.ToLowerInvariant();
                                        if (cLower.Contains("see more") || cLower.Contains("see more actions") || cLower.Contains("more") || cLower.Contains("overflow") || cLower.Contains("...") || cLower.Contains("…"))
                                        {
                                            info = $"MATCH: {childPath} -> {combined}";
                                        }
                                        else
                                        {
                                            // also report items that are AppBarButton or have Icon types
                                            if (childFe.GetType().Name.IndexOf("AppBar", StringComparison.OrdinalIgnoreCase) >= 0 || childFe.GetType().Name.IndexOf("Button", StringComparison.OrdinalIgnoreCase) >= 0)
                                            {
                                                // check Icon property if present
                                                var iconProp = childFe.GetType().GetProperty("Icon");
                                                if (iconProp != null)
                                                {
                                                    var iconVal = iconProp.GetValue(childFe);
                                                    if (iconVal != null)
                                                    {
                                                        var iconType = iconVal.GetType().Name;
                                                        info = $"POTENTIAL ICON: {childPath} icon={iconType} content={content} tooltip={tt} automation={auto}";
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                                catch { }

                                if (info != null)
                                {
                                    results.AppendLine(info);
                                    Debug.WriteLine(info);
                                }
                            }
                        }
                        catch { }

                        Walk(child, childPath);
                    }
                }

                Walk(root, root.GetType().Name);

                try
                {
                    var path = System.IO.Path.Combine(System.IO.Path.GetTempPath(), "WorkOverAchiever_TabView_visual_tree.txt");
                    System.IO.File.WriteAllText(path, results.ToString());
                    Debug.WriteLine("Visual tree dump written to: " + path);
                }
                catch { }
            }
            catch
            {
                // ignore
            }
        }

        private void TryHideTemplateButtons(DependencyObject root)
        {
            if (root == null) return;

            var targetNames = new[] { "AddButton", "AddTabButton", "MoreButton", "OverflowButton", "TabViewAddButton", "TabViewOverflowButton" };

            int count = VisualTreeHelper.GetChildrenCount(root);
            for (int i = 0; i < count; i++)
            {
                var child = VisualTreeHelper.GetChild(root, i);
                try
                {
                    // If a TextBlock child contains visible 'See more' / ellipsis text, collapse its parent (often the clickable control)
                    if (child is TextBlock tb)
                    {
                        try
                        {
                            var txt = tb.Text ?? string.Empty;
                            if (!string.IsNullOrWhiteSpace(txt))
                            {
                                var tnorm = txt.Trim();
                                if (tnorm.IndexOf("See more", StringComparison.OrdinalIgnoreCase) >= 0 || tnorm.IndexOf("See more actions", StringComparison.OrdinalIgnoreCase) >= 0 || tnorm.IndexOf("More", StringComparison.OrdinalIgnoreCase) >= 0 || tnorm.Contains("...") || tnorm.Contains("…"))
                                {
                                    // collapse the clickable parent if present
                                    var parent = VisualTreeHelper.GetParent(tb) as FrameworkElement;
                                    if (parent != null)
                                    {
                                        parent.Visibility = Visibility.Collapsed;
                                    }
                                    else
                                    {
                                        tb.Visibility = Visibility.Collapsed;
                                    }
                                }
                            }
                        }
                        catch
                        {
                            // ignore
                        }
                    }

                    if (child is FrameworkElement fe)
                    {
                        // Hide by exact or partial name match
                        if (!string.IsNullOrEmpty(fe.Name))
                        {
                            var name = fe.Name;
                            if (targetNames.Contains(name) || name.IndexOf("Overflow", StringComparison.OrdinalIgnoreCase) >= 0 || name.IndexOf("More", StringComparison.OrdinalIgnoreCase) >= 0 || name.IndexOf("Add", StringComparison.OrdinalIgnoreCase) >= 0)
                            {
                                fe.Visibility = Visibility.Collapsed;
                            }
                        }

                        // Check AutomationProperties.Name or ToolTip for overflow indicators
                        try
                        {
                            var automationName = fe.GetValue(Microsoft.UI.Xaml.Automation.AutomationProperties.NameProperty) as string;
                            if (!string.IsNullOrWhiteSpace(automationName))
                            {
                                var an = automationName.Trim();
                                if (an.IndexOf("More", StringComparison.OrdinalIgnoreCase) >= 0 || an.IndexOf("Overflow", StringComparison.OrdinalIgnoreCase) >= 0 || an.IndexOf("More options", StringComparison.OrdinalIgnoreCase) >= 0 || an.IndexOf("See more", StringComparison.OrdinalIgnoreCase) >= 0)
                                {
                                    fe.Visibility = Visibility.Collapsed;
                                }
                            }

                            var tt = fe.GetValue(ToolTipService.ToolTipProperty);
                            var ttStr = tt?.ToString() ?? string.Empty;
                            if (!string.IsNullOrWhiteSpace(ttStr))
                            {
                                if (ttStr.IndexOf("More", StringComparison.OrdinalIgnoreCase) >= 0 || ttStr.IndexOf("Overflow", StringComparison.OrdinalIgnoreCase) >= 0 || ttStr.IndexOf("See more", StringComparison.OrdinalIgnoreCase) >= 0)
                                {
                                    fe.Visibility = Visibility.Collapsed;
                                }
                            }
                        }
                        catch
                        {
                            // ignore
                        }

                        // Check for Button/AppBarButton content being ellipsis or 'More' or 'See more'
                        if (fe is Button btn)
                        {
                            try
                            {
                                var contentStr = btn.Content?.ToString() ?? string.Empty;
                                if (!string.IsNullOrWhiteSpace(contentStr))
                                {
                                    var t = contentStr.Trim();
                                    // check ASCII and Unicode ellipsis and common labels
                                    if (t == "..." || t == "\u001f50D" || t == "?" || t.Equals("More", StringComparison.OrdinalIgnoreCase) || t.Equals("More options", StringComparison.OrdinalIgnoreCase) || t.Contains("…") || t.IndexOf("See more", StringComparison.OrdinalIgnoreCase) >= 0)
                                    {
                                        btn.Visibility = Visibility.Collapsed;
                                    }
                                }

                                // Try to detect Icon that indicates overflow (SymbolIcon/FontIcon/PathIcon)
                                var iconProp = btn.GetType().GetProperty("Icon");
                                if (iconProp != null)
                                {
                                    var iconVal = iconProp.GetValue(btn);
                                    if (iconVal != null)
                                    {
                                        var iconTypeName = iconVal.GetType().Name;
                                        if (iconTypeName.IndexOf("SymbolIcon", StringComparison.OrdinalIgnoreCase) >= 0 || iconTypeName.IndexOf("FontIcon", StringComparison.OrdinalIgnoreCase) >= 0 || iconTypeName.IndexOf("PathIcon", StringComparison.OrdinalIgnoreCase) >= 0)
                                        {
                                            // Additional checks: Glyph or Symbol properties
                                            try
                                            {
                                                var glyphProp = iconVal.GetType().GetProperty("Glyph");
                                                if (glyphProp != null)
                                                {
                                                    var glyph = glyphProp.GetValue(iconVal) as string ?? string.Empty;
                                                    if (!string.IsNullOrWhiteSpace(glyph))
                                                    {
                                                        if (glyph.Contains("...") || glyph.Contains("…") || glyph.IndexOf("More", StringComparison.OrdinalIgnoreCase) >= 0 || glyph.IndexOf("See more", StringComparison.OrdinalIgnoreCase) >= 0)
                                                        {
                                                            btn.Visibility = Visibility.Collapsed;
                                                        }
                                                    }
                                                }

                                                var symProp = iconVal.GetType().GetProperty("Symbol");
                                                if (symProp != null)
                                                {
                                                    var symVal = symProp.GetValue(iconVal);
                                                    if (symVal != null && symVal.ToString().IndexOf("More", StringComparison.OrdinalIgnoreCase) >= 0)
                                                    {
                                                        btn.Visibility = Visibility.Collapsed;
                                                    }
                                                }
                                            }
                                            catch
                                            {
                                                // ignore
                                            }

                                            // Fallback: collapse if icon type suggests it's an overflow glyph
                                            btn.Visibility = Visibility.Collapsed;
                                        }
                                    }
                                }
                            }
                            catch
                            {
                                // ignore per-button errors
                            }
                        }

                        // Some controls might be AppBarButton (derived from ButtonBase) - treat similarly
                        if (fe.GetType().Name.IndexOf("AppBar", StringComparison.OrdinalIgnoreCase) >= 0 && fe is Control ctrl)
                        {
                            try
                            {
                                // collapse if name indicates overflow/add
                                if (!string.IsNullOrEmpty(ctrl.Name) && (ctrl.Name.IndexOf("Overflow", StringComparison.OrdinalIgnoreCase) >= 0 || ctrl.Name.IndexOf("More", StringComparison.OrdinalIgnoreCase) >= 0 || ctrl.Name.IndexOf("Add", StringComparison.OrdinalIgnoreCase) >= 0))
                                {
                                    ctrl.Visibility = Visibility.Collapsed;
                                }

                                // check Icon property via reflection
                                var iconProp = ctrl.GetType().GetProperty("Icon");
                                if (iconProp != null)
                                {
                                    var iconVal = iconProp.GetValue(ctrl);
                                    if (iconVal != null)
                                    {
                                        try
                                        {
                                            var glyphProp = iconVal.GetType().GetProperty("Glyph");
                                            if (glyphProp != null)
                                            {
                                                var glyph = glyphProp.GetValue(iconVal) as string ?? string.Empty;
                                                if (!string.IsNullOrWhiteSpace(glyph) && (glyph.Contains("...") || glyph.Contains("…") || glyph.IndexOf("More", StringComparison.OrdinalIgnoreCase) >= 0 || glyph.IndexOf("See more", StringComparison.OrdinalIgnoreCase) >= 0))
                                                {
                                                    ctrl.Visibility = Visibility.Collapsed;
                                                }
                                            }

                                            var symProp = iconVal.GetType().GetProperty("Symbol");
                                            if (symProp != null)
                                            {
                                                var symVal = symProp.GetValue(iconVal);
                                                if (symVal != null && symVal.ToString().IndexOf("More", StringComparison.OrdinalIgnoreCase) >= 0)
                                                {
                                                    ctrl.Visibility = Visibility.Collapsed;
                                                }
                                            }
                                        }
                                        catch
                                        {
                                            // ignore
                                        }

                                        // if the icon type name strongly indicates it's an overflow glyph, collapse
                                        var iconTypeName = iconVal.GetType().Name;
                                        if (iconTypeName.IndexOf("SymbolIcon", StringComparison.OrdinalIgnoreCase) >= 0 || iconTypeName.IndexOf("FontIcon", StringComparison.OrdinalIgnoreCase) >= 0 || iconTypeName.IndexOf("PathIcon", StringComparison.OrdinalIgnoreCase) >= 0)
                                        {
                                            ctrl.Visibility = Visibility.Collapsed;
                                        }
                                    }
                                }
                            }
                            catch
                            {
                                // ignore
                            }
                        }
                    }
                }
                catch
                {
                    // ignore per-node errors
                }

                // recurse
                TryHideTemplateButtons(child);
            }
        }

        private void RestoreThemeFromSettings()
        {
            try
            {
                var localSettings = ApplicationData.Current.LocalSettings;
                if (localSettings.Values.TryGetValue(ThemeSettingKey, out var raw) && raw is string s)
                {
                    var toggle = ThemeToggle; // code-behind x:Name reference
                    if (s == "Dark")
                    {
                        SetTheme(ApplicationTheme.Dark);
                        if (toggle != null) toggle.IsChecked = true;
                    }
                    else if (s == "Light")
                    {
                        SetTheme(ApplicationTheme.Light);
                        if (toggle != null) toggle.IsChecked = false;
                    }
                }
                else
                {
                    // Ensure toggle text reflects current app theme even if not in settings
                    try
                    {
                        if (Application.Current is Application app)
                        {
                            var current = app.RequestedTheme == ApplicationTheme.Dark ? ApplicationTheme.Dark : ApplicationTheme.Light;
                            UpdateThemeToggleContent(current);
                        }
                    }
                    catch
                    {
                        // ignore
                    }
                }
            }
            catch
            {
                // ignore persistence errors
            }
        }

        private void SaveThemeToSettings(string value)
        {
            try
            {
                var localSettings = ApplicationData.Current.LocalSettings;
                localSettings.Values[ThemeSettingKey] = value;
            }
            catch
            {
                // ignore
            }
        }

        private void SetTheme(ApplicationTheme theme)
        {
            try
            {
                // For immediate UI feedback set the RootGrid.RequestedTheme
                if (RootGrid != null)
                {
                    RootGrid.RequestedTheme = theme == ApplicationTheme.Dark ? ElementTheme.Dark : ElementTheme.Light;
                }

                // Also set the Window's RequestedTheme so controls that derive theme at the window level update
                try
                {
                    // Window itself doesn't expose RequestedTheme; set it on the root content if available
                    if (this.Content is FrameworkElement feWindowContent)
                    {
                        feWindowContent.RequestedTheme = theme == ApplicationTheme.Dark ? ElementTheme.Dark : ElementTheme.Light;
                    }
                }
                catch
                {
                    // ignore if we cannot set the window content theme in some hosting scenarios
                }

                // Also set TabView requested theme to ensure its template/theme resources update
                try
                {
                    if (MainTabView != null) MainTabView.RequestedTheme = theme == ApplicationTheme.Dark ? ElementTheme.Dark : ElementTheme.Light;
                }
                catch
                {
                    // ignore
                }

                // Propagate RequestedTheme to visual tree elements to force theme refresh where ThemeResource doesn't update immediately
                try
                {
                    var elementTheme = theme == ApplicationTheme.Dark ? ElementTheme.Dark : ElementTheme.Light;
                    ApplyRequestedThemeToVisualTree(elementTheme);
                }
                catch
                {
                    // ignore
                }

                // Also set an app-level resource so other resources can respond if needed
                if (Application.Current is Application app)
                {
                    // Ensure the application's RequestedTheme is set so ThemeResource/theme dictionaries switch correctly
                    try
                    {
                        app.RequestedTheme = theme;
                    }
                    catch
                    {
                        // some hosting scenarios might not allow changing the app theme at runtime
                    }

                    app.Resources["AppRequestedTheme"] = theme == ApplicationTheme.Dark ? "Dark" : "Light";

                    // Use absolute brush values for all relevant resources so inputs and foreground update immediately
                    if (theme == ApplicationTheme.Dark)
                    {
                        // Backgrounds and foregrounds for dark (exact values per request)
                        var windowBg = new SolidColorBrush(Color.FromArgb(0xFF,0x00,0x00,0x00)); // black
                        var pageText = new SolidColorBrush(Color.FromArgb(0xFF,0xFF,0xFF,0xFF)); // white
                        var controlBg = new SolidColorBrush(Color.FromArgb(0xFF,0x00,0x00,0x00)); // black
                        var controlBorder = new SolidColorBrush(Color.FromArgb(0xFF,0x3A,0x3A,0x3A));
                        var primaryBtnBg = new SolidColorBrush(Color.FromArgb(0xFF,0x0A,0x84,0xFF));
                        var primaryBtnFg = new SolidColorBrush(Color.FromArgb(0xFF,0xFF,0xFF,0xFF));
                        var secondaryBtnBg = new SolidColorBrush(Color.FromArgb(0xFF,0x3A,0x3A,0x3A));
                        var secondaryBtnFg = new SolidColorBrush(Color.FromArgb(0xFF,0xF3,0xF3,0xF3));
                        var resultText = new SolidColorBrush(Color.FromArgb(0xFF,0xFF,0xFF,0xFF)); // white
                        var textControlFg = pageText; // text in TextBox/TextBlock
                        var textControlBg = controlBg; // textbox background

                        Debug.WriteLine($"SetTheme(Dark) assigning brushes: WindowBg={windowBg.Color}, PageText={pageText.Color}, ControlBg={controlBg.Color}, ControlBorder={controlBorder.Color}");

                        app.Resources["WindowBackgroundBrush"] = windowBg;
                        app.Resources["PageTextBrush"] = pageText;
                        app.Resources["ControlBackgroundBrush"] = controlBg;
                        app.Resources["ControlBorderBrush"] = controlBorder;
                        app.Resources["PrimaryButtonBackgroundBrush"] = primaryBtnBg;
                        app.Resources["PrimaryButtonForegroundBrush"] = primaryBtnFg;
                        app.Resources["SecondaryButtonBackgroundBrush"] = secondaryBtnBg;
                        app.Resources["SecondaryButtonForegroundBrush"] = secondaryBtnFg;
                        app.Resources["ResultTextBrush"] = resultText;
                        app.Resources["TextControlForeground"] = textControlFg;
                        app.Resources["TextControlBackground"] = textControlBg;
                    }
                    else
                    {
                        var windowBg = new SolidColorBrush(Color.FromArgb(0xFF,0xFF,0xFF,0xFF)); // white
                        var pageText = new SolidColorBrush(Color.FromArgb(0xFF,0x00,0x00,0x00)); // black
                        var controlBg = new SolidColorBrush(Color.FromArgb(0xFF,0xFF,0xFF,0xFF)); // white
                        var controlBorder = new SolidColorBrush(Color.FromArgb(0xFF,0xCC,0xCC,0xCC));
                        var primaryBtnBg = new SolidColorBrush(Color.FromArgb(0xFF,0x00,0x78,0xD4));
                        var primaryBtnFg = new SolidColorBrush(Color.FromArgb(0xFF,0xFF,0xFF,0xFF));
                        var secondaryBtnBg = new SolidColorBrush(Color.FromArgb(0xFF,0xE0,0xE0,0xE0));
                        var secondaryBtnFg = new SolidColorBrush(Color.FromArgb(0xFF,0x11,0x11,0x11));
                        var resultText = new SolidColorBrush(Color.FromArgb(0xFF,0x00,0x00,0x00));
                        var textControlFg = pageText;
                        var textControlBg = controlBg;

                        Debug.WriteLine($"SetTheme(Light) assigning brushes: WindowBg={windowBg.Color}, PageText={pageText.Color}, ControlBg={controlBg.Color}, ControlBorder={controlBorder.Color}");

                        app.Resources["WindowBackgroundBrush"] = windowBg;
                        app.Resources["PageTextBrush"] = pageText;
                        app.Resources["ControlBackgroundBrush"] = controlBg;
                        app.Resources["ControlBorderBrush"] = controlBorder;
                        app.Resources["PrimaryButtonBackgroundBrush"] = primaryBtnBg;
                        app.Resources["PrimaryButtonForegroundBrush"] = primaryBtnFg;
                        app.Resources["SecondaryButtonBackgroundBrush"] = secondaryBtnBg;
                        app.Resources["SecondaryButtonForegroundBrush"] = secondaryBtnFg;
                        app.Resources["ResultTextBrush"] = resultText;
                        app.Resources["TextControlForeground"] = textControlFg;
                        app.Resources["TextControlBackground"] = textControlBg;
                    }

                    // Apply background and input foreground/background immediately to RootGrid and to input fields
                    try
                    {
                        if (app.Resources.ContainsKey("WindowBackgroundBrush") && RootGrid != null)
                        {
                            var brush = app.Resources["WindowBackgroundBrush"] as Brush;
                            if (brush != null) RootGrid.Background = brush;
                        }

                        Debug.WriteLine($"After applying: RootGrid.Background={(RootGrid?.Background as SolidColorBrush)?.Color.ToString() ?? "null"}");

                        // Set explicit TextBox foreground/background for the visible inputs to ensure immediate update
                        TryApplyInputBrushesFromAppResources(app);

                        // Also update known result text blocks explicitly
                        if (app.Resources.ContainsKey("ResultTextBrush"))
                        {
                            var resFg = app.Resources["ResultTextBrush"] as Brush;
                            if (resFg != null)
                            {
                                if (ResultTextBlock != null) ResultTextBlock.Foreground = resFg;
                                if (StudentResultTextBlock != null) StudentResultTextBlock.Foreground = resFg;
                                Debug.WriteLine($"After applying: ResultTextBlock.Foreground={(ResultTextBlock?.Foreground as SolidColorBrush)?.Color.ToString() ?? "null"}");
                            }
                        }

                        // Update borders that use ControlBorderBrush explicitly
                        if (app.Resources.ContainsKey("ControlBorderBrush"))
                        {
                            var borderBrush = app.Resources["ControlBorderBrush"] as Brush;
                            if (borderBrush != null)
                            {
                                TryApplyBorderBrushToChildBorders(MainTabView, borderBrush);
                                Debug.WriteLine($"After applying: Borders updated with ControlBorderBrush={(borderBrush as SolidColorBrush)?.Color.ToString()}");
                            }
                        }

                        // Ensure TextBlocks, Buttons, and other elements get explicit brushes so contrast is correct
                        TryApplyThemeBrushesToVisualTree(app);
                    }
                    catch (Exception ex)
                    {
                        Debug.WriteLine("SetTheme apply error: " + ex);
                    }
                }

                // Update the toggle content so the user sees the active theme text
                try
                {
                    UpdateThemeToggleContent(theme);
                }
                catch
                {
                    // ignore
                }
            }
            catch (Exception ex)
            {
                Debug.WriteLine("SetTheme error: " + ex);
            }
        }

        private void TryApplyThemeBrushesToVisualTree(Application app)
        {
            try
            {
                if (app == null) return;

                Brush? pageBrush = null;
                Brush? controlBg = null;
                Brush? controlBorder = null;
                Brush? primaryBtnBg = null;
                Brush? primaryBtnFg = null;
                Brush? textCtrlFg = null;
                Brush? textCtrlBg = null;

                if (app.Resources.ContainsKey("PageTextBrush")) pageBrush = app.Resources["PageTextBrush"] as Brush;
                if (app.Resources.ContainsKey("ControlBackgroundBrush")) controlBg = app.Resources["ControlBackgroundBrush"] as Brush;
                if (app.Resources.ContainsKey("ControlBorderBrush")) controlBorder = app.Resources["ControlBorderBrush"] as Brush;
                if (app.Resources.ContainsKey("PrimaryButtonBackgroundBrush")) primaryBtnBg = app.Resources["PrimaryButtonBackgroundBrush"] as Brush;
                if (app.Resources.ContainsKey("PrimaryButtonForegroundBrush")) primaryBtnFg = app.Resources["PrimaryButtonForegroundBrush"] as Brush;
                if (app.Resources.ContainsKey("TextControlForeground")) textCtrlFg = app.Resources["TextControlForeground"] as Brush;
                if (app.Resources.ContainsKey("TextControlBackground")) textCtrlBg = app.Resources["TextControlBackground"] as Brush;

                if (this.Content is DependencyObject root)
                {
                    ApplyBrushesRecursive(root, pageBrush, controlBg, controlBorder, primaryBtnBg, primaryBtnFg, textCtrlFg, textCtrlBg);
                }
            }
            catch
            {
                // ignore
            }
        }

        private void ApplyBrushesRecursive(DependencyObject node, Brush? pageBrush, Brush? controlBg, Brush? controlBorder, Brush? primaryBtnBg, Brush? primaryBtnFg, Brush? textCtrlFg, Brush? textCtrlBg)
        {
            if (node == null) return;
            int count = VisualTreeHelper.GetChildrenCount(node);
            for (int i = 0; i < count; i++)
            {
                var child = VisualTreeHelper.GetChild(node, i);
                try
                {
                    if (child is TextBlock tb && pageBrush != null)
                    {
                        tb.Foreground = pageBrush;
                    }
                    else if (child is Border b)
                    {
                        if (controlBg != null) b.Background = controlBg;
                        if (controlBorder != null) b.BorderBrush = controlBorder;
                    }
                    else if (child is Button btn)
                    {
                        if (primaryBtnBg != null) btn.Background = primaryBtnBg;
                        if (primaryBtnFg != null) btn.Foreground = primaryBtnFg;
                    }
                    else if (child is TextBox txt)
                    {
                        if (textCtrlFg != null) txt.Foreground = textCtrlFg;
                        if (textCtrlBg != null) txt.Background = textCtrlBg;
                    }
                }
                catch
                {
                    // ignore individual assignment failures
                }

                // recurse
                ApplyBrushesRecursive(child, pageBrush, controlBg, controlBorder, primaryBtnBg, primaryBtnFg, textCtrlFg, textCtrlBg);
            }
        }

        private static SolidColorBrush GetContrastingBrush(Color bgColor)
        {
            // Compute perceived brightness using standard NTSC formula
            double brightness = (0.299 * bgColor.R +0.587 * bgColor.G +0.114 * bgColor.B) /255.0;
            // If background is bright, return black; otherwise white
            return brightness >0.5 ? new SolidColorBrush(Color.FromArgb(0xFF,0x00,0x00,0x00)) : new SolidColorBrush(Color.FromArgb(0xFF,0xFF,0xFF,0xFF));
        }

        private void TryApplyInputBrushesFromAppResources(Application app)
        {
            try
            {
                Brush? fg = null;
                Brush? bg = null;
                if (app.Resources.ContainsKey("TextControlForeground")) fg = app.Resources["TextControlForeground"] as Brush;
                if (app.Resources.ContainsKey("TextControlBackground")) bg = app.Resources["TextControlBackground"] as Brush;

                // Apply to known input controls if they exist to force immediate visual update
                if (fg != null)
                {
                    if (CurrentSalaryTextBox != null) CurrentSalaryTextBox.Foreground = fg;
                    if (HoursPerWeekTextBox != null) HoursPerWeekTextBox.Foreground = fg;
                    if (GoalSalaryTextBox != null) GoalSalaryTextBox.Foreground = fg;
                    if (StudentWeeklyTimeTextBox != null) StudentWeeklyTimeTextBox.Foreground = fg;
                    if (StudentCurrentGpaTextBox != null) StudentCurrentGpaTextBox.Foreground = fg;
                    if (StudentCurrentCreditsTextBox != null) StudentCurrentCreditsTextBox.Foreground = fg;
                    if (StudentDesiredGpaTextBox != null) StudentDesiredGpaTextBox.Foreground = fg;
                    if (StudentDesiredCreditsTextBox != null) StudentDesiredCreditsTextBox.Foreground = fg;
                    if (StudentCurrentYearTextBox != null) StudentCurrentYearTextBox.Foreground = fg;
                    if (StudentLastYearTextBox != null) StudentLastYearTextBox.Foreground = fg;
                    if (StudentCreditsPerSemesterTextBox != null) StudentCreditsPerSemesterTextBox.Foreground = fg;
                }
                if (bg != null)
                {
                    if (CurrentSalaryTextBox != null) CurrentSalaryTextBox.Background = bg;
                    if (HoursPerWeekTextBox != null) HoursPerWeekTextBox.Background = bg;
                    if (GoalSalaryTextBox != null) GoalSalaryTextBox.Background = bg;
                    if (StudentWeeklyTimeTextBox != null) StudentWeeklyTimeTextBox.Background = bg;
                    if (StudentCurrentGpaTextBox != null) StudentCurrentGpaTextBox.Background = bg;
                    if (StudentCurrentCreditsTextBox != null) StudentCurrentCreditsTextBox.Background = bg;
                    if (StudentDesiredGpaTextBox != null) StudentDesiredGpaTextBox.Background = bg;
                    if (StudentDesiredCreditsTextBox != null) StudentDesiredCreditsTextBox.Background = bg;
                    if (StudentCurrentYearTextBox != null) StudentCurrentYearTextBox.Background = bg;
                    if (StudentLastYearTextBox != null) StudentLastYearTextBox.Background = bg;
                    if (StudentCreditsPerSemesterTextBox != null) StudentCreditsPerSemesterTextBox.Background = bg;
                }
            }
            catch
            {
                // ignore
            }
        }

        private void ThemeToggle_Checked(object sender, RoutedEventArgs e)
        {
            SetTheme(ApplicationTheme.Dark);
            SaveThemeToSettings("Dark");
        }

        private void ThemeToggle_Unchecked(object sender, RoutedEventArgs e)
        {
            SetTheme(ApplicationTheme.Light);
            SaveThemeToSettings("Light");
        }

        private void ThemeToggle_Click(object sender, RoutedEventArgs e)
        {
            try
            {
                // Determine toggle state; prefer sender but fall back to named control
                bool isChecked = false;
                if (sender is ToggleButton tb)
                {
                    // Click may occur before IsChecked is toggled depending on template; read the new state by toggling current value
                    isChecked = tb.IsChecked == true;
                }
                else if (ThemeToggle != null)
                {
                    isChecked = ThemeToggle.IsChecked == true;
                }

                Debug.WriteLine($"ThemeToggle_Click: isChecked={isChecked}");

                if (isChecked)
                {
                    // ensure the toggle's checked state is set
                    if (ThemeToggle != null) ThemeToggle.IsChecked = true;
                    SetTheme(ApplicationTheme.Dark);
                    SaveThemeToSettings("Dark");
                }
                else
                {
                    if (ThemeToggle != null) ThemeToggle.IsChecked = false;
                    SetTheme(ApplicationTheme.Light);
                    SaveThemeToSettings("Light");
                }
            }
            catch (Exception ex)
            {
                Debug.WriteLine("ThemeToggle_Click error: " + ex);
            }
        }

        // Helper: robust decimal parsing that accepts currency symbols and group separators
        private static bool TryParseDecimal(string input, CultureInfo culture, out decimal value)
        {
            value = 0m;
            if (string.IsNullOrWhiteSpace(input)) return false;

            var s0 = input.Trim();

            //1) try culture-aware currency parse
            if (decimal.TryParse(s0, NumberStyles.Currency, culture, out value)) return true;

            //2) try plain number parse with culture
            if (decimal.TryParse(s0, NumberStyles.Number | NumberStyles.AllowLeadingSign | NumberStyles.AllowDecimalPoint, culture, out value)) return true;

            //3) remove common currency symbols and NBSP, then try culture parse
            var s = s0;
            s = s.Replace("\u00A0", string.Empty); // NBSP
            var currencySymbols = new[] { culture.NumberFormat.CurrencySymbol, "$", "£", "€", "¥", "USD", "EUR" };
            foreach (var sym in currencySymbols)
            {
                if (!string.IsNullOrEmpty(sym)) s = s.Replace(sym, string.Empty, StringComparison.OrdinalIgnoreCase);
            }
            s = s.Trim();
            if (decimal.TryParse(s, NumberStyles.Number | NumberStyles.AllowLeadingSign | NumberStyles.AllowDecimalPoint, culture, out value)) return true;

            //4) Heuristic normalization: decide decimal separator when both '.' and ',' present
            var hasDot = s.IndexOf('.') >= 0;
            var hasComma = s.IndexOf(',') >= 0;
            if (hasDot || hasComma)
            {
                string normalized = s;
                if (hasDot && hasComma)
                {
                    // whichever appears last is likely the decimal separator
                    var lastDot = s.LastIndexOf('.');
                    var lastComma = s.LastIndexOf(',');
                    if (lastDot > lastComma)
                    {
                        // '.' is decimal, remove commas
                        normalized = s.Replace(",", string.Empty);
                    }
                    else
                    {
                        // ',' is decimal, remove dots and replace comma with dot
                        normalized = s.Replace(".", string.Empty).Replace(",", ".");
                    }
                }
                else if (hasComma && !hasDot)
                {
                    // ambiguous: treat comma as decimal if it appears after 3 digits from the end or if culture uses comma
                    if (culture.NumberFormat.NumberDecimalSeparator == ",")
                    {
                        // keep as-is for culture parse attempt
                        if (decimal.TryParse(s, NumberStyles.Number | NumberStyles.AllowDecimalPoint | NumberStyles.AllowLeadingSign, culture, out value)) return true;
                        normalized = s.Replace(",", ".");
                    }
                    else
                    {
                        // assume comma is group separator, remove it
                        normalized = s.Replace(",", string.Empty);
                    }
                }
                else if (hasDot && !hasComma)
                {
                    if (culture.NumberFormat.NumberDecimalSeparator == ".")
                    {
                        if (decimal.TryParse(s, NumberStyles.Number | NumberStyles.AllowDecimalPoint | System.Globalization.NumberStyles.AllowLeadingSign, culture, out value)) return true;
                        normalized = s;
                    }
                    else
                    {
                        // in cultures where comma is decimal, dot might be group separator
                        normalized = s.Replace(".", string.Empty);
                    }
                }

                normalized = normalized.Trim();
                if (decimal.TryParse(normalized, NumberStyles.Number | NumberStyles.AllowDecimalPoint | NumberStyles.AllowLeadingSign, CultureInfo.InvariantCulture, out value)) return true;
            }

            //5) final fallback: strip anything except digits, dot, minus and try invariant
            var filtered = new string(s.Where(c => char.IsDigit(c) || c == '.' || c == '-' || c == ',').ToArray());
            // replace comma with dot if there's no dot but there is comma
            if (filtered.IndexOf('.') < 0 && filtered.IndexOf(',') >= 0)
            {
                filtered = filtered.Replace(',', '.');
            }
            if (decimal.TryParse(filtered, NumberStyles.Number | NumberStyles.AllowDecimalPoint | NumberStyles.AllowLeadingSign, CultureInfo.InvariantCulture, out value)) return true;

            return false;
        }

        private void CalculateButton_Click(object sender, RoutedEventArgs e)
        {
            ResultTextBlock.Text = string.Empty;

            var culture = CultureInfo.CurrentCulture;

            // Accept currency and number formats (commas, currency symbol, decimals)
            if (!TryParseDecimal(CurrentSalaryTextBox.Text, culture, out var currentSalary) || currentSalary < 0)
            {
                ResultTextBlock.Text = "Please enter a valid current salary (numbers, commas and $ are allowed).";
                return;
            }

            if (!decimal.TryParse(HoursPerWeekTextBox.Text, NumberStyles.Number, culture, out var hoursPerWeek) || hoursPerWeek <= 0)
            {
                ResultTextBlock.Text = "Please enter a valid number of hours per week (greater than 0).";
                return;
            }

            if (!TryParseDecimal(GoalSalaryTextBox.Text, culture, out var goalSalary) || goalSalary < 0)
            {
                ResultTextBlock.Text = "Please enter a valid goal salary (numbers, commas and $ are allowed).";
                return;
            }

            // Calculate assuming 52 working weeks
            const decimal weeksPerYear = 52m;

            var currentHourly = currentSalary / (hoursPerWeek * weeksPerYear);
            var requiredHourly = goalSalary / (hoursPerWeek * weeksPerYear);

            decimal increasePercent = 0m;
            if (currentHourly > 0)
            {
                increasePercent = (requiredHourly - currentHourly) / currentHourly * 100m;
            }

            var hoursNeededPerWeek = 0m;
            if (currentHourly > 0)
            {
                hoursNeededPerWeek = goalSalary / (currentHourly * weeksPerYear);
            }

            // Additional breakdowns
            var currentMonthly = currentSalary / 12m;
            var goalMonthly = goalSalary / 12m;
            var currentWeekly = currentSalary / weeksPerYear;
            var goalWeekly = goalSalary / weeksPerYear;

            ResultTextBlock.Text = string.Format(
                "Current hourly: {0:C2}\nRequired hourly for goal (same hours): {1:C2}\nIncrease needed: {2:F1}%\nHours/week needed at current hourly: {3:F1}\n\nCurrent monthly: {4:C2} ({5:C2}/week)\nGoal monthly: {6:C2} ({7:C2}/week)",
                currentHourly, requiredHourly, increasePercent, hoursNeededPerWeek,
                currentMonthly, currentWeekly, goalMonthly, goalWeekly);
        }

        private void ClearButton_Click(object sender, RoutedEventArgs e)
        {
            CurrentSalaryTextBox.Text = string.Empty;
            HoursPerWeekTextBox.Text = string.Empty;
            GoalSalaryTextBox.Text = string.Empty;
            ResultTextBlock.Text = string.Empty;
        }

        private void StudentCalculateButton_Click(object sender, RoutedEventArgs e)
        {
            StudentResultTextBlock.Text = string.Empty;
            var culture = CultureInfo.CurrentCulture;

            if (!decimal.TryParse(StudentWeeklyTimeTextBox.Text, NumberStyles.Number, culture, out var weeklyTime) || weeklyTime < 0)
            {
                StudentResultTextBlock.Text = "Enter a valid total weekly time (hours).";
                return;
            }

            if (!decimal.TryParse(StudentCurrentGpaTextBox.Text, NumberStyles.Number, culture, out var currentGpa) || currentGpa < 0 || currentGpa > 4.5m)
            {
                StudentResultTextBlock.Text = "Enter a valid current GPA (0-4.5).";
                return;
            }

            if (!decimal.TryParse(StudentCurrentCreditsTextBox.Text, NumberStyles.Number, culture, out var currentCredits) || currentCredits < 0)
            {
                StudentResultTextBlock.Text = "Enter valid current credit hours.";
                return;
            }

            if (!decimal.TryParse(StudentDesiredGpaTextBox.Text, NumberStyles.Number, culture, out var desiredGpa) || desiredGpa < 0 || desiredGpa > 4.5m)
            {
                StudentResultTextBlock.Text = "Enter a valid desired GPA (0-4.5).";
                return;
            }

            if (!decimal.TryParse(StudentDesiredCreditsTextBox.Text, NumberStyles.Number, culture, out var desiredCredits) || desiredCredits <= 0)
            {
                StudentResultTextBlock.Text = "Enter valid desired total credit hours.";
                return;
            }

            // New inputs
            if (!int.TryParse(StudentCurrentYearTextBox.Text, out var currentYear) || currentYear < 0)
            {
                StudentResultTextBlock.Text = "Enter a valid current year (e.g.,1-4).";
                return;
            }

            if (!int.TryParse(StudentLastYearTextBox.Text, out var yearsRemaining) || yearsRemaining < 0)
            {
                StudentResultTextBlock.Text = "Enter valid years remaining (e.g.,1-4).";
                return;
            }

            if (!int.TryParse(StudentCreditsPerSemesterTextBox.Text, out var creditsPerSemester) || creditsPerSemester <= 0)
            {
                StudentResultTextBlock.Text = "Enter valid credits per semester (e.g.,15).";
                return;
            }

            // Calculate how many additional credits are needed
            var additionalCreditsNeeded = desiredCredits - currentCredits;
            if (additionalCreditsNeeded < 0) additionalCreditsNeeded = 0;

            // Estimate overall GPA needed in remaining credits to reach desired GPA:
            // currentGpa*currentCredits + requiredGpaInRemaining*additionalCredits = desiredGpa*desiredCredits
            decimal requiredGpaInRemaining = 0m;
            if (additionalCreditsNeeded > 0)
            {
                requiredGpaInRemaining = (desiredGpa * desiredCredits - currentGpa * currentCredits) / additionalCreditsNeeded;
            }
            else
            {
                requiredGpaInRemaining = 0m; // already at or above desired total credits
            }

            // Time calculations
            const decimal defaultHoursPerCreditPerWeek = 3m; // typical heuristic:1 hour class +2 hours study
            const int weeksPerSemester = 15;

            int totalSemestersAvailable = Math.Max(1, yearsRemaining * 2); // two semesters per year

            // If no additional credits are needed, show result and exit
            if (additionalCreditsNeeded == 0)
            {
                StudentResultTextBlock.Text = string.Format(
                    "No additional credits needed.\nYour current GPA: {0:F2}\nDesired GPA: {1:F2}",
                    currentGpa, desiredGpa);
                return;
            }

            // Derive observed hours per credit from the user's reported weekly time and current credits (if available)
            decimal observedHoursPerCredit = defaultHoursPerCreditPerWeek;
            if (currentCredits > 0 && weeklyTime > 0)
            {
                observedHoursPerCredit = weeklyTime / currentCredits;
            }

            // Blend heuristic and observed to avoid extreme jumps; weight observed more to reflect user's reality
            decimal blendedHoursPerCredit = (0.6m * observedHoursPerCredit) + (0.4m * defaultHoursPerCreditPerWeek);
            // Clamp to reasonable bounds
            blendedHoursPerCredit = Math.Clamp(blendedHoursPerCredit, 0.5m, 6m);

            // Factor GPA difficulty: higher required GPA increases workload per credit
            // Use baseline GPA of 3.0 and scale factor per GPA point above/below baseline
            const decimal baselineGpa = 3.0m;
            const decimal perGpaPointMultiplier = 0.20m; // 20% more work per GPA point above baseline
            decimal gpaMultiplier = 1.0m;
            if (requiredGpaInRemaining > 0)
            {
                gpaMultiplier = 1.0m + (requiredGpaInRemaining - baselineGpa) * perGpaPointMultiplier;
            }
            // Clamp multiplier to reasonable range
            gpaMultiplier = Math.Clamp(gpaMultiplier, 0.7m, 2.0m);

            // Effective hours per credit factoring in GPA difficulty
            decimal effectiveHoursPerCredit = blendedHoursPerCredit * gpaMultiplier;

            // EVEN SPREAD across available semesters (additional workload per semester and per week)
            decimal evenCreditsPerSemester = additionalCreditsNeeded / (decimal)totalSemestersAvailable;
            decimal evenWeeklyAdditional = evenCreditsPerSemester * effectiveHoursPerCredit;
            decimal evenWeeklyTotal = weeklyTime + evenWeeklyAdditional;

            // ACCELERATED (overload) approach: try to finish in fewer semesters by increasing credits/semester
            int acceleratedTargetSemesters = Math.Max(1, totalSemestersAvailable / 2); // aim to finish in half of the available semesters
            decimal acceleratedCreditsPerSemester = Math.Ceiling(additionalCreditsNeeded / (decimal)acceleratedTargetSemesters);
            decimal acceleratedWeeklyAdditional = acceleratedCreditsPerSemester * effectiveHoursPerCredit;
            decimal acceleratedWeeklyTotal = weeklyTime + acceleratedWeeklyAdditional;

            // MAX-LOAD approach: take up to creditsPerSemester each semester
            int semestersNeededAtMaxLoad = (int)Math.Ceiling((double)(additionalCreditsNeeded / creditsPerSemester));
            // Weekly additional workload during full-load semesters (may be lower in last semester)
            decimal maxLoadWeeklyAdditionalFull = creditsPerSemester * effectiveHoursPerCredit;
            // Weekly additional workload in the final (possibly partial) semester
            int fullLoadSemesters = additionalCreditsNeeded >= creditsPerSemester ? (int)(additionalCreditsNeeded / creditsPerSemester) : 0;
            decimal lastSemesterCredits = additionalCreditsNeeded - fullLoadSemesters * creditsPerSemester;
            if (lastSemesterCredits < 0) lastSemesterCredits = 0;
            decimal lastSemesterWeeklyAdditional = lastSemesterCredits * effectiveHoursPerCredit;

            // COMPRESSED scenario: required credits per semester to finish within available semesters
            decimal requiredCreditsPerSemesterToFinish = additionalCreditsNeeded / (decimal)totalSemestersAvailable;
            decimal compressedWeeklyAdditional = requiredCreditsPerSemesterToFinish * effectiveHoursPerCredit;
            decimal compressedWeeklyTotal = weeklyTime + compressedWeeklyAdditional;

            // Determine if even distribution is feasible (does not exceed per-semester credit cap)
            bool evenFeasible = evenCreditsPerSemester <= creditsPerSemester;

            // Baseline additional (use even feasible workload as baseline)
            decimal baselineAdditional = evenFeasible ? evenWeeklyAdditional : compressedWeeklyAdditional;
            decimal additionalWeeklyComparedToReported = baselineAdditional; // show additional hours (do not subtract reported weekly)

            // Calculate totals for all scenarios
            decimal maxLoadWeeklyTotalFull = weeklyTime + maxLoadWeeklyAdditionalFull;
            decimal lastSemesterWeeklyTotal = weeklyTime + lastSemesterWeeklyAdditional;

            // Determine peak weekly workload across scenarios
            var totals = new List<decimal> { evenWeeklyTotal, maxLoadWeeklyTotalFull, compressedWeeklyTotal, acceleratedWeeklyTotal };
            if (lastSemesterCredits > 0) totals.Add(lastSemesterWeeklyTotal);
            decimal peakWeekly = totals.Max();

            // Build result message
            var result = new System.Text.StringBuilder();
            result.AppendLine($"Additional credits needed: {additionalCreditsNeeded}");
            result.AppendLine($"Required average GPA in remaining credits: {requiredGpaInRemaining:F2}");
            result.AppendLine();
            result.AppendLine($"Observed hours per existing credit (from your reported time): {observedHoursPerCredit:F2} hours/credit/week");
            result.AppendLine($"Blended hours per credit used for baseline estimates: {blendedHoursPerCredit:F2} hours/credit/week");
            result.AppendLine($"GPA difficulty multiplier applied: {gpaMultiplier:F2} (baseline {baselineGpa}, {perGpaPointMultiplier * 100:F0}% per GPA point)");
            result.AppendLine($"Effective hours per credit after GPA factor: {effectiveHoursPerCredit:F2} hours/credit/week");
            result.AppendLine();
            result.AppendLine($"Semesters available (years remaining={yearsRemaining}): {totalSemestersAvailable}");
            result.AppendLine($"Credits capacity per semester: {creditsPerSemester}");
            result.AppendLine();

            if (evenFeasible)
            {
                result.AppendLine("Option A — Even spread (recommended):");
                result.AppendLine($" • Credits per semester (additional): {evenCreditsPerSemester:F1}");
                result.AppendLine($" • Additional weekly workload while taking those credits: {evenWeeklyAdditional:F1} hours/week");
                result.AppendLine($" • Estimated total weekly workload (your reported + additional): {evenWeeklyTotal:F1} hours/week");
            }
            else
            {
                result.AppendLine("Option A — Even spread (not feasible without overload):");
                result.AppendLine($" • Credits per semester needed: {evenCreditsPerSemester:F1} (exceeds your per-semester cap)");
                result.AppendLine($" • Additional weekly workload if spread evenly: {evenWeeklyAdditional:F1} hours/week");
                result.AppendLine($" • Estimated total weekly workload (your reported + additional): {evenWeeklyTotal:F1} hours/week");
            }

            result.AppendLine();
            result.AppendLine("Option B — Accelerated (overload to finish sooner):");
            result.AppendLine($" • Target semesters to finish (half of available): {acceleratedTargetSemesters}");
            result.AppendLine($" • Credits per semester (accelerated): {acceleratedCreditsPerSemester:F0}");
            result.AppendLine($" • Additional weekly workload while taking accelerated load: {acceleratedWeeklyAdditional:F1} hours/week");
            result.AppendLine($" • Estimated total weekly workload (your reported + accelerated additional): {acceleratedWeeklyTotal:F1} hours/week");
            if (acceleratedCreditsPerSemester > creditsPerSemester)
            {
                result.AppendLine(" • Note: this requires overloads above your normal per-semester cap.");
            }

            result.AppendLine();
            result.AppendLine("Option C — Max-load strategy:");
            result.AppendLine($" • Semesters needed at {creditsPerSemester} credits/semester: {semestersNeededAtMaxLoad}");
            result.AppendLine($" • Weekly additional workload during full-load semesters: {maxLoadWeeklyAdditionalFull:F1} hours/week");
            result.AppendLine($" • Estimated total weekly workload during full-load semesters: {maxLoadWeeklyTotalFull:F1} hours/week");
            if (lastSemesterCredits > 0)
            {
                result.AppendLine($" • Weekly additional workload during final partial semester: {lastSemesterWeeklyAdditional:F1} hours/week");
                result.AppendLine($" • Estimated total weekly workload during final partial semester: {lastSemesterWeeklyTotal:F1} hours/week");
            }
            if (semestersNeededAtMaxLoad > totalSemestersAvailable)
            {
                result.AppendLine();
                result.AppendLine("Note: Max-load strategy requires more semesters than available. Consider extending your study plan or taking overloads/summers.");
            }

            result.AppendLine();
            result.AppendLine($"Compressed to available semesters: required credits/semester = {requiredCreditsPerSemesterToFinish:F1}");
            result.AppendLine($"Additional weekly workload if compressed into available semesters: {compressedWeeklyAdditional:F1} hours/week");
            result.AppendLine($"Estimated total weekly workload (your reported + compressed additional): {compressedWeeklyTotal:F1} hours/week");

            result.AppendLine();
            result.AppendLine($"Your reported weekly time: {weeklyTime:F1} hours/week");
            result.AppendLine($"Additional weekly hours suggested (based on feasible plan): {baselineAdditional:F1} hours/week");
            result.AppendLine($"Peak weekly workload across options: {peakWeekly:F1} hours/week");

            StudentResultTextBlock.Text = result.ToString();
        }

        private void StudentClearButton_Click(object sender, RoutedEventArgs e)
        {
            StudentWeeklyTimeTextBox.Text = string.Empty;
            StudentCurrentGpaTextBox.Text = string.Empty;
            StudentCurrentCreditsTextBox.Text = string.Empty;
            StudentDesiredGpaTextBox.Text = string.Empty;
            StudentDesiredCreditsTextBox.Text = string.Empty;
            StudentCurrentYearTextBox.Text = string.Empty;
            StudentLastYearTextBox.Text = string.Empty;
            StudentCreditsPerSemesterTextBox.Text = string.Empty;
            StudentResultTextBlock.Text = string.Empty;
        }

        private void TryApplyBorderBrushToChildBorders(DependencyObject root, Brush brush)
        {
            try
            {
                if (root == null) return;
                var count = VisualTreeHelper.GetChildrenCount(root);
                for (int i = 0; i < count; i++)
                {
                    var child = VisualTreeHelper.GetChild(root, i);
                    if (child is Border b)
                    {
                        b.BorderBrush = brush;
                    }
                    TryApplyBorderBrushToChildBorders(child, brush);
                }
            }
            catch
            {
                // ignore
            }
        }

        private void ApplyRequestedThemeToVisualTree(ElementTheme theme)
        {
            try
            {
                if (this.Content is DependencyObject root)
                {
                    SetRequestedThemeRecursive(root, theme);
                }
            }
            catch
            {
                // ignore
            }
        }

        private void SetRequestedThemeRecursive(DependencyObject node, ElementTheme theme)
        {
            if (node == null) return;
            int count = VisualTreeHelper.GetChildrenCount(node);
            for (int i = 0; i < count; i++)
            {
                var child = VisualTreeHelper.GetChild(node, i);
                if (child is FrameworkElement fe)
                {
                    try
                    {
                        fe.RequestedTheme = theme;
                    }
                    catch
                    {
                        // ignore
                    }
                }
                // recurse
                SetRequestedThemeRecursive(child, theme);
            }
        }

        // Update the ThemeToggle's displayed content to indicate the active theme
        private void UpdateThemeToggleContent(ApplicationTheme theme)
        {
            try
            {
                if (ThemeToggle == null) return;
                ThemeToggle.Content = theme == ApplicationTheme.Dark ? "Dark" : "Light";
            }
            catch
            {
                // ignore
            }
        }

    }
}

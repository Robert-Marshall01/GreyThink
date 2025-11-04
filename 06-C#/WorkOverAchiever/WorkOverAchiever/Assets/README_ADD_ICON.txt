Place your app image files here:

- app.png    -> the PNG you provided (used as Window/taskbar icon while running)
- appicon.ico -> a multi-size .ico for the EXE shell icon (recommended: include 16x16, 32x32, 48x48, 256x256)

To generate appicon.ico from app.png you can use an external tool (IcoFX, GIMP + plugin, or online converters).

After adding the files, rebuild the project. The EXE file icon will use appicon.ico, and the running window will use app.png as set in MainWindow.xaml.

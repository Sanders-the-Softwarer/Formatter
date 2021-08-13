x:
cd \formatter
del /f /q /s Win32\*.*
del /f /q /s Win64\*.*
rmdir Win32\Debug
rmdir Win32\Release
rmdir Win32
rmdir Win64\Release
rmdir Win64
cd \formatter\exe
zip -P 12345 -9 Formatter.zip DebugTool.exe CmdLineFormatter.exe PLSQLDev_Plugin.32.dll PLSQLDev_Plugin.64.dll 
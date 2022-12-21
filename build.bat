@echo off
if "%VCToolsVersion%"=="" call "C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Auxiliary\Build\vcvars64.bat"
cl /Z7 /DEBUG:FULL main.c /I ..\include user32.lib gdi32.lib shlwapi.lib winmm.lib

@echo off
where sh >nul 2>nul
if errorlevel 1 (
  echo error: this script requires sh.exe (MSYS2/Git Bash) on PATH
  exit /b 1
)
sh -lc "./compile %*"
exit /b %errorlevel%

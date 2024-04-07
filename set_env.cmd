:: Copyright (C) 2016-2020, 2022 Free Software Foundation, Inc.
:: Written by Simon Sobisch
::
:: This file is part of GnuCOBOL.
::
:: The GnuCOBOL compiler is free software: you can redistribute it
:: and/or modify it under the terms of the GNU General Public License
:: as published by the Free Software Foundation, either version 3 of the
:: License, or (at your option) any later version.
::
:: GnuCOBOL is distributed in the hope that it will be useful,
:: but WITHOUT ANY WARRANTY; without even the implied warranty of
:: MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
:: GNU General Public License for more details.
::
:: You should have received a copy of the GNU General Public License
:: along with GnuCOBOL.  If not, see <https://www.gnu.org/licenses/>.

@echo off

:: Check if called already
:: if yes, check if called from here - exit, in any other case
:: raise warning and reset env vars
if not "%COB_MAIN_DIR%" == "" (
   echo.
   if "%COB_MAIN_DIR%" == "%~dp0" (
      echo Information: batch was called alread from "%COB_MAIN_DIR%"
      echo              skipping environment setting...
      if not [%1] == [] goto :call_if_needed
      goto :cobcver
   ) else (
      echo Warning: batch was called before from "%COB_MAIN_DIR%"
      echo          resetting COB_CFLAGS, COB_LDFLAGS
      set "COB_CFLAGS="
      set "COB_LDLAGS="
   )
)

:: Get the main dir from the batch's position (only works in NT environments)
set "COB_MAIN_DIR=%~dp0"

:: settings for cobc
set "COB_CONFIG_DIR=%COB_MAIN_DIR%config"
set "COB_COPY_DIR=%COB_MAIN_DIR%copy"
set "COB_CFLAGS=-I"%COB_MAIN_DIR%include" %COB_CFLAGS%"
set "COB_LDFLAGS=-L"%COB_MAIN_DIR%lib" %COB_LDFLAGS%"

:: settings for libcob
set "COB_LIBRARY_PATH=%COB_MAIN_DIR%extras"

:: Add the bin path of GnuCOBOL (including GCC) to PATH for further references
set "PATH=%COB_MAIN_DIR%bin;%PATH%"

:: Locales
set "LOCALEDIR=%COB_MAIN_DIR%locale"

:: Timezone database
if exist "%COB_MAIN_DIR%share\zoneinfo" (
  set "TZDIR=%COB_MAIN_DIR%share\zoneinfo"
)

:: start executable as requested
:call_if_needed
if not [%1] == [] (
  echo environment is prepared:
  call :cobcver
  echo now starting the requested %1
  call %*
  goto :eof
)

:: new cmd to stay open if not started directly from cmd.exe window
echo %cmdcmdline% | %windir%\system32\find.exe /i "%~0" >nul
if %errorlevel% equ 0 (
  cmd /k "cobc.exe --version && echo. && echo GnuCOBOL 3.2.0 (Jul 28 2023 16:07:51), (MinGW) "13.1.0" && echo GMP 6.2.1, libxml2 2.11.4, cJSON 1.7.15, PDCursesMod 4.3.7, BDB 18.1.40 "
  goto :eof
)

:: Compiler and package version output
:cobcver
echo.
cobc.exe --version
echo. && echo GnuCOBOL 3.2.0 (Jul 28 2023 16:07:51), (MinGW) "13.1.0" && echo GMP 6.2.1, libxml2 2.11.4, cJSON 1.7.15, PDCursesMod 4.3.7, BDB 18.1.40 


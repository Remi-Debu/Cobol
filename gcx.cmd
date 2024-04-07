@ECHO OFF
CLS 
echo.
echo ************************************************************************
echo * Compile a GnuCOBOL program - by Arnold Trembley, 2022-12-28,         *
echo * with contributions from Simon Sobisch and Mark Manning.              *
echo * This .CMD assumes that GnuCOBOL Environment variables and PATH       *
echo * have already been established.  Then we compile the COBOL            *
echo * source program passed as the initial parameter without extension.    *
echo * NOTE:  This .CMD file is designed to be executed in the directory    *
echo * where the GnuCOBOL compiler is installed.  On Successful completion  *
echo * .EXE and .LST files will be created for the COBOL source program.    *
echo ************************************************************************

set cur_prog=%~n1
set cur_ext=%~x1
rem
rem	What happens if no extension is given?
rem
if [%cur_ext%] == [] (
  rem echo.
  rem echo.========================================================================
  rem echo There is NO extension - Figuring it out
  if exist .\%cur_prog%.cob set cur_ext=.cob
  if exist .\%cur_prog%.cbl set cur_ext=.cbl
  rem echo.========================================================================
  rem echo.
)

rem echo CURRENT PROGRAM File = %cur_prog%%cur_ext%

rem Delete .exe and .lst temp files from any previous cur_prog compile 
if exist %temp%\%cur_prog%.exe erase %temp%\%cur_prog%.exe 
if exist %temp%\%cur_prog%.lst erase %temp%\%cur_prog%.lst

REM pause 
rem CALL the "set_env.cmd" script to set GnuCOBOL environment variables
echo call %~dp0\set_env.cmd
call %~dp0\set_env.cmd

if not exist .\%cur_prog%%cur_ext% (
  echo.
  echo.********************************************************************************
  echo COBOL source code file "%cur_prog%%cur_ext%" NOT FOUND found in this directory.  
  echo NO compilation will occur. 
  echo.********************************************************************************
  echo.
  exit /b 1 
) else (
  echo.
  echo.--------------------------------------------------------------------------------
  echo Compile the fixed-format "%cur_prog%" program as executable main program (-x),
  echo enable most warnings (-Wall), with no binary truncation (-fnotrunc) 
  echo. 
  echo cobc.exe -x -Wall -fnotrunc -Xref -T %temp%\%cur_prog%.lst -o %temp%\%cur_prog%.exe %cur_prog%%cur_ext% 
  echo. 
  cobc.exe -x -Wall -fnotrunc -Xref -T %temp%\%cur_prog%.lst -o %temp%\%cur_prog%.exe %cur_prog%%cur_ext%  
  echo.
  echo GnuCOBOL Compile Returncode = %errorlevel%
  echo.--------------------------------------------------------------------------------
  echo.
  
  rem copy .exe and .lst files to local folder
  if exist %temp%\%cur_prog%.exe copy %temp%\%cur_prog%.exe .\
  if exist %temp%\%cur_prog%.lst copy %temp%\%cur_prog%.lst .\

  rem delete temporary .exe and .lst files 
  if exist %temp%\%cur_prog%.exe erase %temp%\%cur_prog%.exe 
  if exist %temp%\%cur_prog%.lst erase %temp%\%cur_prog%.lst
)

echo.

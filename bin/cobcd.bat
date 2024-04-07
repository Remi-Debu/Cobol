@ECHO OFF
SETLOCAL ENABLEEXTENSIONS ENABLEDELAYEDEXPANSION
::
::###########################################################################
::## The version number in the next line is updated by a Linux bash script. #
::## The text "Version x.x" has to appear in square brackets.               #
::## Don't mess with it!                                                    #
::###########################################################################
::
set VERSION=[Version 4.28.6]
::
::
:: Stupendously useful:
:: https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/call

set COMMAND_LINE="python %~dp0cobcd

:parameter_loop
    IF not "%1"=="" (
        SET COMMAND_LINE=%COMMAND_LINE% %1
        )
    shift
    if not "%~1"=="" goto parameter_loop

set COMMAND_LINE=%COMMAND_LINE%"

cmd /q /c %COMMAND_LINE%


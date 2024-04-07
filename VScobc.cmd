@ECHO OFF
CLS 
echo.
:: ************************************************************************
:: * 2022-08-31 - by Arnold Trembley, suggested by Simon Sobisch          *
:: * CALL set_env.cmd to establish GnuCOBOL environment variables, and    *
:: * then execute cobc.exe while passing the arguments from the original  *
:: * call to this script.  This is intended to help compile and test      *
:: * GnuCOBOL programs using VSCodium as the editor and IDE (Integrated   *
:: * Development Environment).  You may need to edit the CALL statement   *
:: * to use the full PATH to the appropriate "set_env.cmd" script.        *
:: ************************************************************************

:: set GnuCOBOL environment variables 
CALL set_env.cmd 

echo. 

:: run cobc.exe compiler with user-supplied arguments 
cobc.exe %*  

echo. 

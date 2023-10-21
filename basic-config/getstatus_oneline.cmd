@echo OFF
set URL=%1
REM based on:  [forum](https://qna.habr.com/q/1313744?e=14072372#comment_3349580) (in Russian)
REM where a oneliner was suggested
REM NOTE: the remote name could not be resolved exception is not caught
powershell.exe -nologo -noprofile -command "forEach-object {[Net.ServicePointManager]::SecurityProtocol = 'Tls12, Tls11, Tls, Ssl3'} ;(invoke-webrequest -usebasicparsing -Uri '%URL%').StatusCode;exit [int]$Error[0].Exception.Status"


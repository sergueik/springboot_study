# see also: ttps://stackoverflow.com/questions/43784702/stderr-and-stdout-for-powershell-with-invoke-expression-has-no-output-or-errors
# https://github.com/PowerShell/PowerShell/issues/10476
# https://itecnote.com/tecnote/powershell-manage-errors-with-invoke-expression/
#
write-output 'output message 1'
[System.Console]::Error.WriteLine('error message 1')
echo 'output message 2'
[System.Console]::Error.WriteLine('error message 2')
exit 0

# 1. Open the Word Application COM Object
$Word = New-Object -ComObject Word.Application

# 2. Make the application window visible (Optional - change to $false to hide)
$Word.Visible = $true

# 3. Create a new blank document
$Doc = $Word.Documents.Add()

# 4. Access the selection framework to type text
$Selection = $Word.Selection
$Selection.TypeText("Hello World! This document was created via PowerShell.")

# 5. Define the save path and save the document
$SavePath = "$HOME\Desktop\PowerShell_Document.docx"
$Doc.SaveAs([ref]$SavePath)

# 6. Close the document and exit the application
$Doc.Close()
$Word.Quit()

# 7. Clean up COM variables from memory
[System.Runtime.Interopservices.Marshal]::ReleaseComObject$Word | Out-Null


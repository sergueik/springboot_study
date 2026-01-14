param (
    [int]$number = 10
)

# --------------------------
# Field dictionary
# --------------------------
$fields = @{
    BRANCHID      = "BR001"
    TELLERID      = "T12345"
    TRANDATE      = "20240130"
    TRANTIME      = "103015"
    ACCOUNTNUMBER = "123456789012"
    AMOUNT        = "0000001234567"
    CURRENCY      = "USD"
    DESCRIPTION   = "ATM WITHDRAWAL"
    APPROVALCODE  = "APR123"
    BRANCH        = "DEP"
}

# --------------------------
# Field order per repeated unit
# --------------------------
$order = @("BRANCHID","TELLERID","TRANDATE","TRANTIME","ACCOUNTNUMBER","AMOUNT","CURRENCY","BRANCH","DESCRIPTION","APPROVALCODE")

# --------------------------
# Initialize outputs
# --------------------------
$record = ""
$regexJava = "public static final String regexString = ""^"" +"

# Track counts for unique group names
$counters = @{}

# Repeat $number units
1..$number | ForEach-Object {
    foreach ($f in $order) {
        if (-not $counters.ContainsKey($f)) { $counters[$f] = 0 }
        $counters[$f]++
        $num = $counters[$f]  # unique suffix

        # ----------------------
        # Prepare fixed-width copybook value and Java regex fragment
        # ----------------------
        switch ($f) {
            "BRANCHID"      { $val = "{0,-5}" -f $fields[$f]; $frag = "(?<$f$num>.{5})" }
            "TELLERID"      { $val = "{0,-6}" -f $fields[$f]; $frag = "(?<$f$num>.{6})" }
            "TRANDATE"      { $val = "{0:D8}" -f [int]$fields[$f]; $frag = "(?<$f$num>\\d{8})" }
            "TRANTIME"      { $val = "{0:D6}" -f [int]$fields[$f]; $frag = "(?<$f$num>\\d{6})" }
            "ACCOUNTNUMBER" { $val = "{0:D12}" -f [int64]$fields[$f]; $frag = "(?<$f$num>\\d{12})" }
            "AMOUNT"        { $val = "{0:D13}" -f [int64]$fields[$f]; $frag = "(?<$f$num>[+-]?\\d{13})" }
            "CURRENCY"      { $val = "{0,-3}" -f $fields[$f]; $frag = "(?<$f$num>[A-Z]{3})" }
            "DESCRIPTION"   { $val = "{0,-30}" -f $fields[$f]; $frag = "(?<$f$num>.{30})" }
            "APPROVALCODE"  { $val = "{0,-6}" -f $fields[$f]; $frag = "(?<$f$num>.{6})" }
            "BRANCH"        { $val = "{0,-5}" -f $fields[$f]; $frag = "(?<$f$num>.{5})" }
        }

        # Append to copybook record
        $record += $val

        # Already Java compatible; no extra replace needed
        $regexJava += "`n""$frag"" +"
    }
}

# Terminate regex
$regexJava += "`n""$"";"

# --------------------------
# Output
# --------------------------
Write-Output "RECORD LENGTH = $($record.Length)"
Write-Output $record
Write-Output ""
Write-Output "JAVA-READY REGEX STRING:"
Write-Output $regexJava


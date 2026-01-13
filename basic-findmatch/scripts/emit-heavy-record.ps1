# --------------------------
# Dictionary of field values
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

# Repeat 10 units (change as needed)
1..10 | ForEach-Object {
    foreach ($f in $order) {
        if (-not $counters.ContainsKey($f)) { $counters[$f] = 0 }
        $counters[$f]++
        $num = $counters[$f]

        # ----------------------
        # Prepare fixed-width copybook value
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

        # Escape backslashes for Java
        $frag_java = $frag -replace '\\','\\\\'

        # Append to Java regex string
        $regexJava += "`n""$frag_java"" +"
    }
}

# Terminate regex
$regexJava += "`n""$"";"

# --------------------------
# Output
# --------------------------
"RECORD LENGTH = $($record.Length)"
$record
""
"JAVA-READY REGEX STRING:"
$regexJava


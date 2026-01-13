#!/usr/bin/env bash

# --------------------------
# Field dictionary
# --------------------------
declare -A fields=(
  [BRANCHID]="BR001"
  [TELLERID]="T12345"
  [TRANDATE]="20240130"
  [TRANTIME]="103015"
  [ACCOUNTNUMBER]="123456789012"
  [AMOUNT]="0000001234567"
  [CURRENCY]="USD"
  [DESCRIPTION]="ATM WITHDRAWAL"
  [APPROVALCODE]="APR123"
  [BRANCH]="DEP"
)

# --------------------------
# Field order per repeated unit
# --------------------------
order=(BRANCHID TELLERID TRANDATE TRANTIME ACCOUNTNUMBER AMOUNT CURRENCY BRANCH DESCRIPTION APPROVALCODE)

record=""
regexJava='public static final String regexString = "^" +'

# --------------------------
# Track occurrences of each field for unique group names
# --------------------------
declare -A counters

# repeat 10 units
for i in $(seq 1 10); do
  for f in "${order[@]}"; do
    counters[$f]=$(( ${counters[$f]:-0} + 1 ))
    num=${counters[$f]}  # unique suffix

    # ----------------------
    # pad values for copybook
    # ----------------------
    case $f in
      BRANCHID)       val=$(printf "%-5s" "${fields[$f]}") ; frag="(?<${f}${num}>.{5})" ;;
      TELLERID)       val=$(printf "%-6s" "${fields[$f]}") ; frag="(?<${f}${num}>.{6})" ;;
      TRANDATE)       val=$(printf "%08d" "${fields[$f]}") ; frag="(?<${f}${num}>\\d{8})" ;;
      TRANTIME)       val=$(printf "%06d" "${fields[$f]}") ; frag="(?<${f}${num}>\\d{6})" ;;
      ACCOUNTNUMBER)  val=$(printf "%012d" "${fields[$f]}") ; frag="(?<${f}${num}>\\d{12})" ;;
      AMOUNT)         val=$(printf "%013d" "${fields[$f]}") ; frag="(?<${f}${num}>[+-]?\\d{13})" ;;
      CURRENCY)       val=$(printf "%-3s" "${fields[$f]}") ; frag="(?<${f}${num}>[A-Z]{3})" ;;
      DESCRIPTION)    val=$(printf "%-30s" "${fields[$f]}") ; frag="(?<${f}${num}>.{30})" ;;
      APPROVALCODE)   val=$(printf "%-6s" "${fields[$f]}") ; frag="(?<${f}${num}>.{6})" ;;
      BRANCH)         val=$(printf "%-5s" "${fields[$f]}") ; frag="(?<${f}${num}>.{5})" ;;
    esac

    # append to record
    record+="$val"

    # double backslashes for Java
    frag_java="${frag//\\/\\\\}"

    # append to regex string
    regexJava+=$'\n'"\"$frag_java\" +"
  done
done

regexJava+=$'\n'"\"$\";"  # terminate regex

# --------------------------
# Output
# --------------------------
echo "RECORD LENGTH = ${#record}"
echo "$record"
echo
echo "JAVA-READY REGEX STRING:"
echo "$regexJava"


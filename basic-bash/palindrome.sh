text=$1
  reverse=""
  for (( i=0; i<${#text}; i++ ))
  do
    reverse=`echo ${text:i:1}$reverse`
  done
  
  if [[ $text == $reverse ]]
  then
    echo "Text is a Palindrome"
  else
    echo "Text is NOT a Palindrome"
  fi

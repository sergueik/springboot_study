while IFS= read -r line
do
	  # take action on $line #
	    echo "rr $line"
    done <<< $(ls -l .) 

# https://www.cyberciti.biz/faq/unix-howto-read-line-by-line-from-file/

#!/usr/bin/env bash
# #!/usr/bin/bash
# NOTE: github bash is different location (/usr/bin/bash) than vanilla ubuntu (/bin/bash)
HOSTNAME=${1:-host01}
DATAFILE=${2:-dummydata.txt}
# fix the date range to avoid wondering where is the data
YEAR=${3:-2022}
MONTH=${4:-09}
DAY=${5:-08}

DUMMYDATA=$(pwd)'/'$DATAFILE
MAXCNT=$(wc -l $DUMMYDATA|cut -f 1 -d ' ')
echo "MAXCNT=${MAXCNT}"
date="$YEAR$MONTH$DAY" # no separators

datadir="data/${HOSTNAME}/"$date
mkdir -p $datadir
pushd $datadir
for H in $(seq 0 1 3) ; do 
  for M in $(seq 0 1 59) ; do
    CNT=$(expr \( $CNT + 1 \) % $MAXCNT )
    CNT1=$(expr \( $CNT + 1 \) )
    # filename="data.txt.${date}${H}${M}"
    # https://linuxize.com/post/bash-printf-command/
    filename=$(printf "data.txt.${date}%02d%02d" $H $M)
    # echo $filename
    # touch $filename
    # TODO: date format to %s
    # TODO; randomize sec
    MEMORY=$(sed -n "${CNT1}p" $DUMMYDATA|awk '{print $3}')

    TIMESTAMP=$(date --date="${YEAR}/${MONTH}/${DAY} ${H}:${M}:00" +%s)
    # pad
    TIMESTAMP="${TIMESTAMP}"
    cat <<EOF>$filename
cpu: $MEMORY
timestamp: ${TIMESTAMP}

EOF
  done
done
popd

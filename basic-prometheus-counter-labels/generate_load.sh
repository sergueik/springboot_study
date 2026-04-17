#!/bin/bash
# smooth demo load for Grafana:
# gradual transition:
# perfect -> problematic -> failing -> recovery

TARGET="http://192.168.99.100:8080/analyze"

#
# phase 1 : mostly perfect
#
for CNT in $(seq 1 1 10); do
curl -X POST \
  -H 'Content-Type: text/plain' \
  --data-binary $'ooooooooooooooo\nooooooooooooooo\nooooooooooooooo\nooooooooxoooooo' \
  -s "$TARGET" > /dev/null
sleep 1
done

#
# phase 2 : some problematic rows appear
#
for CNT in $(seq 1 1 10); do
curl -X POST \
  -H 'Content-Type: text/plain' \
  --data-binary $'ooooooooooooooo\nooooooxooooooo\nooooxxoooooxooo\nooooooooxooooo' \
  -s "$TARGET" > /dev/null
sleep 1
done

#
# phase 3 : quality degradation
#
for CNT in $(seq 1 1 10); do
curl -X POST \
  -H 'Content-Type: text/plain' \
  --data-binary $'oooxxxxoooooxxx\noooxxxoooxxxooo\nxxxxoooooxxxooo\nooooxxxxoooxxxx' \
  -s "$TARGET" > /dev/null
sleep 1
done

#
# phase 4 : heavy failure period
#
for CNT in $(seq 1 1 10); do
curl -X POST \
  -H 'Content-Type: text/plain' \
  --data-binary $'xxxxxxxxxxxxxxx\nxxxxxxoxxxxxxxx\n\nxxxxoxxxxxoxxxx' \
  -s "$TARGET" > /dev/null
sleep 1
done

#
# phase 5 : recovery
#
for CNT in $(seq 1 1 10); do
curl -X POST \
  -H 'Content-Type: text/plain' \
  --data-binary $'ooooooooooooooo\nooooooooxoooooo\nooooooooooooooo\nooooooxooooooo' \
  -s "$TARGET" > /dev/null
sleep 1
done

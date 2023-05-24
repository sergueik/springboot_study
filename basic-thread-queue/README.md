###  Info


Examples from [perlmons](https://www.perlmonks.org/?node_id=1068673) and
[standard python queue module](https://docs.python.org/3/library/queue.html)
__NOTE:__ the Python queue run is apparently not parallel (a.k.a. concurrent). For parallel run check [asyncio library](https://docs.python.org/3/library/asyncio.html)
and it is a big change

### Usage

```sh
python3 test_queue.py 10
```
```text
Send 10 task requests to the worker
Working on 1 item will take 3
Finished.
Working on 2 item will take 6
Finished.
Working on 3 item will take 7
Finished.
Working on 4 item will take 8
Finished.
Working on 5 item will take 4
Finished.
Working on 6 item will take 8
Finished.
Working on 7 item will take 1
Finished.
Working on 8 item will take 5
Finished.
Working on 9 item will take 5
Finished.
Working on 10 item will take 4
Finished.
All work completed
```
the Perl code does not look to run thread concurrently either:
```sh
 perl test_queue.pl  -num 3 -input servers.txt  -cnt 10
```
prints
```text
1: pinging 192.168.0.25
2: pinging 192.168.0.92
3: pinging 192.168.0.64
PING 192.168.0.25 (192.168.0.25) 56(84) bytes of data.
64 bytes from 192.168.0.25: icmp_seq=1 ttl=128 time=5.50 ms
64 bytes from 192.168.0.25: icmp_seq=2 ttl=128 time=3.43 ms
64 bytes from 192.168.0.25: icmp_seq=3 ttl=128 time=2.89 ms
64 bytes from 192.168.0.25: icmp_seq=4 ttl=128 time=4.46 ms
64 bytes from 192.168.0.25: icmp_seq=5 ttl=128 time=4.08 ms
64 bytes from 192.168.0.25: icmp_seq=6 ttl=128 time=4.25 ms
64 bytes from 192.168.0.25: icmp_seq=7 ttl=128 time=3.90 ms
64 bytes from 192.168.0.25: icmp_seq=8 ttl=128 time=5.65 ms
64 bytes from 192.168.0.25: icmp_seq=9 ttl=128 time=4.00 ms
64 bytes from 192.168.0.25: icmp_seq=10 ttl=128 time=4.08 ms

--- 192.168.0.25 ping statistics ---
10 packets transmitted, 10 received, 0% packet loss, time 9013ms
rtt min/avg/max/mdev = 2.897/4.228/5.650/0.798 ms
PING 192.168.0.92 (192.168.0.92) 56(84) bytes of data.
64 bytes from 192.168.0.92: icmp_seq=1 ttl=64 time=3.93 ms
64 bytes from 192.168.0.92: icmp_seq=2 ttl=64 time=3.18 ms
64 bytes from 192.168.0.92: icmp_seq=3 ttl=64 time=176 ms
64 bytes from 192.168.0.92: icmp_seq=4 ttl=64 time=96.2 ms
64 bytes from 192.168.0.92: icmp_seq=5 ttl=64 time=16.8 ms
64 bytes from 192.168.0.92: icmp_seq=6 ttl=64 time=39.0 ms
64 bytes from 192.168.0.92: icmp_seq=7 ttl=64 time=35.7 ms
64 bytes from 192.168.0.92: icmp_seq=8 ttl=64 time=83.7 ms
64 bytes from 192.168.0.92: icmp_seq=9 ttl=64 time=105 ms
64 bytes from 192.168.0.92: icmp_seq=10 ttl=64 time=129 ms

--- 192.168.0.92 ping statistics ---
10 packets transmitted, 10 received, 0% packet loss, time 9014ms
rtt min/avg/max/mdev = 3.188/69.032/176.433/55.489 ms
PING 192.168.0.64 (192.168.0.64) 56(84) bytes of data.
64 bytes from 192.168.0.64: icmp_seq=1 ttl=64 time=0.057 ms
64 bytes from 192.168.0.64: icmp_seq=2 ttl=64 time=0.088 ms
64 bytes from 192.168.0.64: icmp_seq=3 ttl=64 time=0.090 ms
64 bytes from 192.168.0.64: icmp_seq=4 ttl=64 time=0.085 ms
64 bytes from 192.168.0.64: icmp_seq=5 ttl=64 time=0.089 ms
64 bytes from 192.168.0.64: icmp_seq=6 ttl=64 time=0.089 ms
64 bytes from 192.168.0.64: icmp_seq=7 ttl=64 time=0.091 ms
64 bytes from 192.168.0.64: icmp_seq=8 ttl=64 time=0.090 ms
64 bytes from 192.168.0.64: icmp_seq=9 ttl=64 time=0.089 ms
64 bytes from 192.168.0.64: icmp_seq=10 ttl=64 time=0.086 ms

--- 192.168.0.64 ping statistics ---
10 packets transmitted, 10 received, 0% packet loss, time 9177ms
rtt min/avg/max/mdev = 0.057/0.085/0.091/0.012 ms

```
### See Also

  * https://stackoverflow.com/questions/37223846/python-asyncio-wait-for-threads
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)

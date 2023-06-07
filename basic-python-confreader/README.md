### Usage
```sh
python3 read_exec.py  --command "./command.pl data2.conf" -d
```
```text
debug: True
command: "./command.pl data2.conf"
running command: "./command.pl"
[b'#webserver host3', b'']
```
NOTE - not trying to filter out blank lines

```sh
python3 service_config.py -p '.' -c 'data.conf'
```
```text
['#appserver host1',
 '#appserver host2 argument',
 '#webserver host3',
 '',
 '#exec command.pl data2.conf',
 '#dbserver host4']
[['#appserver', 'host1', '', ''],
 ['#appserver', 'host2', 'argument', ''],
 ['#webserver', 'host3', '', ''],
 ['#exec', 'command.pl', 'data2.conf', ''],
 ['#dbserver', 'host4', '', '']]

```
### See Also

  * https://realpython.com/python-getter-setter/
  * https://www.w3schools.com/python/ref_string_splitlines.asp
  * https://stackoverflow.com/questions/22042948/split-string-using-a-newline-delimiter-with-python
  * https://stackoverflow.com/questions/9344345/a-python-one-liner-if-x-in-y-do-x
  * https://stackoverflow.com/questions/4760215/running-shell-command-and-capturing-the-output

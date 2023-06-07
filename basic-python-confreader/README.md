### Usage


```sh
python3
```
```python
>>> 

>>> import sys
>>> sys.path.insert(0,os.path.join(os.path.curdir,'mod'))
>>> import service_config
>>> dir(service_config)
['ServiceConfig', '__builtins__', '__cached__', '__doc__', '__file__', '__loader__', '__name__', '__package__', '__spec__', 'getopt', 'os', 'pprint', 're', 'subprocess', 'sys']
>>> dir(service_config.ServiceConfig)
['__class__', '__delattr__', '__dict__', '__dir__', '__doc__', '__eq__', '__format__', '__ge__', '__getattribute__', '__gt__', '__hash__', '__init__', '__init_subclass__', '__le__', '__lt__', '__module__', '__ne__', '__new__', '__process_command__', '__process_execs__', '__process_includes__', '__read_config__', '__read_file__', '__reduce__', '__reduce_ex__', '__repr__', '__setattr__', '__sizeof__', '__str__', '__subclasshook__', '__weakref__', 'config', 'debug', 'filename', 'lines', 'path']

>>> exit()
```




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

provide dummy config files `data.conf`:

```text
#appserver host1
#appserver host2 argument
#include data1.conf
#exec command.pl data3.conf
#dbserver host4

``` 
`data1.conf`:
```text
#include data2.conf
```
`data2.conf`:
```text
#webserver host3
```
`data3.conf`
```text
#hyperserver host7
```


```sh
python3 service_config_tool.py -p '.' -c 'data.conf'
```
this will print `lines`
```text
['#appserver host1',
 '#appserver host2 argument',
 '#webserver host3',
 '',
 '',
 '#hyperserver host7',
 '#dbserver host4']
```
and `config`
```
[['#appserver', 'host1', '', ''],
 ['#appserver', 'host2', 'argument', ''],
 ['#webserver', 'host3', '', ''],
 ['#hyperserver', 'host7', '', ''],
 ['#dbserver', 'host4', '', '']]
```
### See Also

  * https://realpython.com/python-getter-setter/
  * https://www.w3schools.com/python/ref_string_splitlines.asp
  * https://stackoverflow.com/questions/22042948/split-string-using-a-newline-delimiter-with-python
  * https://stackoverflow.com/questions/9344345/a-python-one-liner-if-x-in-y-do-x
  * https://stackoverflow.com/questions/4760215/running-shell-command-and-capturing-the-output
  * [python method mangling](https://www.geeksforgeeks.org/name-mangling-in-python/)

  * https://stackoverflow.com/questions/377017/test-if-executable-exists-in-python
  * https://stackoverflow.com/questions/16114391/adding-directory-to-sys-path-pythonpath


### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)

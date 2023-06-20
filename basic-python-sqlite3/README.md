### Info

this directory contains a replica of [python flask api running in a docker container](https://github.com/deparkes/docker_flask_example)

### Usage
```sh
export COMPOSE_HTTP_TIMEOUT=600
docker-compose up --build
```
```sh
URL=http://localhost:5000/api/v1/resources/books
curl -s "$URL/all"
```

To debug:
```sh
docker build -t test -f Dockerfile .
docker run  -it test sh
```

then
```sh
python app.py
```

```sh
curl -X GET http://192.168.99.100:5000/book?author=David+Brin
```

```JSON
[
  [
    null,
    1988,
    "David Brin",
    "The Uplift War",
    "There had never been such traffic at Port Helenia\u2019s sleepy landing field\u2014not in all the years Fiben Bolger had lived here."
  ],
  [
    null,
    1984,
    "David Brin",
    "Startide Rising",
    "Fins had been making wisecracks about human beings for thousands of years."
  ]
]
```

```sh
URL=http://$(docker-machine ip):5000/books
curl -sX POST -d 'hostname=localhost&info=something&value=10' "$URL"
```
```html
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8"/>
    <title>Page</title>
  </head>
  <body>
    <div>hostname=localhost</div>
    <div>info=</div>
    <div>value=10.0</div>
    <div>timestamp=1686960404</div>
  </body>
</html>

```
### SQLite Cache Use Scenario

With help of [code2flow](https://app.code2flow.com) - online [graphviz](https://graphviz.org/download/)-like flowchart creator by Code Charm, Inc - one can construct the flowchart of the program in the browser

![Flow](https://github.com/sergueik/springboot_study/blob/master/basic-python-sqlite3/screenshots/capture-flow.png)

in annotated pseudocode
```code
find host info;
// datetime.now() - timedelta(seconds=60)
{compute confidence age}
// SELECT * FROM CACHE WHERE hostname = $1 AND timestamp > $2
{query cache
hostname + timestamp}
if(found?) {
    {get data from cache}
    return host info;
} else
{
  // SELECT * FROM DATABASE WHERE hostname = $1
  {query data from database;}
// INSERT INTO cache (hostname,info,
// datetime.now().strftime('%s'))
{insert data + timestampt to cache }
// DELETE FROM cache WHERE timestamp < $1
  {
    delete old cache data
  }
  return host info
};

```
![Anotated Flow](https://github.com/sergueik/springboot_study/blob/master/basic-python-sqlite3/screenshots/capture-flow-detailed.png)

### See Also

 * the original [blog](https://deparkes.co.uk/2018/03/02/simple-docker-flask-sqlite-api/)
 * [example](https://github.com/maltesander/uwsgi-nginx-flask-python-sqlite-docker-example) of a dockerized Flask webserver with uWSGI,Nginx, REST and SQLIte3
 * [tutorial-academy.com](http://tutorial-academy.com/uwsgi-nginx-flask-python-sqlite-docker-example)
 * [explanaion](https://www.geeksforgeeks.org/with-statement-in-python/) of `with` statement in Python
 * [code2flow](https://app.code2flow.com)
 * [graphviz](https://graphviz.org/download/)
 * [SQLite Autoincrement specifics](https://www.sqlite.org/autoinc.html) - note, the `SERIAL` language keyword [does not exist](https://stackoverflow.com/questions/19726028/sqlite3-serial-type-wasnt-incremented), do use `AUTOINCREMENT` instead
 * [SQLite Indexes](https://www.tutlane.com/tutorial/sqlite/sqlite-indexes)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)

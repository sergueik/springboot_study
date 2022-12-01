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
### See Also 

  * the original [blog](https://deparkes.co.uk/2018/03/02/simple-docker-flask-sqlite-api/)
  * __uwsgi-nginx-flask-python-sqlite-docker-example__ [example](https://github.com/maltesander/uwsgi-nginx-flask-python-sqlite-docker-example) of a dockerized Flask webserver with uWSGI and Nginx with a simple REST API to access a Sqlite database intended to simplify it to a barebones REST server backed by SQLIte3

    * [tutorial-academy.com](http://tutorial-academy.com/uwsgi-nginx-flask-python-sqlite-docker-example)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)

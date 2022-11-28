#!/bin/bash

# define the base URL
# NOTE: operate through HTTP -  currently docker-compose fails with

URL=http://localhost:5000/todo

# query empty todo items list
curl -X GET "$URL"

# create a todo item
curl -d '{"Id":0,"Text":"Buy 2 bottles of milk","Due":"2021-08-23"}' -H 'Content-Type: application/json' -X POST "$URL"

# create another todo item
curl -d '{"Id":0,"Text":"Buy a brezel and butter","Due":"2021-08-24"}' \
     -H 'Content-Type: application/json' -X POST "$URL"

# query todo items list just created
curl -X GET "$URL"

# update the second todo item
curl -d '{"Id":2,"Text":"Buy a brezel and buttermilk","Due":"2021-08-26"}' \
     -H 'Content-Type: application/json' -X PUT "$URL/2"

# query the updated todo item
curl -X GET "$URL/2"

# delete all todo items from database
curl -X DELETE "$URL/1"
curl -X DELETE "$URL/2"

# query todo items list (make sure deleted items are gone)
curl -X GET "$URL"

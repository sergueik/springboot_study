#!/bin/sh
docker-compose -f docker-compose.yml -f docker-compose-express.yml -f docker-compose-client.yml  up --build


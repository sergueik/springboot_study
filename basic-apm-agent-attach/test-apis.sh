#!/bin/bash

api_base_url_predev=http://localhost:8081
api_base_url_dev=http://localhost:8082
api_base_url_staging=http://localhost:8083
api_base_url_prod=http://localhost:8084


for i in {1..10}
  do
    printf "Iteration # %s...\n" "${i}"
    curl "${api_base_url_predev}/super-fast"
    curl "${api_base_url_dev}/super-fast"
    curl "${api_base_url_staging}/super-fast"
    curl "${api_base_url_prod}/super-fast"
    printf "\n"
    curl "${api_base_url_predev}/fast"
    curl "${api_base_url_dev}/fast"
    curl "${api_base_url_staging}/fast"
    curl "${api_base_url_prod}/fast"
    printf "\n"
    curl "${api_base_url_predev}/slow"
    curl "${api_base_url_dev}/slow"
    curl "${api_base_url_staging}/slow"
    curl "${api_base_url_prod}/slow"
    printf "\n"
    curl "${api_base_url_predev}/super-slow" &
    curl "${api_base_url_dev}/super-slow" &
    curl "${api_base_url_staging}/super-slow" &
    # The super-slow api is not on prod
    printf "\n\n"
done

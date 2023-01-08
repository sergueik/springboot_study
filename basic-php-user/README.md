### Info

with laravel it is frequently required to change the user to match the UID of the developer on the host. One possible solution is to
Modify group of the default user `www-data`
Alternatively change PHP to run as `app` user - by  changing
user and group names in config file `www.conf`
Alternatively add `usermod` - Alpine by default doesn't have the usermod command 

### Usage

```sh
docker php:8.2.1-fpm-alpine3.17
```
* build
```
IMAGE=basic-php
docker build -t $IMAGE -f Dockerfile  .
```
```sh
docker run --rm -it $IMAGE sh
```
```sh
grep 'user =' /usr/local/etc/php-fpm.d/www.conf | grep -v '^;'
```
```text
user = app
```

```sh
docker run --rm -u root -it $IMAGE sh
```
```sh
groups www-data
```
```text
www-data app
```
### Cleanup
### See Also

   * https://stackoverflow.com/questions/72739838/how-to-add-a-user-to-multiple-groups-in-alpine-linux

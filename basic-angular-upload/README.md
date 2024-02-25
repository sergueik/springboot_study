### Info

This direcory contains replica of Angular JS file upload from 
 [AngularJS - Upload File](https://www.tutorialspoint.com/angularjs/angularjs_upload_file.htm) post and the version supporting multiple file upload

### Uploading Single File
* open the `upload.html` page in the browser, select and upload multiple files, keeping the developer tools panel open:

* open the `upload.html` page in the browser, select and upload a CSV file, keeping the developer tools panel open
in the developer tools see upload steps and the original CSV data reurned as JSON

![CSV Upload](https://github.com/sergueik/springboot_study/blob/master/basic-angular-upload/screenshots/capture-upload.png)

### Uploading Multipe Files

* start the container `basic-perl-cgi`
```sh
NAME=basic-perl-cgi
ID=$(docker container ls -a | grep $NAME|cut -f 1 -d ' ')
docker start $ID
```
or start Spring-boot `upload` application
```sh
mvn spring-boot:run
```
- will need to update the `uploadUrl` in the page

* open the `multiple_upload.html` page in the browser, select and upload multiple files, keeping the developer tools panel open

![Mutiple File Upload](https://github.com/sergueik/springboot_study/blob/master/basic-angular-upload/screenshots/capture-mutiple-upload.png)

* in the container apache log `/var/log/apache2/error.log` one sees the CGI-BIN script acknowledging  receipt of every file sent


```text
[Fri Jan 26 16:54:16.987401 2024] [cgi:error] [pid 11] [client 192.168.99.1:51627] AH01215: Uploading filename: a1.csv: /var/www/localhost/cgi-bin/upload_multiple_files.cgi
[Fri Jan 26 16:54:16.987527 2024] [cgi:error] [pid 11] [client 192.168.99.1:51627] AH01215: Uploading filename: a2.csv: /var/www/localhost/cgi-bin/upload_multiple_files.cgi
[Fri Jan 26 16:54:16.987622 2024] [cgi:error] [pid 11] [client 192.168.99.1:51627] AH01215: Uploading filename: a3.csv: /var/www/localhost/cgi-bin/upload_multiple_files.cgi
```
### See Also

   * https://www.tutorialspoint.com/angularjs/angularjs_upload_file.htm (also uses custom directive, shorter)

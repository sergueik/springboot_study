CREATE DATABASE cardb;

CREATE USER 'cardbuser'@'localhost' IDENTIFIED BY '123test321';

GRANT ALL PRIVILEGES ON cardb.* TO 'cardbuser'@'localhost';

FLUSH PRIVILEGES;

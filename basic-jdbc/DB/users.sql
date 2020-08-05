CREATE DATABASE cardb;

-- not just @'localhost'

CREATE USER 'cardbuser'@'' IDENTIFIED BY '123test321';

GRANT ALL PRIVILEGES ON cardb.* TO 'cardbuser'@'%';

FLUSH PRIVILEGES;

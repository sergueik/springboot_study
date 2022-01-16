DROP USER 'java'@'%' ;
CREATE USER 'java'@'%' IDENTIFIED WITH mysql_native_password BY '{password}';
GRANT ALL ON `join_check`.* TO 'java'@'%';
FLUSH PRIVILEGES;

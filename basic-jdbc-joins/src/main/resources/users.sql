DROP USER 'java'@'%' ;
CREATE USER 'java'@'' IDENTIFIED BY 'password'
GRANT ALL ON `join_check`.* TO 'java'@'%';
FLUSH PRIVILEGES;

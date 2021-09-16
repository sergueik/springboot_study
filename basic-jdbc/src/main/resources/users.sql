-- not just @'localhost'

CREATE USER 'java'@'' IDENTIFIED BY 'password';

GRANT ALL PRIVILEGES ON cardb.* TO 'java'@'%';

FLUSH PRIVILEGES;

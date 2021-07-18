### Info

Based on [configurable spring security](https://github.com/yevheniyJ/springboot) example through `UserDetailsService` implementation) and thymeleaf templates,
converted to use SQLite JDBC driver. 
Technically with SQLite one should be able to use in-memory or on-disk database. Currently only the second option is working

### Usage

* create sqlite3 database `login.db` on desktop with a table `users`:

in `sqlite3`:
```sh
sqlite3 ~/Desktop/login.db 'CREATE TABLE IF NOT EXISTS `users` (`username` VARCHAR(255) NOT NULL PRIMARY KEY, `password` VARCHAR(255) NOT NULL, `role` VARCHAR(255) NOT NULL)'
```
or in [DB Browser for SQLite](https://sqlitebrowser.org)
```sql
CREATE TABLE IF NOT EXISTS users (
username VARCHAR(255) NOT NULL PRIMARY KEY, 
password VARCHAR(255) NOT NULL, 
role VARCHAR(255) NOT NULL);
```

* run application as usual
```sh
mvn clean spring-boot:run
```

### See Also
  * https://qna.habr.com/q/1020994

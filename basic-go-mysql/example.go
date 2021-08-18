package main
import (
       "fmt"
	"database/sql"
	_ "github.com/go-sql-driver/mysql"
)
func main() {
	db, err := sql.Open("mysql",
		"java:password@tcp(mysql-server:3306)/test")

	if err != nil {
 panic(err.Error())
//		log.Fatal(err)
	}
        err = db.Ping()
     if err != nil {
	// do something here
 panic(err.Error())
}
        fmt.Println("Go MySQL Tutorial")

	defer db.Close()
}

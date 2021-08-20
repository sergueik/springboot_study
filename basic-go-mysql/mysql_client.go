package main
import (
	"fmt"
	"flag"
	"strconv"
	"database/sql"
	_ "github.com/go-sql-driver/mysql"
)
type Tag struct {
	ID	 int	`json:"id"`
	Fname string `json:"fname"`
 	Ds string `json:"ds"`
}
var  (
	databaseConfig DatabaseConfig
)
type DatabaseConfig struct {
	User     string
	Password string
	Database string
	Table    string
	Server   string
	Port     int
}

func main() {
	// process command line args  
	flag.StringVar(&databaseConfig.User, "u", "java", "DB User.")
	flag.StringVar(&databaseConfig.Password, "v", "password", "DB User Password.")
	flag.StringVar(&databaseConfig.Database, "w", "test", "Database.")
	flag.StringVar(&databaseConfig.Server, "x", "127.0.0.1", "DB Server.")
 	flag.IntVar(&databaseConfig.Port, "y", 3306, "DB Server port.")

	flag.StringVar(&databaseConfig.Table, "z", "cache_table", "Table.")
	flag.Parse()
	fmt.Println("User: " + databaseConfig.User + "\n" + "Database: " + databaseConfig.Database + "\n" + "Server: " + databaseConfig.Server + "\n" + "Table: " + databaseConfig.Table + "\n" + "Port: " + strconv.Itoa(databaseConfig.Port) + "\n" )
	// connect to the database

	db, err := sql.Open("mysql", databaseConfig.User + ":" + databaseConfig.Password + "@tcp(" + databaseConfig.Server + ":" +  strconv.Itoa(databaseConfig.Port)  +  ")/" + databaseConfig.Database )

	var query string
	if err != nil { panic(err.Error()) }

	err = db.Ping()
	if err != nil { panic(err.Error()) }
	fmt.Println("ping succeeds")

	defer db.Close()
	query = "SELECT DISTINCT id,fname,ds FROM " + databaseConfig.Table
	rows, err := db.Query(query)

	if err != nil { panic(err.Error()) }
	for rows.Next() {
		var tag Tag
		// for each row, load columns
		err = rows.Scan(&tag.ID, &tag.Fname, &tag.Ds)
		if err != nil { panic(err.Error()) }
		fmt.Println(tag.Fname)
	}

	// defer close query is important if transactions are used
	defer rows.Close()
	var tag Tag
	// Execute and discard the query
    	query = "SELECT DISTINCT id,fname,ds FROM " + databaseConfig.Table + " where id = ?"
	err = db.QueryRow(query, 2).Scan(&tag.ID, &tag.Fname,&tag.Ds)
	if err != nil { panic(err.Error()) }

	fmt.Println(tag.ID)
	fmt.Println(tag.Fname)

	// perform a db.Query delete
	op, err := db.Query("DELETE FROM `" + databaseConfig.Table + "` WHERE fname = ?", "my example")
	if err != nil { panic(err.Error()) }

	// defer close query is important if transactions are used
	defer op.Close()

	// perform a db.Query insert
	insert, err := db.Query("INSERT INTO `" + databaseConfig.Table + "` (INS_DATE, FNAME, DS) VALUES ( now(), 'my example', 'new value')")

	if err != nil { panic(err.Error()) }

	// defer close query is important if transactions are used
	defer insert.Close()

}

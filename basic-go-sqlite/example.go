package main

import (
	"database/sql"
	"log"
	"flag"
	"fmt"
	"os"

	_ "github.com/mattn/go-sqlite3" // Import go-sqlite3 library
)

var  (
	database string = ""
	debug bool = false
)
// see also: https://github.com/mattn/go-sqlite3/blob/master/_example/simple/simple.go
func main() {
	flag.StringVar(&database, "db", "sqlite-database.db", "Database.")
	// TODO: regexp
	flag.BoolVar(&debug, "debug", false, "Debug")
	flag.Parse()
	os.Remove(database)

	log.Println("Creating database file: " + database )
	file, err := os.Create(database)
	if err != nil { panic(err.Error()) }
	file.Close()
	log.Println("database "+ database + " created")

	db, _ := sql.Open("sqlite3", "./" + database)
	defer db.Close()
	createTable(db)

	insertStudent(db, "0002", "Glen Rangel", "Bachelor")
	insertStudent(db, "0003", "Martin Martins", "Master")
	insertStudent(db, "0004", "Alayna Armitage", "PHD")
	insertStudent(db, "0005", "Marni Benson", "Bachelor")
	insertStudent(db, "0006", "Derrick Griffiths", "Master")
	insertStudent(db, "0007", "Leigh Daly", "Bachelor")
	insertStudent(db, "0008", "Marni Benson", "PHD")
	insertStudent(db, "0009", "Klay Correa", "Bachelor")

	displayStudents(db)
}

func createTable(db *sql.DB) {
	sql := `CREATE TABLE student (
		"idStudent" integer NOT NULL PRIMARY KEY AUTOINCREMENT,		
		"code" TEXT,
		"name" TEXT,
		"program" TEXT		
	  );`
	// GO multiline string

	fmt.Println("Create student table...")
	statement, err := db.Prepare(sql) // Prepare SQL Statement
	if err != nil { panic(err.Error()) }
	statement.Exec() // Execute SQL Statements
	log.Println("student table created")
}

func insertStudent(db *sql.DB, code string, name string, program string) {
	log.Println("Inserting student record ...")
	sql := `INSERT INTO student(code, name, program) VALUES (?, ?, ?)`
	statement, err := db.Prepare(sql)
	if err != nil { panic(err.Error()) }
	_, err = statement.Exec(code, name, program)
	if err != nil { panic(err.Error()) }
	defer statement.Close()
}

func displayStudents(db *sql.DB) {
	row, err := db.Query("SELECT * FROM student ORDER BY name")
	if err != nil { panic(err.Error()) }
	defer row.Close()
	for row.Next() { // Iterate and fetch the records from result cursor
		var (
			id int
			code string
			name string
			program string
		)
		row.Scan(&id, &code, &name, &program)
		log.Println("Student: ", code, " ", name, " ", program)
	}
}

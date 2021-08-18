package main
import (
  "fmt"
  "database/sql"
  _ "github.com/go-sql-driver/mysql"
)
type Tag struct {
  ID   int  `json:"id"`
  Name string `json:"name"`
}
func main() {
  db, err := sql.Open("mysql", "java:password@tcp(mysql-server:3306)/test")

  if err != nil { panic(err.Error()) }
  err = db.Ping()
  // do something here
  if err != nil { panic(err.Error()) }
  fmt.Println("ping succeeds")

  defer db.Close()
  // NOTE: `rowsect` is language word
  rows, err := db.Query("SELECT id, name FROM example_table")

  if err != nil { panic(err.Error()) }
  for rows.Next() {
    var tag Tag
    // for each row, load columns
    err = rows.Scan(&tag.ID, &tag.Name)
    if err != nil { panic(err.Error()) }
    fmt.Println(tag.Name)
  }

  // defer close query is important if transactions are used
  defer rows.Close()
  var tag Tag
  // Execute and discard the query
  err = db.QueryRow("SELECT id, name FROM example_table where id = ?", 2).Scan(&tag.ID, &tag.Name)
  if err != nil { panic(err.Error()) }

  fmt.Println(tag.ID)
  fmt.Println(tag.Name)

  // perform a db.Query delete
  op, err := db.Query("DELETE  FROM `example_table` WHERE id = ?", 42)
  if err != nil { panic(err.Error()) }

  // defer close query is important if transactions are used
  defer op.Close()

  // perform a db.Query insert
  insert, err := db.Query("INSERT INTO `example_table` (id, INS_DATE, NAME, VALUE) VALUES ( 42, now(), 'my example', 'new value')")

  if err != nil { panic(err.Error()) }

  // defer close query is important if transactions are used
  defer insert.Close()

}

package main
import (
	"fmt"
	"flag"
	"gopkg.in/yaml.v2"
	"io/ioutil"
	// "log"
	"strconv"
	"strings"
	"os"
	"database/sql"
	_ "github.com/go-sql-driver/mysql"
)
type Tag struct {
	ID	 int	`json:"id"`
	Fname string `json:"fname"`
 	Ds string `json:"ds"`
}

type DbConfig struct {
	User     string `yaml:"user"`
	Password string `yaml:"password"`
	Database string `yaml:"database"`
	Table    string `yaml:"table"`
	Server   string `yaml:"server"`
	Port     int `yaml:"port"`
}
type AppConfig struct {
  Database DbConfig `yaml:"database"`
  Folders FolderScan `yaml:"folders"`
}

type FolderScan struct {
  Collect []string `yaml:"collect"`
  Reject  []string `yaml:"reject"`
}

func (c *AppConfig) getConf(configFile string) *AppConfig {
    if _, err := os.Stat(configFile); err == nil {
      yamlFile, err := ioutil.ReadFile(configFile)
      if err != nil { panic(err.Error()) }
      err = yaml.Unmarshal(yamlFile, &c)
      if err != nil { panic(err.Error()) }
      return c
    } else {
      return nil
    }
}

var  (
	configFile string
	appConfig AppConfig
	dbConfig DbConfig
	folderConfig FolderScan
	rejectFlag string
	collectFlag string
)

func main() {

	fmt.Println("process command line args")
	// to get help about accepted arg pass an invalid one e.g. -h
	flag.StringVar(&configFile, "f", "config.yaml", "Config File.")
	fmt.Println("process config file: " + configFile)
	appConfig.getConf(configFile)
	dbConfig = appConfig.Database
	fmt.Println("database config:" + "\n" + "User: " + dbConfig.User + "\n" + "Database: " + dbConfig.Database + "\n" + "Server: " + dbConfig.Server + "\n" + "Table: " + dbConfig.Table + "\n" + "Port: " + strconv.Itoa(dbConfig.Port) + "\n" )

	folderConfig = appConfig.Folders
	fmt.Println("folder scan config:")
	fmt.Println("collect:")
	for _, v := range folderConfig.Collect {
		fmt.Println(v)
	}
	fmt.Println("reject:")

	for _, v := range folderConfig.Reject {
		fmt.Println(v)
	}

	flag.StringVar(&dbConfig.User, "u", "java", "DB User.")
	flag.StringVar(&dbConfig.Password, "v", "password", "DB User Password.")
	flag.StringVar(&dbConfig.Database, "w", "test", "Database.")
	flag.StringVar(&dbConfig.Server, "x", "127.0.0.1", "DB Server.")
 	flag.IntVar(&dbConfig.Port, "y", 3306, "DB Server port.")
	flag.StringVar(&dbConfig.Table, "z", "cache_table", "Table.")
	flag.StringVar(&collectFlag, "collect", "a,b", "Folders to collect.")
	flag.StringVar(&rejectFlag, "reject", "c,d", "Folders to reject.")
	flag.Parse()
	fmt.Println("User: " + dbConfig.User + "\n" + "Database: " + dbConfig.Database + "\n" + "Server: " + dbConfig.Server + "\n" + "Table: " + dbConfig.Table + "\n" + "Port: " + strconv.Itoa(dbConfig.Port) + "\n" )
	fmt.Println("folder scan config:")
	fmt.Println("collectFlag: " + collectFlag)
	fmt.Println("collect:")
	if len(collectFlag) == 0 {
		folderConfig.Collect = []string{}
		fmt.Println("none")
	} else {
		folderConfig.Collect = strings.Split(collectFlag, ",")
		for _, v := range folderConfig.Collect {
			fmt.Println(v)
		}
	}
	fmt.Println("rejectFlag: " + rejectFlag)
	fmt.Println("reject:")
	if len(rejectFlag ) == 0 {
		folderConfig.Reject = []string{}
		fmt.Println("none")
	} else {
		folderConfig.Reject = strings.Split(rejectFlag, ",")
		for _, v := range folderConfig.Reject {
			fmt.Println(v)
		}
	}

	fmt.Println("connect to the database")

	db, err := sql.Open("mysql", dbConfig.User + ":" + dbConfig.Password + "@tcp(" + dbConfig.Server + ":" +  strconv.Itoa(dbConfig.Port)  +  ")/" + dbConfig.Database )

	var query string
	if err != nil { panic(err.Error()) }

	err = db.Ping()
	if err != nil { panic(err.Error()) }
	fmt.Println("ping succeeds")

	defer db.Close()
	query = "SELECT DISTINCT id,fname,ds FROM " + dbConfig.Table
	rows, err := db.Query(query)

	if err != nil { panic(err.Error()) }
	for rows.Next() {
		var tag Tag
		// for each row, load columns
		err = rows.Scan(&tag.ID, &tag.Fname, &tag.Ds)
		if err != nil { panic(err.Error()) }
		fmt.Println(tag.Fname)
	}

	defer rows.Close()
	var tag Tag
	// Execute and discard the query
    	query = "SELECT DISTINCT id,fname,ds FROM " + dbConfig.Table + " where id = ?"
	err = db.QueryRow(query, 2).Scan(&tag.ID, &tag.Fname,&tag.Ds)
	if err != nil { panic(err.Error()) }

	fmt.Println(tag.ID)
	fmt.Println(tag.Fname)

	// perform a db.Query delete
	op, err := db.Query("DELETE FROM `" + dbConfig.Table + "` WHERE fname = ?", "my example")
	if err != nil { panic(err.Error()) }

	// defer close query is important if transactions are used
	defer op.Close()

	// perform a db.Query insert
	insert, err := db.Query("INSERT INTO `" + dbConfig.Table + "` (INS_DATE, FNAME, DS) VALUES ( now(), 'my example', 'new value')")

	if err != nil { panic(err.Error()) }

	// defer close query is important if transactions are used
	defer insert.Close()

}
/*
To run this, use 
git checkout 273b61ceb49554f5bcc8571dd0336969d0e5fd30 Dockerfile.build
git checkout 273b61ceb49554f5bcc8571dd0336969d0e5fd30 Dockerfile.run

export IMAGE=basic-go-build
docker build -t $IMAGE -f Dockerfile.build .
export NAME=basic-go-build
docker container rm $NAME
docker run -d --name=$NAME $IMAGE
docker cp $NAME:/build/mysql_client .
IMAGE=basic-go-run
docker build -t $IMAGE -f Dockerfile.run  .
docker container rm -f $IMAGE
docker run --link mysql-server --name $IMAGE -v $(pwd)/sample/:/sample -p 9001:9000 -i $IMAGE -u java -v password -w test -x mysql-server -y 3306  -reject x,y,z

*/

package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"math"
	"net/http"
	"os"
	"path/filepath"
	"regexp"
	"gopkg.in/yaml.v2"
	"io/ioutil"
	"strconv"
	"strings"
	"sync"
	"time"

	"github.com/gocarina/gocsv"
	"github.com/mattn/go-zglob"
	"github.com/ziutek/rrd"

	"database/sql"
	// "reflect"
	_ "github.com/go-sql-driver/mysql"
)

type Tag struct {
	ID   int  `json:"id"`
 	Fname string `json:"fname"`
 	Ds string `json:"ds"`
}
var  (
	buildCache bool = false
	legacyCache bool = false
	param string = "param"
	data string = ""
	debug bool = false
	verbose bool = false
	config Config
	configFile string
	appConfig AppConfig
	dbConfig DbConfig
	folderConfig FolderScan
	rejectFlag string
	collectFlag string
)

type QueryResponse struct {
	Target     string      `json:"target"`
	DataPoints [][]float64 `json:"datapoints"`
}

type SearchRequest struct {
	Target string `json:"target"`
}

type QueryRequest struct {
	PanelId interface{} `json:"panelId"`
	Range   struct {
		From string `json:"from"`
		To   string `json:"to"`
		Raw  struct {
			From string `json:"from"`
			To   string `json:"to"`
		} `json:"raw"`
	} `json:"range"`
	RangeRaw struct {
		From string `json:"from"`
		To   string `json:"to"`
	} `json:"rangeRaw"`
	Interval   string `json:"interval"`
	IntervalMs int64  `json:"intervalMs"`
	Targets    []struct {
		Target string `json:"target"`
		RefID  string `json:"refId"`
		Hide   bool   `json:"hide"`
		Type   string `json:"type"`
	} `json:"targets"`
	Format        string `json:"format"`
	MaxDataPoints int64  `json:"maxDataPoints"`
}

type AnnotationResponse struct {
	Annotation string `json:"annotation"`
	Time       int64  `json:"time"`
	Title      string `json:"title"`
	Tags       string `json:"tags"`
	Text       string `json:"text"`
}

type AnnotationCSV struct {
	Time  int64  `csv:"time"`
	Title string `csv:"title"`
	Tags  string `csv:"tags"`
	Text  string `csv:"text"`
}

type AnnotationRequest struct {
	Range struct {
		From string `json:"from"`
		To   string `json:"to"`
	} `json:"range"`
	RangeRaw struct {
		From string `json:"from"`
		To   string `json:"to"`
	} `json:"rangeRaw"`
	Annotation struct {
		Name       string `json:"name"`
		Datasource string `json:"datasource"`
		IconColor  string `json:"iconColor"`
		Enable     bool   `json:"enable"`
		Query      string `json:"query"`
	} `json:"annotation"`
}

type Config struct {
	Server ServerConfig
}

type ServerConfig struct {
	RrdPath            string
	Step               int
	SearchCache        int64
	IpAddr             string
	Port               int
	AnnotationFilePath string
	Multiplier         int
}

type DbConfig struct {
	User     string
	Password string
	Database string
	Table    string
	Server   string
	Port     int
}

type FolderScan struct {
	Collect []string `yaml:"collect"`
	Reject  []string `yaml:"reject"`
}

type AppConfig struct {
	Database DbConfig `yaml:"database"`
	Folders FolderScan `yaml:"folders"`
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

type ErrorResponse struct {
	Message string `json:"message"`
}

type SearchCache struct {
	m     sync.Mutex
	items []string
}

func NewSearchCache() *SearchCache {
	return &SearchCache{}
}

func (w *SearchCache) Get(target string) []string {
	newItems := []string{}
	if legacyCache {
		w.m.Lock()
		defer w.m.Unlock()
		return w.items
	} else {
		db, err := sql.Open("mysql", dbConfig.User + ":" + dbConfig.Password + "@tcp(" + dbConfig.Server + ":" +	strconv.Itoa(dbConfig.Port)	+	")/" + dbConfig.Database )

		if err != nil { panic(err.Error()) }
		defer db.Close()
		var query string
		// var rows *sql.Rows
		// NOTE: no new variables on left side of :=
                // rows = &sql.Rows {}
		var rows *sql.Rows = &sql.Rows {}

		if target != "" {
			query = "SELECT DISTINCT fname,ds FROM " + dbConfig.Table + " WHERE fname = ? ORDER BY fname"
			if verbose {
				fmt.Println("query: " + query)
			}
			rows, err = db.Query(query, target)
		} else {
			if data != "" {
				query = "SELECT DISTINCT fname,ds FROM " + dbConfig.Table + " WHERE expose LIKE ? ORDER BY fname"
				if verbose {
					fmt.Println("query: " + query)
				}
				rows, err = db.Query(query, "%" + data + "%")
			} else {
				query = "SELECT DISTINCT fname,ds FROM " + dbConfig.Table + " ORDER BY fname"
			if verbose {
				fmt.Println("query: " + query)
			}
			rows, err = db.Query(query)
			}
		}
		if err != nil { panic(err.Error()) }
		// fmt.Println(reflect.TypeOf(rows))
		for rows.Next() {
			var tag Tag
			err = rows.Scan(&tag.Fname,&tag.Ds)
			if err != nil { panic(err.Error()) }
			if verbose {
				fmt.Println("item: " + tag.Fname + ":" + tag.Ds)
			}
			newItems = append(newItems, tag.Fname + ":" + tag.Ds)
		}
		defer db.Close()
		// fmt.Errorf("fname %s not found, ", fname )
		return newItems
	}
}

// https://ispycode.com/GO/Collections/Arrays/Check-if-item-is-in-array
func contains(a []string, e string) bool {
	for _, t := range a {
		if t == e {
			return true
		}
	}
	return false
}

func lacks(a []string, e string) bool {
	if len(a) == 0 {
		return false
	}
	for _, t := range a {
		if t == e {
			return false
		}
	}
	return true
}
func (w *SearchCache) Update() {
	newItems := []string{}
	if verbose {
   		fmt.Println("Updating search cache.")
	}
	db, err := sql.Open("mysql", dbConfig.User + ":" + dbConfig.Password + "@tcp(" + dbConfig.Server + ":" +  strconv.Itoa(dbConfig.Port)  +  ")/" + dbConfig.Database )

	if err != nil { panic(err.Error()) }
	if verbose {
		fmt.Println("Connected to database.")
	}
	err = filepath.Walk(strings.TrimRight(config.Server.RrdPath, "/") + "/",
		// https://pkg.go.dev/path/filepath#WalkFunc
		func(path string, info os.FileInfo, err error) error {
			if err != nil {
				return err
			}
			if info.IsDir() {
				if contains(folderConfig.Reject,info.Name()) || lacks(folderConfig.Collect, info.Name()) {
					if verbose {
          					fmt.Println("Skip directory: " + info.Name())
					}
					return filepath.SkipDir
				} else {
					return nil
				}
			}
			if !strings.Contains(info.Name(), ".rrd") {
				return nil
			}
			rel, _ := filepath.Rel(config.Server.RrdPath, path)
			fName := strings.Replace(rel, ".rrd", "", 1)
			// convert to Apple III/ MacOS style path separators
			fName = strings.Replace(fName, "/", ":", -1)

			infoRes, err := rrd.Info(path)
			if err != nil {
				fmt.Println("ERROR: Cannot retrieve information from ", path)
				fmt.Println(err)
				// e.g. after
				// opening 'badlink.rrd': No such file or directory
				return nil
			}
			if ! legacyCache {
				// perform a db.Query delete
				if verbose {
					fmt.Println("Delete from database:" + "\"" + fName + "\"")
				}
				op, err := db.Query("DELETE FROM `" + dbConfig.Table + "` WHERE fname = ?", fName )
				if err != nil { panic(err.Error()) }
				defer op.Close()
			}
			for ds, _ := range infoRes["ds.index"].(map[string]interface{}) {
				if legacyCache {
					newItems = append(newItems, fName+":"+ds)
				} else {
					// TODO: compare file stat with ins_date and skip update if file is older
					// perform a db.Query insert
					if verbose {
						fmt.Println("Inserted into database:" + "\"" + fName + ":" + ds + "\"")
					}
					insert, err := db.Query("INSERT INTO `" + dbConfig.Table + "` (ins_date, fname, ds) VALUES ( now(), ?,  ? )", fName, ds)

					if err != nil { panic(err.Error()) }
					defer insert.Close()
				}
			}
			return nil
		})

	if err != nil {
		fmt.Printf("Error walking path: %s\n", err)
		return
	}

	w.m.Lock()
	defer w.m.Unlock()
	w.items = newItems
	defer db.Close()
	if verbose {
		fmt.Println("Closed database connection.")
		fmt.Println("Finished updating search cache.")
	}
}

var searchCache *SearchCache = NewSearchCache()

func respondJSON(w http.ResponseWriter, result interface{}) {
	json, err := json.Marshal(result)
	if err != nil {
		fmt.Println("ERROR: Cannot convert response data into JSON")
		fmt.Println(err)
	}
	w.Header().Set("Content-Type", "application/json; charset=utf-8")
	w.Header().Set("Access-Control-Allow-Origin", "*")
	w.Header().Set("Access-Control-Allow-Headers", "accept, content-type")
	w.Header().Set("Access-Control-Allow-Methods", "GET,POST,HEAD,OPTIONS")
	w.Write([]byte(json))
}

func hello(w http.ResponseWriter, r *http.Request) {
	if verbose {
		// TODO: echo header back
		for k, v := range r.Header {
			fmt.Printf( "Header field %q, Value %q\n", k, v)
		}
	}
	result := ErrorResponse{Message: "hello"}
	respondJSON(w, result)
}

func search(w http.ResponseWriter, r *http.Request) {
	var searchRequest SearchRequest
	var target string
	switch r.Method {
		case "GET":
			target = ""
		case "POST":
			decoder := json.NewDecoder(r.Body)
		err := decoder.Decode(&searchRequest)
		if err != nil {
			fmt.Println("ERROR: Cannot decode the request")
			fmt.Println(err)
		}
		defer r.Body.Close()
		target = searchRequest.Target
		fmt.Printf("Target: %s\n", target)
		default:
			fmt.Fprintf(w, "Sorry, only GET and POST methods are supported.")
	}
	var key string = strings.Title(strings.ToLower(param))
	data = r.Header.Get(key)
	if verbose {
		fmt.Println("Got: " + key + " = " + data)
	}
	var result = []string{}

	for _, path := range searchCache.Get(target) {
		if strings.Contains(path, target) {
			result = append(result, path)
		}
	}

	respondJSON(w, result)
}

func query(w http.ResponseWriter, r *http.Request) {
	if r.Method == "OPTIONS" {
		w.Header().Set("Access-Control-Allow-Origin", "*")
		w.Header().Set("Access-Control-Allow-Headers", "accept, content-type")
		w.Header().Set("Access-Control-Allow-Methods", "GET,POST,HEAD,OPTIONS")
		w.Write(nil)
		return
	}
	decoder := json.NewDecoder(r.Body)
	var queryRequest QueryRequest
	err := decoder.Decode(&queryRequest)
	if err != nil {
		fmt.Println("ERROR: Cannot decode the request")
		fmt.Println(err)
	}
	defer r.Body.Close()

	from, _ := time.Parse(time.RFC3339Nano, queryRequest.Range.From)
	to, _ := time.Parse(time.RFC3339Nano, queryRequest.Range.To)

	var result []QueryResponse
	for _, target := range queryRequest.Targets {
		ds := target.Target[strings.LastIndex(target.Target, ":")+1 : len(target.Target)]
		rrdDsRep := regexp.MustCompile(`:` + ds + `$`)
		fileSearchPath := rrdDsRep.ReplaceAllString(target.Target, "")
		fileSearchPath = strings.TrimRight(config.Server.RrdPath, "/") + "/" + strings.Replace(fileSearchPath, ":", "/", -1) + ".rrd"

		fileNameArray, _ := zglob.Glob(fileSearchPath)
		for _, filePath := range fileNameArray {
			points := make([][]float64, 0)
			if _, err = os.Stat(filePath); err != nil {
				fmt.Println("File", filePath, "does not exist")
				continue
			}
			infoRes, err := rrd.Info(filePath)
			if err != nil {
				fmt.Println("ERROR: Cannot retrieve information from ", filePath)
				fmt.Println(err)
			}
			lastUpdate := time.Unix(int64(infoRes["last_update"].(uint)), 0)
			if to.After(lastUpdate) && lastUpdate.After(from) {
				to = lastUpdate
			}
			fetchRes, err := rrd.Fetch(filePath, "AVERAGE", from, to, time.Duration(config.Server.Step)*time.Second)
			if err != nil {
				fmt.Println("ERROR: Cannot retrieve time series data from ", filePath)
				fmt.Println(err)
			}
			timestamp := fetchRes.Start
			dsIndex := int(infoRes["ds.index"].(map[string]interface{})[ds].(uint))
			// The last point is likely to contain wrong data (mostly a big number)
			// RowCnt-1 is for ignoring the last point (temporary solution)
			for i := 0; i < fetchRes.RowCnt-1; i++ {
				value := fetchRes.ValueAt(dsIndex, i)
				if !math.IsNaN(value) {
					product := float64(config.Server.Multiplier) * value
					points = append(points, []float64{product, float64(timestamp.Unix()) * 1000})
				} else {
					if verbose {
						// NOTE: golang date format "2021-09-15 22:05:00 +0000 UTC" is different from grafana's ""
						fmt.Println("Ignored missing data at " + timestamp.String() + " " + fmt.Sprintf("%f", float64(timestamp.Unix()) * 1000 ) )
					}
				}
				timestamp = timestamp.Add(fetchRes.Step)
			}
			defer fetchRes.FreeValues()

			extractedTarget := strings.Replace(filePath, ".rrd", "", -1)
			extractedTarget = strings.Replace(extractedTarget, config.Server.RrdPath, "", -1)
			extractedTarget = strings.Replace(extractedTarget, "/", ":", -1) + ":" + ds
			result = append(result, QueryResponse{Target: extractedTarget, DataPoints: points})
		}
	}
	respondJSON(w, result)
}

func annotations(w http.ResponseWriter, r *http.Request) {
	if r.Method == "OPTIONS" {
		w.Header().Set("Access-Control-Allow-Origin", "*")
		w.Header().Set("Access-Control-Allow-Headers", "accept, content-type")
		w.Header().Set("Access-Control-Allow-Methods", "POST")
		w.Write(nil)
		return
	}

	if config.Server.AnnotationFilePath == "" {
		result := ErrorResponse{Message: "Not configured"}
		respondJSON(w, result)
	} else {
		decoder := json.NewDecoder(r.Body)
		var annotationRequest AnnotationRequest
		err := decoder.Decode(&annotationRequest)
		defer r.Body.Close()
		if err != nil {
			result := ErrorResponse{Message: "Cannot decode the request"}
			respondJSON(w, result)
		} else {
			csvFile, err := os.OpenFile(config.Server.AnnotationFilePath, os.O_RDONLY, os.ModePerm)
			if err != nil {
				fmt.Println("ERROR: Cannot open the annotations CSV file ", config.Server.AnnotationFilePath)
				fmt.Println(err)
			}
			defer csvFile.Close()
			annots := []*AnnotationCSV{}

			if err := gocsv.UnmarshalFile(csvFile, &annots); err != nil {
				fmt.Println("ERROR: Cannot unmarshal the annotations CSV file.")
				fmt.Println(err)
			}

			result := []AnnotationResponse{}
			from, _ := time.Parse(time.RFC3339Nano, annotationRequest.Range.From)
			to, _ := time.Parse(time.RFC3339Nano, annotationRequest.Range.To)
			for _, a := range annots {
				if (from.Unix()*1000) <= a.Time && a.Time <= (to.Unix()*1000) {
					result = append(result, AnnotationResponse{Annotation: "annotation", Time: a.Time, Title: a.Title, Tags: a.Tags, Text: a.Text})
				}
			}
			respondJSON(w, result)
		}
	}
}

func SetArgs() {

	// to get help about accepted arg pass an invalid one e.g. -h
	flag.StringVar(&configFile, "f", "config.yaml", "Config File.")
	// NOTE: order is intentionally incorrect here:
	// configFile value is still the default one
	appConfig.getConf(configFile)
	dbConfig = appConfig.Database
	//  fmt.Println("database config:" + "\n" + "User: " + dbConfig.User + "\n" + "Database: " + dbConfig.Database + "\n" + "Server: " + dbConfig.Server + "\n" + "Table: " + dbConfig.Table + "\n" + "Port: " + strconv.Itoa(dbConfig.Port) + "\n" )

	folderConfig = appConfig.Folders
	// fmt.Println("folder scan config:")
	/*
	fmt.Println("collect:")
	for _, v := range folderConfig.Collect {
		fmt.Println(v)
	}
	fmt.Println("reject:")

	for _, v := range folderConfig.Reject {
		fmt.Println(v)
	}
	*/
	flag.StringVar(&dbConfig.User, "u", "java", "DB User.")
	flag.StringVar(&dbConfig.Password, "v", "password", "DB User Password.")
	flag.StringVar(&dbConfig.Database, "w", "test", "Database.")
	flag.StringVar(&dbConfig.Server, "x", "mysql-server", "DB Server.")
 	flag.IntVar(&dbConfig.Port, "y", 3306, "DB Server port.")

	flag.StringVar(&dbConfig.Table, "z", "cache_table", "Table.")
	flag.StringVar(&config.Server.IpAddr, "i", "", "Network interface IP address to listen on. (default: any)")
	flag.IntVar(&config.Server.Port, "p", 9000, "Server port.")
	flag.StringVar(&config.Server.RrdPath, "r", "./sample/", "Path for a directory that keeps RRD files.")
	flag.IntVar(&config.Server.Step, "s", 10, "Step in second.")
	flag.Int64Var(&config.Server.SearchCache, "c", 600, "Search cache in seconds.")
	flag.StringVar(&config.Server.AnnotationFilePath, "a", "", "Path for a file that has annotations.")
	flag.IntVar(&config.Server.Multiplier, "m", 1, "Value multiplier.")
	flag.BoolVar(&buildCache, "update", false, "update cache")
	flag.BoolVar(&verbose, "verbose", false, "verbose")
	_ = buildCache
	flag.BoolVar(&legacyCache, "legacy", false, "use legacy cache")
	_ = legacyCache
	flag.StringVar(&param, "param", "param", "Parameter to read from request headers")
	// NOTE: default values for collect and reject flags are blank
	flag.StringVar(&collectFlag, "collect", "", "Folders to collect.")
	flag.StringVar(&rejectFlag, "reject", "", "Folders to reject.")
	flag.Parse()
	if verbose {
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
	} else {
		if len(collectFlag) == 0 {
			folderConfig.Collect = []string{}
		} else {
			folderConfig.Collect = strings.Split(collectFlag, ",")
		}
		if len(rejectFlag ) == 0 {
			folderConfig.Reject = []string{}
		} else {
			folderConfig.Reject = strings.Split(rejectFlag, ",")
		}
	}
}

func main() {
	SetArgs()
	if (buildCache) {
		searchCache.Update()
		return
	} else {
		http.HandleFunc("/search", search)
		http.HandleFunc("/query", query)
		http.HandleFunc("/annotations", annotations)
		http.HandleFunc("/", hello)
		err := http.ListenAndServe(config.Server.IpAddr+":"+strconv.Itoa(config.Server.Port), nil)
		if err != nil {
			fmt.Println("ERROR:", err)
			os.Exit(1)
		}
	}
}


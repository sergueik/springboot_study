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
	"strconv"
	"strings"
	"sync"
	"time"

	"github.com/gocarina/gocsv"
	"github.com/mattn/go-zglob"
	"github.com/ziutek/rrd"

	"database/sql"
	_ "github.com/go-sql-driver/mysql"
)

type Tag struct {
	ID   int  `json:"id"`
 	Fname string `json:"fname"`
 	Ds string `json:"ds"`
}
var  (
	buildCacheFlag bool = false
	config Config
	databaseConfig DatabaseConfig
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

type DatabaseConfig struct {
	User     string
	Password string
	Database string
	Table    string
	Server   string
	Port     int
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
  db, err := sql.Open("mysql", databaseConfig.User + ":" + databaseConfig.Password + "@tcp(" + databaseConfig.Server + ":" +  strconv.Itoa(databaseConfig.Port)  +  ")/" + databaseConfig.Database )

	if err != nil { panic(err.Error()) }
	defer db.Close()
  var query string
  
  if target != "" {
    query = "SELECT DISTINCT fname,ds FROM " + databaseConfig.Table + " WHERE fname = ?"
  } else { 
    query = "SELECT DISTINCT fname,ds FROM " + databaseConfig.Table 
  }
	fmt.Println("querying the " + databaseConfig.Table + " table: " + query)

  rows, err := db.Query(query, target)
  if err != nil { panic(err.Error()) }
	for rows.Next() {
		var tag Tag
		err = rows.Scan(&tag.Fname,&tag.Ds)
		if err != nil { panic(err.Error()) }
		fmt.Println("item: " + tag.Fname + ":" + tag.Ds)
		newItems = append(newItems, tag.Fname + ":" + tag.Ds)
	}
	defer db.Close()
	w.m.Lock()
	defer w.m.Unlock()
	w.items = newItems
	// TODO: support legacy flag to switch to original implementation
	return w.items
}

func (w *SearchCache) Update() {
	newItems := []string{}

	fmt.Println("Updating search cache.")
	// db, db_err := sql.Open("mysql", "java:password@tcp(mysql-server:3306)/test")
  db, db_err := sql.Open("mysql", databaseConfig.User + ":" + databaseConfig.Password + "@tcp(" + databaseConfig.Server + ":" +  strconv.Itoa(databaseConfig.Port)  +  ")/" + databaseConfig.Database )

	if db_err != nil { panic(db_err.Error()) }
	// go compiler error: no new variables on left side of := 
	fmt.Println("Connected to database.")
	err := filepath.Walk(strings.TrimRight(config.Server.RrdPath, "/")+"/",
		func(path string, info os.FileInfo, err error) error {
			if err != nil {
				return err
			}

			if info.IsDir() || !strings.Contains(info.Name(), ".rrd") {
				return nil
			}
			rel, _ := filepath.Rel(config.Server.RrdPath, path)
			fName := strings.Replace(rel, ".rrd", "", 1)
			fName = strings.Replace(fName, "/", ":", -1)

			infoRes, err := rrd.Info(path)
			if err != nil {
				fmt.Println("ERROR: Cannot retrieve information from ", path)
				fmt.Println(err)
			}
			for ds, _ := range infoRes["ds.index"].(map[string]interface{}) {

				// TODO: support legacy flag
				// newItems = append(newItems, fName+":"+ds)

				// perform a db.Query insert
				fmt.Println("Inserted into database:" + "\"" + fName + ":" + ds + "\"")
				insert, err := db.Query("INSERT INTO `" + databaseConfig.Table + "` (ins_date, fname, ds) VALUES ( now(), ?,  ? )", fName, ds)

				if err != nil { panic(err.Error()) }

				defer insert.Close()
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
	fmt.Println("Closed database connection.")
	fmt.Println("Finished updating search cache.")

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
	result := ErrorResponse{Message: "hello"}
	respondJSON(w, result)
}

func search(w http.ResponseWriter, r *http.Request) {
	decoder := json.NewDecoder(r.Body)
	var searchRequest SearchRequest
	err := decoder.Decode(&searchRequest)
	if err != nil {
		fmt.Println("ERROR: Cannot decode the request")
		fmt.Println(err)
	}
	defer r.Body.Close()

	target := searchRequest.Target
	fmt.Printf("Target: %s\n", target)

	var result = []string{}

	if target != "" {
		for _, path := range searchCache.Get(target) {
			if strings.Contains(path, target) {
				result = append(result, path)
			}
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
  
	flag.StringVar(&databaseConfig.User, "u", "java", "DB User.")
	flag.StringVar(&databaseConfig.Password, "v", "password", "DB User Password.")
	flag.StringVar(&databaseConfig.Database, "w", "test", "Database.")
	flag.StringVar(&databaseConfig.Server, "x", "mysql-server", "DB Server.")
 	flag.IntVar(&databaseConfig.Port, "y", 3306, "DB Server port.")
 
	flag.StringVar(&databaseConfig.Table, "z", "cache_table", "Table.")
	flag.StringVar(&config.Server.IpAddr, "i", "", "Network interface IP address to listen on. (default: any)")
	flag.IntVar(&config.Server.Port, "p", 9000, "Server port.")
	flag.StringVar(&config.Server.RrdPath, "r", "./sample/", "Path for a directory that keeps RRD files.")
	flag.IntVar(&config.Server.Step, "s", 10, "Step in second.")
	flag.Int64Var(&config.Server.SearchCache, "c", 600, "Search cache in seconds.")
	flag.StringVar(&config.Server.AnnotationFilePath, "a", "", "Path for a file that has annotations.")
	flag.IntVar(&config.Server.Multiplier, "m", 1, "Value multiplier.")
	flag.BoolVar(&buildCacheFlag, "update", false, "update cache")
	_ = buildCacheFlag
	flag.Parse()
}

func main() {
	SetArgs()
	if (buildCacheFlag) { 
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


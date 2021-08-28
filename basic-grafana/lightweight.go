package main

import (
	"fmt"
	"flag"
	"net/http"
	"strconv"
	"strings"
)
var (
	port int = 8080
	debug bool = false
	param string = "param"
)
func main() {
	flag.IntVar(&port, "port", 8080, "Port.")
	flag.StringVar(&param, "param", "param", "Parameter to read from request headers")
	flag.BoolVar(&debug, "debug", false, "debug")
	flag.Parse()
	http.HandleFunc("/", index)

	http.ListenAndServe(":" + strconv.Itoa(port) , nil)
}

func index(w http.ResponseWriter, r *http.Request) {
	var key string = strings.Title(strings.ToLower(param))
	data := r.Header.Get(key)
	// alternatively loop over all available headers
	if debug {
		for k, v := range r.Header {
			fmt.Fprintf(w, "Header field %q, Value %q\n", k, v)
		}
	}
	if debug {
		fmt.Println("Got: " + key + " = " + data)
	}

	fmt.Fprintf(w, "Got: " + param + " = " + data)
}

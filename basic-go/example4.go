package main

import (
    "bufio"
    "fmt"
    "encoding/json"
    "strings"
    "os"
)
// autogenerated via https://transform.tools/json-to-go
type Data struct {
  Hi []struct {
    Hello string `json:"hello"`
  } `json:"hi"`
}

// https://stackoverflow.com/questions/5884154/read-text-file-into-string-array-and-write

func readLines(path string) ([]string, error) {

  file, err := os.Open(path)
  if err != nil {
    return nil, err
  }
  defer file.Close()

  var lines []string
  scanner := bufio.NewScanner(file)
  for scanner.Scan() {
    lines = append(lines, scanner.Text())
  }
  return lines, scanner.Err()
}

func main() {
  var (
    obj Data
  )
  lines, err := readLines("test.json")
  if err != nil { panic("read lines: " + err.Error()) }
  data := strings.Join(lines,"")
  fmt.Println(data)
  err = json.Unmarshal([]byte(data), &obj)
  if err != nil { panic("json deserialize: " + err.Error()) }
  s := fmt.Sprint(obj.Hi[0].Hello)
  fmt.Println(s)
}

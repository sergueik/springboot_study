package main

import (
  "fmt"
  "log"
  "os"
  "os/exec"
  "time"
)

func main() {
  if len(os.Args) != 2 {
    os.Exit(1)
  }
  logFilePath := os.Args[1]

  log.Println("Reading " + "\"" + logFilePath + "\"")
  // NOTE: no '-f' flag
  // NOTE: provied full path
  cmd := exec.Command( "/usr/bin/tail",  "-n 20", logFilePath)
  stdout, err := cmd.CombinedOutput()

  if err != nil {
    log.Fatal("failed to Start: " + err.Error())
  }  else {
    fmt.Printf("Started pid=%d\n" , cmd.Process.Pid)
    fmt.Printf(string(stdout))
    time.Sleep(1000 *  time.Millisecond)
  }
}

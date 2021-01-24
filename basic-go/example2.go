package main

import (
  "bufio"
  "fmt"
  "log"
  "os"
  "os/exec"
)

func main() {
  if len(os.Args) != 2 {
    os.Exit(1)
  }
  logFilePath := os.Args[1]

  log.Println("Reading " + logFilePath)


  cmd := exec.Command("/usr/bin/tail", "tail", "-f", "-n 20", logFilePath)
  stdout, err := cmd.StdoutPipe()
  if err != nil {
    log.Fatal(err)
  }

 // https://golang.org/src/bufio/example_test.go
  scanner := bufio.NewScanner(stdout)
  if err := cmd.Start(); err != nil {
    log.Fatal("failed to Start: " + err.Error())
  }
  for scanner.Scan() {
    line := scanner.Text()
    fmt.Printf("SCAN: %q\n", line)
    if line == "~~END~~" {
      break
    }
  }
  if err := scanner.Err(); err != nil {
    fmt.Fprintln(os.Stderr, "Error reading standard input:", err)
  } else {
    log.Println("scanner exit")
  }

  // https://blog.kowalczyk.info/article/wOYk/advanced-command-execution-in-go-with-osexec.html

  log.Printf("Waiting for command to finish...")

  err2 := cmd.Wait()


  if err2 != nil {
    log.Fatal("cmd.Wait error: " + err2.Error())
  } else {
    log.Println("Done")
  }
}


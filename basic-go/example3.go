// https://zetcode.com/golang/exec-command/
package main

import (
    "bytes"
    "fmt"
    "log"
    "os/exec"
    "strings"
)

func main() {
    cmd := exec.Command("/usr/bin/tr", "a-z", "A-Z")

    cmd.Stdin = strings.NewReader("and old falcon")

    var out bytes.Buffer
    cmd.Stdout = &out

    err := cmd.Run()

    if err != nil {
        log.Fatal(err)
    }

    fmt.Printf("translated phrase: %q\n", out.String())
}

package main

import (
	"bytes"
	"os/exec"
	"strconv"
	"strings"
	"time"
)

func main() {
	for j := 10000; j < 20000; j++ {
		println(j)
		jStr := strconv.Itoa(j)
		var buf bytes.Buffer
		okay := true
	ft:
		for i := 1; i < 9; i++ {
			iStr := strconv.Itoa(i)
			cmd := exec.Command("mpiexec", "-hostfile", "hostfile", "-n", iStr, "a.out")
			cmd.Stdin = strings.NewReader(jStr)
			cmd.Stdout = &buf
			c := make(chan bool)
			go func() {
				cmd.Run()
				c <- true
			}()
			select {
			case <-time.NewTimer(20 * time.Second).C:
				okay = false
				if err := cmd.Process.Kill(); err != nil {
					println(err)
				}
				break ft
			case <-c:
			}
			//	println(buf.String())
		}
		if okay {
			println(buf.String())
			break
		}
	}
}

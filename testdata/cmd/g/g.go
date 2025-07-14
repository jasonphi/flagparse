// want package:"pkgHasFlag\\(createsFlag\\)"

package main

import "example.com/flagparse/cmd/g/gflags" // want gflags:"pkgHasFlag\\(createsFlag\\)"

func main() { // want "main creates flags but doesn't call flag\\.Parse\\(\\)\\s+[\\w:/]+gflags\\.go:5:15: flag created\\s+[\\w:/]+gflags\\.go:8:9: flag created" main:"funcHasFlag\\(createsFlag\\)"
	gflags.CreateFoo2()
}

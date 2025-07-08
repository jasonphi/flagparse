package main

import (
	"golang.org/x/tools/go/analysis/singlechecker"

	"jphillips.io/flagparse"
)

func main() { singlechecker.Main(flagparse.Analyzer) }

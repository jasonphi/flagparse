// want package:"pkgHasFlag\\(parsesFlag\\)"

// Parse called without flags

package main

import "flag" // want flag:"pkgHasFlag\\(mayCreateFlag\\|mayParseFlag\\)"

func main() { // want main:"funcHasFlag\\(parsesFlag\\)"  "main calls flag.Parse\\(\\) but doesn't create flags"
	flag.Parse()
}

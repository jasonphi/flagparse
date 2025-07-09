// want package:"pkgHasFlag\\(createsFlag\\|parsesFlag\\)"

// Happy path, flags define and parse called

package main

import "flag" // want flag:"pkgHasFlag\\(mayCreateFlag\\|mayParseFlag\\)"

var f = flag.String("f", "", "f flag") // want f:"isFlag"

func main() { // want main:"funcHasFlag\\(parsesFlag\\)"
	flag.Parse()
}

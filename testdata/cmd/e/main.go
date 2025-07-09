// want package:"pkgHasFlag\\(createsFlag\\|parsesFlag\\)"

// Flags created and parse called but flags created after parse

package main

import "flag" // want flag:"pkgHasFlag\\(mayCreateFlag\\|mayParseFlag\\)"

func main() { // want main:"funcHasFlag\\(createsFlag\\|parsesFlag\\)"
	flag.Parse()
	f := flag.String("f", "", "f flag")
	_ = f
}

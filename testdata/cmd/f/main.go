// want package:"pkgHasFlag\\(createsFlag\\|parsesFlag\\)"

package main

import "flag" // want flag:"pkgHasFlag\\(mayCreateFlag\\|mayParseFlag\\)"

func blah() { // want blah:"funcHasFlag\\(createsFlag\\|parsesFlag\\)"
	f := flag.String("f", "", "f flag")
	flag.Parse()
	_ = f
}

func main() { // want main:"funcHasFlag\\(createsFlag\\|parsesFlag\\)"
	blah()
}

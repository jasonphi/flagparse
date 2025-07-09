// want package:"pkgHasFlag\\(createsFlag\\)"

// Flags created without parse

package main

import "flag" // want flag:"pkgHasFlag\\(mayCreateFlag\\|mayParseFlag\\)"

var f = flag.String("f", "", "f flag") // want f:"isFlag"

func main() {}

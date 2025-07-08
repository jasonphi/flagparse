// want package:"pkgHasFlag\\(createsFlag\\)"

package storage

import "flag" // want flag:"pkgHasFlag\\(mayCreateFlag|mayParseFlag\\)"

var FlagStorage string // want FlagStorage:"isFlag"

func init() { // want init:"funcHasFlag\\(createsFlag\\)"
	flag.StringVar(&FlagStorage, "foo2", "", "foo2 flag")
}

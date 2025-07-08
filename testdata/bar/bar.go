// want package:"pkgHasFlag\\(mayCreateFlag|mayParseFlag\\)"

package bar

import "flag" // want flag:"pkgHasFlag\\(mayCreateFlag|mayParseFlag\\)"

var BarStorage string // want BarStorage:"isFlag"

// BarFlag only creates a flag if called
func BarFlag() { // want BarFlag:"funcHasFlag\\(createsFlag\\)"
	flag.StringVar(&BarStorage, "bar", "", "bar flag")
}

func TransitiveFlag() { // want TransitiveFlag:"funcHasFlag\\(createsFlag\\)"
	BarFlag()
}

func Baz() *string {
	return new(string)
}

func Parse() { // want Parse:"funcHasFlag\\(parsesFlag\\)"
	flag.Parse()
}

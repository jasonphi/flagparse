// want package:"pkgHasFlag\\(createsFlag\\|parsesFlag\\)"

package importbar

import "example.com/flagparse/bar" // want bar:"pkgHasFlag\\(mayCreateFlag|mayParseFlag\\)"

func init() { // want init:"funcHasFlag\\(createsFlag\\|parsesFlag\\)"
	bar.BarFlag()
	bar.Parse()
}

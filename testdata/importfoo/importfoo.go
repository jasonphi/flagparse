// want package:"pkgHasFlag\\(createsFlag\\)"

// Importing foo causes this package to transitively create flags

package importfoo

import _ "example.com/flagparse/foo" // want _:"pkgHasFlag\\(createsFlag\\)"

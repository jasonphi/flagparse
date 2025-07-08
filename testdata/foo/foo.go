// want package:"pkgHasFlag\\(createsFlag\\)"

package foo

import (
	"flag" // want flag:"pkgHasFlag\\(mayCreateFlag|mayParseFlag\\)"
	"fmt"
)

var (
	FooFlag  = flag.Int("foo", 0, "foo flag") // want FooFlag:"isFlag"
	Foo2Flag = CreateFoo2()                   // want Foo2Flag:"isFlag"
)

type Bar int

// Make sure methods named "init" don't get handled incorrectly
func (Bar) init() {
	fmt.Println("init bar")
}

// Make sure methods named "main" don't get handled incorrectly
func (Bar) main() {
	fmt.Println("main bar")
}

func CreateFoo2() *int { // want CreateFoo2:"funcHasFlag\\(createsFlag\\)"
	return flag.Int("foo2", 0, "foo2 flag")
}

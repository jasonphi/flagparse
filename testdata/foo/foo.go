// want package:"pkgHasFlag\\(createsFlag\\)"

package foo

import (
	"flag" // want flag:"pkgHasFlag\\(mayCreateFlag\\|mayParseFlag\\)"
	"fmt"
)

var (
	something     = 10
	somethingElse = "foo"
	FooFlag       = flag.Int("foo", 0, "foo flag") // want FooFlag:"isFlag"
	Foo2Flag      = CreateFoo2()                   // want Foo2Flag:"isFlag"
	// Even though Foo3Flag is dynamic, we can confirm that it's initialized with a flag function
	Foo3Flag = func() *int { // want Foo3Flag:"isFlag"
		return flag.Int("foo3", 0, "foo3 flag")
	}()
	// This function variable is dynamic and there's no way for us to know that when it's
	// called it's still set to a flag function. We tag it as a "maybe" flag function
	Foo4Flag = func() *int { // want Foo4Flag:"funcHasFlag\\(mayCreateFlag\\)"
		return flag.Int("foo4", 0, "foo4 flag")
	}
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

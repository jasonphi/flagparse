package ignoretest

import "fmt"

// The non-test files in this package don't do anything with flags but the test files do.
//
// This test ensures that test files are ignored

func ignoreMe() {
	fmt.Println("ignore me")
}

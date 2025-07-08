package ignoretest

import (
	"flag"
	"testing"
)

// The flags in this test file should be ignored by the analyzer

func Test(t *testing.T) {
	flag.String("foo", "", "")
	flag.Parse()
}

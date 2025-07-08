package flagparse_test

import (
	"testing"

	"golang.org/x/tools/go/analysis/analysistest"

	"jphillips.io/flagparse"
)

func Test(t *testing.T) {
	testdata := analysistest.TestData()

	analysistest.Run(t, testdata, flagparse.Analyzer,
		"example.com/flagparse/bar",
		"example.com/flagparse/baz",
		"example.com/flagparse/foo",
		"example.com/flagparse/importbar",
		"example.com/flagparse/importfoo",
		"example.com/flagparse/ignoretest",
		"example.com/flagparse/storage",
	)
}

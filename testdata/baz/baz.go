// want package:"pkgHasFlag\\(mayCreateFlag|mayParseFlag\\)"

package baz

import "flag" // want flag:"pkgHasFlag\\(mayCreateFlag|mayParseFlag\\)"

func GetFlagset() *flag.FlagSet { // want GetFlagset:"funcHasFlag\\(createsFlag\\)"
	fs := flag.NewFlagSet("baz", flag.ContinueOnError)
	fs.String("foo", "", "foo flag")

	return fs
}

func ParseFlagset(fs *flag.FlagSet, args []string) error { // want ParseFlagset:"funcHasFlag\\(parsesFlag\\)"
	return fs.Parse(args)
}

func GetFlags() *flag.FlagSet { // want GetFlags:"funcHasFlag\\(createsFlag\\)"
	flag.CommandLine.String("foo", "", "foo flag")
	return flag.CommandLine
}

func ParseFlags(args []string) { // want ParseFlags:"funcHasFlag\\(parsesFlag\\)"
	flag.Parse()
}

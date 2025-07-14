package gflags

import "flag"

var FooFlag = flag.Int("foo", 0, "foo flag")

func CreateFoo2() *int {
	return flag.Int("foo2", 0, "foo2 flag")
}

func CreateFoo3() *int {
	return flag.Int("foo3", 0, "foo3 flag")
}

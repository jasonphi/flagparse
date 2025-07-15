package flagparse

import (
	"cmp"
	"fmt"
	"go/ast"
	"go/token"
	"go/types"
	"reflect"
	"slices"
	"strings"

	"golang.org/x/tools/go/analysis"
	"golang.org/x/tools/go/analysis/passes/inspect"
	"golang.org/x/tools/go/ast/inspector"
	"golang.org/x/tools/go/types/typeutil"
)

var Analyzer = &analysis.Analyzer{
	Name:       "flagparse",
	Doc:        "TODO: documentation",
	Run:        run,
	Requires:   []*analysis.Analyzer{inspect.Analyzer},
	ResultType: reflect.TypeOf((*Result)(nil)),
	FactTypes: []analysis.Fact{
		new(pkgHasFlag),
		new(funcHasFlag),
		new(isFlag),
	},
}

type Kind int

func (k Kind) Has(v Kind) bool {
	return k&v == v
}

func (k Kind) Set(v Kind) Kind {
	return k | v
}

func (k Kind) Clear(v Kind) Kind {
	return k &^ v
}

func (k Kind) String() string {
	var s []string
	if k.Has(KindMayCreateFlag) {
		s = append(s, "mayCreateFlag")
	}
	if k.Has(KindMayParseFlag) {
		s = append(s, "mayParseFlag")
	}
	if k.Has(KindCreatesFlag) {
		s = append(s, "createsFlag")
	}
	if k.Has(KindParsesFlag) {
		s = append(s, "parsesFlag")
	}
	if k.Has(KindRootFunc) {
		s = append(s, "rootFunc")
	}
	return strings.Join(s, "|")
}

const (
	KindNone          Kind = 0
	KindMayParseFlag       = 1 << (iota - 1) // Package has one or more functions that parse flags
	KindMayCreateFlag                        // Package has one or more functions that create flags
	KindParsesFlag                           // Function or package parses flags
	KindCreatesFlag                          // Function or package creates flags
	KindRootFunc                             // Func is a root function that creates flags
)

// => *types.Package p has one or more flags, either directly as variables or
// indirectly via a function call
type pkgHasFlag struct {
	Kind  Kind
	Roots []ast.Node
}

func (*pkgHasFlag) AFact() {}

func (p *pkgHasFlag) String() string {
	var s strings.Builder
	s.WriteString("pkgHasFlag(")
	s.WriteString(p.Kind.String())
	s.WriteString(")")
	return s.String()
}

type funcHasFlag struct { // => *types.Func f defines one or more flags
	Kind  Kind
	Roots []ast.Node
}

func (*funcHasFlag) AFact() {}

func (f *funcHasFlag) String() string {
	var s strings.Builder
	s.WriteString("funcHasFlag(")
	s.WriteString(f.Kind.String())
	s.WriteString(")")
	return s.String()
}

// => *types.Var v is itself a flag or may be used as the storage for a flag
type isFlag struct{}

func (*isFlag) AFact() {}

func (f *isFlag) String() string {
	return "isFlag"
}

type Result struct {
	pkgs map[*types.Package]Kind
	objs map[types.Object]Kind
}

func (r *Result) Kind(v any) (Kind, bool) {
	switch v := v.(type) {
	case *types.PkgName:
		// Even though PkgName implements types.Object we handle it like a package
		k, ok := r.pkgs[v.Imported()]
		return k, ok
	case types.Object: // *types.Val or *types.Func
		k, ok := r.objs[v]
		return k, ok
	case *types.Package:
		k, ok := r.pkgs[v]
		return k, ok
	default:
		return KindNone, false
	}
}

type flagWrapper interface {
	Obj() types.Object
	flagWrapper()
}

type funcWrapper struct {
	obj     *types.Func
	fdecl   *ast.FuncDecl
	callers []flagCaller
}

type varWrapper struct {
	obj  *types.Var
	expr ast.Expr
	refs []*types.Func // flag wrapper functions that reference this variable
}

type importWrapper struct {
	obj   *types.PkgName
	idecl *ast.ImportSpec
}

func (f *funcWrapper) Obj() types.Object   { return f.obj }
func (v *varWrapper) Obj() types.Object    { return v.obj }
func (i *importWrapper) Obj() types.Object { return i.obj }
func (f *funcWrapper) flagWrapper()        {}
func (v *varWrapper) flagWrapper()         {}
func (i *importWrapper) flagWrapper()      {}

type flagCaller struct {
	w    *funcWrapper
	call *ast.CallExpr
	fn   *types.Func
}

var isKindFunc = map[string]Kind{
	"flag.Parse":                                           KindParsesFlag | KindRootFunc,
	"flag.Bool":                                            KindCreatesFlag | KindRootFunc,
	"flag.BoolFunc":                                        KindCreatesFlag | KindRootFunc,
	"flag.BoolVar":                                         KindCreatesFlag | KindRootFunc,
	"flag.Duration":                                        KindCreatesFlag | KindRootFunc,
	"flag.DurationVar":                                     KindCreatesFlag | KindRootFunc,
	"flag.Float64":                                         KindCreatesFlag | KindRootFunc,
	"flag.Float64Var":                                      KindCreatesFlag | KindRootFunc,
	"flag.Func":                                            KindCreatesFlag | KindRootFunc,
	"flag.Int":                                             KindCreatesFlag | KindRootFunc,
	"flag.IntVar":                                          KindCreatesFlag | KindRootFunc,
	"flag.Int64":                                           KindCreatesFlag | KindRootFunc,
	"flag.Int64Var":                                        KindCreatesFlag | KindRootFunc,
	"flag.String":                                          KindCreatesFlag | KindRootFunc,
	"flag.StringVar":                                       KindCreatesFlag | KindRootFunc,
	"flag.TextVar":                                         KindCreatesFlag | KindRootFunc,
	"flag.Uint":                                            KindCreatesFlag | KindRootFunc,
	"flag.UintVar":                                         KindCreatesFlag | KindRootFunc,
	"flag.Uint64":                                          KindCreatesFlag | KindRootFunc,
	"flag.Uint64Var":                                       KindCreatesFlag | KindRootFunc,
	"flag.Var":                                             KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Parse":                         KindParsesFlag | KindRootFunc,
	"github.com/spf13/pflag.ParseAll":                      KindParsesFlag | KindRootFunc,
	"github.com/spf13/pflag.Bool":                          KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.BoolP":                         KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.BoolSlice":                     KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.BoolSliceP":                    KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.BoolSliceVar":                  KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.BoolVar":                       KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.BoolVarP":                      KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.BytesBase64":                   KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.BytesBase64P":                  KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.BytesBase64Var":                KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.BytesBase64VarP":               KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.BytesHex":                      KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.BytesHexP":                     KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.BytesHexVar":                   KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.BytesHexVarP":                  KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Count":                         KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.CountP":                        KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.CountVar":                      KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.CountVarP":                     KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Duration":                      KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.DurationP":                     KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.DurationSlice":                 KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.DurationSliceP":                KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.DurationSliceVar":              KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.DurationVar":                   KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.DurationVarP":                  KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Float32":                       KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Float32P":                      KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Float32Slice":                  KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Float32SliceP":                 KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Float32SliceVar":               KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Float32Var":                    KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Float32VarP":                   KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Float64":                       KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Float64P":                      KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Float64Slice":                  KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Float64SliceP":                 KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Float64SliceVar":               KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Float64Var":                    KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Float64VarP":                   KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.IP":                            KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.IPMask":                        KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.IPMaskP":                       KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.IPMaskVar":                     KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.IPMaskVarP":                    KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.IPNet":                         KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.IPNetP":                        KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.IPNetSlice":                    KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.IPNetSliceP":                   KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.IPNetSliceVar":                 KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.IPNetVar":                      KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.IPNetVarP":                     KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.IPP":                           KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.IPSlice":                       KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.IPSliceP":                      KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.IPSliceVar":                    KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.IPVar":                         KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.IPVarP":                        KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Int":                           KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Int16":                         KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Int16P":                        KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Int16Var":                      KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Int16VarP":                     KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Int32":                         KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Int32P":                        KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Int32Slice":                    KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Int32SliceP":                   KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Int32SliceVar":                 KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Int32Var":                      KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Int32VarP":                     KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Int64":                         KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Int64P":                        KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Int64Slice":                    KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Int64SliceP":                   KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Int64SliceVar":                 KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Int64Var":                      KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Int64VarP":                     KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Int8":                          KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Int8P":                         KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Int8Var":                       KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Int8VarP":                      KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.IntP":                          KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.IntSlice":                      KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.IntSliceP":                     KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.IntSliceVar":                   KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.IntVar":                        KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.IntVarP":                       KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.String":                        KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.StringArray":                   KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.StringArrayP":                  KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.StringArrayVar":                KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.StringArrayVarP":               KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.StringP":                       KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.StringSlice":                   KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.StringSliceP":                  KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.StringSliceVar":                KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.StringToInt":                   KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.StringToInt64":                 KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.StringToInt64P":                KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.StringToInt64Var":              KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.StringToInt64VarP":             KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.StringToIntP":                  KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.StringToIntVar":                KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.StringToIntVarP":               KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.StringToString":                KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.StringToStringP":               KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.StringToStringVar":             KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.StringToStringVarP":            KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.StringVar":                     KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.StringVarP":                    KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Uint":                          KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Uint16":                        KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Uint16P":                       KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Uint16Var":                     KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Uint16VarP":                    KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Uint32":                        KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Uint32P":                       KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Uint32Slice":                   KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Uint32SliceP":                  KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Uint32SliceVar":                KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Uint32Var":                     KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Uint32VarP":                    KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Uint64":                        KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Uint64P":                       KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Uint64Slice":                   KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Uint64SliceP":                  KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Uint64SliceVar":                KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Uint64Var":                     KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Uint64VarP":                    KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Uint8":                         KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Uint8P":                        KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Uint8Var":                      KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Uint8VarP":                     KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.UintP":                         KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.UintSlice":                     KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.UintSliceP":                    KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.UintSliceVar":                  KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.UintVar":                       KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.UintVarP":                      KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.Var":                           KindCreatesFlag | KindRootFunc,
	"github.com/spf13/pflag.VarP":                          KindCreatesFlag | KindRootFunc,
	"(*flag.FlagSet).Parse":                                KindParsesFlag | KindRootFunc,
	"(*flag.FlagSet).Bool":                                 KindCreatesFlag | KindRootFunc,
	"(*flag.FlagSet).BoolFunc":                             KindCreatesFlag | KindRootFunc,
	"(*flag.FlagSet).BoolVar":                              KindCreatesFlag | KindRootFunc,
	"(*flag.FlagSet).Duration":                             KindCreatesFlag | KindRootFunc,
	"(*flag.FlagSet).DurationVar":                          KindCreatesFlag | KindRootFunc,
	"(*flag.FlagSet).Float64":                              KindCreatesFlag | KindRootFunc,
	"(*flag.FlagSet).Float64Var":                           KindCreatesFlag | KindRootFunc,
	"(*flag.FlagSet).Func":                                 KindCreatesFlag | KindRootFunc,
	"(*flag.FlagSet).Int":                                  KindCreatesFlag | KindRootFunc,
	"(*flag.FlagSet).IntVar":                               KindCreatesFlag | KindRootFunc,
	"(*flag.FlagSet).Int64":                                KindCreatesFlag | KindRootFunc,
	"(*flag.FlagSet).Int64Var":                             KindCreatesFlag | KindRootFunc,
	"(*flag.FlagSet).String":                               KindCreatesFlag | KindRootFunc,
	"(*flag.FlagSet).StringVar":                            KindCreatesFlag | KindRootFunc,
	"(*flag.FlagSet).TextVar":                              KindCreatesFlag | KindRootFunc,
	"(*flag.FlagSet).Uint":                                 KindCreatesFlag | KindRootFunc,
	"(*flag.FlagSet).UintVar":                              KindCreatesFlag | KindRootFunc,
	"(*flag.FlagSet).Uint64":                               KindCreatesFlag | KindRootFunc,
	"(*flag.FlagSet).Uint64Var":                            KindCreatesFlag | KindRootFunc,
	"(*flag.FlagSet).Var":                                  KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Parse":              KindParsesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).ParseAll":           KindParsesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Bool":               KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).BoolP":              KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).BoolSlice":          KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).BoolSliceP":         KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).BoolSliceVar":       KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).BoolVar":            KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).BoolVarP":           KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).BytesBase64":        KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).BytesBase64P":       KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).BytesBase64Var":     KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).BytesBase64VarP":    KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).BytesHex":           KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).BytesHexP":          KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).BytesHexVar":        KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).BytesHexVarP":       KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Count":              KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).CountP":             KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).CountVar":           KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).CountVarP":          KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Duration":           KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).DurationP":          KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).DurationSlice":      KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).DurationSliceP":     KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).DurationSliceVar":   KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).DurationVar":        KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).DurationVarP":       KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Float32":            KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Float32P":           KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Float32Slice":       KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Float32SliceP":      KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Float32SliceVar":    KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Float32Var":         KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Float32VarP":        KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Float64":            KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Float64P":           KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Float64Slice":       KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Float64SliceP":      KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Float64SliceVar":    KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Float64Var":         KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Float64VarP":        KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).IP":                 KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).IPMask":             KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).IPMaskP":            KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).IPMaskVar":          KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).IPMaskVarP":         KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).IPNet":              KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).IPNetP":             KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).IPNetSlice":         KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).IPNetSliceP":        KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).IPNetSliceVar":      KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).IPNetVar":           KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).IPNetVarP":          KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).IPP":                KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).IPSlice":            KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).IPSliceP":           KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).IPSliceVar":         KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).IPVar":              KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).IPVarP":             KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Int":                KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Int16":              KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Int16P":             KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Int16Var":           KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Int16VarP":          KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Int32":              KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Int32P":             KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Int32Slice":         KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Int32SliceP":        KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Int32SliceVar":      KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Int32Var":           KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Int32VarP":          KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Int64":              KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Int64P":             KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Int64Slice":         KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Int64SliceP":        KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Int64SliceVar":      KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Int64Var":           KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Int64VarP":          KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Int8":               KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Int8P":              KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Int8Var":            KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Int8VarP":           KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).IntP":               KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).IntSlice":           KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).IntSliceP":          KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).IntSliceVar":        KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).IntVar":             KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).IntVarP":            KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).String":             KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).StringArray":        KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).StringArrayP":       KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).StringArrayVar":     KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).StringArrayVarP":    KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).StringP":            KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).StringSlice":        KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).StringSliceP":       KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).StringSliceVar":     KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).StringToInt":        KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).StringToInt64":      KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).StringToInt64P":     KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).StringToInt64Var":   KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).StringToInt64VarP":  KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).StringToIntP":       KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).StringToIntVar":     KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).StringToIntVarP":    KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).StringToString":     KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).StringToStringP":    KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).StringToStringVar":  KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).StringToStringVarP": KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).StringVar":          KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).StringVarP":         KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Uint":               KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Uint16":             KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Uint16P":            KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Uint16Var":          KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Uint16VarP":         KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Uint32":             KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Uint32P":            KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Uint32Slice":        KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Uint32SliceP":       KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Uint32SliceVar":     KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Uint32Var":          KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Uint32VarP":         KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Uint64":             KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Uint64P":            KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Uint64Slice":        KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Uint64SliceP":       KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Uint64SliceVar":     KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Uint64Var":          KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Uint64VarP":         KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Uint8":              KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Uint8P":             KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Uint8Var":           KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Uint8VarP":          KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).UintP":              KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).UintSlice":          KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).UintSliceP":         KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).UintSliceVar":       KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).UintVar":            KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).UintVarP":           KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).Var":                KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).VarP":               KindCreatesFlag | KindRootFunc,
	"(*github.com/spf13/pflag.FlagSet).VarPF":              KindCreatesFlag | KindRootFunc,
}

// Functions in this map have a pointer value as their first argument, which acts as the
// storage for a flag variable.
var hasVarRef = map[string]struct{}{
	"flag.BoolVar":                {},
	"flag.DurationVar":            {},
	"flag.Float64Var":             {},
	"flag.IntVar":                 {},
	"flag.Int64Var":               {},
	"flag.StringVar":              {},
	"flag.TextVar":                {},
	"flag.UintVar":                {},
	"flag.Uint64Var":              {},
	"(*flag.FlagSet).BoolVar":     {},
	"(*flag.FlagSet).DurationVar": {},
	"(*flag.FlagSet).Float64Var":  {},
	"(*flag.FlagSet).IntVar":      {},
	"(*flag.FlagSet).Int64Var":    {},
	"(*flag.FlagSet).StringVar":   {},
	"(*flag.FlagSet).TextVar":     {},
	"(*flag.FlagSet).UintVar":     {},
	"(*flag.FlagSet).Uint64Var":   {},
	"(*flag.FlagSet).Var":         {},
}

// Ignorelist of packages that do weird things with flags but are otherwise innocuous
var ignoredPkgs = map[string]struct{}{
	"k8s.io/klog/v2":    {},
	"net/http/httptest": {},
}

// Packages that act as root packages to terminate roots
var rootPkgs = map[string]struct{}{
	"flag":                   {},
	"github.com/spf13/pflag": {},
}

func run(pass *analysis.Pass) (any, error) {
	res := &Result{
		pkgs: make(map[*types.Package]Kind),
		objs: make(map[types.Object]Kind),
	}
	find(pass, res)
	check(pass)
	return res, nil
}

func find(pass *analysis.Pass, res *Result) error {
	byObj := make(map[types.Object]flagWrapper)
	var (
		funcWrappers   []*funcWrapper
		varWrappers    []*varWrapper
		importWrappers []*importWrapper
	)
	for _, file := range pass.Files {
		// Flags in test files have different semantics than flags in non-test packages. More
		// importantly, functions and identifiers in test files aren't part of the normal package
		// and don't affect importing packages. So don't include test files in the analysis.
		filename := pass.Fset.Position(file.Pos()).Filename
		// Synthetic test packages are written to a temp directory and the generated filenames don't
		// end in ".go"
		if !strings.HasSuffix(filename, ".go") || strings.HasSuffix(filename, "_test.go") {
			continue
		}

		for _, decl := range file.Decls {
			for _, ws := range maybeFlagWrapper(pass.TypesInfo, decl) {
				byObj[ws.Obj()] = ws
				switch w := ws.(type) {
				case *funcWrapper:
					funcWrappers = append(funcWrappers, w)
				case *varWrapper:
					varWrappers = append(varWrappers, w)
				case *importWrapper:
					importWrappers = append(importWrappers, w)
				}
			}
		}
	}

	// Walk the func graph to figure out which are really flag wrappers.
	for _, w := range funcWrappers {
		// Scan function for calls that could be to other flag functions.
		ast.Inspect(w.fdecl.Body, func(n ast.Node) bool {
			call, ok := n.(*ast.CallExpr)
			if !ok {
				return true
			}

			fn, va, kind := flagNameAndKind(pass, call)
			if kind != KindNone {
				if wrap, ok := byObj[va].(*varWrapper); ok {
					wrap.refs = append(wrap.refs, fn)
				}
				checkFlagFwd(pass, w, call, fn, kind, res)
				return true
			}

			// If the call is to another function in this package,
			// maybe we will find out it is flag-like later.
			// Remember this call for later checking.
			if fn != nil && fn.Pkg() == pass.Pkg && byObj[fn] != nil && w.obj != fn {
				callee := byObj[fn].(*funcWrapper)
				callee.callers = append(callee.callers, flagCaller{w: w, call: call, fn: fn})
			}

			return true
		})
	}

	// Now that we've walked all funcs we can analyze all vars to see which ones are initialized
	// by calls to a flag function.
	for _, w := range varWrappers {
		checkVar(pass, w, res)
	}

	for _, w := range importWrappers {
		checkImport(pass, w, res)
	}

	return nil
}

func flagNameAndKind(pass *analysis.Pass, call *ast.CallExpr) (fn *types.Func, va *types.Var, kind Kind) {
	fn, _ = typeutil.Callee(pass.TypesInfo, call).(*types.Func)
	if fn == nil {
		// TODO: here would be where we'd capture calls to named flag variable functions
		return
	}

	// Facts are associated with generic declarations, not instantiations.
	fn = fn.Origin()

	fullName := fn.FullName()
	ok := false
	if kind, ok = isKindFunc[fullName]; ok {
		// If the func is a variant that takes a storage argument
		// try to determine the var it references
		if _, ok = hasVarRef[fullName]; ok {
			args := call.Args
			if len(args) == 0 {
				return
			}

			// The first argument to "var" flag functions is always a pointer
			// to the storage variable
			expr, ok := args[0].(*ast.UnaryExpr)
			if !ok || expr.Op != token.AND {
				return
			}

			ident, ok := expr.X.(*ast.Ident)
			if !ok {
				return
			}

			va, ok = pass.TypesInfo.ObjectOf(ident).(*types.Var)
		}

		return fn, va, kind
	}

	var fact funcHasFlag
	if pass.ImportObjectFact(fn, &fact) {
		return fn, nil, fact.Kind
	}

	return
}

func checkFlagFwd(pass *analysis.Pass, w *funcWrapper, call *ast.CallExpr, callFn *types.Func, kind Kind, res *Result) {
	fn := w.obj

	var fact, callFact funcHasFlag
	pass.ImportObjectFact(fn, &fact)
	pass.ImportObjectFact(callFn, &callFact)

	fact.Kind |= kind.Clear(KindRootFunc)
	fact.Roots = handleFactRoots(
		func() bool {
			_, root := rootPkgs[callFn.Pkg().Path()]
			// Ignore roots from the standard library, and pflag so roots terminate at the top-level functions
			return !root
		},
		fact.Roots,
		callFact.Roots...,
	)
	fact.Roots = handleFactRoots(
		func() bool {
			return kind.Has(KindRootFunc) && call != nil
		},
		fact.Roots,
		call,
	)
	pass.ExportObjectFact(fn, &fact)
	res.objs[fn] = kind
	for _, caller := range w.callers {
		checkFlagFwd(pass, caller.w, caller.call, caller.fn, kind, res)
	}

	var (
		pfact pkgHasFlag
		pkg   = fn.Pkg()
	)
	pass.ImportPackageFact(pkg, &pfact)

	pkgPred := func() bool {
		if w.fdecl.Recv != nil {
			return false
		}
		callerName := w.fdecl.Name.Name
		if callerName == "init" {
			return true
		}
		return pkg.Name() == "main" && callerName == "main"
	}

	// Even if calling this method causes some flag related action to occur, we haven't confirmed
	// this function has been called in the application. If the calling function is
	// "init" or "main" then accept the kind as-is, otherwise convert it to a "may" kind until
	// we handle the function directly from the main package.
	pfact.Kind = handlePackageMaybeKind(pkgPred, pfact.Kind, kind, KindMayCreateFlag, KindCreatesFlag)
	pfact.Kind = handlePackageMaybeKind(pkgPred, pfact.Kind, kind, KindMayParseFlag, KindParsesFlag)

	if pfact.Kind != KindNone {
		pfact.Roots = handleFactRoots(
			func() bool {
				return pkgPred() && call != nil
			},
			pfact.Roots,
			fact.Roots...,
		)
		pfact.Roots = handleFactRoots(
			func() bool {
				return pkgPred() && kind.Has(KindRootFunc) && call != nil
			},
			pfact.Roots,
			call,
		)
		pass.ExportPackageFact(&pfact)
	}

	res.pkgs[pkg] = pfact.Kind
}

func checkVar(pass *analysis.Pass, w *varWrapper, res *Result) {
	va := w.obj
	var (
		flagFact isFlag
		funcFact funcHasFlag
	)

	if pass.ImportObjectFact(va, &flagFact) && pass.ImportObjectFact(va, &funcFact) {
		// If true we already have a fact for this var
		return
	}

	switch {
	case len(w.refs) > 0: // This is storage for a flag variable somewhere
		// Unconditionally add a fact to the variable but don't update the package
		// fact, considering the function initializing the flag may not be
		// called during package init. Let the function call update the package fact.
		pass.ExportObjectFact(va, &flagFact)
	case w.expr != nil: // The variable was initialized to something
		// If this is a not-called function literal then we'd have to track the variable
		// usage at runtime to accurately determine if it's called "as" a flag function.
		// The best we can probably do is mark it as a "maybe" flag function
		_, isFuncLiteral := w.expr.(*ast.FuncLit)

		ast.Inspect(w.expr, func(n ast.Node) bool {
			// Determine if the expression is a call expression
			call, ok := n.(*ast.CallExpr)
			if !ok {
				return true
			}

			callFn, _, kind := flagNameAndKind(pass, call)
			if kind == KindNone {
				return true
			}

			// If the top-level expression is a function literal then it acts more like a
			// flag function than a flag variable.
			if isFuncLiteral {
				pred := func() bool { return false }
				funcFact.Kind = handlePackageMaybeKind(pred, funcFact.Kind, kind, KindMayCreateFlag, KindCreatesFlag)
				funcFact.Kind = handlePackageMaybeKind(pred, funcFact.Kind, kind, KindMayParseFlag, KindParsesFlag)

				pred = func() bool { return kind.Has(KindRootFunc) && call != nil }
				funcFact.Roots = handleFactRoots(pred, funcFact.Roots, call)

				pass.ExportObjectFact(va, &funcFact)
				res.objs[va] = kind
				return true
			}

			// This initializing expression is a flag func of some kind and this is a top-level call
			// so the var and package need new facts
			pass.ExportObjectFact(va, &flagFact)

			var (
				pfact    pkgHasFlag
				callFact funcHasFlag
				pkg      = va.Pkg()
			)
			pass.ImportPackageFact(pkg, &pfact)
			pass.ImportObjectFact(callFn, &callFact)

			pfact.Roots = handleFactRoots(
				func() bool {
					_, root := rootPkgs[callFn.Pkg().Path()]
					// Ignore roots from the standard library, and pflag so roots terminate at the top-level functions
					return !root
				},
				pfact.Roots,
				callFact.Roots...,
			)

			// We know this var is top-level so the predicate always returns true
			pred := func() bool { return true }
			pfact.Kind = handlePackageMaybeKind(pred, pfact.Kind, kind, KindMayCreateFlag, KindCreatesFlag)
			pfact.Kind = handlePackageMaybeKind(pred, pfact.Kind, kind, KindMayParseFlag, KindParsesFlag)

			if pfact.Kind != KindNone {
				pred = func() bool { return kind.Has(KindRootFunc) && call != nil }
				pfact.Roots = handleFactRoots(pred, pfact.Roots, call)
				pass.ExportPackageFact(&pfact)
			}

			res.pkgs[pkg] = pfact.Kind

			return true
		})
	}
}

// Note: this function intentionally disregards the root flag bit
func handlePackageMaybeKind(pred func() bool, pkgKind, otherKind, mayBit, alwBit Kind) Kind {
	// if the current package already has the "always" bit set or the other kind doesn't then accept as-is
	if pkgKind.Has(alwBit) || !otherKind.Has(alwBit) {
		return pkgKind
	}

	// If the predicate is true then merge with the always value
	if pred() {
		return (pkgKind | alwBit).Clear(mayBit)
	}

	return pkgKind | mayBit
}

func handleFactRoots(pred func() bool, roots []ast.Node, nodes ...ast.Node) []ast.Node {
	if !pred() {
		return roots
	}
	roots = append(roots, nodes...)
	roots = slices.DeleteFunc(roots, func(node ast.Node) bool {
		return node == nil
	})
	slices.SortFunc(roots, func(a, b ast.Node) int {
		return cmp.Compare(a.Pos(), b.Pos())
	})
	return slices.CompactFunc(roots, func(a ast.Node, b ast.Node) bool {
		return a.Pos() == b.Pos()
	})
}

func maybeFlagWrapper(info *types.Info, decl ast.Decl) (ret []flagWrapper) {
	// We're looking for functions or top-level variable declarations, which will be analyzed later.
	// We also pay attention to imports because importing a package that initializes flags at the
	// top-level automatically causes dependent packages to initialize flags, as well.
	//
	// * Each function could create a flag or call another function that creates a flag
	// * Each top-level variable could store a flag
	// * Each import could create a flag
	switch decl := decl.(type) {
	case *ast.FuncDecl:
		if decl.Body == nil {
			return nil
		}
		fn, ok := info.Defs[decl.Name].(*types.Func)
		if !ok {
			return nil
		}

		ret = append(ret, &funcWrapper{
			obj:   fn,
			fdecl: decl,
		})

	case *ast.GenDecl:
		switch decl.Tok {
		case token.VAR:
			for _, spec := range decl.Specs {
				spec, ok := spec.(*ast.ValueSpec)
				if !ok {
					// Shouldn't happen but handle just in case
					continue
				}

				for i, name := range spec.Names {
					v, ok := info.Defs[name].(*types.Var)
					if !ok {
						continue
					}

					var expr ast.Expr
					if len(spec.Values) > i {
						// This is a very naive approach to relating the lhs to the rhs expression.
						// But, the base flag functions with return arguments only ever have a
						// single return argument. Functions wrapping a flag function could have
						// different return arguments but we should still roughly map a lhs
						// variable to a flag-like function
						expr = spec.Values[i]
					}

					ret = append(ret, &varWrapper{
						obj:  v,
						expr: expr,
					})
				}
			}
		case token.IMPORT:
			for _, spec := range decl.Specs {
				spec, ok := spec.(*ast.ImportSpec)
				if !ok {
					// Shouldn't happen but handle just in case
					continue
				}

				pkgName := info.PkgNameOf(spec)
				if pkgName == nil {
					continue
				}

				ret = append(ret, &importWrapper{
					obj:   pkgName,
					idecl: spec,
				})
			}
		default:
			return nil
		}
	}

	return ret
}

func checkImport(pass *analysis.Pass, w *importWrapper, res *Result) {
	var (
		currFact, importFact pkgHasFlag
		currPkg, importPkg   = pass.Pkg, w.obj.Imported()
	)
	pass.ImportPackageFact(currPkg, &currFact)
	pass.ImportPackageFact(importPkg, &importFact)

	if importFact.Kind == KindNone {
		return
	}

	// Don't transitively apply package roots and kind for ignored packages
	if _, ok := ignoredPkgs[importPkg.Path()]; ok {
		return
	}

	pass.ExportObjectFact(w.obj, &importFact)

	kind := currFact.Kind

	// If any imports have top-level flag initialization, then that transitively applies to us
	kind = handlePackageMaybeKind(
		func() bool {
			return importFact.Kind.Has(KindCreatesFlag)
		},
		kind,
		importFact.Kind,
		KindMayCreateFlag,
		KindCreatesFlag,
	)
	kind = handlePackageMaybeKind(
		func() bool {
			return importFact.Kind.Has(KindParsesFlag)
		},
		kind,
		importFact.Kind,
		KindMayParseFlag,
		KindParsesFlag,
	)

	if currFact.Kind != kind || len(importFact.Roots) > 0 {
		pred := func() bool { return true }
		currFact.Kind = kind
		currFact.Roots = handleFactRoots(pred, currFact.Roots, importFact.Roots...)
		pass.ExportPackageFact(&currFact)
	}

	res.pkgs[currPkg] = currFact.Kind
}

func check(pass *analysis.Pass) {
	// We only have interesting diagnostics about main packages
	if pass.Pkg.Name() != "main" {
		return
	}

	var pfact pkgHasFlag
	if !pass.ImportPackageFact(pass.Pkg, &pfact) {
		return
	}

	ins := pass.ResultOf[inspect.Analyzer].(*inspector.Inspector)
	nodeFilter := []ast.Node{
		(*ast.FuncDecl)(nil),
	}

	ins.Preorder(nodeFilter, func(n ast.Node) {
		decl, ok := n.(*ast.FuncDecl)
		if !ok || decl.Name.Name != "main" {
			return
		}

		fn, ok := pass.TypesInfo.Defs[decl.Name].(*types.Func)
		if !ok {
			return
		}

		k := pfact.Kind

		switch {
		case k.Has(KindCreatesFlag) && !k.Has(KindParsesFlag):
			var (
				related []analysis.RelatedInformation
				extra   strings.Builder
			)
			extra.WriteString("\n")
			for _, node := range pfact.Roots {
				related = append(related, analysis.RelatedInformation{
					Pos:     node.Pos(),
					End:     node.End(),
					Message: "flag created",
				})
				extra.WriteString(fmt.Sprintf("\t%s: flag created\n", pass.Fset.Position(node.Pos()).String()))
			}
			pass.Report(analysis.Diagnostic{
				Pos:     n.Pos(),
				Message: fmt.Sprintf("%s creates flags but doesn't call flag.Parse()%s", fn.Name(), extra.String()),
				Related: related,
			})
		case k.Has(KindParsesFlag) && !k.Has(KindCreatesFlag):
			var (
				related []analysis.RelatedInformation
				extra   strings.Builder
			)
			extra.WriteString("\n")
			for _, node := range pfact.Roots {
				related = append(related, analysis.RelatedInformation{
					Pos:     node.Pos(),
					End:     node.End(),
					Message: "parse called",
				})
				extra.WriteString(fmt.Sprintf("\t%s: parse called\n", pass.Fset.Position(node.Pos()).String()))
			}
			pass.Report(analysis.Diagnostic{
				Pos:     n.Pos(),
				Message: fmt.Sprintf("%s calls flag.Parse() but doesn't create flags%s", fn.Name(), extra.String()),
				Related: related,
			})
		}
	})
}

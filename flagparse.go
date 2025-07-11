package flagparse

import (
	"go/ast"
	"go/token"
	"go/types"
	"reflect"
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
	return strings.Join(s, "|")
}

const (
	KindNone          Kind = 0
	KindMayParseFlag       = 1 << (iota - 1) // Package has one or more functions that parse flags
	KindMayCreateFlag                        // Package has one or more functions that create flags
	KindParsesFlag                           // Function or package parses flags
	KindCreatesFlag                          // Function or package creates flags
)

// => *types.Package p has one or more flags, either directly as variables or
// indirectly via a function call
type pkgHasFlag struct{ Kind Kind }

func (*pkgHasFlag) AFact() {}

func (p *pkgHasFlag) String() string {
	var s strings.Builder
	s.WriteString("pkgHasFlag(")
	s.WriteString(p.Kind.String())
	s.WriteString(")")
	return s.String()
}

type funcHasFlag struct{ Kind Kind } // => *types.Func f defines one or more flags

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
}

var isKindFunc = map[string]Kind{
	"flag.Parse":                  KindParsesFlag,
	"flag.Bool":                   KindCreatesFlag,
	"flag.BoolFunc":               KindCreatesFlag,
	"flag.BoolVar":                KindCreatesFlag,
	"flag.Duration":               KindCreatesFlag,
	"flag.DurationVar":            KindCreatesFlag,
	"flag.Float64":                KindCreatesFlag,
	"flag.Float64Var":             KindCreatesFlag,
	"flag.Func":                   KindCreatesFlag,
	"flag.Int":                    KindCreatesFlag,
	"flag.IntVar":                 KindCreatesFlag,
	"flag.Int64":                  KindCreatesFlag,
	"flag.Int64Var":               KindCreatesFlag,
	"flag.String":                 KindCreatesFlag,
	"flag.StringVar":              KindCreatesFlag,
	"flag.TextVar":                KindCreatesFlag,
	"flag.Uint":                   KindCreatesFlag,
	"flag.UintVar":                KindCreatesFlag,
	"flag.Uint64":                 KindCreatesFlag,
	"flag.Uint64Var":              KindCreatesFlag,
	"flag.Var":                    KindCreatesFlag,
	"(*flag.FlagSet).Parse":       KindParsesFlag,
	"(*flag.FlagSet).Bool":        KindCreatesFlag,
	"(*flag.FlagSet).BoolFunc":    KindCreatesFlag,
	"(*flag.FlagSet).BoolVar":     KindCreatesFlag,
	"(*flag.FlagSet).Duration":    KindCreatesFlag,
	"(*flag.FlagSet).DurationVar": KindCreatesFlag,
	"(*flag.FlagSet).Float64":     KindCreatesFlag,
	"(*flag.FlagSet).Float64Var":  KindCreatesFlag,
	"(*flag.FlagSet).Func":        KindCreatesFlag,
	"(*flag.FlagSet).Int":         KindCreatesFlag,
	"(*flag.FlagSet).IntVar":      KindCreatesFlag,
	"(*flag.FlagSet).Int64":       KindCreatesFlag,
	"(*flag.FlagSet).Int64Var":    KindCreatesFlag,
	"(*flag.FlagSet).String":      KindCreatesFlag,
	"(*flag.FlagSet).StringVar":   KindCreatesFlag,
	"(*flag.FlagSet).TextVar":     KindCreatesFlag,
	"(*flag.FlagSet).Uint":        KindCreatesFlag,
	"(*flag.FlagSet).UintVar":     KindCreatesFlag,
	"(*flag.FlagSet).Uint64":      KindCreatesFlag,
	"(*flag.FlagSet).Uint64Var":   KindCreatesFlag,
	"(*flag.FlagSet).Var":         KindCreatesFlag,
}

// Functions in this map have a pointer value as their first argument which acts as the
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
				checkFlagFwd(pass, w, call, kind, res)
				return true
			}

			// If the call is to another function in this package,
			// maybe we will find out it is flag-like later.
			// Remember this call for later checking.
			if fn != nil && fn.Pkg() == pass.Pkg && byObj[fn] != nil {
				callee := byObj[fn].(*funcWrapper)
				callee.callers = append(callee.callers, flagCaller{w: w, call: call})
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

func checkFlagFwd(pass *analysis.Pass, w *funcWrapper, call *ast.CallExpr, kind Kind, res *Result) {
	fn := w.obj
	var fact funcHasFlag
	// Retrieve existing kind and compare it with new kind
	if pass.ImportObjectFact(fn, &fact) && (fact.Kind|kind == fact.Kind) {
		return
	}

	fact.Kind |= kind
	pass.ExportObjectFact(fn, &fact)
	res.objs[fn] = kind
	for _, caller := range w.callers {
		checkFlagFwd(pass, caller.w, caller.call, kind, res)
	}

	var (
		pfact pkgHasFlag
		pkg   = fn.Pkg()
	)
	pass.ImportPackageFact(pkg, &pfact)

	pred := func() bool {
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
	pfact.Kind = handlePackageMaybeKind(pred, pfact.Kind, kind, KindMayCreateFlag, KindCreatesFlag)
	pfact.Kind = handlePackageMaybeKind(pred, pfact.Kind, kind, KindMayParseFlag, KindParsesFlag)

	if pfact.Kind != KindNone {
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
	// If true we already have a fact for this var
	if pass.ImportObjectFact(va, &flagFact) || pass.ImportObjectFact(va, &funcFact) {
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

			_, _, kind := flagNameAndKind(pass, call)
			if kind == KindNone {
				return true
			}

			// If the top-level expression is a function literal then it acts more like a
			// flag function than a flag variable.
			if isFuncLiteral {
				pred := func() bool { return false }
				funcFact.Kind = handlePackageMaybeKind(pred, funcFact.Kind, kind, KindMayCreateFlag, KindCreatesFlag)
				funcFact.Kind = handlePackageMaybeKind(pred, funcFact.Kind, kind, KindMayParseFlag, KindParsesFlag)
				pass.ExportObjectFact(va, &funcFact)
				res.objs[va] = kind
				return true
			}

			// This initializing expression is a flag func of some kind and this is a top-level call
			// so the var and package need new facts
			pass.ExportObjectFact(va, &flagFact)

			var (
				pfact pkgHasFlag
				pkg   = va.Pkg()
			)
			pass.ImportPackageFact(pkg, &pfact)

			// We know this var is top-level so the predicate always returns true
			pred := func() bool { return true }
			pfact.Kind = handlePackageMaybeKind(pred, pfact.Kind, kind, KindMayCreateFlag, KindCreatesFlag)
			pfact.Kind = handlePackageMaybeKind(pred, pfact.Kind, kind, KindMayParseFlag, KindParsesFlag)

			if pfact.Kind != KindNone {
				pass.ExportPackageFact(&pfact)
			}

			res.pkgs[pkg] = pfact.Kind

			return true
		})
	}
}

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

	if importFact.Kind != KindNone {
		pass.ExportObjectFact(w.obj, &importFact)
	}

	// If any imports have top-level flag initialization then that transitively applies to us
	currFact.Kind = handlePackageMaybeKind(
		func() bool {
			return importFact.Kind.Has(KindCreatesFlag)
		},
		currFact.Kind,
		importFact.Kind,
		KindMayCreateFlag,
		KindCreatesFlag,
	)
	currFact.Kind = handlePackageMaybeKind(
		func() bool {
			return importFact.Kind.Has(KindParsesFlag)
		},
		currFact.Kind,
		importFact.Kind,
		KindMayParseFlag,
		KindParsesFlag,
	)

	if currFact.Kind != KindNone {
		pass.ExportPackageFact(&currFact)
	}

	res.pkgs[currPkg] = currFact.Kind
}

func match(info *types.Info, arg ast.Expr, param *types.Var) bool {
	id, ok := arg.(*ast.Ident)
	return ok && info.ObjectOf(id) == param
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
			pass.Reportf(n.Pos(), "%s creates flags but doesn't call flag.Parse()", fn.Name())
		case k.Has(KindParsesFlag) && !k.Has(KindCreatesFlag):
			pass.Reportf(n.Pos(), "%s calls flag.Parse() but doesn't create flags", fn.Name())
		}
	})
}

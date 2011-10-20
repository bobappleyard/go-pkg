// Copyright 2011 Bob Appleyard. All rights reserved.
// 
// Redistribution and use in source and binary forms, with or without 
// modification, are permitted provided that the following conditions are met:
// 
//    1. Redistributions of source code must retain the above copyright notice,
//       this list of conditions and the following disclaimer.
// 
//    2. Redistributions in binary form must reproduce the above copyright
//       notice, this list of conditions and the following disclaimer in the 
//       documentation and/or other materials provided with the distribution.
// 
// THIS SOFTWARE IS PROVIDED BY BOB APPLEYARD ''AS IS'' AND ANY EXPRESS OR 
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
// MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO 
// EVENT SHALL BOB APPLEYARD OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
// INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
// OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
// LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
// NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, 
// EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
// 

// Package pkg implements a parser for compiled package files.
//
// Use Open() or Read() to get a package description structure. The parser 
// returns all definitions, exported or not, as well as definitions imported
// from other packages.
//
package pkg

import (
	"bytes"
	"io"
	"io/ioutil"
	"os"
	"reflect"
	"strconv"
	"strings"
	"unicode"
	"utf8"
)

var (
	NoExports = os.NewError("no exports section found")
	UnknownSpec = os.NewError("unknown export spec type")
	UnknownFormat = os.NewError("unknown export spec format")
)

////////////////////////////////////////////////////////////////////////////////
//
// Data representation
//
////////////////////////////////////////////////////////////////////////////////

// A Go type.
//
// What most of the fields mean depends on the kind of the type. This is stored 
// in the Kind field.
//
// Any type may use the Method field. For interfaces, it refers to the methods
// a type must implemenent to satisfy that interface. For everything else, it
// refers to the methods that have been defined for that type.
//
// Any type may use the Addr field for a type describing a pointer to that type.
//
// Structs use the Field field for the fields they define. Embedded types are
// fields with "?" for their name. Fields may have the value field set, in which
// case this is the field's tag.
//
// Arrays, slices, channels, pointers and maps use the Elem field for the type 
// they store or primarily manipulate. Additionally, arrays use the Size field 
// for the number of elements they store, channels use the Dir field for the 
// direction the channel may be used and maps use the Key field for the index 
// type.
//
// Functions use the In field for the parameters and the Out field for the
// return types. Additionally, if the Elem field is not nil the last parameter
// is a rest parameter (i.e. defined with "..." before the type). Unnamed
// entries in either of these fields use "?" for their Name field.
//
// All duplicate types in a package are removed. As a result, == is enough to
// see if two types are the same.
//
type Type struct {
	Named
	Kind reflect.Kind
	In, Out, Field, Method []*Item
	Key, Elem, Addr *Type
	Size int
	Dir reflect.ChanDir
}

// Anything other than a type, really.
type Item struct {
	Named
	Type *Type
	Value string
}

// A Go package file, its exports and imports.
type Pkg struct {
	Name string
	Import []string
	Const, Var, Func []*Item
	Type []*Type
}

// Open and parse a compiled Go file.
func Open(file string) (*Pkg, os.Error) {
	f, err := os.Open(file)
	if err != nil {
		return nil, err
	}
	return Read(f)
}

// Parse a compiled Go file specified by the reader. The entire contents of the
// reader will be read.
func Read(r io.Reader) (*Pkg, os.Error) {
	data, err := ioutil.ReadAll(r)
	if err != nil {
		return nil, err
	}
	s := extractExportSection(data)
	if s == "" {
		return nil, NoExports
	}
	return new(Pkg).parse(s)
}

// All the package items have a name and associated services.
type Named struct {
	Package, Name string
}

// Is the item exported from the package?
func (n *Named) Exported() bool {
	if n.Imported() {
		return false
	}
	r, _ := utf8.DecodeRuneInString(n.Name)
	return unicode.IsUpper(r)
}

// Is the item imported from another package?
func (n *Named) Imported() bool {
	return n.Package != ""
}

////////////////////////////////////////////////////////////////////////////////
//
// Parsing
//
////////////////////////////////////////////////////////////////////////////////

// The export syntax is a fairly simple affair. Everything is named before it
// happens (apart from :"something" bits that keep popping up) and so it can be
// parsed with a simple recursive descent parser.

// this begins and ends the exports section
var sig = []byte("\n\n$$  ")

// pull an export section out of a file
func extractExportSection(data []byte) string {
	from := bytes.Index(data, sig)
	if from == -1 {
		return ""
	}
	data = data[from+len(sig):]
	to := bytes.Index(data, sig)
	if to == -1 {
		return ""
	}
	return string(data[:to])
}

type tokenStream struct {
	ts []string
	p int
}

func (s *tokenStream) token() string {
	return s.ts[s.p]
}

func (s *tokenStream) next() {
	s.p++
}

func (s *tokenStream) add(t string) {
	if t != "" {
		s.ts = append(s.ts, t)
	}
}

func (s *tokenStream) init(line string) *tokenStream {
	cur := ""
	inqu, inesc := false, false
	for _, c := range []byte(line) {
		if inesc {
			inesc = false
			cur += string(c)
			continue
		}
		if inqu {
			switch c {
			case '"':
				inqu = false
			case '\\':
				inesc = true
			}
			cur += string(c)
			continue
		}
		switch c {
		case '\t':
		case ' ':
			s.add(cur)
			cur = ""
		case '"':
			inqu = true
			cur += "\""
		case '(', ')', '{', '}', '[', ']', ',', ';', '*':
			s.add(cur)
			s.add(string(c))
			cur = ""
		default:
			cur += string(c)
		}
	}
	s.add(cur)
	return s
}

// pull out the dotted name into its constituent parts
func parseName(n string) Named {
	parts := strings.Split(n, ".")
	p, err := strconv.Unquote(parts[0])
	if err != nil {
		panic(err)
	}
	return Named{p, parts[1]}
}

// check if our loop in a list of some kind is done
func finished(ts *tokenStream, end, mid string) bool {
	if ts.token() == end {
		ts.next()
		return true
	}
	if ts.token() == mid {
		ts.next()
	}
	return false
}

// skip stuff we're ignoring
func skipUntil(end []string, ts *tokenStream) {
	for {
		for _, e := range end {
			if ts.token() == e {
				return
			}
		}
		ts.next()
	}
}

// generic list: parameters
func parseParams(ts *tokenStream) []*Item {
	if ts.token() != "(" {
		panic(UnknownFormat)
	}
	ts.next()
	res := []*Item{}
	for !finished(ts, ")", ",") {
		a := new(Item)
		a.Name = ts.token()
		ts.next()
		a.Type = new(Type)
		a.Type.parse(ts)
		res = append(res, a)
		if ts.token()[0] == ':' {
			ts.next()
		}
	}
	return res
}

// Parse a package declaration block.
func (p *Pkg) parse(decls string) (*Pkg, os.Error) {
	lines := strings.Split(decls, "\n")
	p.Name = strings.Split(strings.TrimLeft(lines[1], " "), " ")[1]
	for _, line := range lines[2:] {
		err := p.parseDecl(new(tokenStream).init(line))
		if err != nil {
			return nil, err
		}
	}
	return p.resolve()
}

// parse main declaration types
func (p *Pkg) parseDecl(ts *tokenStream) (err os.Error) {
	defer func() {
		if e := recover(); e != nil {
			err = UnknownFormat
		}
	}()
	switch ts.token() {
	case "import":
		s, _ := strconv.Unquote(ts.ts[2])
		p.Import = append(p.Import, s)
	case "var":
		ts.next()
		v := new(Item)
		v.Named = parseName(ts.token())
		ts.next()
		v.Type = new(Type).parse(ts)
		p.Var = append(p.Var, v)
	case "type":
		ts.next()
		t := p.getNamedType(&Type{Named: parseName(ts.token())}, true)
		ts.next()
		t.parse(ts)
	case "func":
		ts.next()
		f := new(Item)
		if ts.token() == "(" {
		// method declaration
			rcv := parseParams(ts)
			f.Name = ts.token()
			ts.next()
			p.addMethod(rcv[0].Type, f)
		} else {
		// function declaration
			p.Func = append(p.Func, f)
			f.Named = parseName(ts.token())
			ts.next()
		}
		f.Type = new(Type).parseFunc(ts)
	case "const":
		ts.next()
		c := new(Item)
		c.Named = parseName(ts.token())
		ts.next()
		// untyped constant
		if ts.token() == "=" {
			ts.next()
			c.Value = ts.token()
			return nil
		}
		c.Type = new(Type).parse(ts)
		if ts.token() != "=" {
			panic(UnknownFormat)
		}
		c.Value = ts.token()
		p.Const = append(p.Const, c)
	default:
		return UnknownSpec
	}
	return nil
}

// dispatch on type kinds
func (t *Type) parse(ts *tokenStream) *Type {
	switch ts.token() {
	case "*":
		ts.next()
		t.parsePtr(ts)
	case "[":
		ts.next()
		t.parseArray(ts)
	case "map":
		ts.next()
		t.parseMap(ts)
	case "func":
		ts.next()
		t.parseFunc(ts)
	case "chan", "<-chan", "chan<-":
		// note that we want to see what kind of channel
		t.parseChan(ts)
	case "struct":
		ts.next()
		t.parseStruct(ts)
	case "interface":
		ts.next()
		t.parseInterface(ts)
	default:
		t.parseOther(ts)
	}
	return t
}

// pointers
func (t *Type) parsePtr(ts *tokenStream) {
	t.Kind = reflect.Ptr
	t.Elem = new(Type).parse(ts)
}

// arrays and slices
func (t *Type) parseArray(ts *tokenStream) {
	if ts.token() == "]" {
		t.Kind = reflect.Slice
		ts.next()
	} else {
		t.Kind = reflect.Array
		s, err := strconv.Atoi(ts.token())
		if err != nil {
			panic(err)
		}
		t.Size = s
		ts.next()
		if ts.token() != "]" {
			panic(UnknownFormat)
		}
		ts.next()
	}
	t.Elem = new(Type).parse(ts)
}

// maps
func (t *Type) parseMap(ts *tokenStream) {
	t.Kind = reflect.Map
	if ts.token() != "[" {
		panic(UnknownFormat)
	}
	ts.next()
	t.Key = new(Type).parse(ts)
	if ts.token() != "]" {
		panic(UnknownFormat)
	}
	ts.next()
	t.Elem = new(Type).parse(ts)
}

// functions
func (t *Type) parseFunc(ts *tokenStream) *Type {
	t.Kind = reflect.Func
	t.In = parseParams(ts)
	l := len(t.In)
	if l != 0 && t.In[l-1].Type.Kind == reflect.Kind(200) {
		a := t.In[l-1]
		t.Elem = a.Type.Elem
		a.Type = t.Elem
	}
	if ts.p < len(ts.ts) && ts.token()[0] != ':' {
		switch ts.token() {
		case ";", ",", "}", ")", "]":
		case "(":
			t.Out = parseParams(ts)
		default:
			t.Out = []*Item{&Item{Type: new(Type).parse(ts)}}
		}
	}
	return t
}

// channels
func (t *Type) parseChan(ts *tokenStream) {
	t.Kind = reflect.Chan
	switch ts.token() {
	case "chan":
		t.Dir = reflect.BothDir
	case "<-chan":
		t.Dir = reflect.RecvDir
	case "chan<-":
		t.Dir = reflect.SendDir
	}
	ts.next()
	t.Elem = new(Type).parse(ts)
}

// structs
func (t *Type) parseStruct(ts *tokenStream) {
	t.Kind = reflect.Struct
	if ts.token() != "{" {
		panic(UnknownFormat)
	}
	ts.next()
	for !finished(ts, "}", ";") {
		f := new(Item)
		f.Name = ts.token()
		ts.next()
		f.Type = new(Type).parse(ts)
		t.Field = append(t.Field, f)
		// field tga
		if ts.token()[0] == ':' {
			tag, err := strconv.Unquote(ts.token()[1:])
			if err != nil {
				panic(err)
			}
			f.Value = tag
			ts.next()
		}
	}
}

// interfaces
func (t *Type) parseInterface(ts *tokenStream) {
	t.Kind = reflect.Interface
	if ts.token() != "{" {
		panic(UnknownFormat)
	}
	ts.next()
	for !finished(ts, "}", ";") {
		m := new(Item)
		m.Name = ts.token()
		ts.next()
		m.Type = new(Type).parseFunc(ts)
		t.Method = append(t.Method, m)
	}
}

// primitive types
type primCase struct {
	test string
	kind reflect.Kind
}

var primCases = []primCase {
	{"bool", reflect.Bool},
	{"int", reflect.Int},
	{"int8", reflect.Int8},
	{"int16", reflect.Int16},
	{"int32", reflect.Int32},
	{"int64", reflect.Int64},
	{"uint", reflect.Uint},
	{"uint8", reflect.Uint8},
	{"uint16", reflect.Uint16},
	{"uint32", reflect.Uint32},
	{"uint64", reflect.Uint64},
	{"uintptr", reflect.Uintptr},	
	{"float32", reflect.Float32},
	{"float64", reflect.Float64},
	{"complex64", reflect.Complex64},
	{"complex128", reflect.Complex128},
	{"string", reflect.String},
}

// everything else
func (t *Type) parseOther(ts *tokenStream) {
	if strings.HasPrefix(ts.token(), "...") {
		// create a ghost type that's swallowed by the function parser
		t.Kind = reflect.Kind(200)
		if ts.token() != "..." {
			ts.ts[ts.p] = ts.token()[3:]
		} else {
			ts.next()
		}
		t.Elem = new(Type).parse(ts)
		return
	}
	n := ts.token()
	for _, c := range primCases {
		if n == c.test {
			t.setPrim(c.kind)
			ts.next()
			return
		}
	}
	t.Kind = reflect.Invalid
	t.Named = parseName(ts.token())
	ts.next()
}

func (t *Type) setPrim(k reflect.Kind) {
	t.Kind = k
	if t.Name == "" {
		t.Name = k.String()
	}
}

////////////////////////////////////////////////////////////////////////////////
//
// Type resolution
//
////////////////////////////////////////////////////////////////////////////////

// resolve all type references. does two things:
//
// looks up names
// makes sure no duplicate types exist
func (p *Pkg) resolve() (*Pkg, os.Error) {
	// globally store all the wonderful types that are found
	set := []*Type{
		&Type {
			Named: Named{"unsafe", "Pointer"},
			Kind: reflect.UnsafePointer,
		},
	}
	for i, t := range p.Type {
		p.Type[i] = t.resolve(p, &set)
		for _, m := range t.Method {
			m.resolve(p, &set)
		}
	}
	for _, v := range p.Var {
		v.resolve(p, &set)
	}
	for _, f := range p.Func {
		f.resolve(p, &set)
	}
	for _, c := range p.Const {
		c.resolve(p, &set)
	}
	return p, nil
}

func (v *Item) resolve(p *Pkg, set *[]*Type) {
	v.Type = v.Type.resolve(p, set)
}

func (t *Type) resolve(p *Pkg, set *[]*Type) *Type {
	if u := p.getCanonicalType(t, set); u != nil {
		return u
	}
	*set = append(*set, t)
	switch t.Kind {
	case reflect.Invalid:
		u := p.getNamedType(t, false)
		(*set)[len(*set)-1] = u
		return u
	case reflect.Map:
		t.Key = t.Key.resolve(p, set)
		fallthrough
	case reflect.Array, reflect.Chan, reflect.Slice, reflect.Ptr:
		t.Elem = t.Elem.resolve(p, set)
	case reflect.Func:
		for _, a := range t.In {
			a.resolve(p, set)
		}
		for _, a := range t.Out {
			a.resolve(p, set)
		}
	case reflect.Interface:
		for _, m := range t.Method {
			m.resolve(p, set)
		}
	case reflect.Struct:
		for _, f := range t.Field {
			f.resolve(p, set)
		}
	}
	return t
}

// add a method to a type
func (p *Pkg) addMethod(t *Type, m *Item) {
	targ := p.getPointer(t, true)
	targ.Method = append(targ.Method, m)
}

// look the type up in the list, creating if necessary
func (p *Pkg) getNamedType(query *Type, create bool) *Type {
	for _, t := range p.Type {
		if t.Name == query.Name && t.Package == query.Package {
			return t
		}
	}
	if create {
		p.Type = append(p.Type, query)
		return query
	}
	return nil
}

// look up a pointer to a named type, creating along the way
func (p *Pkg) getPointer(query *Type, create bool) *Type {
	if query.Kind == reflect.Ptr {
		t := p.getPointer(query.Elem, create)
		if t.Addr == nil {
			t.Addr = query
		}
		return t.Addr
	}
	if query.Kind == reflect.Invalid {
		return p.getNamedType(query, create)
	}
	return query
}

// get the actual type the given type describes
func (p *Pkg) getCanonicalType(t *Type, set *[]*Type) *Type {
	for _, u := range *set {
		if u.match(t) {
			return u
		}
	}
	return nil
}

// check all names are the same
func testNames(a, b []*Item) bool {
	for i := range a {
		if a[i].Name != b[i].Name {
			return false
		}
	}
	return true
}

// check all types are the same
func testTypes(a, b []*Item) bool {
	for i := range a {
		if !a[i].Type.match(b[i].Type) {
			return false
		}
	}
	return true
}

// check if the two types describe the same one
func (t *Type) match(query *Type) bool {
	if t == query {
		return true
	}
	if t.Package == query.Package && t.Name == query.Name {
		return true
	}
	if t.Name != "" || query.Name != "" {
		return false
	}
	// from now on we're dealing with anonymous types, and they're not
	// circular, right?
	if t.Kind != query.Kind {
		return false
	}
	switch t.Kind {
	case reflect.Map:
		return t.Key.match(query.Key) && t.Elem.match(query.Elem)
	case reflect.Array:
		return t.Size == query.Size && t.Elem.match(query.Elem)
	case reflect.Slice, reflect.Ptr:
		return t.Elem.match(query.Elem)
	case reflect.Chan:
		return t.Dir == query.Dir && t.Elem.match(query.Elem)
	case reflect.Struct:
		if len(t.Field) != len(query.Field) {
			return false
		}
		return testNames(t.Field, query.Field) &&
		       testTypes(t.Field, query.Field)
	case reflect.Interface:
		if len(t.Method) != len(query.Method) {
			return false
		}
		return testNames(t.Method, query.Method) &&
		       testTypes(t.Method, query.Method)
	case reflect.Func:
		if len(t.In) != len(query.In) {
			return false
		}
		if len(t.Out) != len(query.Out) {
			return false
		}
		return testTypes(t.In, query.In) && testTypes(t.Out, query.Out)
	}
	return false
}


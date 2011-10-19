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
// Use Open() or Read() to get a package description structure. From there, the
// most interesting fields are probably Type and Func. The parser returns all
// definitions, exported or not, as well as definitions imported from other
// packages.
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

// A Go variable, field, parameter, function or method.
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

// stick spaces on either side if not already there
func padChar(line string, pos int) (string, int) {
	p := pos
	insert := string(line[pos])
	if pos == 0 {
		goto after
	}
	if line[pos-1] == ' ' {
		goto after
	}
	insert = " " + insert
	p++
after:
	if pos == len(line)-1 {
		goto build
	}
	if line[pos+1] == ' ' {
		goto build
	}
	insert = insert + " "
	p++
build:
	return line[:pos] + insert + line[pos+1:], p
}

// pad all instances of a particular character so they will be tokenised alone
func padTokens(line string, c byte) string {
	for i := 0; i < len(line); i++ {
		if line[i] == c {
			line, i = padChar(line, i)
		}
	}
	return line
}

// replace quotes with a marker, return all quotes ina a slice
func extractQuotes(line string) (qu []string, newline string) {
	var inqu, inesc bool
	var cur string
	for _, c := range []byte(line) {
		switch {
		case inqu:
			cur += string(c)
			switch {
			case inesc:
				inesc = false
			case c == '\\':
				inesc = true
			case c == '"':
				qu = append(qu, cur)
				newline += string(1)
				inqu = false
			}
		case c == '"':
			inqu = true
			cur = "\""
		default:
			newline += string(c)
		}
	}
	return
}

// replace marker with relevant quotes in text
func reinsertQuotes(ts, qu []string) []string {
	curq := 0
	for i, t := range ts {
		next := ""
		for _, c := range []byte(t) {
			if c == 1 {
				next += qu[curq]
				curq++
			} else {
				next += string(c)
			}
		}
		ts[i] = next
	}
	return ts
}

// turn a line into a list of tokens
func prepareLine(line string) []string {
	var qu []string
	line = strings.TrimLeft(line, "\t")
	qu, line = extractQuotes(line)
	for _, c := range []byte{',', ';', '(', ')', '[', ']', '*'} {
		line = padTokens(line, c)
	}
	res := strings.Split(line, " ")
	return reinsertQuotes(res, qu)
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
func finished(ts []string, p *int, end, mid string) bool {
	if ts[*p] == end {
		*p++
		return true
	}
	if ts[*p] == mid {
		*p++
	}
	return false
}

// skip stuff we're ignoring
func skipUntil(end, ts []string, p *int) {
	for {
		for _, e := range end {
			if ts[*p] == e {
				return
			}
		}
		*p++
	}
}

// generic list: parameters
func parseParams(ts []string, p *int) []*Item {
	if ts[*p] != "(" {
		panic(UnknownFormat)
	}
	*p++
	res := []*Item{}
	for !finished(ts, p, ")", ",") {
		a := new(Item)
		a.Name = ts[*p]
		*p++
		a.Type = new(Type)
		a.Type.parse(ts, p)
		res = append(res, a)
		if ts[*p][0] == ':' {
			skipUntil([]string{",", ")"}, ts, p)
		}
	}
	return res
}

// Parse a package declaration block.
func (p *Pkg) parse(decls string) (*Pkg, os.Error) {
	lines := strings.Split(decls, "\n")
	p.Name = strings.Split(strings.TrimLeft(lines[1], " "), " ")[1]
	for _, line := range lines[2:] {
		err := p.parseDecl(prepareLine(line))
		if err != nil {
			return nil, err
		}
	}
	return p.resolve()
}

// parse main declaration types
func (p *Pkg) parseDecl(ts []string) (err os.Error) {
	defer func() {
		if e := recover(); e != nil {
			err = UnknownFormat
		}
	}()
	switch ts[0] {
	case "import":
		s, _ := strconv.Unquote(ts[2])
		p.Import = append(p.Import, s)
	case "var":
		v := new(Item)
		v.Named = parseName(ts[1])
		v.Type = new(Type).parse(ts[2:], new(int))
		p.Var = append(p.Var, v)
	case "type":
		t := p.getNamedType(&Type{Named: parseName(ts[1])}, true)
		t.parse(ts[2:], new(int))
	case "func":
		f := new(Item)
		pos := new(int)
		if ts[1] == "(" {
		// method declaration
			*pos++
			rcv := parseParams(ts, pos)
			f.Name = ts[*pos]
			*pos++
			p.addMethod(rcv[0].Type, f)
		} else {
		// function declaration
			p.Func = append(p.Func, f)
			f.Named = parseName(ts[1])
			*pos += 2
		}
		f.Type = new(Type).parseFunc(ts, pos)
	case "const":
		c := new(Item)
		c.Named = parseName(ts[1])
		if ts[2] == "=" {
			c.Value = ts[3]
			return nil
		}
		pos := new(int)
		*pos = 2
		c.Type = new(Type).parse(ts, pos)
		if ts[*pos] != "=" {
			panic(UnknownFormat)
		}
		c.Value = ts[*pos]
		p.Const = append(p.Const, c)
	default:
		return UnknownSpec
	}
	return nil
}

// dispatch on type kinds
func (t *Type) parse(ts []string, p *int) *Type {
	switch ts[*p] {
	case "*":
		*p++
		t.parsePtr(ts, p)
	case "[":
		*p++
		t.parseArray(ts, p)
	case "map":
		*p++
		t.parseMap(ts, p)
	case "func":
		*p++
		t.parseFunc(ts, p)
	// don't advance the pointer for channels, want to see token
	case "chan", "<-chan", "chan<-":
		t.parseChan(ts, p)	
	case "struct":
		*p++
		t.parseStruct(ts, p)
	case "interface":
		*p++
		t.parseInterface(ts, p)
	default:
		t.parseOther(ts, p)
	}
	return t
}

// pointers
func (t *Type) parsePtr(ts []string, p *int) {
	t.Kind = reflect.Ptr
	t.Elem = new(Type).parse(ts, p)
}

// arrays and slices
func (t *Type) parseArray(ts []string, p *int) {
	if ts[*p] == "]" {
		t.Kind = reflect.Slice
		*p++
	} else {
		t.Kind = reflect.Array
		s, err := strconv.Atoi(ts[*p])
		if err != nil {
			panic(err)
		}
		t.Size = s
		*p++
		if ts[*p] != "]" {
			panic(UnknownFormat)
		}
		*p++
	}
	t.Elem = new(Type).parse(ts, p)
}

// maps
func (t *Type) parseMap(ts []string, p *int) {
	t.Kind = reflect.Map
	if ts[*p] != "[" {
		panic(UnknownFormat)
	}
	*p++
	t.Key = new(Type).parse(ts, p)
	if ts[*p] != "]" {
		panic(UnknownFormat)
	}
	*p++
	t.Elem = new(Type).parse(ts, p)
}

// functions
func (t *Type) parseFunc(ts []string, p *int) *Type {
	t.Kind = reflect.Func
	t.In = parseParams(ts, p)
	l := len(t.In)
	if l != 0 && t.In[l-1].Type.Kind == reflect.Kind(200) {
		a := t.In[l-1]
		t.Elem = a.Type.Elem
		a.Type = t.Elem
	}
	if *p < len(ts) && ts[*p][0] != ':' {
		switch ts[*p] {
		case ";", ",", "}", ")", "]":
		case "(":
			t.Out = parseParams(ts, p)
		default:
			t.Out = []*Item{&Item{Type: new(Type).parse(ts, p)}}
		}
	}
	return t
}

// channels
func (t *Type) parseChan(ts []string, p *int) {
	t.Kind = reflect.Chan
	switch ts[*p] {
	case "chan":
		t.Dir = reflect.BothDir
	case "<-chan":
		t.Dir = reflect.RecvDir
	case "chan<-":
		t.Dir = reflect.SendDir
	}
	*p++
	t.Elem = new(Type).parse(ts, p)
}

// structs
func (t *Type) parseStruct(ts []string, p *int) {
	t.Kind = reflect.Struct
	if ts[*p] != "{" {
		panic(UnknownFormat)
	}
	*p++
	for !finished(ts, p, "}", ";") {
		f := new(Item)
		f.Name = ts[*p]
		*p++
		f.Type = new(Type).parse(ts, p)
		t.Field = append(t.Field, f)
		if ts[*p][0] == ':' {
			tag, err := strconv.Unquote(ts[*p][1:])
			if err != nil {
				panic(err)
			}
			f.Value = tag
			*p++
		}
	}
}

// interfaces
func (t *Type) parseInterface(ts []string, p *int) {
	t.Kind = reflect.Interface
	if ts[*p] != "{" {
		panic(UnknownFormat)
	}
	*p++
	for !finished(ts, p, "}", ";") {
		m := new(Item)
		m.Name = ts[*p]
		*p++
		m.Type = new(Type).parseFunc(ts, p)
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
func (t *Type) parseOther(ts []string, p *int) {
	if strings.HasPrefix(ts[*p], "...") {
		// create a ghost type that's swallowed by the function parser
		t.Kind = reflect.Kind(200)
		if ts[*p] != "..." {
			ts[*p] = ts[*p][3:]
		} else {
			*p++
		}
		t.Elem = new(Type).parse(ts, p)
		return
	}
	n := ts[*p]
	for _, c := range primCases {
		if n == c.test {
			t.setPrim(c.kind)
			*p++
			return
		}
	}
	t.Kind = reflect.Invalid
	t.Named = parseName(ts[*p])
	*p++
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


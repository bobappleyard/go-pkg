Go Package Exports Parsing
==========================

This package parses Go package files (*.a or *.6/8/etc) into a structure
detailing what definitions the package introduces.

This package will tell you:

* Which packages are imported.
* Which types are defined, and information about those types.
* Which constants and variables are defined.
* Which functions are defined.
* Of the definitions, which are imported and which are exported.

Possible uses of the library include IDEs and code generation tools.

Installing the library
----------------------

To install the library, type

	git clone git://github.com/bobappleyard/go-pkg.git
	cd go-pkg
	gomake
	gomake install

The package will now be installed into the Go package root.

Using the library
-----------------

	import "go/pkg"

Check the source for a more detailed look at the API. For now, an example of the 
library's use:

	package main
	
	import (
		"fmt"
		"os"
		"go/pkg"
	)
	
	func printMethods(t *pkg.Type) {
		for _, m := range t.Method {
			if !m.Exported() {
				continue
			}
			fmt.Println("   ", m.Name)
		}
	}
	
	func main() {
		p, err := pkg.Open(os.Args[1])
		if err != nil {
			fmt.Println(err)
			return
		}
		for _, t := range p.Type {
			if !t.Exported() {
				continue
			}
			fmt.Println(t.Name)
			printMethods(t)
			if t.Addr != nil {
				printMethods(t.Addr)
			}
		}
	}

This prints the names of all the exported methods on all the exported types, 
along with the exported methods on their pointer types, if they exist.

Testing
-------

Currently to test it I have a program that looks like this:

	package main
	
	import (
		"fmt"
		"go/pkg"
	)
	
	func main() {
		_, err := pkg.Open(os.Args[1])
		if err != nil {
			fmt.Println("FAILED:", os.Args[1])
		} else {
			fmt.Println("OK:    ", os.Args[1])
		}
	}

Then I run something like this:

	find -name "$GOROOT/pkg" -name "*.a" -exec ./test {} \; | grep FAILED

and see if anything pops up. The parser was written this way. Yeah...

This will only pick up if the library falls over in parsing. I used it during
development of the parser. More test cases need to be assembled, but this will 
take time.


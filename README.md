# ERUDITE

[![Quicklisp](http://quickdocs.org/badge/erudite.svg)](http://quickdocs.org/erudite/)
[![MIT License](https://img.shields.io/badge/license-MIT-blue.svg)](./LICENSE)

## Introduction

Erudite is a [Literate Programming](http://en.wikipedia.org/wiki/Literate_programming) System for Common Lisp

## Features

* Interactive development. No tangling phase in which code is extracted from documentation. Documentation is written in Common Lisp comments. This allows for incremental development: you can load your code, work from SLIME, etc, directly, as in any other Lisp project.

* Multiple syntaxes. Multiple type of literate syntax are supported. It is possible to choose from the default Erudite syntax, or use plain Latex or Sphinx syntax, and potentially others.

* Multiple outputs. Like Latex, Sphinx, Markdown, HTML, etc.

* Automatic indexing and cross-references.

* A command line interface.

* It is portable. You can compile and use in several CL systems (has been tested on SBCL, CCL, CLISP, ECL, so far)

## Quickstart

Run `make` to build erudite command line if you want to run it from there, and then `sudo make install` to install.

```
Usage: erudite [-hvd] [+vd] [OPTIONS] FILES...

Erudite is a Literate Programming System for Common Lisp
  -h, --help                  Print this help and exit.
  --version                   Print Erudite version
  -(+)v, --verbose[=yes/no]   Run in verbose mode
                              Fallback: yes
                              Default: no
                              Environment: VERBOSE
  -(+)d, --debug[=on/off]     Turn debugging on or off.
                              Fallback: on
                              Default: off
                              Environment: DEBUG
  -(+)id, --implicit-doc[=yes/no] Treat all comments as documentation
                              Fallback: yes
                              Default: yes
  -(+)ic, --implicit-code[=yes/no] Include all code in documentation
                              Fallback: yes
                              Default: yes
  -o, --output=OUTPUT         The output file. If none is used, result is 
                              printed to stdout
  --output-type=OUTPUT-TYPE   The output type. One of 'latex', 
                              'sphinx','markdown'
                              Default: latex
  --syntax=SYNTAX             The syntax used in source files. One of 'erudite',
                              'latex', 'sphinx', 'markdown'
                              Default: erudite
  --author=AUTHOR             The author to appear in the document
  --title=TITLE               The document title
```

Or run it from lisp, using the `erudite` function described below.

To build erudite own documentation, run `make` under the `doc` directory:

```
make pdf
make sphinx-pdf
make sphinx-html
make markdown
```

Look at the `doc` directory for output examples.

## Functions
### erudite

```lisp
(pathname files &rest args &key (output-type *output-type*)
          (syntax *syntax*) &allow-other-keys)
```

Processes literate lisp files and creates a document.

- **pathname**: Pathname of the file to generate
- **files**: Literate lisp files to compile
- **args**: All sort of options passed to the generation functions
- **output-type**: The kind of document to generate.
                   One of :latex, :sphinx, :markdown
                   Default: :latex
- **syntax**:  The kind of syntax used in the literate source files.
                   One of: :erudite, :latex, :sphinx, :markdown.
                   Default: :erudite

## References

* [Literate Programming](http://en.wikipedia.org/wiki/Literate_programming)
* [Literate Programming in the Large](https://youtu.be/Av0PQDVTP4A)
* [Erudite: a tool for Literate Programming in Common Lisp](http://mmontone-programming.blogspot.com.ar/2015/05/literate-programming-in-common-lisp.html)

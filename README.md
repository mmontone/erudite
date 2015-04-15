# ERUDITE

Erudite is a Literate Programming System for Common Lisp

# Features

* Documentation is written in Common Lisp comments. This is very useful because you can work with your program as if it were not a literate program: you can load it, work from SLIME, etc, directly.

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

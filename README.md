# ERUDITE

Erudite is a Literate Programming System for Common Lisp

## Functions
### erudite

```lisp
(pathname files &rest args &key (output-type *output-type*)
          (input-type *input-type*) &allow-other-keys)
```

Processes literate lisp files and creates a document.

- **pathname**: Pathname of the file to generate
- **files**: Literate lisp files to compile
- **args**: All sort of options passed to the generation functions
- **output-type**: The kind of document to generate.
                   One of :latex, :sphinx
                   Default: :latex
- **input-type**:  The kind of syntax used in the literate source files.
                   One of: :erudite, :latex, :sphinx.
                   Default: :erudite
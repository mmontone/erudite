# ERUDITE

Erudite is a simple Literate Programming system for Common Lisp.

[https://github.com/cldm/erudite/raw/master/doc/erudite.pdf](Example output)

## Functions
### gen-latex-doc

```lisp
(pathname files &key title author template-pathname)
```

Generates a LaTeX document.

- **pathname**: The pathname of the .tex file to generate.
- **files**: The list of .lisp files to compile
- **title**: Title of the document
- **author**: Author of the document
- **template-pathname**: A custom LaTeX template file. If none is specified, a default template is used.




### gen-sphinx-doc

```lisp
(pathname files &key prelude postlude)
```

Generates Sphinx document.

- **pathname**: Pathname of the .rst file to generate.
- **files**: .lisp files to compile.
- **prelude**: String (or pathname) to append before the Sphinx document.
- **postlude**: String (or pathname) to append after the Sphinx document.
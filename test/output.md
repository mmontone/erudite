

# Introduction


This is a test of Erudite output rendering, commands and syntax elements


# Chunks


This is a good chunk

```lisp
<<<chunk1>>>
```

This is a good chunk

```lisp
<<<chunk2>>>
```
This is the chunk:

```lisp
<<chunk2>>=
(+ 1 1)

```


```lisp
<<chunk4>>=
(print "Start")

```
The end

```lisp
<<<chunk4>>>
```

This is the factorial function:

```lisp
(defun factorial (n)
  (if (<= n 1)
<<<base-case>>>
<<<recursive-case>>>
      ))

```
The base case is simple, just check for `n=1` less:

```lisp
<<base-case>>=
      1

```
The recursive step is `n x n - 1`:

```lisp
<<recursive-case>>=
      (* n (factorial (1- n)))

```


# Extracts


Extract test
This has been extracted

```lisp
(+ 1 2)
```

Extract 3

Start
End


# Includes


Include test
This is includeA

```lisp
(print "include A")
(print "Include")
```
This is includeB

```lisp
(print "include B")
```


# Ignore




# Conditional output

When test
This should appear

This is other output text


# Erudite syntax


## Subsection


## Subsubsection



## Verbatim

```
This is in verbatim
```


## Code


```lisp

(defun hello-world ()
(print "Hello world"))
```


## List


*  First item
*  Second item



## Emphasis

*This is emphasized*

**This is in bold**

_This is in italics_


## Inline verbatim

This is in `inline verbatim`


## Link

[Erudite](https://github.com/mmontone/erudite)


## Reference

hello-world


## Label and index



This section is labelled

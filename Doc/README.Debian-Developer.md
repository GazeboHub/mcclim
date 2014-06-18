# README for Developers

**OS Platform:** Debian GNU/Linux and Similar Distributions

## Overview

_Build-depends_ for McCLIM Documentation
- make
- texinfo
- texlive-binaries
- lisp-compiler
- cl-asdf
- cl-ppcre
- slime
- imagemagick | graphicsmagick-imagemagick-compat
- transfig

## Lisp Compiler Dependency

The shell script, `make-tempfiles.sh`, loads a few items of Lisp code,
such as will create representations for the documentation strings of
symbols exported from packages defined in McCLIM, namely in TeXinfo
format.

The file `make-tempfiles.sh` contains an inline set of Lisp
expressions, in "Here doc" section. On evaluation within a Common Lisp
implementation, those Lisp expressions will load the SLIME/Swank,
CL-PPCRE, and McCLIM systems (ASDF), as well as the files
`"docstrings.lisp"` and `"make-docstrings.lisp"`. The latter file will
be loaded in a lexical environment in which the pathname
`#"docstrings/"` is bound to the Common Lisp variable,
`*OUTPUT-DIR*`. The TeXinfo files will therefore be generated in the
"docstrings/" directory.

The file `"docstrings.lisp"` contains a few forms that are
defined by the respective Lisp implementation -- specifically, as
to determine the name and options provided for method combinations
documented by the `"docstrings.lisp"` system. Effectively, those Lisp
forms introduce a dependency onto either SBCL or CCL, whereas those
are the respective Lisp implementatinos to which the code has been
effectively "Ported," thus far.

In the documentation generation engine, procedures managed by
SWANK include: Evaluation of implementation-specific forms,
specifically for function argument list computation.

## Examples

Building the documentation with SBCL:
>    `make all`

Building the documentation with CCL on ARM (installed locally)
>    `make LISP_SYSTEM="armcl -b -Q" all`

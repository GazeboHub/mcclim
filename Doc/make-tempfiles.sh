#!/bin/sh

# Create Texinfo snippets from the documentation of exported symbols.

# I (Troels Henriksen) cribbed this script from SBCL.
#
# This software is in the public domain and is provided with
# absolutely no warranty. See the COPYING and CREDITS files for more
# information.
#
# I (Sean Champ) have ported this for CCL

if [ -z "$1" ]
then
    # assume SBCL
    sbclsystem=$SBCL_PWD/../../src/runtime/sbcl
    sbclcore=$SBCL_PWD/../../output/sbcl.core
    if [ -e $sbclsystem ] && [ -e $sbclcore ]
    then
        LISPRUNTIME="$sbclsystem --core $sbclcore"
    else
        LISPRUNTIME="`which sbcl`"
    fi
    LISP="$LISPRUNTIME --noinform --no-sysinit --noprint --disable-debugger"
else
    LISPRUNTIME="$(echo $1 | awk '{print $1}')"
    LISP="$1"
fi



# Output directory.  This has to end with a slash (it's interpreted by
# Lisp's `pathname' function) or you lose.  This is normally set from
# Makefile.
DOCSTRINGDIR="${DOCSTRINGDIR:-docstrings/}"

echo /creating docstring snippets from LISP=\'$LISPRUNTIME\' for packages \'$PACKAGES\'
$LISP <<EOF
#-ASDF (require :asdf)
(push (pathname "$SYSTEMSDIR") asdf:*central-registry*)
(asdf:oos 'asdf:load-op :swank) ;; use swank/backend
(asdf:oos 'asdf:load-op :cl-ppcre)
(asdf:oos 'asdf:load-op :mcclim)
(load "docstrings.lisp")
(defvar *output-dir* #p"$DOCSTRINGDIR")
(load "make-docstrings.lisp")
#+SBCL (sb-ext:quit)

EOF

# Description:

'align-cljlet' is an emacs addin for aligning let forms. This program
exists because I was tired of manually aligning let statements in
clojure.  This program is designed to quickly and easily allow let
forms to be aligned.  This is my first emacs lisp program and as a
result if probably less than optimal.  Feel free to suggest
improvements or send in patches.

This program was inspired by align-let.el although does not share
any of it's code.  I had considered altering align-let.el to
work correctly with Clojure however it was easiler to simply
start from scratch.
 
# Known limitations:

* This program requires clojure mode to be running in order to
  function correctly.

# Installation:

To use align-cljlet.el, put it in your load-path and add
the following to your .emacs

(require 'align-cljlet)

# Usage:

To invoke simply position anywhere inside the let statement and
invoke:

 M-x align-cljlet

You may wish to bound this to a specific key.

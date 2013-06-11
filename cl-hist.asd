
;;;; Copyright (C) 2013 Frank James
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser General Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(in-package #:asdf)

(defsystem #:cl-hist
  :name "CL-HIST"
  :author "Frank James <frank.a.james@gmail.com>"
  :version "1"
  :maintainer "Frank James <frank.a.james@gmail.com>"
  :licence "Lisp Lesser General Public License (LLGPL)"
  :description "Common Lisp Histogram package"
  :long-description "CL-HIST is a package for creating and manipulating histograms"

  :components
  ((:file "cl-hist")))


   

;;
;;  babel-stream  -  charset encoding/decoding layer for cl-stream
;;
;;  Copyright 2017 Thomas de Grivel <thoxdg@gmail.com>
;;
;;  Permission to use, copy, modify, and distribute this software for any
;;  purpose with or without fee is hereby granted, provided that the above
;;  copyright notice and this permission notice appear in all copies.
;;
;;  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;;  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
;;

(in-package :common-lisp-user)

(defpackage :babel-stream.system
  (:use :common-lisp :asdf))

(in-package :babel-stream.system)

(defsystem :babel-stream
  :name "babel-stream"
  :author "Thomas de Grivel <thoxdg@gmail.com>"
  :version "0.1"
  :description "Charset encoding/decoding layer for cl-stream"
  :depends-on ("babel"
	       "cl-stream")
  :components
  ((:file "package")
   (:file "babel-stream" :depends-on ("package"))))

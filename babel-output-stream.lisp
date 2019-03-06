;;
;;  babel-stream  -  charset encoding/decoding layer for cl-stream
;;
;;  Copyright 2017,2018 Thomas de Grivel <thoxdg@gmail.com>
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

(in-package :babel-stream)

(defclass babel-output-stream (babel-stream output-stream)
  ())

(defmethod stream-write ((stream babel-output-stream) (element integer))
  (assert (typep element '(unsigned-byte 8)))
  (stream-write (stream-underlying-stream stream) element))

(defmethod stream-write ((stream babel-output-stream)
                         (element character))
  (let* ((encoding (stream-external-format stream))
         (mapping (babel::lookup-mapping babel::*string-vector-mappings*
                                         encoding))
         (string (make-string 1 :initial-element element))
         (bytes (make-array '(8) :element-type '(unsigned-byte 8)))
         (length (funcall (the function (babel::encoder mapping))
                          string 0 1 bytes 0)))
    (stream-write-sequence (stream-underlying-stream stream)
                           bytes :end length)))

(defmethod stream-write-sequence ((stream babel-output-stream)
                                  (seq sequence)
                                  &key start end)
  (setf start (or start 0))
  (setf end (or end (length seq)))
  (when (and (< 0 (length seq)) (< (the fixnum start)
                                   (the fixnum end)))
    (etypecase (elt seq 0)
      ((unsigned-byte 8)
       (stream-write-sequence (stream-underlying-stream stream) seq
                              :start start :end end))
      (character
       (let* ((encoding (stream-external-format stream))
              (mapping (babel::lookup-mapping
                        babel::*string-vector-mappings* encoding))
              (size (the fixnum (* 8 (the fixnum (- end start)))))
              (bytes (make-array `(,size)
                                 :element-type '(unsigned-byte 8)))
              (length (funcall (the function (babel::encoder mapping))
                               seq start end bytes 0)))
         (stream-write-sequence (stream-underlying-stream stream)
                                bytes :end length))))))

(defun babel-output-stream (stream &optional (external-format :utf-8))
  (make-instance 'babel-output-stream
                 :external-format external-format
                 :stream stream))

#+test
(let ((s (make-instance 'babel-output-stream
                        :stream (fd-stream:fd-output-stream 1))))
  (stream-write-sequence s "Hello, world ! ÉÀÖÛŸ")
  (flush s))

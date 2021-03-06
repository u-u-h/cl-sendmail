;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; classes.lisp: classes and associated methods
;;;; Copyright (C) 2006 Robert Marlow <bobstopper@bobturf.org>
;;;;
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Library General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2 of the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Library General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Library General Public
;;;; License along with this library; if not, write to the
;;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;;; Boston, MA  02111-1307, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :sendmail)

(defclass mail-output-stream-mixin (fundamental-character-output-stream)
  ((real-stream :initarg :stream :initform nil :accessor mail-output-stream-stream)
   (string-strm :accessor string-strm :initform (make-string-output-stream))
   (subject 
    :initarg :subject
    :accessor subject 
    :initform "")
   (to 
    :initarg :to
    :accessor to 
    :initform nil)
   (from
    :initarg :from
    :accessor from
    :initform nil)
   (reply-to
    :initarg :reply-to
    :accessor reply-to
    :initform nil)
   (cc
    :initarg :cc
    :accessor cc 
    :initform nil)
   (bcc 
    :initarg :bcc
    :accessor bcc 
    :initform nil)
   (attachments
    :initarg :attachments
    :accessor attachments
    :initform nil)
   (other-headers 
    :initarg :other-headers
    :accessor other-headers 
    :initform nil))
  (:documentation "A mixin for the mail-output-streams. Not intended to 
be used directly. Use the other streams"))


(defclass mail-output-stream (mail-output-stream-mixin mime)
  ((content-type :initform "text")
   (content-subtype :initform "plain"))
  (:documentation "The basic MAIL-OUTPUT-STREAM."))


(defclass text-mail-output-stream (mail-output-stream-mixin text-mime) 
  ()
  (:documentation "A MAIL-OUTPUT-STREAM intended for text-only emails"))

(defmethod content ((text-stream text-mail-output-stream))
  "Ensure that non-7-bit content is transformed to octet form."
  (let ((raw-content (call-next-method)))
    (if (and (not (eql (cl-mime:content-transfer-encoding text-stream)
		       (mime::content-encoding text-stream)))
	     (eql (mime::content-encoding text-stream) :7bit))
	(cond
	  ((stringp raw-content)
	   (flexi-streams:make-in-memory-input-stream 
	    (flexi-streams:string-to-octets 
	     raw-content 
	     :external-format (charset text-stream))))
	  ((streamp raw-content)
	   (if (flexi-streams:external-format-equal 
		(flexi-streams:flexi-stream-external-format raw-content)
		(charset text-stream))
	       raw-content
	       (flexi-streams:make-flexi-stream 
		raw-content :external-format (charset text-stream))))
	  ((eq raw-content NIL)
	   NIL)
	  (T
	   (error
	    "text-mail-output-stream content must be a string or a stream., but is ~A" 
	    raw-content)))
	raw-content)))


(defclass multipart-mail-output-stream
    (mail-output-stream-mixin multipart-mime) 
  ()
  (:documentation
   "A MAIL-OUTPUT-STREAM intended for multipart MIME emails"))


(defmethod initialize-instance :after ((object mail-output-stream-mixin) &key)
  (if (null (mail-output-stream-stream object))
      (setf (mail-output-stream-stream object) (string-strm object))
    (setf (mail-output-stream-stream object)
	  (make-broadcast-stream (mail-output-stream-stream object)
				 (string-strm object)))))


(defmethod print-object ((object mail-output-stream-mixin) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "to ~a regarding ~a" (to object) (subject object))))


(defmethod stream-write-string ((stream mail-output-stream-mixin) string
				&optional (start 0) (end (length string)))
  (princ (subseq string start end) (mail-output-stream-stream stream)))


(defmethod stream-write-char ((stream mail-output-stream-mixin) character)
  (princ character (mail-output-stream-stream stream)))


(defmethod stream-line-column ((stream mail-output-stream-mixin))
  (stream-line-column (mail-output-stream-stream stream)))


(defmethod stream-finish-output ((stream mail-output-stream-mixin))
  (finish-output (mail-output-stream-stream stream)))


(defmethod stream-force-output ((stream mail-output-stream-mixin))
  (force-output (mail-output-stream-stream stream)))


(defmethod stream-clear-output ((stream mail-output-stream-mixin))
  (clear-output (mail-output-stream-stream stream)))


(defmethod close ((stream mail-output-stream-mixin) &key abort)
  ; TODO: The following line is probably important.
  ; Why does it wreak havoc in CMUCL?
  #+sbcl (close (mail-output-stream-stream stream) :abort abort)
  (close (string-strm stream) :abort abort))


(define-condition mailer-program-error (error)
  ((error-code :initarg :error-code :accessor error-code))
  (:report (lambda (condition stream)
	     (format stream "Mailer program returned non-zero exit code: ~d."
		     (error-code condition)))))

(define-condition cl-sendmail-limitation (error)
  ((message :initarg :message :accessor message)
   (data :initarg :data :accessor data))
  (:report (lambda (condition stream)
	     (format stream "CL-Sendmail is unable to perform what you requested: ~A.~%You may want to look at: ~A"
		     (message condition)
		     (data condition)))))


(defmethod close :before ((stream mail-output-stream-mixin) &key &allow-other-keys)
    (restart-case
	(send-email stream)
      (retry ()
	     :report "Retry sending mail."
	(close stream))
      (save (pathname)
	    :report "Save mail body to file."
	    :interactive (lambda ()
			   (format *query-io* "~&Please enter a pathname: ")
			   (list (pathname (read-line *query-io*))))
	(with-open-file (s pathname :direction :output :if-exists :error)
	  (write (get-output-stream-string (mail-output-stream-stream stream))
		 :stream s)))));)

(defgeneric all-headers (mail-output-stream)
  (:documentation "Return an alist of all headers.")
  (:method ((mail-output-stream mail-output-stream-mixin))
    (let ((headers (other-headers mail-output-stream)))
      (macrolet ((handle-header (header-field accessor)
		   `(let ((data (funcall ,accessor mail-output-stream)))
		      (when data
			(push (cons ,header-field (if (consp data) data (list data)))
			      headers)))))
	(handle-header "Subject"  #'subject)
	(handle-header "To"       #'to)
	(handle-header "From"     #'from)
	(handle-header "Reply-To" #'reply-to)
	(handle-header "Cc"       #'cc)
	(handle-header "Bcc"      #'bcc))
      headers)))

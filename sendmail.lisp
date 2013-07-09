;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; sendmail.lisp: The Main Program
;;;; Copyright (C) 2004 Robert Marlow <bobstopper@bobturf.org>
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

(defparameter *sendmail* 
  #P"/usr/lib/sendmail"
  "The location of the sendmail program")


(defparameter +cr-lf-tab+ (coerce `(,#\return #\linefeed #\tab) 'string)
  "The CR-LF-TAB header splitting string as per rfc822.")

(defun clean-header-field (string)
  "Remove CR and LF from string as per rfc822 3.1.2."
  (remove-if #'(lambda (c) (or (eq c #\Return) (eq c #\Linefeed))) string))

(defun format-header (stream field args)
  "Output a header to STREAM in accordance with rfc822."
  (let ((header-data (format nil "~A: ~{~A~^,~}" field (if (consp args) args `(,args)))))
    (write-sequence (split-string +cr-lf-tab+ 
				  (clean-header-field header-data)
				  998)
		    stream)
    (terpri stream)))

(defun send-email (mail-output-stream)
  "Handles the actual sending of the email via the sendmail program"
  (unless (listp (to mail-output-stream))
    (setf (to mail-output-stream) (list (to mail-output-stream))))
  (unless (listp (cc mail-output-stream))
    (setf (cc mail-output-stream) (list (cc mail-output-stream))))
  (unless (listp (bcc mail-output-stream))
    (setf (bcc mail-output-stream) (list (bcc mail-output-stream))))
  (let ((sendmail (external-program:process-input-stream
		   (external-program:start *sendmail*
					   `("-f" ,(or (from mail-output-stream)
						       (process-user-name))
						  ,@(to mail-output-stream)
						  ,@(cc mail-output-stream)
						  ,@(bcc mail-output-stream))
					   :input :stream)))
	(mail-output-stream
	 (cond
	   ((attachments mail-output-stream)
	    (build-attachments mail-output-stream))
	   
	   ((and
	     (string-equal (content-type mail-output-stream) "text")
	     (string-equal (content-subtype mail-output-stream) "html"))
	    (build-xhtml-email mail-output-stream))
	   
	   ((string-equal (content-type mail-output-stream) "text")
	    (change-class mail-output-stream
			  'text-mail-output-stream
			  :content (or (content mail-output-stream)
				       (get-output-stream-string
					(mail-output-stream-stream
					 mail-output-stream)))))
	   
	   ((string-equal (content-type mail-output-stream) "multipart")
	    (change-class mail-output-stream
			  'multipart-mail-output-stream)))))

    (dolist (header/val (all-headers mail-output-stream))
      (format-header sendmail (car header/val) (cdr header/val)))

    (print-mime sendmail mail-output-stream t t)

    (close sendmail)))
      
(defmacro with-email ((stream to &key
			      cc
			      bcc
			      subject
			      from
			      reply-to
			      (type "text")
			      (subtype "plain")
			      attachments
			      other-headers)
		      &body body)
  "Binds STREAM to a MAIL-OUTPUT-STREAM created according to the other 
arguments then executes BODY within that context. Automatically closes
the stream and sends the email upon completion."
  `(let ((,stream (make-instance 'mail-output-stream
				 :to ,to
				 :cc ,cc
				 :bcc ,bcc
				 :subject ,subject
				 :from ,from
				 :reply-to ,reply-to
				 :type ,type
				 :subtype ,subtype
				 :attachments ,attachments
				 :other-headers ,other-headers)))
    (unwind-protect
	 (progn
	   ,@body)
      (close ,stream))))


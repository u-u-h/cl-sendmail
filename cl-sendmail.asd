;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; cl-sendmail.asd: System definition
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


(defpackage :cl-sendmail-system (:use #:cl #:asdf))
(in-package :cl-sendmail-system)

(defsystem :cl-sendmail
  :name "CL-SENDMAIL"
  :author "Robert Marlow <rob@bobturf.org>"
  :maintainer "Utz-Uwe Haus <lisp@uuhaus.de>"
  :serial t
  :version "0.6.1"
  :depends-on (:cl-mime
	       ;; the version from <http://mr.gy/maintenance/cl-qprint/>
	       (:version :cl-qprint "0.9" )
	       :cl-base64
	       :babel-streams
	       :external-program
	       :xmls 
	       :xmls-tools
	       :trivial-gray-streams)
  :components ((:file "package")
	       (:file "utilities")
	       (:file "classes")
	       (:file "attachments")
	       (:file "xhtml")
	       (:file "sendmail")))

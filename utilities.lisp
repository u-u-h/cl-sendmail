;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; utilities.lisp: General utilities useful for sendmail
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


(defun split-string (splitter string segment-size)
  (let* ((string-length (length string))
         (out-string
          (make-string (+ string-length
                          (* (floor (/ string-length segment-size))
                             (length splitter))))))
    (do* ((i 0 (1+ i))
          (j 0 (1+ j))
          (newline-p nil (= (mod i segment-size) 0)))
         ((>= i string-length))
      (when newline-p
        (do ((k 0 (1+ k)))
            ((>= k (length splitter)))
          (setf (char out-string j) (char splitter k))
          (incf j)))
      (setf (char out-string j) (char string i)))
    out-string))


(defun read-file (pathname)
  "Reads a file and returns an unsigned byte array holding the contents"
  (with-open-file (in pathname :element-type '(unsigned-byte 8))
    (let ((out (make-array (file-length in)
			   :element-type '(unsigned-byte 8))))
      (read-sequence out in)
      out)))


(defun process-user-name ()
  "Return the user name of the user running the current process. 
Returns a string, or NIL on unsupported lisp implementations."
  #+sbcl (sb-unix:uid-username 
	  (sb-unix:unix-getuid))
  #+cmu (unix:user-info-name
	 (unix:unix-getpwuid
	  (unix:unix-getuid)))
  #+allegro (system:user-name))

;; belongs into external-program/allegro.lisp
(in-package #:external-program)

#+allegro
(defstruct allegro-process
  input output error pid reap-result)

;; At any given time only one process will be alive on the OS side and occupy a PID
;; However: the caller may hang on to our process object even after its os process has
;; died and the pid is re-used. Therefore for each PID we saw we store a weak key-only 
;; hash table with the process objects, to let the GC clean up for us

#+allegro
(defparameter *allegro-process-table*
  (make-hash-table :test #'eql )
  "A hash table keyed on the PID of all processes we create
through run-shell-command to enable reaping dead children. 
Values are weak key-only hash tables containing process structures.")

#+allegro
(defparameter *allegro-unreaped-processes*
  (make-hash-table :test #'eq :values NIL)
  "A has hash table storing all unreaped processes to keep them from being
collected by the GC if the caller disposes of his reference before the process dies.")

#+allegro
(defun store-process (process)
  (let ((tab (or (gethash (allegro-process-pid process) *allegro-process-table*)
		 (setf (gethash (allegro-process-pid process) *allegro-process-table*)
		       (make-hash-table :test #'eq :weak-keys T :values nil)))))
    (setf (gethash process *allegro-process-table*) nil
	  (gethash process tab) process)))

#+allegro
(defun find-process-for-pid (pid)
  "Find the process structure for PID that has not been reaped."
  (let ((tab (gethash pid *allegro-process-table*)))
    (if tab
	(or (loop :for process :being :each :hash-key :of tab
	       :when (null (allegro-process-reap-result process))
	       :return process)
	    (error "PID ~A has no unreaped process." pid))
	(error "PID ~A unknown." pid))))

#+allegro
(defun notice-dead-processes ()
  "Reap all dead children and store their status in *allegro-process-table*."
  (loop :with status :and pid :and sig
     :do (multiple-value-setq (status pid sig)
	   (system:reap-os-subprocess :wait nil))
     :while pid ;; some process exited, and we can store its status
     :do (let ((process (find-process-for-pid pid)))
	   (setf (allegro-process-reap-result process)
		 (if sig
		     (list :SIGNALED sig)
		     (list :EXITED status)))
	   (remhash process *allegro-unreaped-processes*))))

#+allegro
(defmethod start (program args
		  &key input output error environment replace-environment-p
		    &allow-other-keys)
  (when replace-environment-p
    (warn "replace-environment unsupported on Allegro CL."))
  (when (and (or (eq nil output)
		 (eq nil error))
	     (not (probe-file #p"/dev/null")))
    (error "Cannot suppress output on this system."))
  ;; we mis-use this as a hook to reap processes, in the hope that the caller will
  ;; do sensible things to allow us to reap all processes we created:
  ;; whenever we are called we update exit status of all completed processes
  ;; before inquiring and answering
  (notice-dead-processes)
  (let* ((in (cond
	     ;; NIL and T are different on allegro
	     ((eq NIL input) (make-string-input-stream ""))
	     ((eq T input) nil)
	     (T input)))
	 (out (cond
	     ;; NIL and T are different on allegro
	     ((eq NIL output) (open #p"/dev/null" :direction :output :if-exists :supersede))
	     ((eq T input) nil)
	     (T input)))
	 (err (cond
	     ;; NIL and T are different on allegro
	     ((eq NIL input) (open #p"/dev/null" :direction :output :if-exists :supersede))
	     ((eq T input) nil)
	     ((eq :OUTPUT output) out)
	     (T input))))
    (multiple-value-bind (instream outstream errstream pid)
	(excl:run-shell-command
	 (format nil "~A~{~^ ~A~}" program args)
	 :wait nil :separate-streams T
	 :input in :output out :error-output err
	 :environment environment)
      (let ((process (make-allegro-process :input instream :output outstream :error errstream :pid pid
					   :reap-result nil)))
	(store-process process)
	process))))

#+allegro
(defmethod process-p ((process allegro-process))
  T)

#+allegro
(defmethod signal-process (process (signal symbol))
  (let ((sig (assoc signal *signal-mapping*)))
    (if (not sig)
	(error "Symbolic signal ~A not supported." signal)
	(signal-process process sig))))

#+allegro
(defmethod signal-process ((process allegro-process) (signal integer))
  (excl.osi:kill (allegro-process-pid process) signal))

#+allegro
(defmethod process-input-stream ((process allegro-process))
  (allegro-process-input process))

#+allegro
(defmethod process-output-stream ((process allegro-process))
  (allegro-process-output process))

#+allegro
(defmethod process-error-stream ((process allegro-process))
  (allegro-process-error process))

#+allegro
(defmethod process-id ((process allegro-process))
  (allegro-process-pid process))

#+allegro
(defmethod process-status ((process allegro-process))
  ;; we mis-use this as a hook to reap processes, in the hope that the caller will
  ;; do sensible things to allow us to reap all processes we created:
  ;; whenever we are called we update exit status of all completed processes
  ;; before inquiring and answering
  (notice-dead-processes)
  ;; ok, check on process now
  (if (allegro-process-reap-result process)
      (apply #'values (allegro-process-reap-result process))
      (values :RUNNING)))


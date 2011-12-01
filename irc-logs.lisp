#!/usr/local/bin/sbcl --script
;;;; Fetching #lisp and #sbcl logs

(defun usage ()
  (write-line
   "irc-logs.lisp [-n | -h] [-lisp yy.mm.dd] [-sbcl yy.mm.dd]

Fetches #lisp and #sbcl logs using curl to subdirectories named lisp/ and
sbcl/ respectively. If some logs already exist, fetches all later ones --
otherwise logs start from the current month, or the specified starting
date if one is provided.

#lisp logs are fetched from http://tunes.org/~~nef/logs/lisp/, and #sbcl logs
from http://ccl.clozure.com/irc-logs/sbcl/.

Options:

 -n     Dry run. Don't actually do anything, merely show what would be fetched.

 -lisp  Starting date of #lisp logs.

 -sbcl  Starting date of #sbcl logs.
")
  (quit))

(defvar *dry-run* nil)
(defvar *lisp-start* nil)
(defvar *sbcl-start* nil)

(loop with options = (cdr *posix-argv*)
      while options
      for opt = (pop options)
      do (cond ((equal opt "-n")
                (setf *dry-run* t))
               ((equal opt "-lisp")
                (setf *lisp-start* (pop options)))
               ((equal opt "-sbcl")
                (setf *sbcl-start* (pop options)))
               (t
                (usage))))

(defstruct date time)

(defun decode-date (date)
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time (date-time date))
    (declare (ignore sec min hour))
    (values day month year)))

(defmethod print-object ((date date) stream)
    (flet ((pretty (stream)
             (multiple-value-bind (day month year) (decode-date date)
               (declare (ignore sec min hour))
               (format stream "~2,'0D.~2,'0D.~2,'0D" (- year 2000) month day))))
    (if *print-escape*
        (print-unreadable-object (date stream :type t)
          (pretty stream))
        (pretty stream))))

(defun incf-date (date)
  (incf (date-time date) (* 24 60 60))
  date)

(defun date<= (date1 date2)
  (<= (date-time date1) (date-time date2)))

(defun start (&optional default)
  (let ((name (or (car (sort (mapcar #'file-namestring (directory "*.*.*")) #'string>))
                  default)))
    (if name
        (make-date :time (encode-universal-time 0 0 12
                                                (parse-integer name :start 6)
                                                (parse-integer name :start 3 :end 5)
                                                (+ 2000 (parse-integer name :end 2))))
        (multiple-value-bind (sec min hour day month year) (get-decoded-time)
          (declare (ignore sec min hour day))
          (make-date :time (encode-universal-time 0 0 12 1 month year))))))

(defun end ()
  (multiple-value-bind (sec min hour day month year) (get-decoded-time)
    (declare (ignore sec min hour))
    (make-date :time (encode-universal-time 0 0 12 day month year))))

(defun lisp-log-url (date)
  (format nil "http://tunes.org/~~nef/logs/lisp/~A" date))

(defun sbcl-log-url (date)
  (multiple-value-bind (day month year) (decode-date date)
    (format nil
            "http://ccl.clozure.com/irc-logs/sbcl/~4,'0D-~2,'0D/sbcl-~4,'0D.~2,'0D.~2,'0D.txt"
            year
            month
            year
            month
            day)))

(defun fetch-date (channel date url)
  (format t "~&Fetching ~A ~A~%" channel date)
  (unless *dry-run*
    (run-program "curl" (list "-f" "-o" (format nil "~A/~A" channel date)
                              (funcall url date))
                 :search t :output nil)))

(defun update (channel url default-start)
  (let* ((*default-pathname-defaults*
           (or (probe-file channel)
               (let ((dir (merge-pathnames
                           (make-pathname :directory (list :relative channel)))))
                 (if *dry-run*
                     dir
                     (ensure-directories-exist dir)))))
         (start (start default-start))
         (end (end)))
    (unless *dry-run*
      (ignore-errors
       (rename-file (princ-to-string start) ".backup")))
    (loop while (date<= start end)
          do (fetch-date channel start url)
             (incf-date start))))

(update "lisp" #'lisp-log-url *lisp-start*)

(update "sbcl" #'sbcl-log-url *sbcl-start*)

(quit)

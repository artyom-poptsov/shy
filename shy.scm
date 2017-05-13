#!/usr/bin/guile \
-e main -s
!#

<<<<<<< HEAD
=======
;;; shy.scm -- A handy tool to inspect Bash scripts.

;; Copyright (C) 2016 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; The program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with the program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

>>>>>>> origin/master
;; TODO:

;;; Code:

(define debug? #t)

<<<<<<< HEAD
;;; Alert messages
=======
;;;
>>>>>>> origin/master

(define (alert . messages)
  (for-each (lambda (m) (format #t "~a[0;37m~a~a[0m" #\033 m #\033))
            messages))

<<<<<<< HEAD
;;; standard reading file

(define (fsm-read port)
  (let ((ch (read-char port)))
    (unless (eof-object? ch)
      (case ch
        ((#\#)
         (fsm-skip-commentary port))
        ((or (#\& #\>) (#\> #\&))
         (alert-redirection))
        ((#\$)
         (fsm-inspect-dollar port))
        ((#\`)
         (fsm-inspect-backticks port))
        ((#\| #\&)
         (alert-pipeline-amp))
        (else
         (when debug?
           (display ch))
         (fsm-read port))))))

;;; alert redirection

(define alert-redirection
  (alert "Deprecated redirection syntax found\n"
         " -- <http://wiki.bash-hackers.org/scripting/obsolete>\n"))

;;; skip bash comments
=======
;;;
(define (fsm-for-expression port)
  (let ((ch (read-char port)))
    (unless (eof-object? ch)
      (case ch 
        ((and (#\f) (#\o) (#\r))
         (alert "For Deprecated syntax: found"
                " -- <http://wiki.bash-hackers.org/scripting/obsolete>\n")
         (fsm-read port))
        (else
         (fsm-for-expression port))))))

>>>>>>> origin/master

(define (fsm-skip-commentary port)
  (let ((ch (read-char port)))
    (unless (eof-object? ch)
      (case ch
        ((#\newline)
         (fsm-read port))
        (else
         (fsm-skip-commentary port))))))

<<<<<<< HEAD
;;; check two dollar expressions

(define (fsm-inspect-dollar port)
=======
(define (fsm-bracket-expression port)
  (let ((ch (read-char port)))
    (unless (eof-object? ch) 
      (case ch
        ((#\])
         (alert "Bracket Deprecated syntax: found"
                " -- <http://wiki.bash-hackers.org/scripting/obsolete>\n")
         (fsm-read port))
        (else
         (fsm-check-expression port))))))


(define (fsm-inspect-backticks port)
>>>>>>> origin/master
  (let ((ch (read-char port)))
    (unless (eof-object? ch)
      (case ch
        ((#\[)
         (fsm-square-bracket-expression))
        ((#\{ #\v #\a #\r)
         (fsm-curly-brace-expression))
        (else
         (when debug?
          (display ch))
         (fsm-read port))))))

;;; if $[EXPRESSION] found

(define (fsm-square-bracket-expression port)
  (let ((ch (read-char port)))
    (unless (eof-object? ch) 
      (case ch
        ((#\])
         (alert "Square bracket deprecated syntax found\n"
                " -- <http://wiki.bash-hackers.org/scripting/obsolete>\n")
         (fsm-read port))
        (else
         (fsm-square-bracket-expression port))))))

;;; if ${var?msg} or ${var:?msg} found

(define (fsm-curly-brace-expression port)
  (let ((ch (read-char port)))
    (unless (eof-object? ch) 
      (case ch
        ((#\})
         (alert "Curly brace deprecated syntax found\n"
                " -- <http://wiki.bash-hackers.org/scripting/obsolete>\n")
         (fsm-read port))
        (else
         (fsm-curly-brace-expression port))))))

;;; alert < comm |& comm > syntax

(define alert-pipeline-amp
  (alert "Pipeline-ampersand deprecated syntax found\n"
         " -- <http://wiki.bash-hackers.org/scripting/obsolete>\n"))

;;; inspect for backticks

(define (fsm-inspect-backticks port)
  (let ((ch (read-char port)))
    (unless (eof-object? ch)
      (case ch
<<<<<<< HEAD
=======
        ((#\#)
         (fsm-bracket-expression port)
         (fsm-skip-commentary port)
         (fsm-for-expression port))
>>>>>>> origin/master
        ((#\`)
         (alert "Backticks found\n"
                " -- <http://mywiki.wooledge.org/BashFAQ/082>\n")
         (fsm-read port))
        (else
         (fsm-inspect-backticks port))))))

;;; Commands

(define (inspect file)
  "Inspect a FILE."
  (let ((port (open-input-file file)))
    (fsm-read port)))

<<<<<<< HEAD
;;; Help
=======
;;;
>>>>>>> origin/master

(define (print-help-and-exit)
  (display "\
Usage: shy command [args]

Commands:
  help, h        Print this message and exit.
  commentary, c  Print commentary.
  copyright, C   Print copyright.
  deps, d        Print dependency tree of scripts that are sourced from
                 the script.
  inspect, i     Inspect the script for issues
                 (NOTE: this is HIGHLY experimental feature)
")
  (exit))

;;; Entry point

(define (main args)
  ;; (display (cadr args))
  ;; (newline))
  ;; (display #\033)
  ;; (display "[0;37m")
  ;; (write "\033[0;37mtest\033[0m")
  ;; (newline)
  (let ((command (cadr args)))
    (cond
     ((or (string=? command "help") (string=? command "h"))
      (print-help-and-exit))
     ((or (string=? command "inspect") (string=? command "i"))
      ;; TODO:
      (inspect (caddr args))))))

;;; shy.scm ends here.

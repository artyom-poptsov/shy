#!/usr/bin/guile \
-e main -s
!#

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

;; TODO:

;;; Code:

(define debug? #t)

;;; Alert messages

(define (alert . messages)
  (for-each (lambda (m) (format #t "~a[0;37m~a~a[0m" #\033 m #\033))
            messages))

;;; standard reading file

(define (fsm-read port)
  (let ((ch (read-char port)))
    (unless (eof-object? ch)
      (case ch
        ((#\#)
         (fsm-skip-commentary port))
        ((#\&)
         (alert-redirection)
         (fsm-read port))
        ((#\$)
         (fsm-inspect-dollar port))
        ((#\`)
         (fsm-inspect-backticks port))
        ((#\|)
         (alert-pipeline-amp)
         (fsm-read port))
        ((#\f)
         (fsm-inspect-function port))
        (else
         (when debug?
           (display ch))
         (fsm-read port))))))

;;; inspect f expression

(define (fsm-f-test port)
  (let ((ch (read-char port)))
    (unless (eof-object? ch)
      (case ch
        ((#\u)
         (fsm-function-expression port))
        ((#\o)
         (fsm-for-expression port))
      (else 
        (testing port))))))

(define (fsm-function-expression port))

(define (fsm-for-expression port)
  (let ((ch (read-char port)))
    (if (char=? ch #\r)
        (alert "Deprecated for syntax found\n"
               " -- <http://wiki.bash-hackers.org/scripting/obsolete>\n"))
        (fsm-read port))))

;;; alert redirection

(define alert-redirection
  (alert "Deprecated redirection syntax found\n"
         " -- <http://wiki.bash-hackers.org/scripting/obsolete>\n"))

;;; skip bash comments

(define (fsm-skip-commentary port)
  (let ((ch (read-char port)))
    (unless (eof-object? ch)
      (case ch
        ((#\newline)
         (fsm-read port))
        (else
         (fsm-skip-commentary port))))))

;;; check for two dollar expressions

(define (fsm-inspect-dollar port)
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

;;; Help

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
  (let ((command (cadr args)))
    (cond
     ((or (string=? command "help") (string=? command "h"))
      (print-help-and-exit))
     ((or (string=? command "inspect") (string=? command "i"))
      ;; TODO:
      (inspect (caddr args))))))

;;; shy.scm ends here.

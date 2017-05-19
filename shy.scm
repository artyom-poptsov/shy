#!/usr/bin/guile \
-e main -s
!#

(use-modules (ice-9 unicode))

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
         (fsm-check-amp port))
        ((#\>)
         (fsm-triangular-bracket port))
        ((#\$)
         (fsm-inspect-dollar port))
        ((#\|)
         (pipeline-amp port))
        ((#\`)
         (fsm-inspect-backticks port))
        ((#\f)
         (fsm-f-test port))
        ((#\s)
         (fsm-error-handling port))
        (else
         (when debug?
           (display ch))
         (fsm-read port))))))

;;; skip bash comments

(define (fsm-skip-commentary port)
  (let ((ch (read-char port)))
    (unless (eof-object? ch)
      (case ch
        ((#\newline)
         (fsm-read port))
        (else
         (fsm-skip-commentary port))))))

;;; redirection syntax

(define (fsm-check-amp port)
  (let ((ch (read-char port)))
    (unless (eof-object? ch)
      (case ch
        ((#\>)
         (alert "Deprecated redirection syntax found\n"
                " -- <http://bit.ly/2r1cCgm>\n"))
      (else
        (fsm-read port))))))

;;;

(define (fsm-triangular-bracket port)
  (let ((ch (read-char port)))
    (unless (eof-object? ch)    
      (case ch
        ((#\&)
         (fsm-read port))
        ((#\f)
         (fsm-triangular-bracket port))
        ((#\1)
         (fsm-read port))
      (else
        (alert "Deprecated redirection syntax found\n"
               " -- <http://bit.ly/2r1cCgm>\n"))))))

;;; inspect f expression

(define (fsm-f-test port)
  (let ((ch (read-char port)))
    (unless (eof-object? ch)
      (case ch
        ((#\u)
         (fsm-function-recognite port))
        ((#\o)
         (fsm-for-expression port))
      (else 
        (fsm-read port))))))

;;; dirty function inspecting

(define (fsm-function-recognite port)
  (let ((ch (read-char port)))
    (unless (eof-object? ch)
      (case ch
        ((#\n #\c #\t #\i #\o)
         (fsm-func-determine port))
      (else 
        (fsm-read port))))))

;;;

(define (fsm-func-determine port)
  (let ((ch (read-char port)))
    (unless (eof-object? ch)
      (case ch
        ((#\x0028 #\x0029 #\{ #\})          ;;; parentheses
         (alert "Deprecated function syntax found\n"
                " -- <http://bit.ly/2r1cCgm>\n")
         (fsm-read port))
      (else
        (fsm-read port))))))

;;;

(define (fsm-for-expression port)
  (let ((ch (read-char port)))
    (if (char=? ch #\r)
        (alert "Deprecated for syntax found\n"
               " -- <http://bit.ly/2r1cCgm>\n"))
        (fsm-read port)))

;;; check for two dollar expressions  (uncomplete)

(define (fsm-inspect-dollar port)
  (let ((ch (read-char port)))
    (unless (eof-object? ch)
      (case ch
        ((#\[)
         (fsm-square-bracket-expression port))
        ((#\{ #\v #\a #\r)
         (fsm-curly-brace-expression port))
        (else
         (when debug?
          (display ch))
         (fsm-read port))))))

;;;

(define (fsm-square-bracket-expression port)
  (let ((ch (read-char port)))
    (unless (eof-object? ch) 
      (case ch
        ((#\])
         (alert "Square bracket deprecated syntax found\n"
                " -- <http://bit.ly/2r1cCgm>\n")
         (fsm-read port))
        (else
         (fsm-square-bracket-expression port))))))

;;;

(define (fsm-curly-brace-expression port)
  (let ((ch (read-char port)))
    (unless (eof-object? ch) 
      (case ch
        ((#\})
         (alert "Curly brace deprecated syntax found\n"
                " -- <http://bit.ly/2r1cCgm>\n")
         (fsm-read port))
        (else
         (fsm-curly-brace-expression port))))))

;;; pipeline-ampersand inspect

(define (pipeline-amp port)
  (let ((ch (read-char port)))
    (unless (eof-object? ch) 
      (case ch
        ((#\&)
         ((alert "Pipeline-ampersand deprecated syntax found\n"
                 " -- <http://bit.ly/2r1cCgm>\n")))
      (else
        (fsm-read port))))))

;;; inspect for backticks

(define (fsm-inspect-backticks port)
  (let ((ch (read-char port)))
    (unless (eof-object? ch)
      (case ch
        ((#\`)
         (alert "Backticks found\n"
                " -- <http://bit.ly/2qG43Vl>\n")
         (fsm-read port))
        (else
         (fsm-inspect-backticks port))))))

;;; error handling, set commands

(define (fsm-error-handling port)
  (let ((ch (read-char port)))
    (unless (eof-object? ch)
      (cond 
        ((eqv? ch #\e)
         (fsm-error-handling port))
        ((eqv? ch #\t)
         (fsm-set-args port))
      (else 
        (fsm-read port))))))

;;; alerts

(define (set-handling)
  (alert "Deprecated error handling syntax found\n"
         " -- <http://bit.ly/2r1cCgm>\n"))

;;;

(define (fsm-set-args port)
  (let ((ch (read-char port)))
    (unless (eof-object? ch)
      (case ch
        ((#\e #\u)
         (set-handling)
         (fsm-read port))
        ((#\o)
         (fsm-o-set-property port))
      (else 
        (fsm-read port))))))

;;; set-option determine (uncomp)

(define (fsm-o-set-property port)
  (let ((ch (read-char port)))
    (unless (eof-object? ch)
      (fsm-read port))))

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

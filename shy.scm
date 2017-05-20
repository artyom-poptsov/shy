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

(define (set-handling)
  (alert "Deprecated error handling syntax found\n"
         " -- <https://bit.ly/2rCTrpa>\n"))

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
         (fsm-error-handling port 1))
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
                " -- <https://bit.ly/2rCTrpa>\n"))
      (else
        (fsm-read port))))))

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
               " -- <https://bit.ly/2rCTrpa>\n"))))))

;;; inspect f expressions

(define (fsm-f-test port)
  (let ((ch (read-char port)))
    (unless (eof-object? ch)
      (case ch
        ((#\u)
         (fsm-function-recognite port 0))
        ((#\o)
         (fsm-for-expression port))
      (else 
        (fsm-read port))))))

(define function "function ")

(define (fsm-function-recognite port n)
  (let ((ch (read-char port))
        (ch-check (string-ref function (+ n 2))))
    (unless (eof-object? ch)
      (cond 
        ((= n 6)
         (fsm-func-determine port))
        ((eqv? ch ch-check)
         (fsm-function-recognite
 port (+ n 1)))
      (else
        (fsm-read port))))))

(define (fsm-func-determine port)
  (let ((ch (read-char port)))
    (unless (eof-object? ch)
      (case ch
        ((#\x0028 #\x0029 #\{ #\})          ;;; parentheses
         (alert "Deprecated function syntax found\n"
                " -- <https://bit.ly/2rCTrpa>\n")
         (fsm-read port))
      (else
        (fsm-read port))))))

(define (fsm-for-expression port)
  (let ((ch (read-char port)))
    (if (eqv? ch #\r)
        (alert "Deprecated for syntax found\n"
               " -- <https://bit.ly/2rCTrpa>\n"))
        (fsm-read port)))

;;; check for two dollar expressions  (uncomplete)

(define (fsm-inspect-dollar port)
  (let ((ch (read-char port)))
    (unless (eof-object? ch)
      (case ch
        ((#\[)
         (fsm-square-bracket port))
        ((#\{)
         (fsm-curly-brace port))
        (else
         (when debug?
          (display ch))
         (fsm-read port))))))

(define (fsm-square-bracket port)
  (let ((ch (read-char port)))
    (unless (eof-object? ch) 
      (case ch
        ((#\])
         (alert "Square bracket deprecated syntax found\n"
                " -- <https://bit.ly/2rCTrpa>\n")
         (fsm-read port))
        (else
         (fsm-square-bracket port))))))

(define (fsm-curly-brace port)
  (let ((ch (read-char port)))
    (unless (eof-object? ch)
      (case ch
        ((#\: #\?)
         (fsm-curly-brace-end port))
      (else
        (fsm-read port))))))

(define (fsm-curly-brace-end port)
  (let ((ch (read-char port)))
    (unless (eof-object? ch) 
      (case ch
        ((#\})
         (alert "Curly brace deprecated syntax found\n"
                " -- <https://bit.ly/2rCTrpa>\n")
         (fsm-read port))
        (else
         (fsm-curly-brace-end port))))))

;;; pipeline-ampersand inspect

(define (pipeline-amp port)
  (let ((ch (read-char port)))
    (unless (eof-object? ch) 
      (case ch
        ((#\&)
         ((alert "Pipeline-ampersand deprecated syntax found\n"
                 " -- <https://bit.ly/2rCTrpa>\n")))
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

(define (fsm-error-handling port n)
  (let ((ch (read-char port))
        (ch-check (string-ref set n)))
    (unless (eof-object? ch)
      (cond 
        ((= n 3)
         (fsm-set-args port))
        ((eqv? ch ch-check)
         (fsm-error-handling port (+ n 1)))      
      (else 
        (fsm-read port))))))

(define set "set ")
(define set-pair (cons "errexit " "nouset "))

(define (fsm-set-args port)
  (let ((ch (read-char port)))
    (unless (eof-object? ch)
      (case ch
        ((#\x0020)                      ;;; whitespace char
         (fsm-set-args port))
        ((#\-)
         (fsm-set-args-middle port))
      (else 
        (fsm-read port))))))

(define (fsm-set-args-middle port)
  (let ((ch (read-char port)))
    (unless (eof-object? ch)
      (case ch
        ((#\e #\u)
         (set-handling)
         (fsm-read port))
        ((#\o)
         (fsm-set-args-end port 0 0))
      (else 
        (fsm-read port))))))

(define (fsm-set-args-end port n k)
  (let ((ch (read-char port))
        (ch-1st-check (string-ref (car set-pair) n))
        (ch-2nd-check (string-ref (cdr set-pair) k)))
    (unless (eof-object? ch)
      (cond
        ((eqv? ch #\x0020)
         (fsm-set-args-end port n k))
        ((= k 5)
         (set-handling)
         (fsm-read port)) 
        ((= n 6)
         (set-handling)
         (fsm-read port))
        ((eqv? ch ch-1st-check)
         (fsm-set-args-end port (+ n 1) (+ k 1)))
        ((eqv? ch ch-2nd-check)
         (fsm-set-args-end port (+ n 1) (+ k 1)))      
      (else 
        (fsm-read port))))))

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

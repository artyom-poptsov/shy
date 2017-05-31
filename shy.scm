#!/usr/bin/guile \
-e main -s
!#

(use-modules (ice-9 unicode)
             (ice-9 getopt-long))

;;; Commentary:

;; TODO:

;;; Code:

(define debug? #t)

;;; Alert messages

(define (alert . messages)
  (for-each (lambda (m) (format #t "~a[0;37m~a~a[0m" #\033 m #\033))
            messages))

(define (set-handling)
  (alert "\nDeprecated error handling syntax found"
         " -- <https://bit.ly/2rCTrpa>\n"))
(define (alert-typeset)
  (alert "\nDeprecated typeset syntax found"
         " -- <https://bit.ly/2rCTrpa>\n"))
(define (alert-eval)
  (alert "\nQuestionable 'eval' syntax found."
         " -- <https://bit.ly/2rCTrpa>\n"
         "Often code can be restructured to use better alternatives.\n"))
(define (alert-let)
  (alert "\nDeprecated let syntax found"
         " -- <https://bit.ly/2rCTrpa>\n"))
(define (alert-for)
        (alert "\nDeprecated for syntax found"
               " -- <https://bit.ly/2rCTrpa>\n"))

(define (alert-no-shebang)
  (alert "\nNo proper shebang found"
         " -- <http://wiki.bash-hackers.org/scripting/newbie_traps>\n"))

;;; standard reading file

(define (fsm-shebang-check-bang port count)
  (let ((ch (read-char port)))
    (case ch
      ((#\!)
       (fsm-read port count))
      (else
       (alert-no-shebang)
       (unread-char ch port)
       (fsm-read port count)))))

(define (fsm-shebang-check port count)
  (let ((ch (read-char port)))
    (case ch
      ((#\#)
       (fsm-shebang-check-bang port count))
      (else
       (alert-no-shebang)
       (unread-char ch port)
       (fsm-read port count)))))

(define (fsm-read port count)
  (let ((ch (read-char port)))
    (unless (eof-object? ch)
      (case ch
        ((#\newline)
         (fsm-read port (+ count 1)))
        ((#\#)
         (fsm-skip-commentary port count))
        ((#\&)
         (fsm-check-amp port count))
        ((#\>)
         (fsm-triangular-bracket port count))
        ((#\$)
         (fsm-inspect-dollar port count))
        ((#\|)
         (pipeline-amp port count))
        ((#\`)
         (fsm-inspect-backticks port (string ch) count))
        ((#\f)
         (fsm-f-test port count))
        ((#\s)
         (fsm-error-handling port 1 count))
        ((#\t)
         (fsm-read-typeset port count))
        ((#\l)
         (fsm-read-let port count))
        ((#\e)
         (fsm-read-eval port count))
        (else
 ;;        (when debug?
 ;;        (display ch))
         (fsm-read port count))))))

;;; skip bash comments

(define (fsm-skip-commentary port count)
  (let ((ch (read-char port)))
    (unless (eof-object? ch)
      (case ch
        ((#\newline)
         (fsm-read port (+ 1 count)))
        (else
         (fsm-skip-commentary port count))))))

;;; redirection syntax

(define (fsm-check-amp port count)
  (let ((ch (read-char port)))
    (unless (eof-object? ch)
      (case ch
        ((#\newline)
         (fsm-read port (+ 1 count)))
        ((#\>)
         (display "line ") (display count) (display ": ") (display "&>")  
         (alert "\nDeprecated redirection syntax found"
                " -- <https://bit.ly/2rCTrpa>\n")
         (fsm-read port count))
        (else
         (fsm-read port count))))))

(define (fsm-triangular-bracket port count)
  (let ((ch (read-char port)))
    (unless (eof-object? ch)
      (case ch
        ((#\newline)
         (fsm-read port (+ 1 count)))
        ((#\&)
         (fsm-triangular-end port count))
        (else
         (fsm-read port count))))))

(define (fsm-triangular-end port count)
  (let ((ch (read-char port)))
    (unless (eof-object? ch)
      (case ch
        ((#\newline)
         (fsm-read port (+ 1 count)))
        ((#\1)
         (fsm-read port count))
        (else
         (display "line ")(display count) (display ": ") (display ">&") 
         (alert "\nDeprecated redirection syntax found"
                " -- <https://bit.ly/2rCTrpa>\n")
         (fsm-read port count))))))

;;; inspect f expressions

(define (fsm-f-test port count)
  (let ((ch (read-char port)))
    (unless (eof-object? ch)
      (case ch
        ((#\newline)
         (fsm-read port (+ 1 count)))
        ((#\u)
         (fsm-function-recognite port 0 count))
        ((#\o)
         (fsm-for-expression port count))
        (else
         (fsm-read port count))))))

(define function "function ")

(define (fsm-function-recognite port n count)
  (let ((ch (read-char port))
        (ch-check (string-ref function (+ n 2))))
    (unless (eof-object? ch)
      (cond
       ((eqv? ch #\newline)
        (fsm-read port (+ 1 count)))
       ((= n 6)
        (fsm-func-determine port (string #\null) count))
       ((eqv? ch ch-check)
        (fsm-function-recognite
         port (+ n 1) count))
       (else
        (fsm-read port count))))))

(define (fsm-func-determine port follow count)
  (let ((ch (read-char port)))
    (unless (eof-object? ch)
      (case ch
        ((#\newline)
         (fsm-read port (+ 1 count)))
        ((#\x0029 #\})          ;;; parentheses
         (display "line ") (display count) (display ": ")
         (display function) (display follow) (display ch)
         (alert "\nDeprecated function syntax found"
                " -- <https://bit.ly/2rCTrpa>\n")
         (fsm-read port count))
        (else
         (fsm-func-determine port
          (string-append follow (string ch)) count))))))

(define (fsm-for-expression port count)
  (let ((ch (read-char port)))
    (case ch
        ((#\r)
         (fsm-for-middle port (string #\null) count))
        ((#\newline)
         (fsm-read port (+ 1 count)))
        (else 
         (fsm-read port count)))))

(define (fsm-for-middle port follow count)
  (let ((ch (read-char port)))
    (unless (eof-object? ch)
      (case ch
        ((#\newline)
         (fsm-read port (+ 1 count)))
        ((#\x0028)
         (fsm-read port count))
        ((#\x003B)
         (fsm-for-end port (string-append follow (string ch)) count))
      (else
        (fsm-for-middle port (string-append follow (string ch)) count))))))

(define (fsm-for-end port follow count)
  (let ((ch (read-char port)))
    (unless (eof-object? ch)
      (case ch
        ((#\newline)
         (fsm-read port (+ 1 count)))
        ((#\})
         (display "line ") (display count) (display ": ")
         (display "for") (display follow) (display #\})
         (alert-for)
         (fsm-read port count))
      (else
        (fsm-for-end port (string-append follow (string ch)) count))))))

;;; check for two dollar expressions

(define (fsm-inspect-dollar port count)
  (let ((ch (read-char port)))
    (unless (eof-object? ch)
      (case ch
        ((#\newline)
         (fsm-read port (+ 1 count)))
        ((#\[)
         (fsm-square-bracket port (string ch) count))
        ((#\{)
         (fsm-curly-brace port (string ch) count))
        (else
         (fsm-read port count))))))

(define (fsm-square-bracket port follow count)
  (let ((ch (read-char port)))
    (unless (eof-object? ch)
      (case ch
        ((#\newline)
         (fsm-read port (+ 1 count)))
        ((#\])
         (display "line ") (display count) (display ": ")
         (display "$") (display follow) (display ch)
         (alert "\nSquare bracket deprecated syntax found"
                " -- <https://bit.ly/2rCTrpa>\n")
         (fsm-read port count))
        (else
         (fsm-square-bracket port 
          (string-append follow (string ch)) count))))))

(define (fsm-curly-brace port follow count)
  (let ((ch (read-char port)))
    (unless (eof-object? ch)
      (case ch
        ((#\newline)
         (fsm-read port (+ 1 count)))
        ((#\: #\?)
         (fsm-curly-brace-end port 
          (string-append follow (string ch)) count))
        (else
         (fsm-curly-brace port 
          (string-append follow (string ch)) count))))))

(define (fsm-curly-brace-end port follow count)
  (let ((ch (read-char port)))
    (unless (eof-object? ch)
      (case ch
        ((#\newline)
         (fsm-read port (+ 1 count)))
        ((#\})
         (display "line ") (display count) (display ": ")
         (display #\$) (display follow) (display ch)
         (alert "\nCurly brace deprecated syntax found"
                " -- <https://bit.ly/2rCTrpa>\n")
         (fsm-read port count))
        (else
         (fsm-curly-brace-end port
          (string-append follow (string ch)) count))))))

;;; pipeline-ampersand inspect

(define (pipeline-amp port count)
  (let ((ch (read-char port)))
    (unless (eof-object? ch)
      (case ch
        ((#\newline)
         (fsm-read port (+ 1 count)))
        ((#\&)
         (display "line ") (display count) (display ": ") (display "|&")
         (alert "\nPipeline-ampersand deprecated syntax found"
                 " -- <https://bit.ly/2rCTrpa>\n")
         (fsm-read port count))
        (else
         (fsm-read port count))))))

;;; inspect for backticks

(define (fsm-inspect-backticks port follow count)
  (let ((ch (read-char port)))
    (unless (eof-object? ch)
      (case ch
        ((#\newline)
         (fsm-read port (+ 1 count)))
        ((#\`)
         (display "line ") (display count) (display ": ")
         (display follow) (display ch)
         (alert "\nBackticks found"
                " -- <http://bit.ly/2qG43Vl>\n")
         (fsm-read port count))
        (else
         (fsm-inspect-backticks port 
          (string-append follow (string ch)) count))))))

;;; error handling, set commands

(define set "set ")
(define set-pair (cons "errexit " "nouset "))

(define (fsm-error-handling port n count)
  (let ((ch (read-char port))
        (ch-check (string-ref set n)))
    (unless (eof-object? ch)
      (cond
       ((eqv? ch #\newline)
        (fsm-read port (+ 1 count)))
       ((= n 3)
        (fsm-set-args port count))
       ((eqv? ch ch-check)
        (fsm-error-handling port (+ n 1) count))
       (else
        (fsm-read port count))))))

(define (fsm-set-args port count)
  (let ((ch (read-char port)))
    (unless (eof-object? ch)
      (case ch
        ((#\newline)
         (fsm-read port (+ 1 count)))
        ((#\x0020)                      ;;; whitespace char
         (fsm-set-args port count))
        ((#\-)
         (fsm-set-args-middle port count))
        (else
         (fsm-read port count))))))

(define (fsm-set-args-middle port count)
  (let ((ch (read-char port)))
    (unless (eof-object? ch)
      (case ch
        ((#\newline)
         (fsm-read port (+ 1 count)))
        ((#\e)
         (display "line ") (display count) (display ": ")
         (display "set -e")
         (set-handling)
         (fsm-read port count))
        ((#\u)
         (display "line ") (display count) (display ": ")
         (display "set -u")
         (set-handling)
         (fsm-read port count))
        ((#\o)
         (fsm-set-args-end port 0 0 count))
        (else
         (fsm-read port count))))))

(define (fsm-set-args-end port n k count)
  (let ((ch (read-char port))
        (ch-1st (string-ref (car set-pair) n))
        (ch-2nd (string-ref (cdr set-pair) k)))
    (unless (eof-object? ch)
      (cond
       ((eqv? ch #\newline)
        (fsm-read port (+ 1 count)))
       ((eqv? ch #\x0020)
        (fsm-set-args-end port n k count))
       ((= k 5)
        (display "line ") (display count) (display ": ")
        (display "set -o nounset")        
        (set-handling)
        (fsm-read port count))
       ((= n 6)
        (display "line ") (display count) (display ": ")
        (display "set -o errexit")
        (set-handling)
        (fsm-read port count))
       ((eqv? ch ch-1st)
        (fsm-set-args-end port (+ n 1) (+ k 1) count))
       ((eqv? ch ch-2nd)
        (fsm-set-args-end port (+ n 1) (+ k 1) count))
       (else
        (fsm-read port count))))))

;;; typeset, eval, let operators

(define typeset "typeset")
(define let-expr "let")
(define eval-expr "eval")

(define (read-word port buffer expected-word handler count)
  "Read an EXPECTED-WORD from a PORT, call a HANDLER if a word is
fully read."
  (let ((ch (read-char port)))
    (cond
     ((eqv? ch #\newline)
      (fsm-read port (+ 1 count)))
     ((= (+ 1 (string-length buffer)) (string-length expected-word))
      (display "line ") (display count) (display ": ")
      (display expected-word)
      (handler)
      (fsm-read port count))
     ((char=? ch (string-ref expected-word (string-length buffer)))
      (read-word port (string-append buffer (string ch))
                 expected-word handler count))
     (else
      (fsm-read port count)))))

(define (fsm-read-typeset port count)
  "Read data from a PORT, check for deprecated 'typeset' syntax."
  (read-word port (string #\t) typeset alert-typeset count))

(define (fsm-read-let port count)
  "Read data from a PORT, check for deprecated 'let' syntax."
  (read-word port (string #\l) let-expr alert-let count))

(define (fsm-read-eval port count)
  "Read data from a PORT, check for questionable 'eval' syntax."
  (read-word port (string #\e) eval-expr alert-eval count))

;;; Commands

(define (inspect file)
  "Inspect a FILE."
  (let ((port (open-input-file file)))
    (fsm-shebang-check port 1)))

;;; Help

(define (print-help-and-exit)
  (display "\
Usage: shy command [args]

Commands:
  -h, --help        Print this message and exit.
  -i, --inspect     Inspect the script for issues.
  -c, --commentary  Print commentary.
  -d, --deps        Print dependency tree of scripts.
  -v, --version     Print version and exit.
")
  (exit))

;;; Version

(define (print-version-and-exit)
  (display "\nShy 0.1v
Copyright (C) 2017 Free Software Foundation, Inc.

License LGPLv3+: GNU LGPL 3 or later <http://gnu.org/licenses/lgpl.html>.
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.\n")
  (exit))

;;; Dependency tree

(define (print-deps-and-exit)
  (newline)
  (display "tree")
  (exit))

;;; Comments

(define (print-comments-and-exit)
  (display "comments")
  (exit))

;;; Entry point

(define %option-spec
  '((help       (single-char #\h) (value #f))
    (version    (single-char #\v) (value #f))
    (commentary (single-char #\c) (value #f))
    (deps       (single-char #\d) (value #f))
    (inspect    (single-char #\i) (value #f))))

(define (main args)
  (let* ((options            (getopt-long args %option-spec))
         (help-needed?       (option-ref options 'help #f))
         (version-needed?    (option-ref options 'version #f))
         (commentary-needed? (option-ref options 'commentary #f))
         (deps-needed?       (option-ref options 'deps #f))
         (inspect?           (option-ref options 'inspect #f))
         (args               (option-ref options '() #f)))
    (cond
     ((or (zero? (length args)) help-needed?)
      (print-help-and-exit))
     (version-needed?
      (print-version-and-exit))
     (commentary-needed?
      (print-comments-and-exit))
     (deps-needed?
      (print-deps-and-exit))
     (inspect?
      (inspect (car args))))))

;;; shy.scm ends here.

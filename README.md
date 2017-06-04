* shy
  A handy tool for inspecting bash scripts.

  The inspiration for such a tool partly comes from =guild= program
  that is shipped with [[https://www.gnu.org/software/guile/][GNU Guile]].

** Usage

*** Print help

    Print reference of program:
#+BEGIN_EXAMPLE
$ shy --help
$ shy -h                     # Shorthand
$ shy                        # Without command
#+END_EXAMPLE

*** Inspect a script

    Inspect a script for potentially problematic and/or obsolete
    things:
#+BEGIN_EXAMPLE
$ shy --inspect <script>
$ shy -i <script>            # Shorthand
#+END_EXAMPLE

*** Print commentary of deprecated syntax
    
    You can print this section as follows:
#+BEGIN_EXAMPLE
$ shy --commentary <script>
$ shy -c <script>            # Shorthand
#+END_EXAMPLE

*** Print dependencies

    Print script dependencies, that is, scripts that are sourced in
    the given script:
#+BEGIN_EXAMPLE
$ shy --deps <script>
$ shy -d <script>            # Shorthand
#+END_EXAMPLE

*** Print version

    Print version of program:
#+BEGIN_EXAMPLE
$ shy --version
$ shy -v                     # Shorthand
#+END_EXAMPLE

*** Documentation

    Read the documentation:
#+BEGIN_EXAMPLE
$ man shy
$ info shy
#+END_EXAMPLE


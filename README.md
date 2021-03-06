# Shy

  A handy tool for inspecting bash scripts.

  The inspiration for such a tool partly comes from =guild= program
  that is shipped with [GNU Guile](https://www.gnu.org/software/guile/).

## Usage

### Print help

    Print reference of program:

```
$ shy --help
$ shy -h                   # Shorthand
$ shy                      # Without command
```

### Inspect a script

    Inspect a script for potentially problematic and/or obsolete things:

```
$ shy --inspect [file]
$ shy -i [file]            # Shorthand
```

### Print commentary of deprecated syntax
    
    You can print this section as follows:

```
$ shy --commentary [file]
$ shy -c [file]            # Shorthand
```

### Print version

    Print version of program:

```
$ shy --version
$ shy -v                   # Shorthand
```

### Documentation

    Read the documentation:

```
$ man shy
$ info shy
```

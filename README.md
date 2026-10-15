# Zeta

A format describing the syntax of terminal commands, and an algorithm `Zeta` to evaluate a terminal entry to a formatted representation of the entry. The project aims to be a general approach to parse terminal entries despites the variety of terminal commands syntax.

## Folder structure

```
- docs/ : markdown documentation of the project
- evaluator/ : a executable to browse commands written in the registry and to evaluate terminal entries (see 'Usage')
- format/ : the definition of the Zeta format, describing a command syntax
- registry/ : a collection of described commands
- representation/ : the format describing the representation of an evaluated terminal entry
- test/ : test utility to check the algorithm against real terminal entries
- zeta/ : the algorithm evaluating a terminal entry into its formated representation
```

## Usage

The `evaluator` executable has two subcommands:

```sh
# browse interactively the commands of the registry, their features, arguments, anchors and options
dune exec evaluator/main.exe browse

# evaluate interactively a terminal entry with Zeta
dune exec evaluator/main.exe -- eval '<entry>'
```

`eval` opens a line editor prefilled with the entry. Press Enter to evaluate it: the matching command, the matching features and the configuration computed by Zeta are displayed, then the entry can be edited again. Press Ctrl-C to quit. Quote the entry (or put it after `--`) so that its options are not read as options of the evaluator.

Example:

```
$ dune exec evaluator/main.exe -- eval 'git commit -m "hello world"'
Enter: evaluate the entry   Ctrl-C: quit
entry> git commit -m "hello world"
✔ command  git : Git version control system
✔ feature  commit
    options
      --message : string option = "hello world"
entry> git commit -m "hello world"
```

When the output is not a terminal (in a script or a pipe), `browse` prints the list of the commands and their features, and `eval` evaluates the entry once and prints the result of every feature. The exit code of `eval` is then `0` when at least one feature matches the entry, `1` otherwise.

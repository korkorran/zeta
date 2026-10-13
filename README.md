# Open-Command

A format describing the syntax of terminal commands, and an algorithm `Zeta` to evaluate a terminal entry to a formatted representation of the entry. The project aims to be a general approach to parse terminal entries despites the variety of terminal commands syntax.

## Folder structure

```
- docs/ : markdown documentation of the project
- evaluator/ : a executable to browse commands written in the registry and to evaluate terminal entries (see 'Usage')
- format/ : the definition of the Open-Command format, describing a command syntax
- registry/ : a collection of described commands
- representation/ : the format describing the representation of an evaluated terminal entry
- test/ : test utility to check the algorithm against real terminal entries
- zeta/ : the algorithm evaluating a terminal entry into its formated representation
```

## Usage

The `evaluator` executable has two subcommands:

```sh
# list the commands of the registry, with their features
dune exec evaluator/main.exe browse

# evaluate a terminal entry against every feature of its command
dune exec evaluator/main.exe -- eval '<entry>'
```

Quote the entry (or put it after `--`) so that its options are not read as options of the evaluator. The exit code is `0` when at least one feature matches the entry, `1` otherwise.

Example:

```
$ dune exec evaluator/main.exe -- eval 'git commit -m "hello world"'
git commit -m "hello world"
  [fail] checkout : the entry does not match 'checkout'
  [fail] clone : the entry does not match 'clone'
  [ok]   commit
    options:
      --message = "hello world"
  [fail] create_branch : the entry does not match 'branch'
  ...
```

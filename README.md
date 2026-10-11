# Open-Command

A format describing the syntax of terminal commands, and an algorithm `Zeta` to evaluate a terminal entry to a formatted representation of the entry. The project aims to be a general approach to parse terminal entries despites the variety of terminal commands syntax.

## Folder structure

```
- format/ : the definition of the Open-Command format, describing a command syntax
- registry/ : a collection of described commands
- test/ : test utility to check the algorithm against real terminal entries
- zeta/ : the algorithm evaluating a terminal entry into its formated representation
```


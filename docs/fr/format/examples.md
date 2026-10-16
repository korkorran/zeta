# Exemples dans un langage de description

Les commandes du registre sont aujourd'hui décrites directement en OCaml (voir `registry/`). Cette page propose un pseudo-langage plus lisible, qui pourrait devenir le langage de description des commandes lu par Zeta. Les deux exemples ci-dessous décrivent exactement les fonctionnalités `compile` de `gcc` (`registry/gcc.ml`) et `move_or_rename` de `mv` (`registry/mv.ml`).

## `gcc` : la fonctionnalité `compile`

```
command gcc
  version 14.2.0
  description "GNU project C and C++ compiler"

  feature compile
    description "Compile and link source files"
    path gcc §options <sources>

    anchor §options (after-last-directive)
      option repeatable include_dir  -I<path>    "Add the directory to the list of directories searched for header files"
      option repeatable library_dir  -L<path>    "Add the directory to the list of directories searched for libraries"
      option repeatable library      -l<word>    "Link with the library"
      option output                  -o <path>   "Place the output in the file"

    argument sources  <path>+  "Source files to compile"
```

Cette fonctionnalité accepte par exemple `gcc -Iinclude src/main.c -Llib -lmaLib -o monprog`.

## `mv` : la fonctionnalité `move_or_rename`

```
command mv
  version 8.32
  description "Move or rename a file, a directory, or a symlink"

  feature move_or_rename
    description "Move or rename a file, a directory, or a symlink"
    path mv §options <source> <destination>

    anchor §options (exact-location)
      option force        -f  "Force move"
      option verbose      -v  "Verbose output"
      option interactive  -i  "Prompt before move"

    argument source       <word>+  "Source file/directory"
    argument destination  <word>   "Destination file/directory"
```

Cette fonctionnalité accepte par exemple `mv -v a.txt b.txt dir`.

## Lecture du langage

Une commande regroupe ses fonctionnalités. Dans une fonctionnalité :

- `path` reprend la notation de `FeaturePath` : les directives sont écrites telles quelles, `§nom` est une ancre et `<nom>` est un emplacement d'argument ;
- les options sont déclarées sous l'ancre à laquelle elles sont rattachées : c'est leur `location` ;
- chaque `argument` porte le nom utilisé dans le `path`.

### Valeurs (`TokenEncoding`)

| Notation | Signification | Type OCaml |
|---|---|---|
| `-v`, `-a \| --all` | token fixe (`FixedString`) | `unit tokenEncoding` |
| `<string>` | chaîne quelconque (`FormattedString None`) | `string tokenEncoding` |
| `<quoted>` | chaîne entre guillemets (`FormattedString (Some QuotedString)`) | `string tokenEncoding` |
| `<word>` | mot sans espace (`FormattedString (Some SingleWord)`) | `string tokenEncoding` |
| `<path>` | chemin de fichier (`FormattedString (Some FilePath)`) | `string tokenEncoding` |
| `<int>` | entier (`Integer`) | `int tokenEncoding` |
| `{ours \| theirs}` | valeur parmi une liste (`Enum`) | `string tokenEncoding` |
| `-I<path>` | préfixe collé à une valeur (`Prefixed`) | celui de la valeur |

### Options (`CommandOption`)

| Notation | Signification |
|---|---|
| `option name -v` | une occurrence en un seul token (`Once (SingleToken …)`) |
| `option name -o <path>` | une clé suivie d'une valeur dans le token suivant (`Once (ValueNextToken …)`) |
| `option repeatable name …` | option qui peut apparaître plusieurs fois (`Repeatable …`) |

Sans le mot-clé `repeatable`, une option est `Once` : si elle est répétée, la dernière occurrence l'emporte.

La différence entre `-I<path>` (préfixe collé, `SingleToken (Prefixed …)`) et `-o <path>` (valeur dans le token suivant, `ValueNextToken`) tient à l'espace entre la clé et la valeur.

### Arguments (`Argument`)

L'arité d'un argument s'écrit avec les quantificateurs des expressions régulières :

| Notation | Arité | Type de la valeur |
|---|---|---|
| `<word>` | exactement 1 (`Single`) | `'a` |
| `<word>?` | 0 ou 1 (`Optional`) | `'a option` |
| `<word>+` | au moins 1 (`Over (1, …)`) | `'a list` |
| `<word>*` | au moins 0 (`Over (0, …)`) | `'a list` |
| `<word>{2,}` | au moins 2 (`Over (2, …)`) | `'a list` |
| `<word>{1,3}` | entre 1 et 3 (`Between (1, 3, …)`) | `'a list` |

### Ancres (`OptionAnchor`)

Les contraintes d'une ancre sont écrites entre parenthèses après son nom : `(exact-location)`, `(after-last-directive)`, ou rien pour une ancre sans contrainte.

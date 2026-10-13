# Le module `TokenEncoding`

Le module `format/tokenEncoding.ml` décrit comment une valeur est écrite dans un token du terminal. Il sert à décrire les tokens des options et des arguments (voir [feature.md](feature.md)).

## Le type `tokenEncoding`

```ocaml
type 'a tokenEncoding =
  | FixedString : string list -> unit tokenEncoding
  | FormattedString : stringFormat option -> string tokenEncoding
  | Integer : int tokenEncoding
  | Enum : string list -> string tokenEncoding
```

C'est une GADT : le paramètre `'a` est le type OCaml de la valeur portée par le token.

- `FixedString ["-a"; "--all"]` : un token fixe, qui doit être l'une des chaînes données. Il ne porte aucune valeur, d'où le type `unit` ;
- `FormattedString fmt` : une chaîne de caractères, éventuellement contrainte par un `stringFormat` ;
- `Integer` : un entier ;
- `Enum ["a"; "b"]` : une chaîne qui doit faire partie de la liste donnée.

## Le type `stringFormat`

`stringFormat` précise la forme attendue d'une chaîne :

- `QuotedString` : une chaîne entre guillemets (`"Initial commit"`) ;
- `SingleWord` : un mot sans espace ;
- `FilePath` : un chemin de fichier, composé de caractères alphanumériques et de ponctuation.

`None` signifie que toute chaîne est acceptée. Le format se contente de nommer ces contraintes : leur vérification est faite par la bibliothèque `repr` (`Typeformat.validate`), au moment de valider une configuration.

La conversion entre un token et sa valeur est faite par la bibliothèque `repr` : `Typeformat.value_of_token` lit un token (et renvoie `None` s'il ne correspond pas à l'encodage), et `Typeformat.token_of_value` écrit une valeur sous forme de token.

## Exemple

L'option `-m` de `git commit` est un token fixe (`-m` ou `--message`) suivi d'un message entre guillemets :

```ocaml
ValueNextToken (TokenEncoding.FixedString ["-m"; "--message"], TokenEncoding.FormattedString (Some TokenEncoding.QuotedString))
```

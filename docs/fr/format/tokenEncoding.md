# Le module `TokenEncoding`

Le module `format/tokenEncoding.ml` décrit comment une valeur est écrite dans un token du terminal. Il sert à typer la valeur d'une option `ValueSuffix` et celle d'un argument (voir [feature.md](feature.md)).

## Le type `tokenEncoding`

```ocaml
type 'a tokenEncoding =
  | SimpleString : stringFormat option -> string tokenEncoding
  | Integer : int tokenEncoding
  | Enum : string list -> string tokenEncoding
```

C'est une GADT : le paramètre `'a` est le type OCaml de la valeur portée par le token.

- `SimpleString fmt` : une chaîne de caractères, éventuellement contrainte par un `stringFormat` ;
- `Integer` : un entier ;
- `Enum ["a"; "b"]` : une chaîne qui doit faire partie de la liste donnée.

## Le type `stringFormat`

`stringFormat` précise la forme attendue d'une chaîne :

- `QuotedString` : une chaîne entre guillemets (`"Initial commit"`) ;
- `SingleWord` : un mot sans espace ;
- `FilePath` : un chemin de fichier, composé de caractères alphanumériques et de ponctuation.

`None` signifie que toute chaîne est acceptée. Le format se contente de nommer ces contraintes : leur vérification est faite par la bibliothèque `repr` (`Typeformat.validate`), au moment de valider une configuration.

## `decodeToken` et `encodeToken`

Ces deux fonctions convertissent un token dans les deux sens, en suivant son encodage :

- `decodeToken enc s` transforme le token `s` en une valeur de type `'a` ;
- `encodeToken enc v` transforme la valeur `v` en token.

`decodeToken` ne vérifie pas que le token est valide : sur un `Integer`, un token qui n'est pas un entier lève une exception, et sur un `Enum`, la valeur n'est pas comparée à la liste. La lecture des tokens avec vérification est faite par la bibliothèque `repr` (`Typeformat.value_of_token`).

## Exemple

L'option `-m` de `git commit` attend un message entre guillemets :

```ocaml
ValueSuffix (["-m"; "--message"], TokenEncoding.SimpleString (Some TokenEncoding.QuotedString))
```

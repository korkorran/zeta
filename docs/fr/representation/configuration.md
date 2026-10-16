# Le module `Configuration`

Le module `representation/configuration.ml` (bibliothèque `repr`) décrit le résultat de l'évaluation d'une entrée par Zeta : la **configuration**, qui associe chaque option et chaque argument d'une fonctionnalité à sa valeur. Par exemple, l'entrée `gcc -Ia -Ib main.c -o prog`, évaluée avec la fonctionnalité `compile` de `gcc`, donne :

```
options : include_dir = ["a"; "b"]      output = Some "prog"
args    : sources = ["main.c"]
```

Le module décrit aussi le **template** d'une fonctionnalité, c'est-à-dire le type attendu de chacune de ses valeurs. Le template sert à valider une configuration, par exemple avant de reconstruire une entrée à partir de cette configuration (`zeta/reverse.ml`).

Les valeurs et leurs types sont définis dans le module `Typeformat` (voir [typeformat.md](typeformat.md)).

## Le type `configuration`

```ocaml
type configuration = {
  options : Typeformat.value StringMap.t;
  args : Typeformat.value StringMap.t
}
```

Les deux tables sont indexées par l'identifiant de l'option ou le nom de l'argument. Chaque valeur porte son typeformat : `Value (TList (TString None), ["a"; "b"])`.

## Le type `template`

```ocaml
type template = {
  optionsT: Typeformat.any StringMap.t;
  argsT: Typeformat.any StringMap.t;
}
```

Le template a la même forme que la configuration, mais il associe à chaque option et à chaque argument un typeformat au lieu d'une valeur. Le type d'une valeur découle de son encodage dans le format :

| Encodage | Typeformat |
|---|---|
| option `Once` | `TOption` du type de la valeur |
| option `Repeatable` | `TList` du type de la valeur |
| argument `Single` | type de la valeur |
| argument `Optional` | `TOption` du type de la valeur |
| argument `Over`, `Between` | `TList` du type de la valeur |

## Les fonctions principales

- **`getConfigurationTemplate feature`** calcule le template d'une fonctionnalité à partir des encodages de ses options et de ses arguments.
- **`getEmptyConfiguration template`** construit une configuration vide : chaque option et chaque argument reçoit la valeur par défaut de son type (`None`, `[]`, `""`, `0`…). Comme toutes les options ont un type `option` ou `list`, elles valent `None` ou `[]`.
- **`validateConfiguration config template`** vérifie qu'une configuration respecte un template. Elle renvoie la liste des erreurs de type `configurationError`, chacune indiquant :
  - s'il s'agit d'une option ou d'un argument (`Opt` ou `Arg`) ;
  - son nom ;
  - l'erreur : une valeur qui ne respecte pas son typeformat (`Format`), ou un élément absent du template (`ItemNotInTemplate`).

  `configurationError_to_string` met une erreur sous forme de texte.

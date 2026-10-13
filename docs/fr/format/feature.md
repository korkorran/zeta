# Le module `Feature`

Le module `format/feature.ml` décrit les fonctionnalités d'une commande. Une fonctionnalité (`feature`) correspond à une forme d'appel de la commande dans le terminal, par exemple `git commit -m "message"` ou `mv a.txt b.txt dir`.

## Le type `feature`

```ocaml
type feature = {
  path: FeaturePath.EncodedPath.t;
  options: CommandOption.any StringMap.t;
  anchors: OptionAnchor.t StringMap.t;
  args: Argument.any StringMap.t;
  description: string;
}
```

- `path` : la suite des tokens qui structure l'appel. Il y a trois sortes de tokens :
  - les directives (`Directive "commit"`), qui doivent apparaître telles quelles ;
  - les ancres (`Anchor "options"`, notées `§options`), qui marquent l'endroit où les options peuvent apparaître ;
  - les emplacements d'arguments (`ArgumentLocation "source"`, notés `<source>`).
- `options` : les options, indexées par leur identifiant.
- `anchors` : les contraintes de position de chaque ancre, indexées par le nom de l'ancre.
- `args` : les arguments, indexés par le nom utilisé dans les `ArgumentLocation` du `path`.
- `description` : une description lisible de la fonctionnalité.

## Les options : `CommandOption`

Une option est décrite par son encodage, une description et sa `location`, c'est-à-dire le nom de l'ancre à laquelle elle est rattachée.

Il existe deux encodages :

- `SingleToken encodage` : une option écrite en un seul token, décrit par un `TokenEncoding`. Par exemple, le drapeau `-a` / `--all` s'écrit `SingleToken (FixedString ["-a"; "--all"])` ;
- `ValueNextToken (clé, encodage)` : une option écrite en deux tokens, une clé fixe suivie d'une valeur. Par exemple, `-m "message"` s'écrit `ValueNextToken (FixedString ["-m"; "--message"], FormattedString (Some QuotedString))`.

## Les arguments : `Argument`

Un argument est décrit par son encodage et une description. L'encodage précise le type de la valeur (via un `TokenEncoding`) et l'arité de l'argument :

| Encodage | Nombre de valeurs | Type de la valeur |
|---|---|---|
| `Single enc` | exactement 1 | `'a` |
| `Optional enc` | 0 ou 1 | `'a option` |
| `Over (n, enc)` | au moins `n` | `'a list` |
| `Between (min, max, enc)` | entre `min` et `max` | `'a list` |

## Les ancres : `OptionAnchor`

Chaque ancre porte une liste de contraintes, qui précisent quand ses options sont acceptées :

- `ExactLocation` : les options ne sont acceptées qu'à la position exacte de l'ancre dans le `path` ;
- `AfterLastDirective` : les options sont acceptées dès que toutes les directives qui précèdent l'ancre ont été lues.

Une ancre sans contrainte (`[]`) n'impose aucune restriction de position.

## Les types `any`

Les encodages d'options et d'arguments sont des GADT : le paramètre `'a` indique le type de la valeur produite. Les options et les arguments d'une même fonctionnalité n'ont pas tous le même type, donc ils sont stockés dans les tables enveloppés dans `CommandOption.Any` ou `Argument.Any`.

## `featureMap` et `addFeature`

`featureMap` (`feature StringMap.t`) associe un identifiant à chaque fonctionnalité d'une commande. La fonction `addFeature id feature map` ajoute une fonctionnalité à cette table.

## Exemple

La fonctionnalité `move_or_rename` de `mv`, simplifiée (voir `registry/mv.ml`) :

```ocaml
Feature.addFeature "move_or_rename" {
  path = FeaturePath.Token.[Directive "mv"; Anchor "options"; ArgumentLocation "source"; ArgumentLocation "destination"];
  options = StringMap.empty
    |> StringMap.add "verbose" (CommandOption.Any { encoding = SingleToken (FixedString ["-v"]); description = "Verbose output"; location = "options" });
  anchors = StringMap.empty |> StringMap.add "options" [OptionAnchor.ExactLocation];
  args = StringMap.empty
    |> StringMap.add "source" (Argument.Any { encoding = Over (1, FormattedString (Some SingleWord)); description = "Source file/directory" })
    |> StringMap.add "destination" (Argument.Any { encoding = Single (FormattedString (Some SingleWord)); description = "Destination file/directory" });
  description = "Move or rename a file, a directory, or a symlink";
}
```

Cette fonctionnalité accepte `mv -v a.txt b.txt dir`. L'option `-v` doit se trouver juste après `mv`, puis viennent une ou plusieurs sources et enfin une destination.

# L'évaluation des tokens : `evalTokens`

La fonction `evalTokens` de `zeta/evalFeature.ml` fait avancer l'évaluation d'**une étape** : à partir d'un état (voir [state.md](state.md)), elle lit le prochain token de l'entrée et/ou de la commande et renvoie les états suivants.

```ocaml
val evalTokens : Feature.feature -> ongoingState -> string list -> ongoingState list
```

Le troisième paramètre est la liste des ancres applicables à cette étape (voir [introduction.md](introduction.md)).

## Plusieurs états suivants : le backtracking

`evalTokens` ne renvoie pas un état mais une **liste** d'états : tous les états suivants possibles. Une même entrée peut en effet être lue de plusieurs façons. Par exemple, avec `mv §options <source> <destination>`, le token `b.txt` de `mv a.txt b.txt dir` peut être une deuxième source, ou la destination. `evalTokens` renvoie les deux possibilités, et `evalFeature` poursuit l'évaluation de chacune.

Une liste vide ou un état d'échec abandonne une piste, sans empêcher les autres d'aboutir. C'est une forme de backtracking : l'algorithme explore toutes les lectures possibles de l'entrée, puis retient un succès s'il en existe un.

En pratique, seule l'évaluation des arguments produit plusieurs états (voir [argument.md](argument.md)). Les autres cas en produisent un seul.

## Le pattern matching sur `restEntry` et `restCommand`

`evalTokens` examine le prochain token de l'entrée (`he`) et celui de la commande (`hc`) :

```ocaml
match stacks.restEntry, stacks.restCommand with
```

| Entrée | Commande | Résultat |
|---|---|---|
| vide | vide | `Success` |
| vide | ancre `§x` | l'ancre est passée |
| vide | argument `<x>` | l'argument est fermé s'il peut rester vide, sinon `Incomplete` |
| vide | directive | `Incomplete` |
| `he` | vide | `he` est lu comme une option, sinon `Overflow he` |
| `he` | `hc` | voir ci-dessous |

Quand il reste des tokens des deux côtés, `evalTokens` essaie d'abord de lire une **option** des ancres applicables. Si une option correspond, elle est lue et c'est le seul état suivant : les options sont prioritaires. Sinon, le token de la commande décide :

- **directive** : si `he` est égal à la directive, les deux tokens sont lus ; sinon `Unmatched` ;
- **ancre** : l'ancre est passée, sans lire de token de l'entrée ;
- **emplacement d'argument** : `he` est évalué par `Argument.evalArgument`.

## La boucle d'exploration : `evalFeature`

`evalFeature` applique `evalTokens` à chaque état encore `Ongoing`, jusqu'à ce que tous les états soient terminés. Il renvoie le premier succès, ou à défaut l'échec le plus avancé.

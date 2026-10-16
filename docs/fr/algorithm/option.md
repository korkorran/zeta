# L'évaluation des options

Les options sont évaluées par le module `zeta/option.ml`. À chaque étape, `evalTokens` essaie de lire une option avant tout autre token (voir [tokens.md](tokens.md)).

## Les options candidates

Seules les options rattachées aux **ancres applicables** à cette étape sont essayées (voir [introduction.md](introduction.md)). `getOptionsFromAnchor` donne les options dont la `location` est une ancre donnée. `evalTokens` essaie ces options dans l'ordre, et retient **la première** qui correspond à l'entrée : la lecture d'une option ne crée pas d'alternative.

## La lecture d'une occurrence

`readOccurrence` lit une occurrence de l'option en tête de l'entrée, selon la façon dont elle s'écrit :

- **`SingleToken`** : un seul token, décodé selon son encodage. Par exemple `-v`, ou `-Iinclude` avec un préfixe ;
- **`ValueNextToken`** : deux tokens, une clé fixe puis une valeur. Par exemple `-o prog`.

Si l'entrée ne correspond pas, l'option n'est pas lue.

## L'enregistrement de la valeur

`evalOption` enregistre la valeur lue dans la configuration, selon le nombre d'occurrences permis :

- **`Once`** : la valeur devient `Some v`. Si l'option apparaît plusieurs fois, la dernière occurrence l'emporte ;
- **`Repeatable`** : la valeur est ajoutée à la liste des valeurs déjà lues (`appendValue`). Par exemple, `-Ia -Ib` donne `["a"; "b"]`.

`evalOption` renvoie l'état suivant (les tokens de l'option sont lus), ou `None` si l'entrée ne commence pas par cette option.

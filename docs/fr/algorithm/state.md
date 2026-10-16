# Le module `State`

Le module `zeta/state.ml` décrit l'état de l'évaluation d'une entrée par rapport à une fonctionnalité. L'évaluation avance en lisant à la fois les tokens de l'entrée et ceux du `path` de la fonctionnalité (la « commande »). L'état retient où en est chacune de ces lectures, la configuration calculée jusque-là, et si l'évaluation est terminée.

## Les piles de tokens

```ocaml
type tokenStacks = {
  evaluatedEntry: string list;
  restEntry: string list;
  evaluatedCommand: FeaturePath.Token.t list;
  restCommand: FeaturePath.Token.t list;
}
```

L'entrée et la commande sont chacune coupées en deux : ce qui a déjà été lu (`evaluated…`) et ce qui reste à lire (`rest…`). Une étape de l'évaluation fait passer des tokens du reste vers la partie lue.

Par exemple, pendant l'évaluation de `mv -v a.txt dir` avec le path `mv §options <source> <destination>`, après la lecture de `-v` :

```
evaluatedEntry   = ["mv"; "-v"]          restEntry   = ["a.txt"; "dir"]
evaluatedCommand = [mv]                  restCommand = [§options; <source>; <destination>]
```

## Le cas de l'évaluation

```ocaml
type ongoingCase =
  | Ongoing
  | Success
  | Unmatched of FeaturePath.Token.t
  | Incomplete of FeaturePath.Token.t
  | Overflow of string
```

- `Ongoing` : l'évaluation continue ;
- `Success` : l'entrée et la commande ont été entièrement lues ;
- `Unmatched t` : le token de l'entrée ne correspond pas au token `t` de la commande ;
- `Incomplete t` : l'entrée est terminée alors que la commande attend encore `t` ;
- `Overflow s` : la commande est terminée alors que l'entrée contient encore `s`.

## L'état

```ocaml
type ongoingState = {
  configuration : Configuration.configuration;
  tokenStacks : tokenStacks;
  case : ongoingCase;
}
```

Un état regroupe la configuration calculée jusque-là, les piles de tokens et le cas de l'évaluation.

## `compare_ongoingState`

Quand aucun état final n'est un succès, l'algorithme retient l'échec le plus pertinent. `compare_ongoingState` compare deux états d'abord par le nombre de tokens de l'entrée déjà lus, puis par leur cas.

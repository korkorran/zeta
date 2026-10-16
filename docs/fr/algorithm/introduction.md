# L'algorithme Zeta

Zeta évalue une entrée du terminal (par exemple `gcc -Ia main.c -o prog`) par rapport aux fonctionnalités d'une commande décrite dans le format Open-Command. Pour chaque fonctionnalité, il indique si l'entrée lui correspond et, si c'est le cas, calcule la **configuration** de l'entrée : la valeur de chaque option et de chaque argument (voir [la représentation](../representation/configuration.md)).

## Vue d'ensemble

1. **Découpage de l'entrée** (`zeta/entry.ml`) : `Entry.evalEntry` découpe l'entrée en tokens avec la bibliothèque `entry_tokenizer`, en respectant les chaînes entre guillemets. Puis il évalue l'entrée avec chaque fonctionnalité de la commande.
2. **Évaluation d'une fonctionnalité** (`zeta/evalFeature.ml`) : `evalFeature` part d'un état initial, où aucun token n'a encore été lu, puis fait avancer l'évaluation token par token avec `evalTokens`. Une étape peut produire plusieurs états suivants. L'algorithme les explore tous, jusqu'à ce qu'aucun état ne soit plus en cours.
3. **Choix du résultat** : parmi les états finaux, `evalFeature` retient le premier succès. S'il n'y en a pas, il retient l'échec le plus avancé (celui qui a lu le plus de tokens de l'entrée), pour expliquer pourquoi l'entrée ne correspond pas.

## Les modules

| Module | Rôle | Documentation |
|---|---|---|
| `State` | l'état de l'évaluation | [state.md](state.md) |
| `EvalFeature` | une étape de l'évaluation (`evalTokens`) et la boucle d'exploration (`evalFeature`) | [tokens.md](tokens.md) |
| `Argument` | l'évaluation d'un emplacement d'argument | [argument.md](argument.md) |
| `Option` | l'évaluation d'une option | [option.md](option.md) |
| `Anchor` | les ancres dont les options sont acceptées à une étape donnée | ci-dessous |
| `Entry` | le découpage de l'entrée et l'évaluation de toutes les fonctionnalités | ci-dessus |

## Les ancres applicables

À chaque étape, `Anchor.getApplicableAnchors` détermine les ancres dont les options peuvent être lues, d'après leurs contraintes :

- `ExactLocation` : l'ancre est le prochain token de la commande ;
- `AfterLastDirective` : toutes les directives qui précèdent l'ancre ont été lues.

Aucune option n'est acceptée avant que le premier token de la commande ait été lu.

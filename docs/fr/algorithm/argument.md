# L'évaluation des arguments

La fonction `evalArgument` de `zeta/argument.ml` évalue l'emplacement d'argument `<x>` qui se trouve en tête de la commande. Elle lit le prochain token de l'entrée selon l'encodage de l'argument (voir [feature.md](../format/feature.md)), et renvoie les états suivants possibles. C'est elle qui produit plusieurs états quand l'entrée peut être lue de plusieurs façons (voir [tokens.md](tokens.md)).

La valeur lue est décodée avec `Typeformat.value_of_token`, qui vérifie aussi le format du token (par exemple `FilePath`), puis elle est enregistrée dans la configuration.

## Selon l'encodage

- **`Single`** : le token est lu, puis l'argument est fermé. Si le token ne respecte pas l'encodage, on obtient `Unmatched`.
- **`Optional`** : deux états suivants. Dans le premier, le token est lu comme valeur de l'argument (`Some v`). Dans le second, l'argument est laissé vide (`None`) et le token sera lu par la suite de la commande.
- **`Over` et `Between`** (listes) : jusqu'à trois états suivants.
  - **laissé vide** : seulement si l'argument accepte zéro valeur et qu'aucune valeur n'a encore été lue ;
  - **valeur lue, liste fermée** : seulement si la liste contient alors assez de valeurs (au moins `min`) ;
  - **valeur lue, liste gardée ouverte** : seulement tant que la liste n'a pas atteint son maximum. L'emplacement d'argument reste alors en tête de la commande, et le prochain token pourra s'y ajouter.

## Quand l'entrée est terminée

Si l'entrée est terminée alors que la commande attend encore l'argument, celui-ci est fermé avec une valeur vide (`None` ou `[]`) s'il accepte zéro valeur et n'a encore rien reçu. Sinon, on obtient `Incomplete`.

Une liste gardée ouverte qui a déjà reçu des valeurs donne aussi `Incomplete` : l'état qui la ferme a déjà été produit au moment où sa dernière valeur a été lue.

# Aperçu du format

## Le module `Command`

Le module `format/command.ml` définit le type `command`, qui décrit une commande du terminal (`git`, `ls`, `tar`…). Une commande est composée de deux parties :

- `info` : les métadonnées de la commande, c'est-à-dire son nom (`commandName`), sa version (`version`) et une description (`description`) ;
- `features` : l'ensemble des fonctionnalités de la commande, de type `Feature.featureMap`.

Une commande est donc avant tout une collection de fonctionnalités. Chacune décrit une manière d'utiliser la commande depuis le terminal.

## Le type `Feature.featureMap`

`Feature.featureMap` associe un identifiant (par exemple `"commit"` ou `"create_branch"`) à une `feature`. Une `feature` décrit une forme d'appel de la commande :

- `path` : la suite des tokens attendus dans le terminal. Ce sont des directives (`git commit`), des ancres où peuvent se placer les options (`§options`) et des emplacements d'arguments (`<branch_name>`) ;
- `options` : les options disponibles, avec leur encodage (`-a`, `-m "message"`…) et l'ancre où elles peuvent apparaître ;
- `anchors` : les contraintes de position de chaque ancre d'options ;
- `args` : les arguments, avec leur encodage et leur arité (`Single`, `Optional`, `Over`, `Between`) ;
- `description` : une description de la fonctionnalité.

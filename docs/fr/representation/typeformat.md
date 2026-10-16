# Le module `Typeformat`

Le module `representation/typeformat.ml` (bibliothèque `repr`) décrit les valeurs que Zeta calcule à partir d'une entrée du terminal. Quand Zeta évalue `gcc -Ia -Ib main.c -o prog`, il produit une configuration qui associe chaque option et chaque argument à une valeur : `["a"; "b"]` pour `-I`, `Some "prog"` pour `-o`, `["main.c"]` pour les sources. `Typeformat` définit à la fois le type de ces valeurs et les valeurs elles-mêmes.

Ce module fait le lien entre les deux bibliothèques :

- le format (`ocf`) décrit comment une valeur est écrite dans un token (`TokenEncoding`) ;
- `Typeformat` décrit la valeur une fois lue, avec son type OCaml.

## Le type `typeformat`

```ocaml
type 'a typeformat = TUnit : unit typeformat
                | TInt : int typeformat
                | TFloat : float typeformat
                | TBool : bool typeformat
                | TString : TokenEncoding.stringFormat option -> string typeformat
                | TList : 'a typeformat -> 'a list typeformat
                | TOption : 'a typeformat -> 'a option typeformat
                | TPair : 'a typeformat * 'b typeformat -> ('a * 'b) typeformat
                | TEnum : string list -> string typeformat
```

C'est une GADT : un `'a typeformat` décrit les valeurs de type OCaml `'a`. Par exemple, `TList (TString None)` est un `string list typeformat`. `TString` et `TEnum` portent en plus les contraintes de la valeur : le format d'une chaîne, ou la liste des valeurs permises.

Pour ranger des typeformats de types différents dans une même table, par exemple les templates d'une configuration, on les enveloppe dans `any` :

```ocaml
type any = Any : 'a typeformat -> any
```

## Le type `value`

```ocaml
type value = Value : 'a typeformat * 'a -> value
```

Une valeur de configuration est une valeur OCaml accompagnée de son typeformat : `Value (TList (TString None), ["a"; "b"])`. Le type `'a` est caché dans `value`, ce qui permet de ranger dans une même configuration des options et des arguments de types différents. On retrouve le type d'une valeur avec son typeformat (voir `equal_type` ci-dessous).

## Les fonctions principales

- **Comparaison de types** : `equal_type tf1 tf2` vérifie que deux typeformats décrivent le même type OCaml. Elle renvoie alors une preuve d'égalité (`Type.eq`) qui permet de lire la valeur cachée dans un `value`. `equal_value` compare deux valeurs.
- **Validation** :
  - `validate_stringFormat` vérifie qu'une chaîne respecte un `stringFormat` ;
  - `validate tf v` vérifie qu'une valeur respecte les contraintes de son typeformat (format des chaînes, valeurs d'une énumération) ;
  - `validate_value` fait la même chose sur un `value` en vérifiant d'abord son type. Les erreurs sont du type `validationError`.
- **Lien avec le format** :
  - `typeformat_of_tokenEncoding` donne le typeformat de la valeur portée par un token ;
  - `value_of_token` lit un token (et renvoie `None` s'il ne respecte pas son encodage) ;
  - `token_of_value` écrit une valeur sous forme de token, ce qui sert à reconstruire une entrée à partir d'une configuration.
- **Affichage** : `pp` et `pp_value` affichent une valeur, `pp_typeformat` et `pp_any` affichent un typeformat.

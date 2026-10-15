# Références

Travaux proches du problème traité par Open-Command : l'analyse syntaxique des commandes de terminal.

## Analyse des arguments des utilitaires

- **NL2Bash** — Xi Victoria Lin, Chenglong Wang, Luke Zettlemoyer, Michael D. Ernst, *« NL2Bash: A Corpus and Semantic Parser for Natural Language Interface to the Linux Operating System »*, LREC 2018 ([ACL Anthology](https://aclanthology.org/L18-1491/), [code](https://github.com/TellinaTool/nl2bash)). Pour normaliser les commandes de leur corpus, les auteurs ont écrit un analyseur qui s'appuie sur une grammaire de chaque utilitaire, tirée des pages `man` : options, arguments, arité. Leur dépôt contient ces grammaires. C'est sans doute la référence la plus proche du format Open-Command.
- **NLC2CMD** — Mayank Agarwal, Tathagata Chakraborti, Quchen Fu, David Gros, Xi Victoria Lin, Jaron Maene, Kartik Talamadupula, Zhongwei Teng, Jules White, *« NeurIPS 2020 NLC2CMD Competition: Translating Natural Language to Bash Commands »*, Proceedings of the NeurIPS 2020 Competition and Demonstration Track, PMLR 133, 2021 ([PMLR](https://proceedings.mlr.press/v133/agarwal21b.html)). Elle reprend l'infrastructure de NL2Bash, et sa mesure de qualité compare les commandes au niveau de l'utilitaire, de ses options et de ses arguments.
- **PaSh** — Nikos Vasilakis, Konstantinos Kallas, Konstantinos Mamouras, Achilles Benetopoulos, Lazar Cvetković, *« PaSh: Light-touch Data-Parallel Shell Processing »*, EuroSys 2021 ([arXiv](https://arxiv.org/abs/2007.09436), [code](https://github.com/binpash/pash)). Il propose un langage d'annotation qui décrit, pour chaque utilitaire, ses options et le rôle de ses arguments (entrée, sortie…). Le but est la parallélisation, mais il s'agit bien d'une description déclarative de la syntaxe des commandes.
- **ABash** — Karl Mazurak, Steve Zdancewic, *« ABash: Finding Bugs in Bash Scripts »*, PLAS 2007 ([PDF](https://www.cis.upenn.edu/~stevez/papers/MZ07.pdf)). Cet analyseur statique utilise des signatures qui décrivent les commandes Unix externes pour analyser les arguments qui leur sont passés.

## Analyse du langage shell

Ces travaux portent sur le niveau du langage shell lui-même (pipes, redirections, développements) plutôt que sur les arguments d'un utilitaire.

- **Morbig** — Yann Régis-Gianas, Nicolas Jeannerod, Ralf Treinen, *« Morbig: A Static Parser for POSIX Shell »*, SLE 2018 ([page de la conférence](https://conf.researchr.org/details/sle-2018/papers/10/Morbig-A-Static-Parser-for-POSIX-Shell-)). C'est un analyseur écrit en OCaml, issu du projet **CoLiS**. Ce projet a aussi modélisé le comportement d'utilitaires Unix, options comprises, pour analyser les scripts des paquets Debian : Benedikt Becker, Nicolas Jeannerod, Claude Marché, Yann Régis-Gianas, Mihaela Sighireanu, Ralf Treinen, *« Analysing installation scenarios of Debian packages »*, TACAS 2020 ([Springer](https://link.springer.com/chapter/10.1007/978-3-030-45237-7_14)).
- **Smoosh** — Michael Greenberg, Austin J. Blatt, *« Executable Formal Semantics for the POSIX Shell »*, POPL 2020 ([arXiv](https://arxiv.org/abs/1907.05308)).

## Approches similaires hors de la recherche

- **Spécifications de complétion** : les specs de Fig (`withfig/autocomplete`, aujourd'hui Amazon Q), carapace, et les complétions zsh et bash. Elles décrivent options, arguments, arité et types pour des centaines de commandes. C'est probablement la plus grande base de descriptions comparable au `registry` d'Open-Command.
- **docopt** : déduit un analyseur de la syntaxe du texte d'aide d'un programme.

## Ce qui distingue Open-Command

À la connaissance de l'auteur de cette liste, aucun article ne traite l'ensemble des points suivants :

- un aller-retour typé : entrée → configuration → entrée (`zeta/reverse.ml`) ;
- un format de description indépendant de toute commande ;
- des ancres qui rendent explicites les ambiguïtés de position des options.

# Aprenda programação funcional

Este repósitório irá te ensinar programação funcional do zero com Haskell :) também falaremos um pouco de Prolog e Agda aqui.

# Como contribuir

Para contribuir com o projeto, você deve seguir as seguintes regras:

1. Sem comentários ou exemplos que possam ofender os demais
2. Uma explicação clara, direta e sem uso de muitos termos complexos
3. Um código menor e não complexo (aonde o fluxo do programa se interage de várias maneiras entre si criando abstrações) será priorizado
4. Caso você encontre alguma informação errada ou algum erro, faça o pull request explicando exatamente o problema
5. A explicação deve estar no idioma português. Sobre a escrita do código, tem que ser em inglês. E sobre comentários, evite fazê-los, apenas se forem exemplos muito complexos, caso contrário, tente explicar tudo no enunciado. Comentários devem estar em português.

obs: se você sabe bem matemática, eu ficaria bastante grato se você pudesse ajudar a criar um capítulo sobre matemática ;)

# Conteúdo

coisas que irão cair no curso:
* instalação
    * instalando o stack
    * configurando o stack
    * stack vs cabal
    * instalando o SWI-prolog
    * instalando o Emacs
        * Windows
        * Linux
    * configurando o Emacs e o MELPA
    * instalando Agda e o agda-stdlib
    * configurando o nosso .ghci + hoogle
* história da programação funcional e do lambda-calculus
    * LISP
    * ML
    * evolução do lambda-calculus
* características funcionais
    * dados imutáveis
    * transparência referencial
    * sem nulos e exceções
    * toda função retorna algo
    * first class functions
    * sem globais
    * pureza
    * side effects
* lambda-calculus
    * funções simples
    * números
    * lógica booleana
* programação lógica
    * o que é programação lógica?
    * a linguagem prolog
    * predicados
    * modus ponens
    * backtracking
    * resolução SLD
    * cut, negação e a resolução SLDNF
* programação funcional no geral
    * morfismo
    * polimorfismo
    * função id
    * isomorfismo
    * pattern matching
    * composição
    * lifting
    * curry e point free / programação tácita
    * Higher Order Functions (HOF) & closures
    * recursão
    * tail call recursion & tail call optimization
    * total functions & partial functions
    * list comprehension
    * map
    * filter
    * reduce
    * fold / reduce
    * zip
    * continuation passing style (CPS)
    * tipos em haskell
* introdução a teoria das categorias
    * o que é uma categoria?
    * endomorfismo
    * idempotência
    * monomorfismo
    * o que são domínios e codomínios?
    * setóide
    * semigrupo
    * bijeção
    * injeção
    * surjeção
    * o que é uma operação binária?
    * o que são funtores?
    * o que são endofuntores?
    * o que são monóides?
    * o que são applicative functors?
    * o que são arrows?
    * o que são monads?
    * o prefixo co
    * comonads
    * transformações naturais
    * produtos e coprodutos
* lazy programming
    * o que é laziness?
    * o que é strictness?
    * bang patterns
    * irrefutable patterns
    * o que são thunks?
    * WHNF
    * day's plot twist: laziness são impuras e seq te permite ter efeitos observáveis
* quantificação
    * quantificação universal
    * quantificação existencial
    * rankNTypes
    * Hindley-Milner
    * System F
    * System Fω
    * System FC
    * ScopedTypeVariables
    * tipos impredicativos
* type-level programming
    * typeclasses
    * subtipagem
    * variância
    * tipos de dados abstratos (ADTs) / sum types, nullary e unários
    * Higher Kinded Types (HKTs)
    * sinônimos de tipos
    * phantom types
    * Void / bottom types
    * o que é unsoundness?
    * o que é um sistema de tipos decidível?
    * o problema da parada
    * o que turing-complete tem a ver com o problema da parada?
    * absurd
    * bottom values
    * tipos de dados algébricos generalizados (GADTs)
    * type families e data families
    * o que é type-level programming?
    * closed type families
    * o que são promotions?
    * HLists
* coisas específicas de Haskell
    * boolean blindness
    * traversable
    * TypeApplications
    * FlexibleInstances
    * FlexibleContexts
    * OverloadedStrings
    * OverloadedLists
    * Text e ByteString
    * free monads
    * funções de ponto fixo
    * design by contract
* recursion schemes
    * catamorfismo
    * anamorfismo
    * hilomorfismo
    * apomorfismo
    * paramorfismo
    * homomorfismo
* lenses
* tipos dependentes
    * o que são tipos dependentes?
    * universos
    * refinamento de tipos
    * singleton types
* livros recomendados
    * what I wish I knew when learning Haskell
    * haskell programming from first principles
    * category theory for programmers
    * SICP
    * CTMCP
    * learn prolog now
    * programming languages foundations in Agda
    * types and programming languages
    * HtDP
    * HoTT
    * purely functional data structures

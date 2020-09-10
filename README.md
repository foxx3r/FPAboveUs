# Aprenda programação funcional

Este repósitório irá te ensinar programação funcional do zero com Haskell :) também falaremos um pouco de Prolog e Agda aqui.

# Como contribuir

Para contribuir com o projeto, você deve seguir as seguintes regras:

1. Sem comentários ou exemplos que possam ofender os demais
2. Uma explicação clara, direta e sem uso de muitos termos complexos.
3. Um código menor e não complexo (aonde o fluxo do programa se interage de várias maneiras entre si criando abstrações) será priorizado.
4. Caso você encontre alguma informação errada ou algum erro, faça o pull request explicando exatamente o problema.
5. A explicação deve estar no idioma português. Sobre a escrita do código, tem que ser em inglês. E sobre comentários, evite fazê-los, apenas se forem exemplos muito complexos, caso contrário, tente explicar tudo no enunciado. Comentários devem estar em português.
6. Todo novo tópico deve ser incluído no prefácio, e todo novo header deve conter um id específicado manualmente.
7. Apenas abrevie id se é normal abreviar a frase. E fique atento para não usar um id mais já utilizado.
8. Os nomes dos ids não devem conter caracteres especiais a menos que seja o `-`, e todas as letras devem ser em maiúsculas.
9. Você deve explicitar o motivo do pull request, caso contrário, o PR pode não ser aceito (caso você mude muitas coisas, você pode colocar algo como "alguns erros fixados").
10. Se for modificar as regras, faça-o em um PR separado.
11. O commit deve estar em inglês.

obs: se você sabe bem matemática, eu ficaria bastante grato se você pudesse ajudar a criar um capítulo sobre matemática ;)

# Conteúdo

coisas que irão cair no curso:
* [instalação](#instalação)
    * [instalando o stack](#instalando-o-stack)
    * [configurando o stack](#configurando-o-stack)
    * [stack vs cabal](#stack-vs-cabal)
    * [instalando o SWI-prolog](#instalando-o-swi-prolog)
    * [instalando o Emacs](#instalando-o-emacs)
        * [Emacs no Windows](#emacs-no-windows)
        * [Emacs em Unix-like](#emacs-em-unix-like)
    * [configurando o Emacs](#configurando-o-emacs)
    * [instalando Agda e o agda-stdlib](#instalando-agda-e-o-agda-stdlib)
    * [configurando o nosso .ghci + hoogle](#configurando-o-nosso-ghci-e-hoogle)
* [história da programação funcional e do lambda-calculus](#história-da-programação-funcional-e-do-lambda-calculus)
    * [LISP](#lisp)
    * [ML](#ml)
    * [evolução do lambda-calculus](#evolução-do-lambda-calculus)
* [características funcionais](#características-funcionais)
    * [dados imutáveis](#dados-imutáveis)
    * [transparência referencial](#transparência-referencial)
    * [sem nulos e exceções](#sem-nulos-e-exceções)
    * [funções tem tipos únicos](#funções-tem-tipos-únicos)
    * [toda função retorna algo](#toda-função-retorna-algo)
    * [first class functions](#first-class-Functions)
    * [sem globais](#sem-globais)
    * [side effects](#side-effects)
    * [pureza](#pureza)
* [lambda-calculus](#lambda-calculus)
    * [funções simples](#funções-simples)
    * [números em lambda-calculus](#números-em-lambda-calculus)
    * [lógica booleana em lambda-calculus](#lógica-booleana-em-lambda-calculus)
* [programação lógica](#programacao-logica)
    * [a linguagem prolog](#a-linguagem-prolog)
    * [o paradigma de programação lógico](#o-paradigma-de-programação-lógico)
    * [o paradigma de programação declarativo](#o-paradigma-de-programação-declarativo)
    * [predicados](#predicados)
    * [modus ponens](#modus-ponens)
    * [backtracking](#backtracking)
    * [resolução SLD](#resolução-sld)
    * [cut, negação e a resolução SLDNF](#cut-negação-e-resolução-sldnf)
* [programação funcional no geral](#programação-funcional-no-geral)
    * [morfismo](#morfismo)
    * [polimorfismo](#polimorfismo)
    * [função id](#função-id)
    * [isomorfismo](#isomorfismo)
    * [pattern matching](#pattern-matching)
    * [composição](#composição)
    * [lifting](#lifting)
    * [constraints](#constraints)
    * [declarativismo](#declarativismo)
    * [curry e point-free](#curry-e-point-free)
    * [Higher Order Functions e closures](#higher-order-functions-e-closures)
    * [recursão](#recursão)
    * [tail call recursion e tail call optimization](#tail-call-recursion-e-tail-call-optimization)
    * [total functions e partial functions](#total-functions-e-partial-functions)
    * [list comprehension](#list-comprehension)
    * [fmap](#fmap)
    * [filter](#filter)
    * [fold ou reduce](#fold-ou-reduce)
    * [zip](#zip)
    * [continuation passing style](#continuation-passing-style)
    * [tipos em haskell](#tipos-em-haskell)
* [introdução a teoria das categorias](#introdução-a-teoria-das-categorias)
    * [o que é uma categoria?](#o-que-é-uma-categoria)
    * [endomorfismo](#endomorfismo)
    * [idempotência](#idempotencia)
    * [monomorfismo](#monomorfismo)
    * [o que são domínios e codomínios?](#o-que-sao-dominios-e-codominios)
    * [setóide](#setóide)
    * [semigrupo](#semigrupo)
    * [bijeção](#bijeção)
    * [injeção](#injeção)
    * [surjeção](#surjeção)
    * [o que é uma operação binária?](#o-que-e-uma-operacao-binaria)
    * [o que são funtores?](#o-que-sao-funtores)
    * [o que são endofuntores?](#o-que-sao-endofuntores)
    * [o que são monóides?](#o-que-sao-monoides)
    * [o que são applicative functors?](#o-que-sao-applicative-functors)
    * [o que são arrows?](#o-que-sao-arrows)
    * [o que são monads?](#o-que-sao-monads)
    * [o prefixo co](#o-prefixo-co)
    * [comonads](#comonads)
    * [transformações naturais](#transformacoes-naturais)
    * [produtos e coprodutos](#produtos-e-coprodutos)
* [lazy programming](#lazy-programming)
    * [o que é laziness?](#o-que-e-laziness)
    * [o que é strictness?](#o-que-e-strictness)
    * [bang patterns](#bang-patterns)
    * [irrefutable patterns](#irrefutable-patterns)
    * [o que são thunks?](#o-que-sao-thunks)
    * [WHNF](#whnf)
    * [day's plot twist: laziness são impuras e seq te permite ter efeitos observáveis](#plor-twist)
* [quantificação](#quantificacao)
    * [quantificação universal](#quantificacao-universal)
    * [quantificação existencial](#quantificacao-existencial)
    * [rankNTypes](#rankntypes)
    * [Hindley-Milner](#hindley-milner)
    * [System F](#system-f)
    * [System Fω](#system-f-omega)
    * [System FC](#system-fc)
    * [ScopedTypeVariables](#scoped-type-variables)
    * [tipos impredicativos](#tipos-impredicativos)
* [type-level programming](#type-level-programming)
    * [typeclasses](#typeclasses)
    * [subtipagem](#subtipagem)
    * [variância](#variancia)
    * [tipos de dados abstratos (ADTs) / sum types, nullary e unários](#adts)
    * [Higher Kinded Types (HKTs)](#hkts)
    * [sinônimos de tipos](#sinonimos-de-tipos)
    * [phantom types](#phantom-types)
    * [type roles](#type-roles)
    * [Void / bottom types](#bottom-types)
    * [o que é unsoundness?](#o-que-e-unsoundness)
    * [o que é um sistema de tipos decidível?](#o-que-e-sistema-indecidivel)
    * [o problema da parada](#o-problema-da-parada)
    * [o que turing-complete tem a ver com o problema da parada?](#turing-complete)
    * [absurd](#absurd)
    * [bottom values](#bottom-values)
    * [tipos de dados algébricos generalizados (GADTs)](#gadts)
    * [type families e data families](#type-families)
    * [o que é type-level programming?](#o-que-e-type-level-programming)
    * [closed type families](#closed-type-families)
    * [o que são promotions?](#o-que-sao-promotions)
    * [HLists](#hlists)
* [coisas específicas de Haskell](#coisas-especificas-de-haskell)
    * [boolean blindness](#boolean-blindness)
    * [traversable](#traversable)
    * [TypeApplications](#typeapplications)
    * [FlexibleInstances](#flexibleinstances)
    * [FlexibleContexts](#flexiblecontexts)
    * [OverloadedStrings](#overloadedstrings)
    * [OverloadedLists](#overloadedlists)
    * [Text e ByteString](#text-e-bytestring)
    * [free monads](#free-monads)
    * [funções de ponto fixo](#funcoes-de-ponto-fixo)
    * [design by contract](#design-by-contract)
* [recursion schemes](#recursion-schemes)
    * [catamorfismo](#catamorfismo)
    * [anamorfismo](#anamorfismo)
    * [hilomorfismo](#hilomorfismo)
    * [apomorfismo](#apomorfismo)
    * [paramorfismo](#paramorfismo)
    * [homomorfismo](#homomorfismo)
* [lenses](#lenses)
* [tipos dependentes](#tipos-dependentes)
    * [o que são tipos dependentes?](#o-que-sao-tipos-dependentes)
    * [universos](#universos)
    * [refinamento de tipos](#refinamento-de-tipos)
    * [singleton types](#singleton-types)
* [livros recomendados](#livros-recomendados)
    * [what I wish I knew when learning Haskell](#wiwikwlh)
    * [haskell programming from first principles](#haskell-book)
    * [category theory for programmers](#category-theory-for-programmers)
    * [SICP](#sicp)
    * [CTMCP](#ctmcp)
    * [learn prolog now](#lpn)
    * [programming languages foundations in Agda](#plfa)
    * [types and programming languages](#types-and-programming-languages)
    * [HtDP](#htdp)
    * [HoTT](#hott)
    * [purely functional data structures](#purely-functional-data-structures)

## instalação

### instalando o stack

O stack vai ser o seu segundo melhor amigo daqui pra frente, atrás apenas do GHC. Ele è um gerenciador de pacotes e resolve muitos problemas por você. Para instalá-lo, acesse <https://docs.haskellstack.org/en/stable/README/> e siga o manual de instalação para o seu sistema operacional. Como nem tudo nesta vida é fácil, recomendo você ler a documentação do stack em <https://docs.haskellstack.org/en/stable/GUIDE/> para não ter problemas futuros, e caso os tenha, que saiba resolver. E digo isso por experiência própria :)

### configurando o stack

Para configurar o stack, você primeiro deve rodar o comando:

`$ stack setup`

A partir daí, vai demorar um pouco até que a instalação seja concluída. Ele vai instalar todas as ferramentas necessárias para o nosso ambiente Haskell e você pode conferí-las (e no futuro quem sabe poder apagá-las) no diretório `$HOME/.stack`. Caso você por alguma razão queira mudar a versão do GHC, consulte a [questão no stack overflow](https://stackoverflow.com/questions/44346435/change-ghci-version-on-stack) que se trata sobre isso. Caso alguma das libs que formos usar não esteja incluída junto do GHC, tente instalar elas pelo nome, e.g: Control.Comonad seria um `stack (ou cabal) install comonad`. Para as outras libs, procure na internet como instalá-las.

Agora, vamos instalar o cabal. Basicamente, como a Wikipédia diz:

> O Cabal foi introduzido para simplificar o empacotamento de software e módulos Haskell. Ele foi adicionado ao Glasgow Haskell Compiler versão 6.4 como gerenciador de pacotes padrão, junto com o gerenciador interno ghc-pkg do GHC. O binário real cabal e a biblioteca Cabal são desenvolvidas em pacotes diferentes.

Fonte: [Wikipedia-EN](https://en.m.wikipedia.org/wiki/Cabal_(software))

Para instalar o cabal, digite o seguinte comando no seu terminal:

`$ stack install Cabal cabal-install`

Iremos discutir no capítulo a seguir as diferenças dele pro stack.

### stack vs cabal

Basicamente o stack usa o cabal por baixo, mas usa o stackage como repositório ao invés do hackage como o cabal. E pelo stack usar o cabal por baixo, você não tem perda de compatibilidade. Mas é apenas isso?! Não!! O stack é um cabal melhorado ou mais automatizado. Basicamente o stack evita de você ter as cabal hells, uma dor de cabeça imensa para programadores Haskell no passado! Apesar do cabal ter evoluído bastante nos últimos tempos com o `cabal sandbox` e os comandos new-*, iremos usar o stack neste tutorial. Mas fica a sua escolha.

### instalando o SWI-prolog

Iremos ensinar um pouco sobre a linguagem prolog aqui, e já é bom ter de antemão, o interpretador instalado. Para instalá-lo, confira no site oficial e instale de acordo com o seu sistema operacional (ou você pode pesquisar no gerenciador de pacotes da sua distribuição, mas não é uma coisa que geralmente se recomenda) em <https://www.swi-prolog.org/Download.html>.

### instalando o Emacs

Basicamente o Emacs é um editor de texto muito poderoso (podendo acessar o telegram, músicas, servir como daemon init do sistema, ser usado para fazer programação literária e muito mais), que usa a linguagem Elisp, uma DSL parecida com Common LISP. Caso você esteja acostumado com o VI/VIM/neovim, não se preocupe, ensinaremos a instalar o evil depois, que irá nos permitir usar keybindings (combinações de teclas) iguais as do VI no Emacs. Mas até mesmo usando as keybindings do VI, acho que é importante saber Emacs, portanto, eu recomendo você ler o [tour sob o Emacs](https://www.gnu.org/software/emacs/tour).

Mas por que usar o Emacs? Já que não importa a maneira como seu código será escrito, não é? Mas infelizmente (ou felizmente para você, leitor que está descobrindo novas coisas) a linguagem de programação Agda é muito dependente do Emacs.

#### Emacs no Windows

Para instalar o Emacs no seu sistema operacional Windows, acesse a [página de download](https://ftp.gnu.org/gnu/emacs/windows/) do projeto GNU e escolha a melhor opção para o seu sistema.

#### Emacs em Unix-like

Para instalar o Emacs em um sistema unix-like, você pode instalar ele pelo gerenciador de pacotes mesmo que não tem problema. Mas caso não confie no gerenciador de pacotes da sua distribuição, instale pelo [site oficial](https://www.gnu.org/software/emacs/download.html).

### configurando o Emacs

Basicamente, o Emacs tem 3 implementações de gerenciamento de pacotes:

- [GNU ELPA](https://elpa.gnu.org/) - talvez o nais popular de todos, e bem pequeno, mantido pelo projeto GNU.
- [MELPA](https://melpa.org) - é um repositório não oficial e também é o repósitório com a maior quantidade de pacotes.
    - [MELPA stable](https://stable.melpa.org) - é um MELPA que inclui apenas paco5es estáveis. Ele è o que tem menos pacotes de todos.
- [Marmalade](https://marmalade-repo.org) - o marmalade è um projeto já morto, e não faz sentido de usarmos aqui. Apesar dele ter sido extensivamente utilizado no passado.

Enfim... Bora parar de falar. Basicamente, o Emacs tem os arquivos de configuração localizados em `$HOME/.emacs.d/init.el` (ambos Windows e Linux) ou em `$HOME/.emacs`. Muita gente usa o `.emacs`, mas o correto seria usar o `.emacs.d` pois seus arquivos de configuração ficam mais organizados. Após instalar o Emacs, insira isso dentro do `$HOME/.emacs.d/init.el`:

```el
(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)
```

Isso irá habilitar o repositório MELPA. Agora, é hora de usar o Emacs... Por causa do Emacs demorar muito para ser iniciado, muita gente prefere usar o emacsclient porque ele deixa o processo rodando com os arquivos já carregados, e isso fez meu Emacs sair de 15 segundos de inicialização para 2 segundos. Para isso, rode o comando:

`$ emacs --daemon`

Após isso, ele carregará todos os processos. A partir daí, não use mais emacs, e sim emacsclient, exemplo:

`$ emacsclient foo.hs`

Após isso, entre em qualquer arquivo com o emacs e tecle `M-x`, aonde a tecla M (meta) é o Alt do seu teclado. Agora, iremos atualizar o repositório digitando `package-refresh-contents` após digitar `M-x` (obs: se você ver o símbolo RET, ele se refere ao enter/return key). Isso vai atualizar a lista de pacotes do repósitório. Agora, rode `M-x package-install RET helm`, basicamente o helm é uma interface pro `M-x` mais moderna e intuitiva. Para configurá-lo, adicione estas linhas no seu arquivo de configuração:

```el
(require 'helm-config)

(use-package lsp-ui)

(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)

(helm-mode 1)
```

Isto irá habilitar o helm. Para sair do Emacs, basta digitar `C-x C-c` aonde C è o control. Agora, vamos instalar o evil da mesma forma que instalamos o helm: `M-x package-install RET evil RET`. Após isso, adicione as seguintes linhas de configuração:

```el
(require 'evil)
(evil-mode 1)
```

E por último, mas não menos importante, vamos habilitar a contagem de linhas e o auto-complete do Emacs adicionando as seguintes linhas de configurações:

```el
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

(add-hook 'emacs-startup-hook
          (lambda ()
            (auto-complete-mode t)))
```

No futuro, iremos ensinar a como configurar mais ainda o Emacs :)

### instalando Agda e o agda-stdlib

Agda é uma linguagem de provação de teoremas dependentemente tipada que iremos discutir no último capítulo do curso, e portanto, iremos instalar agora, com os seguintes comandos:

```
$ cabal update
$ cabal install Agda
```

Ou caso você queira mais informações sobre a instalação, você pode conferir no [site oficial](https://agda.readthedocs.io/en/v2.6.1/getting-started/installation.html).

Depois de ter instalado Agda, rode o seguinte comando:

`$ agda-mode setup`

Lembra que eu disse que Agda é muito dependente do Emacs? O agda-mode setup irá configurar o agda-mode no Emacs, para que possamos digitar unicodes e poder interpretar Agda pelo Emacs.

### configurando o nosso .ghci e hoogle

Antes de tudo, vamos configurar o interpretador do Haskell. O arquivo de configuração fica em `$HOME/.ghci`, no qual aceita código válido dentro do GHCi. Caso você ainda não saiba, comentários de uma única linha em Haskell são feitos usando `--` e comentários de múltiplas linhas são feitos usando `{- -}`. Vamos colocar isso no arquivo de configuração do GHCi:

```hs
:set prompt "λ " -- muda o tipo do prompt
:set prompt-cont "∈ " -- muda o tipo do prompt multi-linha
:set +m -- te permite ter multi-linha com blocos do e case
:set +t -- sempre retorna o tipo de uma expressäo
```

Agora que configuramos coisas básicas, vamos instalar o hoogle. Basicamente o hoogle ê o google do Haskell, com ele, você pode pesquisar o tipo de uma função e ele te devolverá funções com o mesmo tipo. E ele também procura por nomes de funções e de qul biblioteca ela vem. Para instalá-lo, rode:

`$ cabal install hoogle`

Após isso, temos que gerar as databases do hoogle (que pode demorar um pouquinho, e consumir um bom espaço no seu HD):

`$ hoogle generate`

Após isso, inclua as seguintes configurações ao seu `.ghci`:

```hs
:def hoogle \x -> return $ ":!hoogle \"" ++ x ++ "\""
:def doc \x -> return $ ":!hoogle --info \"" ++ x ++ "\""
```

Agora, para testar, rode:

`$ stack exec ghci`

E então, rode:

`λ :hoogle "a -> a"`

## história da programação funcional e do lambda calculus

A programação funcional nasceu como um modelo matemático arquitetado por Alan Church, o lambda-calculus. O Church era professor do Alan Turing, que inventou o modelo concorrente: o modelo de turing. E basicamente o modelo de turing foi o mais adotado com o passar dos anos, e o lambda-calculus se tornou mais acessível para pessoas acadêmicas. Mas o cenário mudou um pouco quando Haskell conseguiu resolver o maior problema da programação funcional: I/O. Após isso, várias linguagens funcionais também cresceram, como Clojure, Elixir, Scala, os LISPs voltaram a vida e Scheme também embarcou nessa... F# e OCaml foram criados, Rust mais recentemente... E linguagens imperativas agora estão adotando cada vez mais features funcionais

Mas por quê? O que há de tão especial com funcional? Basicamente, funcional é mais seguro porque é matematicamente consistente (porém, não necessariamente você precisa saber matemática para aprender), e você consegue codificar problemas em funções, eliminando a necessidade de design patterns, além de que você tem mais ergonomia. Além de tudo, é meio difícil e controverso classificar linguagens de programação funcionais...

Aliás, várias coisas nasceram de funcional e você nem deve saber, como sistema de tipos por exemplo, que nasceram como provadores de propriedades do seu código para provar teoremas matemáticos.

### LISP

Aiai... LISP, o que eu posso dizer desta `((((((((maravilhosa))))))))` linguagem? Basicamente, LISP foi a segunda linguagem criada no mundo, a primeira linguagem a ter suporte a UTF-8, a primeira linguagem interpretada, a primeira linguagem a ter um GC, a primeira linguagem homoicônica, a primeira linguagem com computação simbólica, a primeira linguagem a ter if, a primeira linguagem a ter meta-programação, a primeira linguagem funcional, a primeira linguagem reflexiva, a pioneira em linguística e inteligência artificial, sem falar das LISPs machines, além de seu inventor ter inventado o time sharing, impulsionou a criação de DSLs (linguagens de domínio específico), pioneira em recursão, estruturas de dados, self-hosting compiler, tipagem dinâmica... E não para por aí. Se eu quisesse, faria um artigo inteiro sobre LISP e o John McCarthy.

LISP não é só importante por ter sido a primeira linguagem funcional, mas ela é um elemento importante para a próxima linguagem que iremos falar adiante: ML, a mãe de Haskell e da programação funcional moderna.

### ML

Não, não é Machine Learning desta vez... ML significa MetaLanguage, e nasceu lá na década de 70, quando Robin Milner queria dar tipagem ao LISP, mas não conseguia fazer isso de forma matematicamente consistente. Até que ele olhou para uma idéia promissora na época: a teoria dos tipos.

E com isso, nasceu a linguagem ML, que antes era mais uma DSL para um provador de teoremas do que uma linguagem. Por anos, ML foi apenas nicho e não servia para o mundo real... Até que alguém muito perspicaz pegou a ML e transformou-a em uma linguagem multi-propósito, que permite vários outros paradigmas. E assim, até hoje ML é conhecida por ser principalmente funcional, mas também, **multi-propósito**.

E desde então, ML não é só mais uma linguagem, é uma família de linguagens... comppsta principalmente por Standard ML, OCaml, F#, F*, Haskell e Rust.

Espera, Haskell e Rust? Como o assunto aqui não é Rust, pesquisem na internet sobre. Mas sobre Haskell, há controvérsias... Assim como Rust. E eu creio firmemente que Haskell seja uma ML, apesar de ter certas dúvidas em relação a Rust. Mas por que eu creio que Haskell seja uma ML? Porque Haskell surgiu como um lazy ML, e o argumento que muita gente contrária diz "Haskell foi uma junção de várias linguagens na época, incluindo algumas MLs, mas o seu maior inspirador foi o Miranda..No final, foi um processo inevitável, eram MLs, mas não quer dizer que Haskell também seja, porque existiam não-MLs" não vale nada, já que Haskell queria ser um lazy ML, e também, muitas coisas em Haskell poderiam "melhorar", mas eles queriam que a linguagem fosse mais parecida com ML.

### evolução do lambda-calculus

Alonzo Church cria o lambda-calculus na década 30;

Haskell Curry cria o currying e permite múltiplos argumentos no lambda-calculus;

O lambda-calculus tipado é inventado;

Howard cria o isomorfismo de Curry-Howard e relaciona provas matemáticas com programação;

R. Hindley e D. Milner criam o sistema de tipos Hindley-Milner e permitem lambda-calculus parametricamente tipado, e ainda criam a linguagem ML que mais tarde viria a se tornar uma família;

John C. Reynolds cria o system F, uma extensão do Hindley-Milner, e ainda cria o system F ômega, uma extensão do system F que permitiam ter construtores e type families;

em 1994, Augustsson e Petersson criam GADTs (estruturas de dados álgebricos generalizadas) no ALF. Cheney & Hinze levaram GADTs a ML e Haskell em 2003, mas só que Haskell se saiu melhor porque eles tiveram que inventar o system FC, e o Hindley-Milner era fraco demais para permitir compatibilidade com o system FC (como todos sabem, as MLs são famosas por serem Hindley-Milner);

E em 1991, o Henk desenhou o lambda cube... Assim, chegando a última evolução do lambda-calculus atualmente: O lambda-pi-calculus... Que deveria se chamar apenas pi-calculus (sim, se tornou incompatível com o lambda-calculus) mas já existia uma formalização com este nome;

## características funcionais

Agora, chegou a hora em que vamos realmente ter contato com o Haskell. No futuro, iremos ensinar IO e, consequentemente, a como compilar um arquivo Haskell. Por enquanto, abra o interpretador com:

`$ stack exec ghci`

Agora, se você quiser digitar múltiplas linhas dentro do GHCi (como por exemplo, tipps), digite `:{` para inserir múltiplas linhas e `:}` para terminar.

Ou então se você preferir (e é o que eu recomendo), abra um arquivo e digite as funções nele, e então chame `:l meu_arquivo.hs` no ghci e ele vai importar todas as funções deste arquivo e você poderá rodá-las, e para recompilar o arquivo, basta digitar `:r`

### dados imutáveis

Basicamente, linguagens funcionais te previnem de ter efeitos colaterais, e eles são causados principalmente por mutabilidade. Em uma linguagem como Python, o seguinte seria possível:

```py
x = 1
x = x + 1
```

E x viraria 2. Agora, preste atenção: Haskell permite redefinição de variável (também chamada de constante polimórfica), mas não é mutabilidade. Um exemplo simples:

```hs
x = 1
x = 2
```

Neste pequeno exemplo, a gente só redefiniu a constante polimórfica. Se a gente fizesse isso por exemplo nos tipos, um tipo que se referiria ao seu tipo que foi redefinido, teria um tipo parecido com `Ghci2.MeuTipo`, ou seja, no final, você apenas invalida aquilo, e só o programa que teve acesso ao antigo valor, vai ter acesso a ele no futuro. Agora um outro exemplo em Haskell:

```hs
x = 1
x = x + 1
```

Este código funciona, mas ê um loop infinito, mas por que? Porque simplesmente, Haskell não permite mutabilidade. Neste caso, x foi redefinido, e ele não se lembra mais do valor antigo de x, assim, formando uma recursão infinita + 1 (sim, funções em Haskell tem a mesma sintaxe que variáveis).

Obs: para ver o tipo de uma variável no GHCi, use `:t variavel`, ou `:i variavel` para mais informações.

### transparência referencial

Basicamente, a transparência referencial quer dizer que você pode trocar toda a lógica de uma função por um código inline. Ainda não entendeu? Então veja um exemplo:

```hs
foo = "hello world"
print foo -- argumentos não precisam de parênteses
          -- a menos que se faça alguma operação dentro deles

sum x y = x + y -- função com argumentos x e y
print (sum 4 6)
```

Poderia ser substituído por:

```hs
print "hello world"

print (4 + 6)
```

E o exemplo é bem simples, talvez todas as linguagens permitem definir este exemplo simples, mas não para tudo... Nem tudo em uma linguagem pode ser transparentemente referencial. E portanto, as linguagens funcionais comumente tem a característica de serem transparentemente referenciais.

### sem nulos e exceções

Basicamente, o `null` ê o erro do século. Ele basicamente consegue quebrar o sistema de tipos, e você não tem como saber por onde ele virá, e talvez ele foi feito para quebrar o sistema de tipos... Um erro grave de arquitetura, porque o `null` não funciona sem você abrir uma exceção para ele no sistema de tipos, ou torná-lo menos rigoroso.

Linguagens funcionais não tem `null`, no caso, Haskell tem o `Maybe` para caso a função possa não retornar nada, que contém os construtores `Just` para caso retorne e `None` para caso não retorne, e isso é perfeitamente seguro porque eles são do mesmo tipo. Um exemplo:

```hs
div _ 0 = None -- "_" é uma boa prática porque não o usamos
               -- e 0 quer dizer que se o valor y for 0, então retorne None
div x y = Just $ x / y -- caso o segundo valor seja 0
                       -- retorne Just $ x / y
                       -- aonde $ ê tipo um parêntese
```

Obs: para funções múltiplas linhas como essa, insira `:{` e `:}` no GHCi

Mas como o `Maybe` é definido? Simples:

```hs
data Maybe a = None | Just a
```

Você não precisa entender este código agora, só precisamos saber que criamos um tipo `Maybe` que recebe um parâmetro `a` de qualquer tipo, e tem o construtor sem argumentos `None` e `Just` que é unário (recebe apenas 1 argumento). E não é importante agora entender o que eu falei, mas sim entender que este código existe... Porque linguagens funcionais geralmente não tem exceções, e no caso de Haskell, a gente usa o `Either`, que é definido como:

```hs
data Either a b = Left a | Right b
```

Aonde `Left` significa erro e `Right` significa sucesso. Mas você deve estar se perguntando "exceções não são naturalmente geradas? Podendo dar erro em tempo de execução como UB em C++ ou `RuntimeError` como em Python? Ou você tem que usar `try/catch` porque determinado tipo pode vir errado e dividir uma string por 6?"... E a resposta é NÃO! Haskell tem um sistema de tipos forte o bastante para garantir exceções não sejam geradas automaticamente. Agora, vamos parsear um `Either` com uma função que recebe `Either`:

```hs
parse (Left x) = error $ "an error has occured " ++ x -- ++ concatena
parse (Right _) = print $ "hello, you have a good luck"

parse (Left "I missed the bus") -- erro
parse (Right "I won the lottery") -- "hello, you have a good luck"
```

### funções tem tipos únicos

Em linguagens funcionais, funções tem tipos concretos, exemplo:

```hs
foo 0 = 100
foo x = print x

bar 1 = "hello"
bar _ = 0
```

Por que os dois exemplos não compilam? Porque na primeira linha, ele infere que o tipo do primeiro argumento de foo ê `Int`, mas no segundo, `print` só pode receber `String`, então, ele inferirá que x é `String`, mas não tem como x ser `Int` e `String` ao mesmo tempo. E no bar, ele inferiu que o tipo de entrada é um número, e o tipo de saída é uma string, mas abaixo, a gente retorna um 0.

### toda função retorna algo

Em funcional, tudo é baseado em funções, aonde toda função retorna algo, um exemplo:

```hs
print "foo"      :: IO ()
sum x y = x + y  :: Int
"foo" ++ "bar"   :: String
[1, 2] ++ [3, 4] :: [Int]
```

Aonde `::` é o tipo de retorno.

### first class functions

Uma linguagem é chamada first class se as funções dela são tratadas como qualquer outro tipo de dado da linguagem, isso inclui as seguintes regras:

**1.** Uma variável pode ser uma função

```js
const foo = () => {
    console.log("hello")
}
```

Dei o exemplo em JavaScript porque em Haskell ê menos explícito, já que variáveis são funções. Mas você pode ter lambdas em Haskell, assim:

```hs
sum x y = x + y
sum x = \y -> x + y
sum = \x y -> x + y
sum = \x -> \y -> x + y
```

Todas estas funções acima são iguais, e na aula sobre currying, iremos descobrir o porque.

**2.** Passar uma função como argumento

```hs
map (+ 1) [1..10]
```

Aonde map aplica +1 a todos os elementos da lista. Aonde + é uma função que recebeu o argumento 1 (e por causa do currying, podemos passar argumentos incompletos, mas iremos falar disso mais a frente) e uma lista de 1 a 10. Iremos ver mais sobre map/fmap no capítulo sobre functors, e funções como argumentos no capítulo sobre Higher Order Functions (HOF).

**3.** Retornar uma função

```js
function sum() {
    return (x, y) => x + y
}
```

Mais um exemplo em JavaScript porque em Haskell isso é implícito, já que por causa do currying, funções sempre retornam funções. Mas está aqui um pequeno exemplo para você que já deve estar curioso para saber o que é o curry:

```hs
sum = \x -> \y -> x + y -- função que retorna 2 funções recebendo 1 argumento cada
sumWith10 = sum 10 -- sempre irá somar com 10
sumWith10 20 -- 30
```

E como Haskell tem auto-currying, `sum x y = ...` é o mesmo que `sum x = \y -> ...`, `sum = \x y -> ...` e `sum = \x -> \y -> ...` (sendo este último, a forma mais correta), e o exemplo acima também é possível de simular em JavaScript, porque simplesmente curry são lambdas aninhados, veja:

```js
function sum() {
    return (x) => (y) => x + y
}

const sumWith10 = sum()(10)
sumWith10(20) // == sumWith10()(10)(20)
```

Aonde cada parêntese se refere a um espaço em Haskell.

### sem globais

Todas as variáveis em funcional são locais, assim como funções aninhadas... Aliás, lembra que em Haskell, uma variável (constante polimórfica) é só uma função (ou tratada como -- first class), funções/variáveis dentro de funções/variáveis (que são a mesma coisa) não são acessíveis fora dela. Isso é muito mais seguro porque código global é uma ameaça, e código local só você pode ter o controle.

### side effects

O termo side effect quer dizer que uma função pode ter efeitos colaterais, e geralmente elas são classificadas com as seguintes regras:

1. Mudar o valor de uma variável
2. Escrever/ler dados do disco
3. Escrever na GUI

E linguagens funcionais não tem side effects... Mas pera, não dá para programar em GUI ou escrever/ler do disco??? Calma, Haskell tem side effects controlados pela monad IO.

### pureza

En funcional, pureza se refere a funções que tem seus resultados determinados pelos seus argumentos, e nunca por uma váriavel global. E elas também não podem ter side effects.

## lambda-calculus

### funções simples

Basicamente, com o lambda-calculus, você consegue criar funções, aonde λ é o lambda e tudo antes do ponto é um argumento. Alguns exemplos:

```
id := λx.x -- retorna o argumento recebido
sum := λx.λy.x+y -- usando currying para múltiplos argumentos, e retornando a soma deles
```

E basicamente, uma expressäo lambda-calculus pode ser composta de:

- variável - o argumento
- abstração - o corpo da função
- aplicação - a aplicação dos argumentos a função

Exemplos:

```
-- variável
λx
-- abstração
λx.x
-- aplicação
(λx.x+1)(3) -- retorna 4
```

### números em lambda-calculus

Os números do lambda-calculus são definidos pelo numeral de Church, assim:

```
0 := λf.λx.x
1 := λf.λx.fx
2 := λf.λx.f(fx)
3 := λf.λx.f(f(fx))
```

Ou então:

```
0 := λfx.x
1 := λfx.f x
2 := λfx.f (f x)
3 := λfx.f (f (f x))
```

Aonde f seria um:

`λx.x+1`

### lógica booleana em lambda-calculus

Agora que você já sabe formar passos em lambda-calculus na sua cabeça (caso contrário, leia a lição anterior), vou deixar você pensar sozinho:

```
true := λab.a
false := λab.b

and := λab.a b false
or := λab.a true b
not := λabc.a c b
if := λabc.a b c
eq := λxy.if x == 0 then true else false
```

## programação lógica

A programação lógica é baseada na resolução SLD e nas claúsulas de horn, e usa backtracking por baixo para resolver claúsulas, e calma, apesar dos nomes difíceis, Prolog é a linguagem mais simples que eu já vi na minha vida :)

### a linguagem Prolog

A linguagem de programação Prolog foi criada em 1972 com intuito em programação lógica, linguística e inteligência artificial. Mas o que faz esta linguagem se destacar tanto entre as linguagens de programação lógica? Bem, eu te dou 3 motivos:

1. Prolog é simples, ainda mais por ser do paradigma declarativo.
2. Nenhuma outra linguagem de programação lógica inovou tanto quanto Prolog.
3. Prolog além de suportar a resolução SLDNF, suporta cut da árvore de backtracking, assim sendo muito mais rápida e eficiente.

### o paradigma de programação declarativo

O paradigma declarativo te permite não mais dizer passo a passo o que seu computador deve fazer, mas sim, o que seu computador deve fazer para chegar a determinado resultado. Por exemplo, em SQL temos a seguinte declaração:

```sql
SELECT * FROM people WHERE age > 10 ORDER BY country DESC
```

Agora, imagine isso em Python, seria algo como:

```py
accumulator = []
for person in people:
    if person.age > 10:
        accumulator.append(person)
accumulator.reverse_by_country("desc")
```

Mas por que tivemos que especificar tudo em Python e fazer quase nada em SQL apenas dizendo como eu queria? Porque simplesmente SQL é declarativo. Python não. Haskell e Prolog são dois exemplos de linguagem declarativa.

### predicados

Agora, você escreve seus predicados em um arquivo .pl ou .pro, e testa eles (com ?-) entrando no interpretador com:

`$ swipl meu_arquivo.pl`

Agora, vamos ver o que são predicados. Basicamente, um programa lógico se consiste de predicados, aonde:

```pl
% isso é um comentário
homem(joao). % fato, joao é homem
             % programas em prolog terminam com ponto
mae(ana, joao). % ana é mãe de joao, fato
mae(X, Y) :- filho(Y, X). % regra, X é mae de Y se Y for filho de X

% exemplo
filho(gabriel, cris).
filho(nicolas, cris).
mae(X, Y) :- filho(Y, X).
?- mae(cris, gabriel). % query: cris é mae de gabriel?
% true
```

Agora que você sabe isto, você já pode escrever programas mais complexos em Prolog. Viu como é simples? Você consegue até entender um programa de árvore genealógica que eu fiz <https://github.com/foxx3r/genealogy_prolog>, e basicamente Prolog é pura lógica.

### modus ponens

Basicamente, diz que em `P ⊦ Q`, quer dizer "se P implica Q, e Q é verdadeiro, então P também é". Uma curiosidade é que vírgula em Prolog significa AND e ponto-e-vírgula significa OR. Vamos ver um exemplo:

```pl
irmao(X, Y) :- pai(P, X), pai(P, Y), X \= Y, homem(X).
irmao(X, Y) :- mae(M, X), mae(M, Y), X \= Y, homem(X).
```

Aqui, irmao é um modus ponens verdadeiro para todos que tem o mesmo pai/mãe que ele, e que não é ele próprio e contanto que seja homem.

### backtracking

Bem, o backtracking é o algorítmo de resolução que o Prolog usa, e basicamente ele consegue fazer coisas como testar e eliminar possibilidades. Vamos ver um pequeno exemplo:

`backtracking.pl`
```pl
mulher(cris).
mulher(ana).
mulher(maria).

filho(gabriel, cris).
filho(bernardo, cris).
filho(nicolas, cris).
filho(juninho, ana).
filho(jose, maria).

mae(X, Y) :- filho(Y, X), mulher(X).
```

Agora, importe ele com:

`$ swipl backtracking.pl`

E agora digite:

`?- mae(X, Y).`

E ele tentará adivinhar todas as possibilidades de X e Y, mas... O que é isso? É simplesmente o algorítmo de backtracking trabalhando. E isso irá retornar:

```
X = cris,
Y = gabriel ;
X = cris,
Y = bernardo ;
X = cris,
Y = nicolas ;
X = ana,
Y = juninho ;
X = maria,
Y = jose.
```

E o que acontece na real, é que minúsculas em Prolog são átomos (valores que correspondem a eles mesmo, por exemplo: true é true, e 1 é 1). Aqui está a explicação de backtracking da Wikipédia:

> Backtracking é um tipo de algoritmo que representa um refinamento da busca por força bruta, em que múltiplas soluções podem ser eliminadas sem serem explicitamente examinadas. O termo foi cunhado pelo matemático estado-unidense D. H. Lehmer na década de 1950.
> O procedimento é usado em linguagens de programação como Prolog. Uma busca inicial em um programa nessa linguagem segue o padrão busca em profundidade, ou seja, a árvore é percorrida sistematicamente de cima para baixo e da esquerda para direita. Quando essa pesquisa falha ou é encontrado um nodo terminal da árvore, entra em funcionamento o mecanismo de backtracking. Esse procedimento faz com que o sistema retorne pelo mesmo caminho percorrido com a finalidade de encontrar soluções alternativas.

### resolução SLD

Basicamente, a resolução SLD trabalha como o algorítmo de inferência do Prolog sob as claúsulas horn. Ele trabalha com unificações, um exemplo de unificação:

```pl
?- true = true.
true.
?- false = false.
true.
?- mia = mia.
true.
?- 'mia' = mia.
true.
?- foo = bar.
false.
?- X = Y.
X = Y.
?- X = a.
X = a.
```

Aonde em casos que ele não pode afirmar nada e nem negar (como no caso de `X = Y`), ele apenas concorda com você.

### cut, negação e a resolução SLDNF

Primeiro, vamos falar sobre o controverso cut... Mas por que controverso? Porque simplesmente, ele foi adicionado apenas por motivos de eficiência em Prolog e não segue as claúsulas horn. A lógica do cut é cortar a árvore de busca de possibilidades do backtracking, assim, permitindo maior performance. Um exemplo, aonde `!` é o cut operator:

```pl
a(X, Y) :- b(X), !, c(Y).
b(1).
b(2).
b(3).

c(1).
c(2).
c(3).
```

Agora, vamos rodar:

```pl
?- a(X, Y).
X = Y, Y = 1 ;
X = 1,
Y = 2 ;
X = 1,
Y = 3.
```

Ele achou todas as possibilidades possíveis, né? Mas repare que o valor do X nunca muda, porque a gente diz que não é mais necessário produzir um valor novo para o X, caso contrário, sem o cut, o código rodaria assim:


```pl
?- a(X, Y).
X = Y, Y = 1 ;
X = 1,
Y = 2 ;
X = 1,
Y = 3 ;
X = 2,
Y = 1 ;
X = Y, Y = 2 ;
X = 2,
Y = 3 ;
X = 3,
Y = 1 ;
X = 3,
Y = 2 ;
X = Y, Y = 3.
```

Aqui você pode ver que o X e Y foram repetidos, mas... Isso é realmente necessário? Não! Por isso usamos o cut. Agora, iremos falar sobre a resolução SLDNF, que é basicamente a resolução SLD com "negação por falha". Mas o que quer dizer "negação por falha"? Simplesmente quando passamos um predicado simples para o Prolog, como `foo(X) :- bar(X)`, você verifica se X é bar, mas e se der falha? Você pode usar o operador `\+` para negar a expressäo caso dê falha, ou seja, uma expressäo falsa virá a se tornar verdadeira. A expressäo acima ficaria `foo(X) :- \+ bar(X)`.

## programação funcional no geral

Agora neste capítulo iremos te ensinar features funcionais

### morfismo

Basicamente, morfismo quer dizer uma "forma", em Haskell, ela diz a forma de um objeto/função, ou seja, é a anotação de tipos. Vamos ver a anotação de uma função matemática:

`foo : A → A → A`

Basicamente, esta função pega 2 argumentos do tipo A e retorna um argumento do tipo A. Agora vamos fazer isso em Haskell'

```hs
foo :: a -> a -> a
```

Viu como é bem simples? Mas você deve estar se perguntando "por que não separaram o tipo de retorno dos argumentos?" e iremos explicar mais para frente falando do currying. Agora, vamos escrever algumas funções anotando os tipos delas:

```hs
head :: [a] -> a
head (x:_) = x
-- x é o primeiro elemento, e _ o resto da lista

tail :: [a] -> [a]
tail (_:xs) = xs

fact :: Int -> Int
fact 1 = 1
fact n = fact (n - 1) * n

id :: a -> a
id x = x

-- função recebe qualquer função que receba "a" e retorna "b",
-- depois como argumento recebe "a" e como desejado, retorna "b"
ex :: (a -> b) -> a -> b
ex f b = f b
-- não entendeu? fique analisando a lógica até chegar lá

parse :: Maybe String -> String
parse (Just x) = x
parse None = "failed"

-- ou...

parse :: Maybe a -> a
parse (Just x) = x -- não sabemos o tipo de x, então o que faremos em None?
parse None = error "foo"
```

Wow, pera lá, por que podemos dae erro? Como sabemos que ele sempre vai ser do tipo do nosso tipo? Bem, claramente não iremos falar disso agora, mas em Haskell (e basicamente todas as linguagens turing completas com polimorfismo paramétrico), você pode ter funções que sejam de todos os tipos, os famosos bottom values.

### polimorfismo

Basicamente o polimorfismo significa "muitas formas", e se você veio de alguma linguagem como TypeScript, Rust, Java, Dart, Swift ou C++, você deve conhecer esta feature pelo nome "generics". E se não veio, basicamente ela quer dizer que um argumento/retorno pode ter várias formas. E bem, como você já deve saber, o sistema de tipos de Haskell é bem forte e te previne de muito erro, então você não pode fazer algo como `foo :: x -> y`, tipos de retornos genéricos tem que se basear no tipo de entrada, caso contrário, você terá que explicitar o tipo retornado. Alguns exemplos de polimorfismo em Haskell:

```hs
id :: x -> x
id x = x

-- os dois tipos podem ser diferentes ou iguais
const :: a -> b -> a
const a _ = a

-- os dois tipos devem ser do mesmo tipo
const :: a -> a -> a
const a _ = a

const 1 7
-- 1
```

### função id

Na matemática, o elemento identidade é o elemento que não muda em nada a operação. Por exemplo, `1 + 0` é igual a 1, então o elemento identidade é o 0. Mas... No caso de Haskell, estamos falando da função identidade, e por que ela é tão importante? Basicamente, se uma função te obriga a passar uma função para dentro dela, mas você não quer modificar o resultado, então você passa a função id. Um exemplo:

```hs
-- aqui nós forçamos para a função receber e retornar Int
-- e como id recebe qualquer tipo, então será válido
func :: (Int -> Int) -> Int
func f = f 5

func id
-- 5
```

### isomorfismo

Basicamente, a palavra isomorfismo quer dizer que um objeto X é o mesmo que um Y. Por exemplo, como a gente já havia discutido aqui antes `foo x = ...` é isomórfico a `foo = \x -> ...`.

### pattern matching

Basicamente, pattern matching são uma espécie de if mais poderoso, e você vai entender o porque agora:

```hs
number :: Int -> String
number 0 = "zero"
number 1 = "one"
number 2 = "two"
number 3 = "three"
number 4 = "four"
number 5 = "five"
numbwr _ = "unknown number"

number 6
-- unknown number

number 3
-- three

letter :: Char -> String
letter 'a' = "abc"
letter 'b' = "bc"
letter 'c' = "c"

letter 'd'
-- Non-exhaustive patterns in function letter

letter 'a'
-- abc
```

Basicamente, é uma forma de evitar bastante if pelo código e também fazer desconstruções no código. E se você não cobrir todos os casos, a função pode gerar uma exceção "non-exhaustive pattern". Existem também outras 3 formas de testar condições em Haskell:

**1. if**

Um if sempre deve ter um bloco else e não pode ter mais de uma verificação (a menos que você coloque dentro do else).

```hs
id :: Int -> Maybe Int
id = if x == 0 then None else Just x
```

**2. case**

Basicamente, pattern matching são desconstruídos para case. Vamos ver um exemplo com o exemplo de pattern matching que demos agora há pouco:

```hs
number :: Int -> String
number x = case x of
    0 -> "zero"
    1 -> "one"
    2 -> "two"
    3 -> "three"
    4 -> "four"
    5 -> "five"
    _ -> "unknown number"
```

**3. guards**

São uma forma mais bonita e que permite múltiplos casos em relação ao if. Um exemplo bem famoso:

```hs
bmiTell bmi  
    | bmi <= 18.5 = "You're underweight, you emo, you!"  
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise   = "You're a whale, congratulations!"  
```

Aonde o otherwise seria o "caso contrário".

### composição

Basicamente, a composição matemática é algo como:

`(f • g)(x)`

E você provavelmente já deve tê-lo visto por aí, por exemplo, composição em matemática é só uma forma de não ter que fazer `f(g(x))` e sim usar `(f • g)(x)`, em shell script, o código `f(g(x))` seria `f ${g x}`, ou poderia ser `x | g | f`, a mesma coisa em Elixir/F# que poderia ser `f (g x)` ou `x |> g |> f`. Então, basicamente a composição matemática seria da esquerda para a direita e a da computação da direita para a esquerda. Em Haskell temos ambos, porém, o da direita pra esquerda é acessível apenas via biblioteca. E ele não é tão bom porque não podemos nos aproveitar do currying com ele. Agora vamos deixar de papo e mostrar como é definido por baixo o operador `.` de composição de Haskell:

```hs
(.) :: (b -> c) -> (a -> b) -> a -> c
(.) f g = \x -> f (g x)
```

Agora, pare um pouco e pense nos tipos e tente raciocinar. Basicamente, colocamos o `.` entre parênteses pra ele poder ser chamado de forma infixa (no meio de dois termos), agora, vamos ver um exemplo:

```hs
foo x = x * x
bar x y = x + y

baz = foo . bar 3

baz 5
-- 64 == (3 + 5) * (3 + 5) == foo (bar 3 5)
```

### lifting

Basicamente o termo lifting se refere a pegar um valor e converter ele para um contexto, no qual iremos explicar daqui há a algumas aulas, aqui estão alguns exemplos:

```hs
λ a = 3
a :: Num p => p
λ a = return 3
a :: (Monad m, Num a) => m a
λ a = "hello"
a :: String
λ a = lift "foo"
a :: MonadTrans t => t [] String
```

### constraints

Basicamente, uma constraint é uma restrição que podemos fazer no sistema de tipos, ou no nosso caso, é para deixar o código mais polimórfico. Um exemplo:

```hs
foo :: Num p => p -> p
foo x = x + 1
```

E basicamente ao fazer isso, estamos dizendo que p é qualquer tipo que instancie de `Num`, ou seja, como nós sabemos, Haskell tem vários tipos de inteiros, como `Int`, `Integer`, `Word` e outros. Mas o que fazem eles serem números válidos? Basicamente Haskell é feito em Haskell, então o `Num` foi feito em Haskell, o `Int`, o `Integer` e etc... Mas o que os torna realmente um número? Basicamente todo tipo de número instancia `Num`, e então ao invés de aceitarmos apenas um tipo de número, a gente pode aceitar todos os tipos de número. Mesma coisa com a typeclasse `Show`, aonde todo tipo que pode ser printável tem que derivar de `Show`, porque `Show` oferece a função `show` que basicamente o que o GHCi faz por baixo quando você faz algo como `id 3`, ele transforma para `show (id 3)`. Se você fizer algo como:

```hs
foo :: x -> IO () -- IO () é o tipo de retorno de print
foo x = print x
```

Não vai passar, porque o `print` requer que o valor recebido instancie `Show` porque o sistema de tipos nos previne de fazer código errado, vamos ver a declaração do `print`:

```hs
print :: Show a => a -> IO ()
```

Então, para a função acima dar certo, a gente faz:

```hs
foo :: Show x => x -> IO ()
foo x = print x
```

E se quisermos, você pode adicionar mais de uma constraint fazendo algo como `(Show p, Monad p, Functor f) => ...`. Agora, vamos criar o nosso próprio número, e no final explico cada código:

```hs
data Foo = Foo Integer deriving (Show)

instance Num Foo where
    fromInteger x = Foo x

bar :: Foo
bar = 3

print bar
-- Foo 3
```

Obs: isto pode dar warnings, mas é comum porque não satisfazemos todas as funções.

Basicamente na primeira linha nós criamos um tipo de dado `Foo` que contém un construtor `Foo` que recebe um `Integer`. Logo após, a declaração `deriving (Show)` nos permite instanciar automaticamente o `Show`. Logo abaixo, a gente instancia o `Foo` para `Num`, e dizemos que a função `fromInteger` recebe um x e retorna `Foo x`. E por que `Foo x`? Porque basicamente o `Num` foi criado assim:

```hs
class Num a where
  (+)         :: a -> a -> a
  (-)         :: a -> a -> a
  (*)         :: a -> a -> a
  negate      :: a -> a
  abs         :: a -> a
  signum      :: a -> a
  fromInteger :: Integer -> a
```

Aonde `a` é o seu tipo. Poderiamos ter feito algo como:

```hs
instance Num Foo where
    (Foo x) * (Foo y) = Foo (x * y)
    (Foo x) + (Foo y) = Foo (x + y)
    (Foo x) - (Foo y) = Foo (x - y)
    abs (Foo x)       = Foo (abs x)
    signum (Foo x)    = Foo (signum x)
    fromInteger x     = Foo x
```

Agora observe o código do `Num` e tente raciocinar. Agora, vamos ver alguns exemplos:

```hs
Foo 3 * Foo 2
-- Foo 6
Foo 7 + Foo 2
-- Foo 9
Foo 2 - Foo 2
-- Foo 0
abs (Foo (-3))
-- Foo 3
signum (Foo 4)
-- Foo 1
fromInteger 6 :: Foo
-- Foo 6
```

Mas iremos falar disso depois.

### declarativismo

Basicamente, Haskell suporta programação declarativa com o `where`, assim:

```hs
divide x = x / pi
    where
        pi = 3.14

joao = name
    where
        name = "joao"
```

### curry e point-free

Ah... O curry, o maravilhoso curry, tão suculen... Espwra, a gente não está falando do molho, e sim do currying do haskell curry (um matemático). Basicamente, o curry são lambda-calculus aninhados, e lambda-calculus são funções, certo? Então... `foo x y = ...` é igual a `foo = \x -> \y -> ...`, e tudo em Haskell é curried automaticamente. Existe uma função chamada `curry` que pega uma função de 2 argumentos (se você quiser mais, você vai precisar criar uma função pra cada elemento, esta é uma das desvantagens de não usar curry) e retorna ela sem curry, por exemplo:

```hs
λ foo x y = x + y
foo :: Num a => a -> a -> a
λ :t uncurry
uncurry :: (a -> b -> c) -> (a, b) -> c
λ bar = uncurry foo
bar :: Num c => (c, c) -> c
λ -- bar 3 2 daria erro
λ bar (2, 6)
8
it :: Num c => c
```

E o curry não acontece apenas a nível de função não... Lembra que eu perguntei a Vocês o porque de Haskell não ter adotado uma anotação diferente para diferenciar os argumentos e o retorno? Então...

```hs
foo :: a -> b -> c -> d

-- é isomórfico a

foo :: ((a -> b) -> c) -> d
```

E com isso, podemos fazer coisas como:

```hs
sum :: Num p => p -> p -> p
sum x y = x + y

sumWith10 :: Num p => p -> p
sumWith10 = sum 10
```

Agora que já sabemos como o currying trabalha... Vamos te apresentar o point-free style, ou programação tácita. Basicamente é a programação "orientada a currying", aonde você geralmente faz uso de currying para não ter argumentos explícitos, e.g:

```hs
foo :: Num p => [p]
foo = fmap (*5)

-- ao invés de

foo :: Num p => [p]
foo x = fmap (*5) x
```

### Higher Order Functions e closures

As famosas HOF são um termo que se refere a passar uma função como argumento, um exemplo:

```hs
foo :: a -> (a -> b) -> b
foo x f = f x

foo 3 (\x -> x + 1)
-- 4
```

Aonde elas são denotadas por `(a -> b -> c -> d)` no sistema de tipos. Já o termo closure, é o termo dado a retornar uma função de uma outra função. Em Haskell não é tão explícito porque é exatamente isso que o curry faz, por isso, iremos dar um exemplo em JS:

```js
function closure() {
    return x => y => x + y
}
```

### recursão

Um ponto que difere Haskell de outras linguagens, é que usa-se recursão ao invés de loop, isto quer dizer que, executamos um loop chamando a própria função. Vamos ver um exemplo:

```hs
fact 1 = 1
fact n = fact (n - 1) * n
```

Simplesmente leia como se fosse "se o argumento for 1, retorne 1, caso contrário, então chame `fact (n - 1) * n`", isto é chamado de pattern matching. Agora vamos desconstruir a função com o argumento 4:

```hs
fact 4
4 * (fact 3)
4 * (3 * (fact 2))
4 * (3 * (2 * (fact 1)))
4 * (3 * (2 * 1))
4 * (3 * 2)
4 * 6
24
```

Relaxa, com o tempo você pega a prática... E quem sabe, você até ache o processo linear iterativo mais fácil assim como eu. E você não precisa ser um matemático para saber lidar com recursão. E nem mesmo precisará contar nos dedos toda vez que for fazer uma recursão. É só você treinar que você pega a prática.

Para calcular uma recursão, geralmente não precisa de um cálculo na mão, e sim um pouco de lógica. Vamos usar a sintaxe (x:xs) aonde x é o primeiro elemento da lista, e xs é o resto. Sabendo disso, vamos ao exemplo:

```hs
-- se a lista estiver vazia
product :: [Int] -> Int
product [] = 0
product (x:xs) = x * product xs
```

Aqui, ao fazermos "x * product xs", estamos falando para ele multiplicar o primeiro elemento da lista pelo resto, e quando ele for executar "product xs", o primeiro elemento da lista agora vai ser o segundo elemento, e por assim vai. Esta é a lógica da função, multiplicar todos os elementos da lista, fazendo [1 * 2 * 3 * 4], aonde o x vai ser o 1, depois o 2, o 3, o 4, e depois []. Mas... Espera aí, se você executar a função, ela retornará sempre 0, mas por que? Bem... Vamos dar uma olhada em como é feito por baixo:

```hs
product [1, 2, 3, 4]
1 * product [2, 3, 4]
1 * 2 * product [3, 4]
1 * 2 * 3 * product [4]
1 * 2 * 3 * 4 * product []
1 * 2 * 3 * 4 * 0
0
```

Bem... Estamos retornando 0, como vocês todos sabem, a gente está fazendo multiplicação, e tudo multiplicado a 0 é 0, isto é um perigo pra nossa aplicação. Se fosse +, tudo bem. Mas a gente quer um valor que não modifique o resultado, então usaremos o 1, pois qualquer número multiplicado a 1 é igual a ele mesmo. Vamos ver como ficaria:

```hs
product :: [Int] -> Int
product [] = 0
product (x:xs) = x * product xs
```

Agora sim, vamos ver como é calculado por baixo:

```hs
product [1, 2, 3, 4]
1 * product [2, 3, 4]
1 * 2 * product [3, 4]
1 * 2 * 3 * product [4]
1 * 2 * 3 * 4 * product []
1 * 2 * 3 * 4 * 1
24
```

### tail call recursion e tail call optimization

Conhecida também como processk iterativo linear por schemeiros/SICPeiros
.. Este tipo de recursão é iterativo, ou seja, diferente da recursão convencional, ele não vai acumulando `1 * 2 * 3 * 4` por exemplo, ele vai fazendo avaliando a cada iteração esse valor e produzindo o resultado, assim, as probabilidades da complexidade do algoritmo ser O(n) são muito grandes, um exemplo de como um fatorial é calculado por baixo:

```lisp
(factorial 6)
(6 * (factorial 5))
(6 * (5 * (factorial 4)))
(6 * (5 * (4 * (factorial 3))))
(6 * (5 * (4 * (3 * (factorial 2)))))
(6 * (5 * (4 * (3 * (2 * (factorial 1))))))
(6 * (5 * (4 * (3 * (2 * 1)))))
(6 * (5 * (4 * (3 * 2))))
(6 * (5 * (4 * 6)))
(6 * (5 * 24))
(6 * 120)
720
```

Você percebe que a função cresceu pra depois diminuir? Agora vamos ver como é o fatorial na forma linear iterativa:

```lisp
(factorial 6)
(fact-iter 1 1 6)
(fact-iter 1 2 6)
(fact-iter 2 3 6)
(fact-iter 6 4 6)
(fact-iter 24 5 6)
(fact-iter 120 6 6)
(fact-iter 720 7 6)
```

Com a forma linear iterativa, a linguagem consegue fazer um processo de otimização chamado TCO (Tail Call Optimization), aonde o processador consegue limpar a stack a cada loop, assim a memória na stack não vai se acumulando e provavelmente não vai causar um stack overflow. Agora vamos ver como se faz, e a diferença da recursão convencional e a tail recursion:

```hs
fact :: Int -> Int
fact 1 = 1
fact n = fact (n - 1) * n

fact' :: Int -> Int
fact' n = fact_iter n 1
    where
        fact_iter 1 acc = acc
        fact_iter product acc = fact_iter (product - 1) (acc * product)
```

Conseguiram perceber como faz?

**1:**
```hs
n - 1
```
**2:**
```hs
product - 1
```

**1:**
retorna 1
**2:**
retorna `acc`

**1:**
`fact(...) * n`
**2:**
```hs
acc * product
```

Aonde acc é o acumulador. Agora vamos ver como é o fibonacci:

```hs
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fib' n = fib_iter n 1 0 where
    fib_iter n cur prev | n <= 0    = cur
                        | otherwise = fib_iter (n - 1) (cur + prev) cur
```

Por baixo:

```hs
fibonacci 3
fibonacci 2 + fibonacci 1
fibonacci 1 + fibonacci 0 + fibonacci 1
1 + fibonacci 0 + fibonacci 1
1 + 1 + fibonacci 1
1 + 1 + 1
2 + 1
3

fib' 3
fib_iter 3 1 0
fib_iter 2 1 1
fib_iter 1 2 1
fib_iter 0 3 2
3
```

Aonde current é o quanto de iterações que o programa já rodou, e o previous é o número anterior a essa iteração. a pergunta que vocês devem estar se perguntando agora é:

Por que temos um `current` e `previous`? Porque não é assim em fatorial

Simples, porque a gente só precisa dos dois números anteriores na fibonacci:

```hs
fib(n - 1) + fib(n - 2)
```

Precisamos de um número anterior e de um anterior a esse.

Então a cada iteração, dizemos que:

```hs
current = current + previous
previous = cur
```

Neste código, o acumulador é o current.

Para você saber qual é o acumulador, você simplesmente vê qual está sendo retornado, um exemplo:

```hs
fib' 3
fib' 3 1 0
fib' 2 1 1
fib' 1 2 1
fib' 0 3 2
3
```

Deu 3, porque o segundo parâmetro é o acumulador. Iniciamos com `fib(n, 1, 0)`, depois previous vira o valor do `current` (1), e temos um `n - 1`.

Depois disso, é realizado um `n - 1` novamente, o que o faz virar 1, e temos `cur + prev` que dá 2, e `prev` vira `cur`, que dá 1

Depois, temos `n - 1` que vira 0, e `cur + prev (2 + 1)` dá 3, e `prev` vira `cur`, que dá 2. Agora aqui, ele só retorna 3. Não precisa desencapsular nem nada, só retornar o valor que foi incrementado durante todo o processo.

E basicamente se a linguagem suportar a otimização TCO (Taill Call Optimization), o compilador conseguirá tratar essa recursão como um loop iterativo e otimizá-lo.

### total functions e partial functions

Uma função parcial em Haskell é uma função cujo argumento de retorno pode ser diferente, por exemplo, um bottom, que como discutimos já, permite que algumas funções se encaixem em tosos os tipos. Um exemplo de função parcial é o `head` e o `tail`, que podem retornar erro caso a lista esteja vazia. E o motivo de Haskell não ter adotado `Maybe` ou `Either` nestes casos é por compatibilidade, porque Haskell queria se parecer com um lazy ML. E total functions são o contrário de partial functions. Vamos criar um `head` total:

```hs
head :: [a] -> Maybe a
head [] = None
head (x:_) = Just x
```

### list comprehension

Basicamente, a compreensão de listas tem a seguinte sintaxe:

`[estrutura dos dados | associação dos dados, condições]`

E além disso, é comum usar a sintaxe `x..y`, aonde `x` é um número menor que `y`, então isso diz que uma lista de `x` até `y`. E `x..` gera um valor infinito.  E `x, y...z` gera um valor de `y` a `z` pulando de `x` em `x`, e para funcionar como o esperado, `y` deve ser o dobro de `x`, e caso `x == y`, então o valor gerado é infinito, e caso `y < x`, o valor retornado é uma lista vazia.

Alguns exemplos:

```hs
λ [x | x <- [1..10]]
[1,2,3,4,5,6,7,8,9,10]
λ [x * x | x <- [1..10]]
[1,4,9,16,25,36,49,64,81,100]
λ [(x, y) | x <- [1, 2, 3], y <- [1..7], y > 4]
[(1,5),(1,6),(1,7),(2,5),(2,6),(2,7),(3,5),(3,6),(3,7)]
λ [x | x <- [2, 4..20]]
[2,4,6,8,10,12,14,16,18,20]
λ [x | x <- [2, 1..20]]
[]
λ [x | x <- [2, 6..20]]
[2,6,10,14,18]
```

### fmap

Basicamente, o `fmap` quer dizer functor map, no qual iremos falar mais a frente em teoria das categorias. Basicamente, com `fmap`, a gente consegue aplicar funções em valores/tipos que derivem de `Functor`. Vamos ver a definição de `Functor` com `:i`:

```hs
type Functor :: (* -> *) -> Constraint
class Functor f where
  fmap :: (a -> b) -> f a -> f b
  (<$) :: a -> f b -> f a
  {-# MINIMAL fmap #-}
        -- Defined in ‘GHC.Base’
instance Functor (Map k) -- Defined in ‘Data.Map.Internal’
instance Functor (Array i) -- Defined in ‘GHC.Arr’
instance Functor (Either a) -- Defined in ‘Data.Either’
instance Functor [] -- Defined in ‘GHC.Base’
instance Functor Maybe -- Defined in ‘GHC.Base’
instance Functor IO -- Defined in ‘GHC.Base’
instance Functor ((->) r) -- Defined in ‘GHC.Base’
instance Functor ((,,,) a b c) -- Defined in ‘GHC.Base’
instance Functor ((,,) a b) -- Defined in ‘GHC.Base’
instance Functor ((,) a) -- Defined in ‘GHC.Base’
```

Aonde cada instance é uma instância de `Functor`. Alguns exemplos:

```hs
fmap (* 2) [2..8]
-- [2 * 2, 3 * 2, 4 * 2, 5 * 2, 6 * 2, 7 * 2, 8 * 2]
-- [4, 6, 8, 10, 12, 14, 16]
fmap (+5) (Just 4)
-- Just 9
fmap (++ "!") ["I win", "I get", "I feel"]
-- ["I win!", "I get!", "I feel!"]
fmap id [1, 2, 3]
-- [1, 2, 3]
```

E um fato interessante é que o `map` é um `fmap` que funciona apenas para listas e foi um erro de design, e até hoje é mantido apenas por compatibilidade. Então, sempre usem `fmap`.

### filter

Basicamente o filter filtra os resultados em uma lista, por exemplo:

```hs
filter odd [3,6,7,9,12,14]
-- [3, 7, 9,]
filter False [1..30]
-- []
filter True [1, 2, 3, 4, 5]
-- [1, 2, 3, 4, 5]
filter (\x -> length x > 4) ["a", "ab", "abc", "abcd", "abcde", "abcdef", "abcdefg"]
-- ["abcde", "abcdef", "abcdefg"]
```

### fold ou reduce

Basicamente, o que você deve conhecer como `reduce` de outras linguagens, é o `fold` de Haskell. Haskell tem 2 tipos de fold, ou melhor... 8. Mas já vamos discutir sobre cada um. Vamos começar com o `foldl`, basicamente, fold significa dobrar. O `foldl` dobra da esquerda pra direita, ou seja:

```hs
foldl (+) 0 [1..10]
-- == 0 + 1 + 2 + 3 + ...
```

Aonde o 0 é o acumulador. Tome cuidado com qual acumulador usar. Por exemplo, usar 1 em operações com + aumentaria o resultado em 1, e usar 0 em operações com * sempre retornaria 0, porque todo número multiplicado por 0 é igual a 0. Já o `foldr` começa a dobrar da direi5a para a esquerda, e só vai se diferenciar do `foldl` se a operação não for comutativa (e.g: `a + b` é `b + a`, portanto, adição é comutativa). Agora, temos o `foldl1` e `foldr1`, aonde o acumulador por padrão é 1, e é bem interessante você usar semore que puder. Já o `foldl'` e `foldr'` (conta com o `foldl1'` e `foldr1'`) são os que eu recomendo sempre usar, ele são uma versão sem stack overflow dos `fold`s, que basicamente usa strictness evaluation que iremos explicar nos capítulos sobre lazy programming.

### zip

Basicamenre, o zip nos permite pegar 2 listas, e juntar o index 1 da primeira lista com o index 1 da outra lista numa mesma tupla, e assim por diante. Alguns exemplos:

```hs
zip [1, 2, 3] [9, 8, 7]
-- [(1, 9), (2, 8), (3, 7)]
zip [1..5] [9, 8]
-- [(1, 9), (2, 8)]
```

### continuation passing style 

Basicamente, o CPS é um caso de callback, aonde você se aproveita da computação de recursos na stack, e aonde você acumula valores... Uhmm, acumular, vocês se lembram desta frase? Bora ver um exemplo de callback em JavaScript:

```js
const sum = (x, y, k) => k(x + y)
```

Aonde `k` é o callback. Mas o que seria uma CPS? Seria uma continuação da continuação! Tipo o que fizemos no exemplo de Tail Call Optimization! Mas de uma forma otimizada parecida com callbacks.

```hs
fact' :: Int -> Int
fact' n k = fact_iter n 1
    where
        fact_iter 1 acc = acc
        fact_iter product acc = k $ fact_iter (product - 1) (acc * product)
```

Aonde `fact_iter` é uma continuação de `fact'` e `fact_iter` (a recursão) é uma continuação de si mesma, aonde `k` é o callback. Repare que a continuação é sempre a última coisa que a função faz.

### tipos em Haskell

Como todos sabemos, o `Int` em Haskell é um inteiro normal, que deriva de `Num`, e `Integer` é conhecido como big int, aonde temos um tamanho infinito de possibilidades de números. `Char` é apenas um caractere, `[a]` é uma lista com tipos `a` dentro dela, e `String` é um sinônimo para `[Char]`. `Float` são números de ponto flutuante e `Fractional` é um número de ponto flutuante que pode ser representado fracionalmente. `Word` equivale ao word do processador. `(Int, Int, Int)` é uma tupla com 3 `Int`s. E `Bool` são booleanos. `*` é o tipo dos tipos nos quais iremos falar no capítulo sobre type-level programming, e `Constraint`, bem... A maneira mais simplesnde explicá-lo mas talvez não tão correta/perfeccionista, seria que ele é geralmente retornado por typeclasses e quer dizer que ela pode servir com constraint.

## introdução a teoria das categorias

A teoria das categorias pode não ser tão útil ou amplamente utilizada por matemáticos, mas se encaixa exatamente com programação, porque você tem uma generalização da matemática toda em categorias, e categorias são possíveis de serem expressadas em programação de forma simples.

### o que é uma categoria?

Basicamente, uma categoria seria um monte de objetos e setas entre eles (algo como `A → B`), e eles devem obedecer 3 regras:

**1. composição** - se há uma seta `A` para `B` e de `B` para `C`, então existe uma seta de `A` para `C`, que seria sua composição:

```
f : A → B
g : B → C

composition : A → C
composition(a) = g(f(a))
```

**2. identidade** - a identidade em categorias é um pouco diferente da identidade que conhecemos por função... Basicamente ela faz com que a seta do objeto retorne para si mesma.

**3. composição associativa** - isso quer dizer que a composição de `a . b . c` deve ser igual a `(a . b) . c` ou `a . (b . c)`.

Agora, vamos criar uma categoria usando typeclasses:

```hs
{-# LANGUAGE KindSignatures #-}

class Cat (cat :: * -> * -> *) where
    id_   :: cat a a
    (...) :: cat b c -> cat a b -> cat a c
```

A anotação `cat :: * -> * -> *` quer dizer que `cat` deve ser um tipo que recebe 2 argumentos. E o `KindSignatures` é a extensão que nos permite escrever explicitamente o tipo de `cat`. Agora, vamos mostrar um exemplo:

```hs
data Foo a b = Foo a b deriving (Show)

instance Cat Foo where
    id_ = Foo undefined undefined -- tem que ser um bottom, porque como o a é 
                                  -- quantificado (não vem de nenhum 
                                  -- arg de fora), ele tem que ser de todos os
                                  -- tipos, e não um dinâmico como o cat
    (...) f g = \x -> f (g x) -- associativo

f :: Cat cat => cat a a
f = id_

g :: Foo a a
g = id_
```

Agora, para finalizar, vamos definir o comportamento da nossa categoria para funções, porém, iremos nos aprofundar mais quando estivermos falando sobre arrows. Vamos ver:

```hs
instance Cat ((->)) where
    id_ = id
    (...) f g = \x -> f (g x)

test :: Num a => a -> a
test = (+1) ... (+2)

test 6
-- 9
```

Viu como basicamente definimos o comportamento da nossa typeclass para funções?

## lazy programming

## quantificação

## type-level programming

## coisas específicas de Haskell

## recursion schemes

## lenses

## tipos dependentes

## livros recomendados

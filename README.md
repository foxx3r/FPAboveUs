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
    * [o que é programação lógica?](#o-que-e-programacao-logica)
    * [a linguagem prolog](#a-linguagem-prolog)
    * [predicados](#predicados)
    * [modus ponens](#modus-ponens)
    * [backtracking](#backtracking)
    * [resolução SLD](#resolucao-sld)
    * [cut, negação e a resolução SLDNF](#cut-negacao-e-resolucao-sldnf)
* [programação funcional no geral](#programacao-funcional-no-geral)
    * [morfismo](#morfismo)
    * [polimorfismo](#polimorfismo)
    * [função id](#funcao-id)
    * [isomorfismo](#isomorfismo)
    * [pattern matching](#pattern-matching)
    * [composição](#composicao)
    * [lifting](#lifting)
    * [curry e point free / programação tácita](#programacao-tacita)
    * [Higher Order Functions (HOF) & closures](#higher-order-functions)
    * [recursão](#recursao)
    * [tail call recursion & tail call optimization](#tail-call-recursion)
    * [total functions & partial functions](#total-functions)
    * [list comprehension](#list-comprehension)
    * [map](#map)
    * [filter](#filter)
    * [fold / reduce](#fold-reduce)
    * [zip](#zip)
    * [continuation passing style (CPS)](#continuation-passing-style)
    * [tipos em haskell](#tipos-em-haskell)
* [introdução a teoria das categorias](#introducao-a-teoria-das-categorias)
    * [o que é uma categoria?](#o-que-e-uma-categoria)
    * [endomorfismo](#endomorfismo)
    * [idempotência](#idempotencia)
    * [monomorfismo](#monomorfismo)
    * [o que são domínios e codomínios?](#o-que-sao-dominios-e-codominios)
    * [setóide](#setoide)
    * [semigrupo](#semigrupo)
    * [bijeção](#bijecao)
    * [injeção](#injecao)
    * [surjeção](#surjecao)
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

Aiai... LISP, o que eu posso dizer desta `((((((((maravilhosa))))))))` linguagem? Basicamente, LISP foi a segunda linguagem criada no mundo, a primeira linguagem interpretada, a primeira linguagem a ter um GC, a primeira linguagem homoicônica, a primeira linguagem com computação simbólica, a primeira linguagem a ter if, a primeira linguagem a ter meta-programação, a primeira linguagem funcional, a primeira linguagem reflexiva, a pioneira em linguística e inteligência artificial, sem falar das LISPs machines, além de seu inventor ter inventado o time sharing, impulsionou a criação de DSLs (linguagens de domínio específico), pioneira em recursão, estruturas de dados, self-hosting compiler, tipagem dinâmica... E não para por aí. Se eu quisesse, faria um artigo inteiro sobre LISP e o John McCarthy.

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

1. Uma variável pode ser uma função

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

2. Passar uma função como argumento

```hs
map (+ 1) [1..10]
```

Aonde map aplica +1 a todos os elementos da lista. Aonde + é uma função que recebeu o argumento 1 (e por causa do currying, podemos passar argumentos incompletos, mas iremos falar disso mais a frente) e uma lista de 1 a 10. Iremos ver mais sobre map/fmap no capítulo sobre functors, e funções como argumentos no capítulo sobre Higher Order Functions (HOF).

3. Retornar uma função

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

O termo side effet qyer dizer que uma função pode ter efeitos colaterais, e geralmente elas são classificadas com as seguintes regras:

1. Mudar o valor de uma variável
2. Escrever/ler dados do disco
3. Escrever na GUI

E linguagens funcionais não tem side effets... Mas pera, não dá para programar em GUI ou escrever/ler do disco??? Calma, Haskell tem side effects controlados pela monad IO.

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

## programação funcional no geral

## introdução a teoria das categorias

## lazy programming

## quantificação

## type-level programming

## coisas específicas de Haskell

## recursion schemes

## lenses

## tipos dependentes

## livros recomendados

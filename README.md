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
* [instalação](instalacao)
    * [instalando o stack](instalando-o-stack)
    * [configurando o stack](configurando-o-stack)
    * [stack vs cabal](stack-vs-cabal)
    * [instalando o SWI-prolog](instalando-o-swi-prolog)
    * [instalando o Emacs](instalando-o-emacs)
        * [Windows](emacs-windows)
        * [Unix](emacs-unix-like)
    * [configurando o Emacs](configurando-o-emacs)
    * [instalando Agda e o agda-stdlib](instalando-agda-e-o-agda-stdlib)
    * [configurando o nosso .ghci + hoogle](configurando-o-nosso-ghci-e-hoogle)
* [história da programação funcional e do lambda-calculus](historia-da-programacao-funcional)
    * [LISP](lisp)
    * [ML](ml)
    * [evolução do lambda-calculus](evolucao-do-lambda-calculus)
* [características funcionais](caracteristicas-funcionais)
    * [dados imutáveis](dados-imutaveis)
    * [transparência referencial](transparencia-referencial)
    * [sem nulos e exceções](sem-nulos-e-excecoes)
    * [toda função retorna algo](toda-funcao-retorna-algo)
    * [first class functions](first-class-Functions)
    * [sem globais](sem-globais)
    * [pureza](pureza)
    * [side effects](side-effects)
* [lambda-calculus](lambda-calculus)
    * [funções simples](funcoes-simples)
    * [números](numeros)
    * [lógica booleana](logica-booleana)
* [programação lógica](programacao-logica)
    * [o que é programação lógica?](o-que-e-programacao-logica)
    * [a linguagem prolog](a-linguagem-prolog)
    * [predicados](predicados)
    * [modus ponens](modus-ponens)
    * [backtracking](backtracking)
    * [resolução SLD](resolucao-sld)
    * [cut, negação e a resolução SLDNF](cut-negacao-e-resolucao-sldnf)
* [programação funcional no geral](programacao-funcional-no-geral)
    * [morfismo](morfismo)
    * [polimorfismo](polimorfismo)
    * [função id](funcao-id)
    * [isomorfismo](isomorfismo)
    * [pattern matching](pattern-matching)
    * [omposição](composicao)
    * [lifting](lifting)
    * [curry e point free / programação tácita](programacao-tacita)
    * [Higher Order Functions (HOF) & closures](higher-order-functions)
    * [recursão](recursao)
    * [tail call recursion & tail call optimization](tail-call-recursion)
    * [total functions & partial functions](total-functions)
    * [list comprehension](list-comprehension)
    * [map](map)
    * [filter](filter)
    * [fold / reduce](fold-reduce)
    * [zip](zip)
    * [continuation passing style (CPS)](continuation-passing-style)
    * [tipos em haskell](tipos-em-haskell)
* [introdução a teoria das categorias](introducao-a-teoria-das-categorias)
    * [o que é uma categoria?](o-que-e-uma-categoria)
    * [endomorfismo](endomorfismo)
    * [idempotência](idempotencia)
    * [monomorfismo](monomorfismo)
    * [o que são domínios e codomínios?](o-que-sao-dominios-e-codominios)
    * [setóide](setoide)
    * [semigrupo](semigrupo)
    * [bijeção](bijecao)
    * [injeção](injecao)
    * [surjeção](surjecao)
    * [o que é uma operação binária?](o-que-e-uma-operacao-binaria)
    * [o que são funtores?](o-que-sao-funtores)
    * [o que são endofuntores?](o-que-sao-endofuntores)
    * [o que são monóides?](o-que-sao-monoides)
    * [o que são applicative functors?](o-que-sao-applicative-functors)
    * [o que são arrows?](o-que-sao-arrows)
    * [o que são monads?](o-que-sao-monads)
    * [o prefixo co](o-prefixo-co)
    * [comonads](comonads)
    * [transformações naturais](transformacoes-naturais)
    * [produtos e coprodutos](produtos-e-coprodutos)
* [lazy programming](lazy-programming)
    * [o que é laziness?](o-que-e-laziness)
    * [o que é strictness?](o-que-e-strictness)
    * [bang patterns](bang-patterns)
    * [irrefutable patterns](irrefutable-patterns)
    * [o que são thunks?](o-que-sao-thunks)
    * [WHNF](whnf)
    * [day's plot twist: laziness são impuras e seq te permite ter efeitos observáveis](plor-twist)
* [quantificação](quantificacao)
    * [quantificação universal](quantificacao-universal)
    * [quantificação existencial](quantificacao-existencial)
    * [rankNTypes](rankntypes)
    * [Hindley-Milner](hindley-milner)
    * [System F](system-f)
    * [System Fω](system-f-omega)
    * [System FC](system-fc)
    * [ScopedTypeVariables](scoped-type-variables)
    * [tipos impredicativos](tipos-impredicativos)
* [type-level programming](type-level-programming)
    * [typeclasses](typeclasses)
    * [subtipagem](subtipagem)
    * [variância](variancia)
    * [tipos de dados abstratos (ADTs) / sum types, nullary e unários](adts)
    * [Higher Kinded Types (HKTs)](hkts)
    * [sinônimos de tipos](sinonimos-de-tipos)
    * [phantom types](phantom-types)
    * [type roles](type-roles)
    * [Void / bottom types](bottom-types)
    * [o que é unsoundness?](o-que-e-unsoundness)
    * [o que é um sistema de tipos decidível?](o-que-e-sistema-indecidivel)
    * [o problema da parada](o-problema-da-parada)
    * [o que turing-complete tem a ver com o problema da parada?](turing-complete)
    * [absurd](absurd)
    * [bottom values](bottom-values)
    * [tipos de dados algébricos generalizados (GADTs)](gadts)
    * [type families e data families](type-families)
    * [o que é type-level programming?](o-que-e-type-level-programming)
    * [closed type families](closed-type-families)
    * [o que são promotions?](o-que-sao-promotions)
    * [HLists](hlists)
* [coisas específicas de Haskell](coisas-especificas-de-haskell)
    * [boolean blindness](boolean-blindness)
    * [traversable](traversable)
    * [TypeApplications](typeapplications)
    * [FlexibleInstances](flexibleinstances)
    * [FlexibleContexts](flexiblecontexts)
    * [OverloadedStrings](overloadedstrings)
    * [OverloadedLists](overloadedlists)
    * [Text e ByteString](text-e-bytestring)
    * [free monads](free-monads)
    * [funções de ponto fixo](funcoes-de-ponto-fixo)
    * [design by contract](design-by-contract)
* [recursion schemes](recursion-schemes)
    * [catamorfismo](catamorfismo)
    * [anamorfismo](anamorfismo)
    * [hilomorfismo](hilomorfismo)
    * [apomorfismo](apomorfismo)
    * [paramorfismo](paramorfismo)
    * [homomorfismo](homomorfismo)
* [lenses](lenses)
* [tipos dependentes](tipos-dependentes)
    * [o que são tipos dependentes?](o-que-sao-tipos-dependentes)
    * [universos](universos)
    * [refinamento de tipos](refinamento-de-tipos)
    * [singleton types](singleton-types)
* [livros recomendados](livros-recomendados)
    * [what I wish I knew when learning Haskell](wiwikwlh)
    * [haskell programming from first principles](haskell-book)
    * [category theory for programmers](category-theory-for-programmers)
    * [SICP](sicp)
    * [CTMCP](ctmcp)
    * [learn prolog now](lpn)
    * [programming languages foundations in Agda](plfa)
    * [types and programming languages](types-and-programming-languages)
    * [HtDP](htdp)
    * [HoTT](hott)
    * [purely functional data structures](purely-functional-data-structures)

## instalação {#instalacao}

### instalando o stack {#instalando-o-stack}

O stack vai ser o seu segundo melhor amigo daqui pra frente, atrás apenas do GHC. Ele è um gerenciador de pacotes e resolve muitos problemas por você. Para instalá-lo, acesse <https://docs.haskellstack.org/en/stable/README/> e siga o manual de instalação para o seu sistema operacional. Como nem tudo nesta vida é fácil, recomendo você ler a documentação do stack em <https://docs.haskellstack.org/en/stable/GUIDE/> para não ter problemas futuros, e caso os tenha, que saiba resolver. E digo isso por experiência própria :)

### configurando o stack {#configurando-o-stack}

Para configurar o stack, você primeiro deve rodar o comando:

`$ stack setup`

A partir daí, vai demorar um pouco até que a instalação seja concluída. Ele vai instalar todas as ferramentas necessárias para o nosso ambiente Haskell e você pode conferí-las (e no futuro quem sabe poder apagá-las) no diretório `$HOME/.stack`. Caso você por alguma razão queira mudar a versão do GHC, consulte a [questão no stack overflow](https://stackoverflow.com/questions/44346435/change-ghci-version-on-stack) que se trata sobre isso. Caso alguma das libs que formos usar não esteja incluída junto do GHC, tente instalar elas pelo nome, e.g: Control.Comonad seria um `stack (ou cabal) install comonad`. Para as outras libs, procure na internet como instalá-las.

Agora, vamos instalar o cabal. Basicamente, como a Wikipédia diz:

> O Cabal foi introduzido para simplificar o empacotamento de software e módulos Haskell. Ele foi adicionado ao Glasgow Haskell Compiler versão 6.4 como gerenciador de pacotes padrão, junto com o gerenciador interno ghc-pkg do GHC. O binário real cabal e a biblioteca Cabal são desenvolvidas em pacotes diferentes.

Fonte: [Wikipedia-EN](https://en.m.wikipedia.org/wiki/Cabal_(software))

Iremos discutir no capítulo a seguir as diferenças dele pro stack.

### stack vs cabal {#stack-vs-cabal}

Basicamente o stack usa o cabal por baixo, mas usa o stackage como repositório ao invés do hackage como o cabal. E pelo stack usar o cabal por baixo, você não tem perda de compatibilidade. Mas é apenas isso?! Não!! O stack é um cabal melhorado ou mais automatizado. Basicamente o stack evita de você ter as cabal hells, uma dor de cabeça imensa para programadores Haskell no passado! Apesar do cabal ter evoluído bastante nos últimos tempos com o `cabal sandbox` e os comandos new-*, iremos usar o stack neste tutorial. Mas fica a sua escolha.

### instalando o SWI-prolog {#instalando-o-swi-prolog}

Iremos ensinar um pouco sobre a linguagem prolog aqui, e já é bom ter de antemão, o interpretador instalado. Para instalá-lo, confira no site oficial e instale de acordo com o seu sistema operacional (ou você pode pesquisar no gerenciador de pacotes da sua distribuição, mas não é uma coisa que geralmente se recomenda) em <https://www.swi-prolog.org/Download.html>.

### instalando o Emacs {#instalando-o-emacs}

Basicamente o Emacs é um editor de texto muito poderoso (podendo acessar o telegram, músicas, servir como daemon init do sistema, ser usado para fazer programação literária e muito mais), que usa a linguagem Elisp, uma DSL parecida com Common LISP. Caso você esteja acostumado com o VI/VIM/neovim, não se preocupe, ensinaremos a instalar o evil depois, que irá nos permitir usar keybindings (combinações de teclas) iguais as do VI no Emacs. Mas até mesmo usando as keybindings do VI, acho que é importante saber Emacs, portanto, eu recomendo você ler o [tour sob o Emacs](https://www.gnu.org/software/emacs/tour).

Mas por que usar o Emacs? Já que não importa a maneira como seu código será escrito, não é? Mas infelizmente (ou felizmente para você, leitor que está descobrindo novas coisas) a linguagem de programação Agda é muito dependente do Emacs.

#### Windows {#emacs-windows}

Para instalar o Emacs no seu sistema operacional Windows, acesse a [página de download](https://ftp.gnu.org/gnu/emacs/windows/) do projeto GNU e escolha a melhor opção para o seu sistema.

#### Unix {#emacs-unix-like}

Para instalar o Emacs em um sistema unix-like, você pode instalar ele pelo gerenciador de pacotes mesmo que não tem problema. Mas caso não confie no gerenciador de pacotes da sua distribuição, instale pelo [site oficial](https://www.gnu.org/software/emacs/download.html).

### configurando o Emacs {#configurando-o-emacs}

Basicamente, o Emacs tem 3 implementações de gerenciamento de pacotes:

- [GNU ELPA](https://elpa.gnu.org/) talvez o nais popular de todos, e bem pequeno, mantido pelo projeto GNU.
- [MELPA](https://melpa.org) é um repositório não oficial e também é o repósitório com a maior quantidade de pacotes.
    - [MELPA stable](https://stable.melpa.org) é um MELPA que inclui apenas paco5es estáveis. Ele è o que tem menos pacotes de todos.
- [Marmalade](https://marmalade-repo.org) o marmalade è um projeto já morto, e não faz sentido de usarmos aqui. Apesar dele ter sido extensivamente utilizado no passado.

Enfim... Bora parar de falar. Basicamente, o Emacs tem os arquivos de configuração localizados em $HOME/.emacs.d/init.el (ambos Windows e Linux) ou em $HOME/.emacs. Muita gente usa o .emacs, mas o correto seria usar o .emacs.d pois seus arquivos de configuração ficam mais organizados. Após instalar o Emacs, insira isso dentro do `$HOME/.emacs.d/init.el`:

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

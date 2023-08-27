# Projeto Haskell - Sudoku

Programa é capaz de gerar um tabuleiro aleatório e entregar uma das várias soluções possíveis.
Além disso, proporciona a dificuldade do tabuleiro e numero de soluções.

## Precisamos fazer:

> Solver: :done:
Melhor método a utilizar será lista com lista de caracteres. Representando os números de 1 a 9 e vazio como .

> Generator: :done: AAAAAAAAAAAAAA
Sabendo que puzzles devem ter pelo menos 17 casas preenchidas.
Pego tabuleiro vazio, sorteio uma dificuldade.
Insiro o numero de casas de acordo com a dificuldade.
Imprimo o Tabuleiro e sua dificuldade.

--> Definir quantas vezes repetir o processo
--> Impor condição -> Se o elem for =! "." repete a função
--> Sortear elemento a ser colocado
--> Impondo condição, se ja estiver o numero na linha coluna ou quadrado
sorteia outro numero


> Reconhecedor de Dificuldade: suporta gerador

> Validar tabuleiro como possível ou não: 
Verifica se é possivel resolver o tabuleiro
--> adaptar função é valido para retornar se o tabuleiro da pra resolver ou não

> Se Possível, Printar Tabuleiro formatado: :done:
Resolve o tabuleiro e printa uma das soluções

> Printar qtd de Soluções lengh do solve

> Método alternativo, mais integração com o usuario.
Escolha da dificuldade,
Você deseja Resolver o Tabuleiro?
Verificar Solução.
Sim é uma solução possível!

> Main agregando todas essas funções

# Amanha eu me matoooooo yaaaaaaaaaaaay
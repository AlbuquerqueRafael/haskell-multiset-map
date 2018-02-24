# haskell-multiset-map

# Como rodar

### 1 - Execute o module GenerateFile.hs 

1.1 - No windows apenas necessita clicar duas vezes no file

1.2 - No Linux precisa rodar utilizando o comando ghci

### 2 - Após executado, utilize o comando main "umastringqualquer"

2.1 - Após a execução do comando será criado um arquivo json com nome "result.json". O arquivo em questão contém informações acerca da execução dos testes

2.2 - A chave matricula será igual ao parametro passado na função main.

2.2.1 - Ex: main "22343234" -> "matricula" : "22343234"

# Como rodar o script

### 1 - Execute o module script.hs

1.1 - No windows apenas necessita clicar duas vezes no file

1.2 - No Linux precisa rodar utilizando o comando ghci

### 2 - Após executado, utilize o comando main
2.1 - Após a execução do comando será rodado todos os testes presentes no diretório alunos.

2.2 – Os resultados dos testes serão escritos em arquivos json’s no diretório results. Os nomes dos arquivos de resultados serão iguais aos nomes do arquivos de testes do diretório alunos.

Ex: Temos o arquivo 122343543.hs. O teste será rodado e os resultados serão escritos em: 122343543.hs.json(a retirada no .hs está entre as melhorias a serem feitas na próxima versão) no mesmo formato explicado na seção inicial.


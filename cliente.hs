module Cliente where

type Chave = Int
type Nome = String			
type Idade = Int 			
type Num_Conta = Int 		
type Valor = Double			
type NI = Int 				
type Dat_Nasc = String  	
type Num_Tel = Int 			
type Dados = (Chave, Nome, NI, Idade, Dat_Nasc, Num_Conta, Valor, Num_Tel)  --Tupla dos Dados
type Pessoa = [Dados]       -- Lista de Tuplas de Dados

listar :: Pessoa -> Pessoa 	--Lista todos elementos Cadastrados
listar lista 
			|((lista_Vazia lista) == True) = []
			| otherwise = lista

tamanho_da_Lista :: Pessoa -> Int 	--Retorna o tamanho da lista
tamanho_da_Lista [] = 0
tamanho_da_Lista (x:xs) = (1 + tamanho_da_Lista xs)

lista_Vazia :: Pessoa -> Bool	--Verifica se a Base de Dados esta Vazia
lista_Vazia [] = True
lista_Vazia _ = False

pesquisa_Chave :: Pessoa -> Chave -> Bool 	--Verifica se uma numero de identificacao existe
pesquisa_Chave [] _ = False
pesquisa_Chave ((chave, _, _, _, _, _, _, _):xs) chav
			|(chave == chav) = True
			|otherwise = pesquisa_Num_Ident xs chav

existe_Nome :: Pessoa -> Nome -> Bool 	--Verificar se um nome ja existe
existe_Nome [] novoNome = False
existe_Nome ((_, nome, _, _, _, _, _, _):xs) novoNome
			|(nome == novoNome) = True
			|otherwise = existe_Nome xs novoNome

pesquisa_Num_Ident :: Pessoa -> NI -> Bool 	--Verifica se uma numero de identificacao existe
pesquisa_Num_Ident [] _ = False
pesquisa_Num_Ident ((_, _, ni, _, _, _, _, _):xs) num_i
			|(ni == num_i) = True
			|otherwise = pesquisa_Num_Ident xs num_i


pesquisa_Num_Conta :: Pessoa -> Num_Conta -> Bool 	--Verifica se um Número de Conta ja Existe
pesquisa_Num_Conta [] _ = False
pesquisa_Num_Conta ((_, _, _, _, _, num_conta, _, _):xs) novo_num_conta
			|(num_conta == novo_num_conta) = True
			|otherwise = pesquisa_Num_Conta  xs novo_num_conta

pesquisa_Num_Tel :: Pessoa ->Num_Tel -> Bool 	--Verifica se um Numero de Telefone ja Existe
pesquisa_Num_Tel [] _ = False
pesquisa_Num_Tel ((_, _, _, _, _, _, _, num_tel):xs) novo_num_tel
			|(num_tel == novo_num_tel) = True
			|otherwise = pesquisa_Num_Tel  xs novo_num_tel


existe_Num_Conta :: Pessoa -> Num_Conta -> Bool 		--verifica se o numero da conta informada existe no cadastro
existe_Num_Conta [] _ = False
existe_Num_Conta ((ch, no, ni, id, dat, num_conta, valor, num_t):xs) num
			|(num_conta == num) = True
			|otherwise = existe_Num_Conta xs num


teste_do_valor_a_Levantar :: Pessoa -> Num_Conta -> Valor -> Bool  --Garante que o Cliente não Levante um valor a cima do seu Saldo
teste_do_valor_a_Levantar [] _ _ = False
teste_do_valor_a_Levantar ((ch, no, ni, id, dat, num_conta, valor, num_t):xs) num_C valor_Lev
			|((num_conta == num_C) && (valor > valor_Lev)) = True
			|otherwise = teste_do_valor_a_Levantar xs num_C valor_Lev


funcao_de_Busca :: Pessoa -> Chave -> Dados 	--Busca um unico elemento
funcao_de_Busca [] _ = error "Lista Vazia"
funcao_de_Busca ((ch, no, ni, id, dat, num, valor, num_t):xs) chave
			| (ch == chave) = (ch, no, ni, id, dat, num, valor, num_t)
			| otherwise = funcao_de_Busca xs chave


busca_nome :: Pessoa -> Chave -> IO Nome 	--Busca o Nome 
busca_nome [] _ = error "Lista Vazia"
busca_nome ((ch, no, ni, id, dat, num, valor, num_t):xs) chave = do			
			if(ch == chave) then do
				return no
			else do
				busca_nome xs chave


busca_nome_2 :: Pessoa -> Num_Conta -> IO Nome 	--Busca o Nome 
busca_nome_2 [] _ = error "Lista Vazia"
busca_nome_2 ((ch, no, ni, id, dat, num, valor, num_t):xs) num_C = do			
			if(num == num_C) then do
				return no
			else do
				busca_nome_2 xs num_C

busca_NI :: Pessoa -> Chave -> IO NI 	--Busca Numero de Identificacao
busca_NI [] _ = error "Lista Vazia"
busca_NI ((ch, no, ni, id, dat, num, valor, num_t):xs) chave = do			
			if(ch == chave) then do
				return ni
			else do
				busca_NI xs chave

busca_idade :: Pessoa -> Chave -> IO Idade 	--Busca a Idade
busca_idade [] _ = error "Lista Vazia"
busca_idade ((ch, no, ni, id, dat, num, valor, num_t):xs) chave = do			
			if(ch == chave) then do
				return id
			else do
				busca_idade xs chave


busca_data :: Pessoa -> Chave -> IO Dat_Nasc 	--Busca a Data de Nascimento
busca_data [] _ = error "Lista Vazia"
busca_data ((ch, no, ni, id, dat, num, valor, num_t):xs) chave = do			
			if(ch == chave) then do
				return dat
			else do
				busca_data xs chave


busca_num_da_Conta :: Pessoa -> Chave -> IO Num_Conta 	--Busca o Numero da Conta
busca_num_da_Conta [] _ = error "Lista Vazia"
busca_num_da_Conta ((ch, no, ni, id, dat, num_conta, valor, num_t):xs) chave = do			
			if(ch == chave) then do
				return num_conta
			else do
				busca_num_da_Conta xs chave


busca_valor :: Pessoa -> Chave -> IO Valor 	--Busca o Numero da Conta
busca_valor [] _ = error "Lista Vazia"
busca_valor ((ch, no, ni, id, dat, num_conta, valor, num_t):xs) chave= do			
			if(ch == chave) then do
				return valor
			else do
				busca_valor xs chave


busca_num_tel :: Pessoa -> Chave -> IO Num_Tel 	--Busca o Numero de Telefone
busca_num_tel [] _ = error "Lista Vazia"
busca_num_tel ((ch, no, ni, id, dat, num, valor, num_t):xs) chave = do			
			if(ch == chave) then do
				return num_t
			else do
				busca_num_tel xs chave

eliminar :: Pessoa -> Chave -> Pessoa  --Elimina um elemento e retorna uma lista nova
eliminar [] _ = error "Lista Vazia"
eliminar ((ch, no, ni, id, dat, num, valor, num_t):xs) chave
			| (ch == chave) = xs
			| otherwise = (ch, no, ni, id, dat, num, valor, num_t):(eliminar xs chave)


atualizar :: Pessoa -> Chave -> Dados -> Pessoa 	--Atualiza os dados do Cliente
atualizar [] _ _= error "Lista Vazia"
atualizar ((chav, nome, num_id, idade, dat_nasc, num_conta, valor, num_tel):xs) chave dados
			| (chav == chave) = [dados]++xs
			| otherwise = (chav, nome, num_id, idade, dat_nasc, num_conta, valor, num_tel):(atualizar xs chave dados)
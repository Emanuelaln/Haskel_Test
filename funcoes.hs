module Funcoes where
--ESTE MODULO CONTEM AS FUNCOES COMPLEMENTARES PARA O FUNCIONAMENTO DO SISTEMA
import System.IO
import Data.Unique
import System.IO.Error
import qualified Senhas
import qualified Cliente
import qualified Verificacao
import System.Random (randomRIO) 
import qualified Control.Exception
 

gerando_chave :: Cliente.Pessoa -> IO Cliente.Chave
gerando_chave lista = do 
			chave <- newUnique
			if((Cliente.pesquisa_Chave lista (hashUnique chave)) == True) then do 
				gerando_chave lista
			else do 
				return (hashUnique chave)

incrementar :: Int -> Int 	--Funcão de incremento padrão
incrementar a = a+1

gerar_conta :: Cliente.Pessoa -> IO Int
gerar_conta lista = do
		{catch(ler_arquivo) tratar_erro;} --Verifica se ja existe um arquivo, caso nao captura o erro 
		where
			ler_arquivo = do 			--Le o arquivo caso ja exista
			{
				arquivo <- openFile "conta.txt" ReadMode;	--Arquivo da lista de clientes
				conta <- hGetLine arquivo;
				hClose arquivo;

				gerando_Numero_da_Conta lista (read conta)
				
			}
			tratar_erro erro = if isDoesNotExistError erro then do --Caso nao exista Arquivo algum Esta funcao Cria
			{
				arquivo <- openFile "conta.txt" WriteMode;		--Cria Arquivo da lista de clientes
				hPutStrLn arquivo "101";
				hClose arquivo;

				gerando_Numero_da_Conta lista 101
				
			}
			else
				ioError erro


gerando_Numero_da_Conta :: Cliente.Pessoa -> Int -> IO Int	--Gera um Número da Conta unico
gerando_Numero_da_Conta lista num_conta = do		
			if((Cliente.pesquisa_Num_Conta lista num_conta)==True) then do --Garaante que haja Numeros unicos
				gerando_Numero_da_Conta lista (incrementar num_conta)
			else do

				arq <- openFile "dados.txt" WriteMode   --Abre o Ficheiro para Escrita
				hPutStrLn arq (show num_conta)   --Escreve no Ficheiro
				hClose arq 

				return num_conta

gerando_numero_de_Telefone :: Cliente.Pessoa -> IO Cliente.Num_Tel 	--Gera um Número de Telefone unico
gerando_numero_de_Telefone lista = do
			putStr "\nNumero de Telefone : "
			num_tel <- getLine
			if((Cliente.pesquisa_Num_Tel lista (read num_tel))==True) then do --Garaante que haja numeros unicos
				putStr "\nEste Numero ja Foi Cadastrado\n\nPrime Enter e Tente Novamente\n"
				getChar
				gerando_numero_de_Telefone lista
			else do
				return (read num_tel)


login_nome :: String -> Int -> IO String	--valorerifica se o nome introduzido bate com o nome de usuario contido no arquivo
login_nome nome cont = do 		
		if (cont == 0) then do 	
			putStr "\nOpção Cancelada\nTente de Novo Mais Tarde\nClique Enter\n"
			getChar
			return "0"
		else do
			putStr "\nNome : "
			nome_l <- getLine
			if(nome_l == nome) then do	
				return nome		--caso bata entao retorna o nome como verdadeiro	
			else do
				putStrLn "\nUtilizador Invalido\nPressione Enter e Tenta de Novo\n"
				getChar
				login_nome nome (Verificacao.decrementar cont) --caso nao Volta a introduzir o nome ate acertar nas 3 tentativas, ou cancela a operacao
					
login_senha :: String -> Int -> IO String	--valorerifica se a senha introduzida bate com a senha contida no arquivo
login_senha senha cont = do 		
		if (cont == 0) then do 	
			putStr "\nOpção Cancelada\nTente de Novo Mais Tarde\nClique Enter\n"
			getChar
			return "0"
		else do
			putStr "\nSenha : "
			senha_l <- getLine
			if(senha_l == senha) then do --Caso bata, retorna a senha como verdadeira	
				return senha_l			
			else do
				putStrLn "\nSenha Incorrecta\nPressione Enter e Tenta de Novo\n"
				getChar
				login_senha senha (Verificacao.decrementar cont) --caso nao volta a tentar ate acertar nas 3 tentativas, ou cancela a operacao

funcao_chamar :: Senhas.Fila -> IO Char
funcao_chamar fila = do
		if((Senhas.listaVazia fila)==True) then do
			putStr "Lista Vazia\nClique Enter para voltar ao Menu"
			getChar
			return '0'
		else do	
			opcao <- randomRIO(1,6::Int)
			if ((Senhas.auxiliar_ao_randomico fila opcao)== True) then do
				senha <- (Senhas.chamar fila opcao)
				putStrLn (", Chamando Senha "++senha++"\nClique Enter para Efectuar a Operacao")
				getChar

				arquivo2 <- openFile "fila.txt" WriteMode   --Abre o Ficheiro para Escrita
				hPutStrLn arquivo2 (show(Senhas.eliminar_senha fila senha))   --Escreve no Ficheiro
				hClose arquivo2 
				return opcao
				case (opcao) of 
					0 -> return '0'
					1 -> return '1'
					2 -> return '2'
					3 -> return '3'
					4 -> return '4'
					5 -> return '5'
					6 -> return '6'
					7 -> return '7'
			else do
				funcao_chamar fila

listar_senhas :: Senhas.Fila -> IO Senhas.Fila
listar_senhas fila = do
		putStrLn ("\n\nTotal de Senhas: "++(show(Senhas.tamanho_da_Lista fila))++"\n\nSenhas : "++(show(Senhas.listarCadastro fila)))
		putStr "\n\nPressione Enter Para Voltar ao Menu "
		getChar
		return fila

criar_conta :: Cliente.Pessoa -> IO Cliente.Pessoa  --cadastra nova conta
criar_conta lista = do 
		putStr "\n=====================================\n\tCadastrando Novo Cliente\n=====================================\n"
		nome <- Verificacao.verifica_nome lista 3
		if(nome == "0") then do 
			return lista
		else do	
			putStr "\nNumero de Identificacao :"
			num_id <- getLine
			putStr "\nData de Nascimento :"
			dia <- Verificacao.verifica_dia 3	--garante o dia seja valido no > que 0 e < que 32
			if (dia == "0") then do 		--caso seja invalido retorna 0 e cancela a operacao
				return lista
			else do
				mes <- Verificacao.verificar_mes 3	--garante o mes seja valido no > que 0 e < que 13
				if(mes == "0") then do 			--caso seja invalido retorna 0 e cancela a operacao
					return lista
				else do 
					ano <- Verificacao.verifica_ano 3	--garante o ano seja valido no > a 1930 e < que 2004
					if (ano == "0") then do 		--caso seja invalido retorna 0 e cancela a operacao
						return lista
					else do 
						let data_nasc = "("++dia++"/ "++mes++"/ "++ano++")"
						let idade = (2021 - (read ano))
							
						chave <- gerando_chave lista
						num_conta <- gerar_conta lista
						num_tel <- gerando_numero_de_Telefone lista
						let valor = 0.0 --valor em dinheiro na conta

						putStrLn ("\n=====================================\n\tSeus Dados\n\n")
						putStrLn ("\tNome : "++ nome ++"\n\tNumero de Identificacao : "++num_id++"\n\tIdade : "++ (show idade) ++"\n\tData de Nascimento : "++ data_nasc ++"\n\tNumero da Conta : "++ (show num_conta) ++"\n\tValor : " ++ (show valor) ++"\n\tNumero de Telefone : "++ (show num_tel))
					
						--dados e um dado abstrato que recebe todos dados do cliente
						dados <- menu_Confirmar_Atualizar lista chave nome (read num_id) idade data_nasc num_conta valor num_tel	
						putStrLn ("\nCliente Cadastrado com Sucesso")

						--ARQUIVO
						arq <- openFile "dados.txt" WriteMode   --Abre o Ficheiro para Escrita
						hPutStrLn arq (show(lista ++[dados]))   --Escreve no Ficheiro
						hClose arq 								--Fecha o Ficheiro

						putStr "\n\n====================================\nClique Enter Para Voltar ao Menu "
						getChar
						getChar
						return (lista ++[dados]) --retorna para o menu uma lista nova

listar_um_cliente :: Cliente.Pessoa -> IO Cliente.Pessoa 	--lista todos elementos cadastrados
listar_um_cliente lista = do
		putStr "\n=====================================\nInforme a Chave: "
		chave <- getLine 
		if ((Cliente.pesquisa_Chave lista (read chave)) == True) then do
			putStrLn ("\n=====================================\nResultado da Busca\n=====================================\n")
			putStrLn ("\n\t\tChave Encontrada \n\nListando O Elemento : ("++(show(Cliente.funcao_de_Busca lista (read chave)))++")")
			putStr "\n=====================================\nClique Enter Para Voltar ao Menu"
			getChar
			return lista
		else do	
			putStrLn ("\n=====================================\nResultado da Busca\n=====================================\nSenha Não Encontrada\n\n=====================================\nClique Enter para Continuar ")
			getChar
			return lista --retorna para o menu

eliminar_conta :: Cliente.Pessoa -> IO Cliente.Pessoa 	--Elimina Um determinado elemento
eliminar_conta  lista = do
		putStr "\n=====================================\nInforme o Numero de Identificacao: "
		chave <- getLine 
		if ((Cliente.pesquisa_Chave lista (read chave)) == True) then do
			putStrLn ("\n==========================================================================\nResultado da Busca\n==========================================================================\n")
			putStrLn ("\n\t\tSenha Encontrada \n\n\tCerteza que Quer Eliminar : ("++(show(Cliente.funcao_de_Busca lista (read chave)))++") ?")
			pequeno_menu lista (read chave)		
		else do	
			putStrLn ("\n=====================================\nResultado da Busca\n==========================================================================\n")
			putStrLn ("\nSenha Não Encontrada\n=====================================\nClique Enter para Continuar ")
			getChar
			return lista --retorna para o menu

atualizar_conta :: Cliente.Pessoa -> IO Cliente.Pessoa 	--Actualiza um determinado elemento
atualizar_conta lista  = do
		putStr "\n=====================================\nInforme o Numero de Identificacao: "
		chave <- getLine 

		if ((Cliente.pesquisa_Chave lista (read chave)) == True) then do
			putStrLn ("\n=====================================\nResultado da Busca\n=====================================\n")
			putStrLn ("\n\t\tSenha Encontrada \n\nO Elemento : "++(show(Cliente.funcao_de_Busca lista (read chave)))++"\n")
			
			a <- Cliente.busca_nome lista (read chave)  		-- a recebe o nome do cliente cujo a Chave e igual a Chav
			b <- Cliente.busca_NI lista (read chave)			-- b recebe o numero de identificao
			c <- Cliente.busca_idade lista (read chave)			-- c recebe a idade do cliente cujo a Chave e igual a Chav
			d <- Cliente.busca_data lista (read chave)			-- d recebe a data de nascimento do cliente cujo a Chave e igual a Chav
			e <- Cliente.busca_num_da_Conta lista (read chave)	-- r recebe o numero da conta do cliente cujo a Chave e igual a Chav
			f <- Cliente.busca_valor lista (read chave)				-- f recebe o valor do Cliente cuja a Chave é igual a Chav
			g <- Cliente.busca_num_tel lista (read chave)		-- h recebe o numero de telefone do cliente cujo a Chave e igual a Chav

			dados <- menu_Actual lista (read chave) a b c d e f g --atualizacao dos Dados
					
			arq <- openFile "dados.txt" WriteMode   --Abre o Ficheiro para Escrita
			hPutStrLn arq (show(lista++[dados])) --Escreve no Ficheiro
			hClose arq

			putStr "\nAtualizacao Bem Sucedida\n\n\tSeus Dados\n\n"
			putStrLn("\nDados :(Nome, Numero de Identificacao, Idade, Data de Nascimento, Numero da Conta, Numero de Telefone")
			putStrLn ("\t"++(show dados))
			putStr "\n=====================================\nClique Enter para Continuar "
			getChar	
			return (Cliente.atualizar lista (read chave) dados) -- retorna a nova lista para o 		
		else do	
			putStrLn ("\n=====================================\nResultado da Busca\n=====================================\nSenha Não Encontrada\n\n=====================================\nClique Enter para Continuar ")
			getChar
			return lista --retorna para o menu


--menu auxiliar 1 para o cadstro de cliente
menu_Confirmar_Atualizar :: Cliente.Pessoa -> Cliente.Chave -> Cliente.Nome -> Cliente.NI -> Cliente.Idade -> Cliente.Dat_Nasc -> Cliente.Num_Conta -> Cliente.Valor -> Cliente.Num_Tel -> IO Cliente.Dados
menu_Confirmar_Atualizar lista a b c d e f g h = do
		putStr ("\n\t1 - Concluir \t\t2 - Atualizar\n\n\tDigite a Opcao : ")
		opcao <- getChar
		opcao_menu_Confirmar_Atualizar lista opcao a b c d e f g h

--opcoes do menu auxiliar 1
opcao_menu_Confirmar_Atualizar :: Cliente.Pessoa -> Char -> Cliente.Chave -> Cliente.Nome -> Cliente.NI -> Cliente.Idade -> Cliente.Dat_Nasc -> Cliente.Num_Conta -> Cliente.Valor -> Cliente.Num_Tel -> IO Cliente.Dados
opcao_menu_Confirmar_Atualizar lista '1' a b c d e f g h = do
		return (a,b,c,d,e,f,g, h)
opcao_menu_Confirmar_Atualizar lista '2' a b c d e f g h = menu_Actual lista a b c d e f g h
opcao_menu_Confirmar_Atualizar lista _ a b c d e f g h= do
		putStrLn ("\nOpção Invalida\nPressione Enter Para Voltar ao Menu ")
		getChar
		menu_Confirmar_Atualizar lista a b c d e f g h

pequeno_menu :: Cliente.Pessoa -> Cliente.Chave -> IO Cliente.Pessoa 	--Menu auxiliar 2 para Eliminar Elementos
pequeno_menu lista chave = do
		putStr ("\n\t1 - Para Eliminar Elemento\t\t2 - Para Cancelar Operacao\n\n\tDigite a Opcao : ")
		opcao <- getChar
		opcoa_pequeno_menu lista opcao chave

opcoa_pequeno_menu :: Cliente.Pessoa -> Char -> Cliente.Chave -> IO Cliente.Pessoa 	--Opcoes do menu auxiliar 2
opcoa_pequeno_menu lista '1' chave = do
		putStrLn ("\n\tElemento : ("++(show(Cliente.funcao_de_Busca lista chave))++") Eliminado Com Sucesso")
		putStr "\n=====================================\nClique Enter Para Voltar ao Menu "
		getChar
		getChar

		--ARQUIVO
		arq <- openFile "dados.txt" WriteMode   --Abre o Ficheiro para Escrita
		hPutStrLn arq (show((Cliente.eliminar lista chave)))   --Escreve no Ficheiro
		hClose arq 
		return (Cliente.eliminar lista chave)

opcoa_pequeno_menu lista '2' chave = do
		putStrLn ("\n\t\tOperação Cancelada")
		putStr "\n=====================================\nClique Enter Para Voltar ao Menu "
		getChar
		return lista
opcoa_pequeno_menu lista _ chave = pequeno_menu lista chave

--menu auxiliar 3 para atualizacao de dados
menu_Actual :: Cliente.Pessoa -> Cliente.Chave -> Cliente.Nome -> Cliente.NI -> Cliente.Idade -> Cliente.Dat_Nasc -> Cliente.Num_Conta -> Cliente.Valor -> Cliente.Num_Tel -> IO Cliente.Dados
menu_Actual lista ch no ni id dat numC val num = do
		putStrLn "\t===========================================\n\t\t\tAtualizar\n\t==========================================="
		putStrLn "\t\t1  Atualizar Nome\n\t\t2  Atualizar Data de Nacimento\n\t\t3  Atualizar Numero de Telefone\n\t\t0  Concluir\n\t===========================================\n\tOpção: "
		op <- getChar
		getChar
		executarActual lista op ch no ni id dat numC val num

--opcoes do menu auxiliar 3
executarActual :: Cliente.Pessoa -> Char -> Cliente.Chave -> Cliente.Nome -> Cliente.NI -> Cliente.Idade -> Cliente.Dat_Nasc -> Cliente.Num_Conta -> Cliente.Valor -> Cliente.Num_Tel -> IO Cliente.Dados
executarActual lista '0' ch no ni id dat numC val num = do
		return (ch, no,ni,id,dat,numC,val,num)
executarActual lista '1' ch no ni id dat numC val num = do
		putStrLn("Nome a Mudar : "++no)
		novo_nome <- Verificacao.verifica_nome lista 3
		if (novo_nome == "0") then do 
			menu_Actual lista ch no ni id dat numC val num
		else do
			putStr "Alteração  Feita com Sucesso\nPressione Enter Para Voltar ao Menu "
			getChar
			menu_Actual lista ch novo_nome ni id dat numC val num 

executarActual lista '2' ch no ni id dat numC val num = do 	--actualizar data de nascimento
		putStr "\nData de Nascimento :"
		dia <- Verificacao.verifica_dia 3
		if (dia == "0") then do 
			menu_Actual lista ch no ni id dat numC val num
		else do
			mes <- Verificacao.verificar_mes 3
			if(mes == "0") then do 
				menu_Actual lista ch no ni id dat numC val num
			else do 
				ano <- Verificacao.verifica_ano 3
				if (ano == "0") then do 
					menu_Actual lista ch no ni id dat numC val num
				else do 
					let nova_data = dia++"/ "++mes++"/ "++ano
					let nova_idade = (2021 - (read ano))
					putStr "\nPressione Enter Para Voltar ao Menu "
					getChar
					menu_Actual lista ch no ni nova_idade nova_data numC val num 

executarActual lista '3' ch no ni id dat numC val num = do
		putStrLn("Numero a Mudar : "++(show num))
		putStr "Novo Numero : "
		novo_num <- getLine
		putStr "Alteração  Feita com Sucesso,\nPressione Enter Para Voltar ao Menu "
		getChar
		menu_Actual lista ch no ni id dat numC val (read novo_num) 

executarActual lista _ ch no ni id dat numC val num = do
		putStrLn ("\nOpção Invalida, Pressione Enter Para Voltar ao Menu ")
		getChar
		menu_Actual lista ch no ni id dat numC val num
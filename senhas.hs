module Senhas where
--ESTE MODULO CONTEM TODAS OPERACOES SOBRE AS SENHAS NA FILA DE ESPERA
import System.IO
import System.IO.Error
import System.Random (randomRIO)
import qualified Control.Exception

type Fila = [Se]	  		--Lista de Senhas
data Se = Senha String 	  	--Tipo algebrico formato da senha
	deriving (Show, Read)   --Para leitura e apresentacao da senha

go :: IO () 			--Dá inicio a ao menu das senhas
go = do
		{catch(ler_arquivo) tratar_erro;} --Verifica se ja existe um arquivo, caso nao captura o erro 
		where
			ler_arquivo = do 			--Le o arquivo caso ja exista
			{
				arquivo <- openFile "fila.txt" ReadMode;	--Arquivo da lista de senhas
				fila <- hGetLine arquivo;
				hClose arquivo;

				arquivo2 <- openFile "controla_senha.txt" ReadMode;	--Arquivo da lista de senhas
				fila_2 <- hGetLine arquivo2;
				hClose arquivo2;

				menu_fila (read fila_2) (read fila) 0 0 0 0 0 0;	 --A fila começa sempre vazia
				return ()
			}
			tratar_erro erro = if isDoesNotExistError erro then do --Caso nao exista Arquivo algum Esta funcao Cria
			{
				arquivo <- openFile "fila.txt" WriteMode;		--Cria Arquivo da lista de clientes
				hPutStrLn arquivo "[]";
				hClose arquivo;

				arquivo2 <- openFile "controla_senha.txt" WriteMode;		--Cria Arquivo da lista de clientes
				hPutStrLn arquivo2 "[]";
				hClose arquivo2;

				menu_fila [] [] 0 0 0 0 0 0;	--A fila começa sempre vazia 
				return ()
			}
			else
				ioError erro


pega_do_ficheiro :: IO Fila
pega_do_ficheiro = do 
	  	arquivo2 <- openFile "fila.txt" ReadMode;	--Arquivo da lista de senhas
		fila <- hGetLine arquivo2;
		hClose arquivo2;
		return (read fila)

listaVazia :: Fila -> Bool 	--Verifica se a lista está vazia
listaVazia [] = True
listaVazia _ = False

existe_senha :: Fila -> String -> Bool
existe_senha [] _ = False
existe_senha ((Senha senha):xs) se
		|(senha == se) = True
		|otherwise = existe_senha xs se

pertence :: String -> Char -> Bool --Verifica na string de senha se o primeiro caracter bate com o caracter de teste
pertence [] char = error ""
pertence (x:xs) char 
		|(x == char) = True
		|otherwise = False

verificar_randomico :: Fila -> Char -> Bool 	--Verifica existe na lista o randomico a chamar 
verificar_randomico [] char = False
verificar_randomico ((Senha senha):xs) char
		|((pertence senha char) == True) = True
		|otherwise = verificar_randomico xs char


auxiliar_ao_randomico :: Fila -> Int -> Bool 	--Auxilia a funcao verificar randomico
auxiliar_ao_randomico [] _ = error ""
auxiliar_ao_randomico fila 1 = verificar_randomico fila 'A'
auxiliar_ao_randomico fila 2 = verificar_randomico fila 'B'
auxiliar_ao_randomico fila 3 = verificar_randomico fila 'C'
auxiliar_ao_randomico fila 4 = verificar_randomico fila 'D'
auxiliar_ao_randomico fila 5 = verificar_randomico fila 'E'
auxiliar_ao_randomico fila 6 = verificar_randomico fila 'F'

listarCadastro :: Fila -> Fila  	--Lista as senhas existentes
listarCadastro fila
		|((listaVazia fila) == True) = []
		| otherwise = fila

eliminar_senha :: Fila -> String -> Fila 	--Elimina umA senha e retorna uma lista nova
eliminar_senha [] se = error "Lista Vazia"
eliminar_senha ((Senha senha):xs) se
		| (senha == se) = xs
		| otherwise = (Senha senha):(eliminar_senha xs se)


tamanho_da_Lista :: Fila -> Int
tamanho_da_Lista [] = 0
tamanho_da_Lista (x:xs) = (1 + tamanho_da_Lista xs)

incrementar :: Int -> Int 	--Funcão de incremento padrão
incrementar a = a+1	

dando_senha :: Fila -> Char -> IO String 	--Funcao que retorna a senha a chamar
dando_senha [] char = error "Lista Vazia"
dando_senha ((Senha senha):xs) char = do
		if ((pertence senha char) == True) then do 
			return senha
		else do
			dando_senha xs char


chamar :: Fila ->Int -> IO String 	--Funcao que executa a chamada
chamar [] _ = error "Lista Vazia"
chamar fila 1 = do
		senha <- (dando_senha fila 'A')
		putStr "Balcao nº 1"
		return senha
chamar fila 2 = do
		senha <- (dando_senha fila 'B')
		putStr "Balcao nº 2"
		return senha
chamar fila 3 = do
		senha <- (dando_senha fila 'C')
		putStr "Balcao nº 3"
		return senha
chamar fila 4 = do
		senha <- (dando_senha fila 'D')
		putStr "Balcao nº 4"
		return senha
chamar fila 5 = do
		senha <- (dando_senha fila 'E')
		putStr "Balcao nº 5"
		return senha
chamar fila 6 = do
		senha <- (dando_senha fila 'F')
		putStr "Balcao nº 6"
		return senha


criar_conta :: Fila -> Fila -> Int -> Int -> Int -> Int -> Int -> Int -> IO Fila
criar_conta fila fila_2 a b c d e f = do	
		let senha = "A"++(show(incrementar a))
		fila_2 <- pega_do_ficheiro
			
		if((existe_senha fila senha) == True) then do	
			criar_conta fila fila_2 (incrementar a) b c d e f 			
		else do
			putStr "\n=====================================\nVoce Escolheu Abertura de Conta\n"
			putStrLn ("\nSua Senha É : "++senha)
			arq <- openFile "fila.txt" WriteMode   --Abre o Ficheiro para Escrita
			hPutStrLn arq (show(fila_2++[(Senha senha)])) --Escreve no Ficheiro
			hClose arq
			putStr "\nAguarde a Sua Chamada\n=====================================\nClique Enter para Voltar ao Menu"
					
			arq <- openFile "controla_senha.txt" WriteMode   --Abre o Ficheiro para Escrita
			hPutStrLn arq (show(fila++[(Senha senha)])) --Escreve no Ficheiro
			hClose arq

			getChar
			menu_fila (fila++[(Senha senha)]) (fila_2++[(Senha senha)]) (incrementar a) b c d e f


deposito :: Fila -> Fila -> Int -> Int -> Int -> Int -> Int -> Int -> IO Fila
deposito fila fila_2 a b c d e f = do		   		
		let senha = "B"++(show(incrementar b))
		fila_2 <- pega_do_ficheiro

		if((existe_senha fila senha) == True) then do	
			deposito fila fila_2 a (incrementar b) c d e f 
		else do	
			putStr "\n=====================================\nVoce Escolheu Deposito\n"
			putStrLn ("\nSua Senha É : "++senha)
			arq <- openFile "fila.txt" WriteMode   --Abre o Ficheiro para Escrita
			hPutStrLn arq (show(fila_2++[(Senha senha)])) --Escreve no Ficheiro
			hClose arq
			putStr "\nAguarde a Sua Chamada\n=====================================\nClique Enter para Voltar ao Menu"
					
			arq <- openFile "controla_senha.txt" WriteMode   --Abre o Ficheiro para Escrita
			hPutStrLn arq (show(fila++[(Senha senha)])) --Escreve no Ficheiro
			hClose arq

			getChar
			menu_fila (fila++[(Senha senha)])  (fila_2++[(Senha senha)]) a (incrementar b) c d e f


levantamento :: Fila -> Fila -> Int -> Int -> Int -> Int -> Int -> Int -> IO Fila
levantamento fila fila_2 a b c d e f = do	   		
		let senha = "C"++(show(incrementar c))
		fila_2 <- pega_do_ficheiro

		if((existe_senha fila senha) == True) then do	
			levantamento fila fila_2 a b (incrementar c) d e f 
		else do	
			putStr "\n=====================================\nVoce Escolheu Levantamento\n"
			putStrLn ("\nSua Senha É : "++senha)
			arq <- openFile "fila.txt" WriteMode   --Abre o Ficheiro para Escrita
			hPutStrLn arq (show(fila_2++[(Senha senha)])) --Escreve no Ficheiro
			hClose arq

			arq <- openFile "controla_senha.txt" WriteMode   --Abre o Ficheiro para Escrita
			hPutStrLn arq (show(fila++[(Senha senha)])) --Escreve no Ficheiro
			hClose arq

			putStr "\nAguarde a Sua Chamada\n=====================================\nClique Enter para Voltar ao Menu"
			getChar
			menu_fila (fila++[(Senha senha)]) (fila_2++[(Senha senha)]) a b (incrementar c) d e f


transferencia :: Fila -> Fila -> Int -> Int -> Int -> Int -> Int -> Int -> IO Fila
transferencia fila fila_2 a b c d e f = do		
		let senha = "D"++(show(incrementar d))
		fila_2 <- pega_do_ficheiro

		if((existe_senha fila senha) == True) then do
			transferencia fila fila_2 a b c (incrementar d) e f 
		else do	
			putStr "\n=====================================\nVoce Escolheu Transferencia\n"
			putStrLn ("\nSua Senha É : "++senha)
			arq <- openFile "fila.txt" WriteMode   --Abre o Ficheiro para Escrita
			hPutStrLn arq (show(fila_2++[(Senha senha)])) --Escreve no Ficheiro
			hClose arq

			arq <- openFile "controla_senha.txt" WriteMode   --Abre o Ficheiro para Escrita
			hPutStrLn arq (show(fila++[(Senha senha)])) --Escreve no Ficheiro
			hClose arq

			putStr "\nAguarde a Sua Chamada\n=====================================\nClique Enter para Voltar ao Menu"
			getChar
			menu_fila (fila++[(Senha senha)]) (fila_2++[(Senha senha)]) a b c (incrementar d) e f


extrato :: Fila -> Fila -> Int -> Int -> Int -> Int -> Int -> Int -> IO Fila
extrato fila fila_2 a b c d e f = do	
		let senha = "E"++(show(incrementar e))
		fila_2 <- pega_do_ficheiro

		if((existe_senha fila senha) == True) then do
			extrato fila fila_2 a b c d (incrementar e) f 
		else do	
			putStr "\n=====================================\nVoce Escolheu Extrato\n"
			putStrLn ("\nSua Senha É : "++senha)
			arq <- openFile "fila.txt" WriteMode   --Abre o Ficheiro para Escrita
			hPutStrLn arq (show(fila_2++[(Senha senha)])) --Escreve no Ficheiro
			hClose arq

			arq <- openFile "controla_senha.txt" WriteMode   --Abre o Ficheiro para Escrita
			hPutStrLn arq (show(fila++[(Senha senha)])) --Escreve no Ficheiro
			hClose arq

			putStr "\nAguarde a Sua Chamada\n=====================================\nClique Enter para Voltar ao Menu"
			getChar
			menu_fila (fila++[(Senha senha)]) (fila_2++[(Senha senha)]) a b c d (incrementar e) f


consulta :: Fila -> Fila -> Int -> Int -> Int -> Int -> Int -> Int -> IO Fila
consulta fila fila_2 a b c d e f = do	
		putStr "\n=====================================\nVoce Escolheu Consulta\n"
		let senha = "F"++(show(incrementar f))
		fila_2 <- pega_do_ficheiro

		if((existe_senha fila senha) == True) then do
			consulta fila fila_2 a b c d e (incrementar f) 
		else do	
			putStrLn ("\nSua Senha É : "++senha)
			arq <- openFile "fila.txt" WriteMode   --Abre o Ficheiro para Escrita
			hPutStrLn arq (show(fila_2++[(Senha senha)])) --Escreve no Ficheiro
			hClose arq

			arq <- openFile "controla_senha.txt" WriteMode   --Abre o Ficheiro para Escrita
			hPutStrLn arq (show(fila++[(Senha senha)])) --Escreve no Ficheiro
			hClose arq

			putStr "\nAguarde a Sua Chamada\n=====================================\nClique Enter para Voltar ao Menu"
			getChar
			menu_fila (fila++[(Senha senha)])  (fila_2++[(Senha senha)]) a b c d e (incrementar f)
	

menu_fila :: Fila -> Fila -> Int -> Int -> Int -> Int -> Int -> Int -> IO Fila 	--Menu da fila de espera
menu_fila fila fila_2 a b c d e f = do
		putStrLn "\t===========================================\n\t\t\tFila de Espera\n\t==========================================="
		putStrLn "\t1 Para Abrir Conta\n\t2 Para Deposito\n\t3 Para Levantamento\n\t4 Para Transferencia\n\t5 Para Extrato\n\t6 Para Consulta\n\t===========================================\n\tOpção : "
		op <- getChar
		getChar
		executarOpcao fila fila_2 op a b c d e f

--Opções do menu fila de espera
executarOpcao :: Fila -> Fila -> Char -> Int -> Int -> Int -> Int -> Int -> Int -> IO Fila
executarOpcao fila fila_2 '0' a b c d e f = do
		putStrLn ("\n=====================================\nTerminando ...\n")
		return fila
executarOpcao fila fila_2 '1' a b c d e f = criar_conta fila fila_2 a b c d e f
executarOpcao fila fila_2 '2' a b c d e f = deposito fila fila_2 a b c d e f
executarOpcao fila fila_2 '3' a b c d e f = levantamento fila fila_2 a b c d e f
executarOpcao fila fila_2 '4' a b c d e f = transferencia fila fila_2 a b c d e f
executarOpcao fila fila_2 '5' a b c d e f = extrato fila fila_2 a b c d e f
executarOpcao fila fila_2 '6' a b c d e f = consulta fila fila_2 a b c d e f						
executarOpcao fila fila_2 _ a b c d e f = do
		putStrLn ("\nOpção Invalida\nPressione Enter Para Voltar ao Menu")
		getChar
		menu_fila fila fila_2 a b c d e f
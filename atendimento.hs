module Atendimento where
--ESTE MODULO CONTEM CHAMA AS FUNCOES DESENVOLVIDAs NALGUNS MODULOS, EM SEUS MENUS 
import System.IO 
import System.IO.Error
import qualified Funcoes
import qualified Cliente
import qualified Servico
import qualified Control.Exception

go:: IO ()  --Funcao que da inicio ao programa
go = do		
		{catch(ler_arquivo) tratar_erro;} --Verifica se ja existe um arquivo, caso nao captura o erro 
		where
			ler_arquivo = do 			--Le o arquivo caso ja exista
			{
				arquivo <- openFile "dados.txt" ReadMode;	--Arquivo da lista de clientes
				dados <- hGetLine arquivo;
				hClose arquivo;

				arqUs <- openFile "usuario.txt" ReadMode;	--Arquivo do nome de usuario
				usuario <- hGetLine arqUs;
				hClose arqUs;

				arqSe <- openFile "senha.txt" ReadMode; 	--Arquivo da senha
				senha <- hGetLine arqSe;
				hClose arqSe;

				inicio_2 (read dados) usuario senha;	--Recebe os dados dos arquivos e manda para o Inicio
				return ()
			}
			tratar_erro erro = if isDoesNotExistError erro then do --Caso nao exista Arquivo algum Esta funcao Cria
			{
				arquivo <- openFile "dados.txt" WriteMode;		--Cria Arquivo da lista de clientes
				hPutStrLn arquivo "[]";
				hClose arquivo;

				arqUs <- openFile "usuario.txt" WriteMode;		--Cria Arquivo do Nome de Usuario
				hPutStrLn arqUs "stark";
				hClose arqUs;

				arqSe <- openFile "senha.txt" WriteMode;		--Cria Arquivo da Senha
				hPutStrLn arqSe "33";
				hClose arqSe;

				inicio_2 [] "stark" "33"; --Como nao existe ainda dados, mandamos uma lista vazia de clientes e senha, e um nome de usuario e senha pre definidos
				return ()
			}
			else
				ioError erro

inicio_2 :: Cliente.Pessoa -> String -> String -> IO ()		
inicio_2 lista no se = do
		putStrLn "\t===========================================\n\t\t\tInciar Sessão\n\t==========================================="
		putStrLn "\t\t1 Entrar\n\t\t0 Sair\n\t===========================================\n\tOpção: "
		op <- getChar
		getChar
		executarlogin lista no se op

executarlogin :: Cliente.Pessoa -> String -> String -> Char -> IO () 	--opcoes do menu da fila de espera
executarlogin lista no se '0' = do 
		putStrLn ("\n=====================================\nTerminando ...\n")
		return ()
executarlogin lista no se '1' = do
		putStrLn "\t===========================================\n\t\t\tLogIn\n\t==========================================="
		nome <- Funcoes.login_nome no 3
		if(nome == "0") then do 
			go
		else do 
			senha <- Funcoes.login_senha se 3
			if(senha == "0") then do 
				go
			else do 
				putStrLn "\t===========================================\n\t\t\tWelcome\n\t==========================================="
				putStr "\tClique Enter para Continuar "
				getChar
				putStrLn ""
				putStrLn ""
				menu_atend lista
				return ()				
executarlogin lista no se _ = go

menu_atend :: Cliente.Pessoa -> IO Cliente.Pessoa  --menu do modulo atendimento
menu_atend lista = do 
		putStrLn "\t===========================================\n\t\t\tAtendimento\n\t==========================================="
		putStrLn "\t\t1 Atendimento Geral\n\t\t2 Operacoes\n\t\t3 Relatório Geral\n\t\t0 Terminar\n\t===========================================\n\tOpção: "
		op <- getChar
		getChar
		executar_op_atend lista op

executar_op_atend :: Cliente.Pessoa -> Char -> IO Cliente.Pessoa 
executar_op_atend lista '0' = do
		go
		return lista
executar_op_atend lista '1' = menu_chamar lista
executar_op_atend lista '2' = menu_operacoes lista
executar_op_atend lista _  = menu_atend lista


servico :: Cliente.Pessoa -> Char -> IO Cliente.Pessoa
servico lista '0' = menu_chamar lista
servico lista '1' = do
		list <- Servico.servico_abertura_conta lista
		menu_chamar lista
servico lista '2' = do 
		list <- Servico.servico_deposito lista
		menu_chamar lista
servico lista '3' = do 
		list <- Servico.servico_levantamento lista
		menu_chamar lista
servico lista '4' = do 
		list <- Servico.servico_transferencia lista
		menu_chamar lista
servico lista '5' = do 
		list <- Servico.servico_consulta lista
		menu_chamar lista
servico lista '6' = do
		lista <- Servico.servico_extrato lista
 		menu_chamar lista


menu_chamar :: Cliente.Pessoa -> IO Cliente.Pessoa  --menu para a chamada de senhas
menu_chamar lista = do 
		putStrLn "\t===========================================\n\t\t\tAtendimento Geral\n==========================================="
		putStrLn "\t\t1 Chamar\n\t\t2 Ver Lista de Senhas\n\t\t3 Reiniciar Senhas\n\t\t0 Sair\n\t===========================================\n\tOpção: "
		op <- getChar
		getChar
		executar_op_chamar lista op 

executar_op_chamar :: Cliente.Pessoa -> Char -> IO Cliente.Pessoa  
executar_op_chamar lista '0' = menu_atend lista
executar_op_chamar lista '1' = do
		{catch(ler_arquivo) tratar_erro;} --Verifica se ja existe um arquivo, caso nao captura o erro 
		where
			ler_arquivo = do 			--Le o arquivo caso ja exista
			{
				arquivo2 <- openFile "fila.txt" ReadMode;	--Arquivo da lista de senhas
				fila <- hGetLine arquivo2;
				hClose arquivo2;

				ser <- Funcoes.funcao_chamar (read fila);
				servico lista ser
			}
			tratar_erro erro = if isDoesNotExistError erro then do --Caso nao exista Arquivo algum Esta funcao Cria
			{
				arquivo2 <- openFile "fila.txt" WriteMode;		--Cria Arquivo da lista de clientes
				hPutStrLn arquivo2 "[]";
				hClose arquivo2;
 
				arquivo2 <- openFile "fila.txt" ReadMode;	--Arquivo da lista de senhas
				fila <- hGetLine arquivo2;
				hClose arquivo2;

				ser <- Funcoes.funcao_chamar (read fila);
				servico lista ser
			}
			else
				ioError erro
			
executar_op_chamar lista '2' = do	
		{catch(ler_arquivo) tratar_erro;} --Verifica se ja existe um arquivo, caso nao captura o erro 
		where
			ler_arquivo = do 			--Le o arquivo caso ja exista
			{
				arquivo2 <- openFile "fila.txt" ReadMode;	--Arquivo da lista de senhas
				fila <- hGetLine arquivo2;
				hClose arquivo2;

				ser <- Funcoes.listar_senhas (read fila);
				menu_chamar lista
			}
			tratar_erro erro = if isDoesNotExistError erro then do --Caso nao exista Arquivo algum Esta funcao Cria
			{
				arquivo2 <- openFile "fila.txt" WriteMode;		--Cria Arquivo da lista de clientes
				hPutStrLn arquivo2 "[]";
				hClose arquivo2;

				arquivo2 <- openFile "fila.txt" ReadMode;	--Arquivo da lista de senhas
				fila <- hGetLine arquivo2;
				hClose arquivo2;

				Funcoes.listar_senhas (read fila);
				menu_chamar lista	
			}
			else
				ioError erro

executar_op_chamar lista '3' = do
		arquivo <- openFile "fila.txt" WriteMode;		--Cria Arquivo da lista de clientes
		hPutStrLn arquivo "[]";
		hClose arquivo;

		arquivo2 <- openFile "controla_senha.txt" WriteMode;		--Cria Arquivo da lista de clientes
		hPutStrLn arquivo2 "[]";
		hClose arquivo2;

		putStr "\n\n\tLista de Senhas Reiniciada\nClique Enter para voltar ao Menu\n"
		getChar
		menu_chamar lista

executar_op_chamar lista _ = menu_chamar lista
	
menu_operacoes :: Cliente.Pessoa -> IO Cliente.Pessoa   --meno pa as operacoes bancarias
menu_operacoes lista = do
		putStrLn "\t===========================================\n\t\t\tOperacoes\n\t==========================================="
		putStrLn "\t\t1 Criar Conta\n\t\t2 Deposito\n\t\t3 Levantamento\n\t\t4 Transfêrencia\n\t\t5 Consulta\n\t\t6 Extrato\n\t\t0 Sair\n\t===========================================\n\tOpção: "
		op <- getChar
		getChar
		executar_op_operacoes lista op

executar_op_operacoes :: Cliente.Pessoa -> Char -> IO Cliente.Pessoa 
executar_op_operacoes lista '1' = do 
		list <- (Servico.servico_abertura_conta lista)
		menu_operacoes list
executar_op_operacoes lista '2' = do 
		list <- Servico.servico_deposito lista
		menu_operacoes list
executar_op_operacoes lista '3' = do 
		list <- Servico.servico_levantamento lista
		menu_operacoes list		
executar_op_operacoes lista '4' = do 
		list <- Servico.servico_transferencia lista
		menu_operacoes list		
executar_op_operacoes lista '5' = do 
		list <- (Servico.servico_consulta lista)
		menu_operacoes list
executar_op_operacoes lista '6' = do
 		list <- Servico.servico_extrato lista
 		putStrLn "aaaaa"
 		menu_operacoes list
executar_op_operacoes lista _ = menu_operacoes lista

{-

menu_rel_geral :: Cliente.Pessoa -> IO Cliente.Pessoa  
menu_rel_geral lista = do
	putStrLn "\t===========================================\n\t\t\tRelatorio Diario\n\t==========================================="
	putStrLn "\t\t1 Criar Conta\n\t\t2 Deposito\n\t\t3 Levantamento\n\t\t4 Transfêrencia\n\t\t5 Consulta\n\t\t6 Extrato\n\t\t7 Estatisticas\n\t\t0 Sair\n\t===========================================\n\tOpção: "
	op <- getChar
	getChar
	executar_op_rel_geral lista op

executar_op_operacoes :: Cliente.Pessoa -> Char -> IO Cliente.Pessoa 
executar_op_operacoes lista '0' = menu_atend lista
executar_op_operacoes lista '1' = do  
executar_op_operacoes lista '2' = do 
executar_op_operacoes lista '3' = do	
executar_op_operacoes lista '4' = do 
executar_op_operacoes lista '5' = do 
executar_op_rel_Diario lista '6' = 
0executar_op_rel_Diario lista '7' =
executar_op_rel_Diario lista _  = menu_operacoes lista-}
module Servico where
--ESTE MODULO CONTEM AS FUNCOES QUE FAZEM OS SERVICOS BANCARIOS 
import System.IO
import Data.Time
import System.IO.Error
import qualified Cliente
import qualified Funcoes
import qualified Extrato
import qualified Operacoes
import qualified Verificacao
import qualified Control.Exception

servico_abertura_conta :: Cliente.Pessoa -> IO Cliente.Pessoa
servico_abertura_conta lista = do
		lista <- menu_Cli lista
		return lista

servico_deposito :: Cliente.Pessoa -> IO Cliente.Pessoa
servico_deposito lista = do
		putStr "\n=====================================\n\tDeposito\n=====================================\n"
		num <- Verificacao.teste_de_numero lista 3
		if(num == 0) then do 
			return lista
		else do 	
			valor <- Verificacao.teste_de_valor 3
			if(valor == 0.0) then do 
				return lista
			else do 
				nome <- Cliente.busca_nome_2 lista num

				arq <- openFile "Deposito.txt" AppendMode   --Abre o Ficheiro para Escrita
				hPutStrLn arq ("\tDeposito\nNome : "++nome++"\nConta : "++(show num)++"\nValor : "++(show valor)++"\n\n")   --Escreve no Ficheiro
				hClose arq
				putStr "\nOperação Concluidda com Sucessso\nClique Enter para Voltar ao Menu\n"

				arq <- openFile "dados.txt" WriteMode   --Abre o Ficheiro para Escrita
				hPutStrLn arq (show((Operacoes.deposito lista num valor)))   --Escreve no Ficheiro
				hClose arq 								--Fecha o Ficheiro
	
				data_op <- getZonedTime
				Extrato.main_extrato "Deposito" num 0 valor (take 19 (show data_op))
				
				getChar
				return (Operacoes.deposito lista num valor)

servico_levantamento :: Cliente.Pessoa -> IO Cliente.Pessoa
servico_levantamento lista = do 
		putStr "\n=====================================\n\tLevantamento\n=====================================\n"
		if(Cliente.lista_Vazia lista) then do
			putStr "\nImpossivel Fazer levantamento\nClique Enter para voltar ao Menu\n"
			getChar
			return lista
		else do
			num_conta <- Verificacao.teste_de_numero lista 3
			if (num_conta == 0) then do 
				return lista
			else do
				valor <- Verificacao.teste_de_valor 3
				if(valor == 0.0) then do 
					return lista
				else do 
					if ((Cliente.teste_do_valor_a_Levantar lista num_conta valor) == True) then do 
						nome <- Cliente.busca_nome_2 lista num_conta

						arq <- openFile "Levantamento.txt" AppendMode   --Abre o Ficheiro para Escrita
						hPutStrLn arq ("\tLevantamento\nNome : "++nome++"\nConta : "++(show num_conta)++"\nValor : "++(show valor)++"\n\n")   --Escreve no Ficheiro
						hClose arq 

						putStr "\n\tOperação Concluidda com Sucessso\nClique Enter para voltar ao Menu"
						getChar

						arq <- openFile "dados.txt" WriteMode   --Abre o Ficheiro para Escrita
						hPutStrLn arq (show((Operacoes.levantamento lista num_conta valor)))   --Escreve no Ficheiro
						hClose arq

						data_op <- getZonedTime
						Extrato.main_extrato "Levantamento" num_conta 0 valor (take 19 (show data_op))

						return (Operacoes.levantamento lista num_conta valor)
					else do 
						putStr "\nOperação Bloqueada\nOseu Valor em Banco é Menor ao Valor que Pretende Levantar\nClique Enter e Tenta de Novo"
						getChar
						return lista

servico_transferencia :: Cliente.Pessoa -> IO Cliente.Pessoa
servico_transferencia lista = do 
		putStr "\n=====================================\n\tTranferência\n=====================================\n"
		if((Cliente.lista_Vazia lista) || (Cliente.tamanho_da_Lista lista)==1) then do 
			putStr "\nImpossivel Fazer Transferencia de Momento\nClique Enter para voltar ao Menu\n"
			getChar
			return lista
		else do 
			putStr "\nNumero da Conta Origem : "
			num_conta_or <- Verificacao.teste_de_numero lista 3
			if (num_conta_or == 0) then do 
				return lista
			else do
				putStr "\tNumero da Conta Destino : "
				num_conta_de <- Verificacao.teste_de_numero lista 3
				if (num_conta_de == 0) then do 
					return lista
				else do
					if (num_conta_or == num_conta_de) then do
						putStr "\n\tOs Números de Origem e Destino Não poderm ser Iguais\nClique Enter e Tenta de Novo"
						return lista
					else do
						valor <- Verificacao.teste_de_valor 3
						if(valor == -1) then do 
							return lista 
						else do 
							nome <- Cliente.busca_nome_2 lista num_conta_or
							nome_2 <- Cliente.busca_nome_2 lista num_conta_de

							arq <- openFile "Transferencia.txt" AppendMode   --Abre o Ficheiro para Escrita
							hPutStrLn arq ("\tTranferencia\nNome Origem: "++nome++"\nConta Origem: "++(show num_conta_or)++"\nNome Destino: "++nome_2++"\nConta Destino : "++(show num_conta_de)++"\nValor : "++(show valor)++"\n\n")   --Escreve no Ficheiro
							hClose arq 

							arq <- openFile "dados.txt" WriteMode   --Abre o Ficheiro para Escrita
							hPutStrLn arq (show((Operacoes.transferencia_aumento (Operacoes.transferencia_desconto lista num_conta_or valor) num_conta_de valor)))   --Escreve no Ficheiro
							hClose arq

							data_op <- getZonedTime
							Extrato.main_extrato "Transferencia" num_conta_or num_conta_de valor (take 19 (show data_op))

							putStr "\n\tOperação Concluidda com Sucessso\nClique Enter para voltar ao Menu"
							getChar
							return (Operacoes.transferencia_aumento (Operacoes.transferencia_desconto lista num_conta_or valor) num_conta_de valor)

servico_consulta :: Cliente.Pessoa -> IO Cliente.Pessoa
servico_consulta lista = do 
		putStr "\n=====================================\n\tConsulta\n=====================================\n"
		num_conta <- Verificacao.teste_de_numero lista 3
		if(num_conta == 0) then do 
			return lista 
		else do 
			saldo <- Operacoes.consulta lista num_conta
			nome <- Cliente.busca_nome_2 lista num_conta

			arq <- openFile "Consulta.txt" AppendMode   --Abre o Ficheiro para Escrita
			hPutStrLn arq ("\tConsulta\nNome :"++nome++"\nConta : "++(show num_conta)++"\nSaldo : "++(show saldo)++"\n\n")   --Escreve no Ficheiro
			hClose arq

			data_op <- getZonedTime
			Extrato.main_extrato "Consulta" num_conta 0 0 (show data_op)

			putStrLn ("\nNome : "++nome++"\nConta :"++(show num_conta)++"\nSaldo : "++(show saldo))
			putStr "\n\nClique Enter Para Voltar ao Menu\n"
			getChar
			return lista


servico_extrato :: Cliente.Pessoa -> IO Cliente.Pessoa
servico_extrato lista = do 
		{catch(ler_arquivo) tratar_erro;} --Verifica se ja existe um arquivo, caso nao captura o erro 
		where
			ler_arquivo = do 			--Le o arquivo caso ja exista
			{
				arquivo <- openFile "lista_extrato.txt" ReadMode;	--Arquivo da lista de clientes
				extrato <- hGetLine arquivo;
				hClose arquivo;

				num_conta <- Verificacao.teste_de_numero lista 3;
				Extrato.listar_extrato (read extrato) num_conta;

				data_op <- getZonedTime;
				Extrato.main_extrato "Extrato" num_conta 0 0 (take 19 (show data_op));
				return lista
			}
			tratar_erro erro = if isDoesNotExistError erro then do --Caso nao exista Arquivo algum Esta funcao Cria
			{
				putStrLn("\n\nSem Extrato\nClique Enter");
				getChar;
				return lista	
			}
			else
				ioError erro







menu_Cli :: Cliente.Pessoa -> IO Cliente.Pessoa 	--menu para a abertura de contas
menu_Cli lista = do
		putStrLn "\t===========================================\n\t\t\tCadastro\n\t==========================================="
		putStrLn "\t\t1 Cadastrar\n\t\t2 Listar Cadastros\n\t\t3 Listar Um Cadastro\n\t\t4 Eliminar Conta\n\t\t0 Sair\n\t===========================================\n\tOpção: "
		op <- getChar
		getChar
		executarOpcao lista op

executarOpcao :: Cliente.Pessoa -> Char -> IO Cliente.Pessoa 	--opcoes do menu da fila de espera
executarOpcao lista '0' = return lista
executarOpcao lista '1' = do
		lista_2 <- Funcoes.criar_conta lista
		menu_Cli lista_2
				
executarOpcao lista '2' = do
		putStrLn("\n=====================================\nTotal de Cadastrados : "++(show(Cliente.tamanho_da_Lista lista)))
		putStrLn("\n=====================================\n\nDados :(Chave, Nome, Numero de Identificação, Idade, Data de Nascimento, Numero da Conta, Saldo, Numero de Telefone)\n\nCadastrados : "++(show(Cliente.listar lista)))
		getChar
		menu_Cli lista

executarOpcao lista '3' = do
		lista_2 <- Funcoes.listar_um_cliente lista
		menu_Cli lista_2

executarOpcao lista '4' = do
		lista_2 <- Funcoes.eliminar_conta lista
		menu_Cli lista_2

executarOpcao lista _ = do
		putStrLn ("\nOpção Invalida\nPressione Enter Para Voltar ao Menu ")
		getChar
		menu_Cli lista
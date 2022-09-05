module Extrato where
--ESTE MODULO SE ENCARREGA DE REGISTRAR EM UMA LISTA O EXTRATO DE TODOS OS SERVICOS REALIZADOS
import System.IO
import System.IO.Error
import qualified Verificacao
import qualified Control.Exception

type Conta = Int
type Conta_2 = Int
type Operacao = String
type Valor = Double
type Data = String
type Info = (Operacao, Conta, Conta_2, Valor, Data)
type Extrato = [Info]


verificar_extrato :: Extrato -> Conta -> Bool
verificar_extrato [] _ = False
verificar_extrato ((op, conta, conta_2, valor, data_op): xs) num_conta
			| (conta == num_conta) = True
			| otherwise = verificar_extrato xs num_conta


extrato :: Extrato -> Conta -> Extrato
extrato [] _ = []
extrato ((op, conta, conta_2, valor, data_op): xs) num_conta
			| (conta == num_conta) = (op, conta, conta_2, valor, data_op):(extrato xs num_conta)
			| otherwise = extrato xs num_conta


listar_extrato :: Extrato -> Conta -> IO Extrato
listar_extrato [] _ = error "Lista Vazia"
listar_extrato extrat num_conta= do 
			if(num_conta == 0) then do 
				return extrat
			else do 
				if ((verificar_extrato extrat num_conta) == True) then do
					putStrLn ("\n\t\tSeu Extrato\n\n")					
					putStrLn (show (extrato extrat num_conta))
					putStrLn ("\n\nClique Enter para Voltar ao Menu\n\n")
					getChar
					return extrat	
				else
						
					return extrat	


adicionar_extrato :: Extrato -> Operacao -> Conta -> Conta_2 -> Valor -> Data -> IO Extrato
adicionar_extrato extrato op co co2 vl dt = do 
			let info = (op, co, co2, vl, dt)

			arq <- openFile "lista_extrato.txt" WriteMode   --Abre o Ficheiro para Escrita
			hPutStrLn arq ((show(extrato ++[info]))++"\n")   --Escreve no Ficheiro
			hClose arq 								--Fecha o Ficheiro

			return (extrato++[info])



main_extrato :: Operacao -> Conta -> Conta_2 -> Valor -> Data -> IO Extrato
main_extrato op conta conta_2 valor data_op = do		
		{catch(ler_arquivo) tratar_erro;} --Verifica se ja existe um arquivo, caso nao captura o erro 
		where
			ler_arquivo = do 			--Le o arquivo caso ja exista
			{
				arquivo <- openFile "lista_extrato.txt" ReadMode;	--Arquivo da lista de clientes
				extrato <- hGetLine arquivo;
				hClose arquivo;

				adicionar_extrato (read extrato) op conta conta_2 valor data_op
				
			}
			tratar_erro erro = if isDoesNotExistError erro then do --Caso nao exista Arquivo algum Esta funcao Cria
			{
				arquivo <- openFile "lista_extrato.txt" WriteMode;		--Cria Arquivo da lista de clientes
				hPutStrLn arquivo "[]";
				hClose arquivo;

				adicionar_extrato [] op conta conta_2 valor data_op
				
			}
			else
				ioError erro
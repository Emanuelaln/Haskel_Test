module Verificacao where
--ESTE MODULO SE ENCARREGA DE FAZER ALGUMAS VERIFICOES QUANTO AS VALIDACOES
import qualified Cliente

decrementar :: Int -> Int 	--Funcão de incremento padrão para controlar as oportunidades
decrementar a = a-1	


verifica_dia :: Int -> IO String     {-Para data-}
verifica_dia cont = do 
			if (cont == 0) then do 	
				putStr "\nOperação Cancelada\nTente de Novo Mais Tarde\nClique Enter\n"
				getChar
				return "0"
			else do
				putStr "\nDia : "
				dia <- getLine
				if(((read dia) > 0) && ((read dia) < 32)) then do --Garaante que não Haja Repetição do Numero	
					return dia
				else do
					putStr "\nDia Invalido\n\nPrime Enter e Tente Novamente\n"
					getChar
					verifica_dia (decrementar cont)


verificar_mes :: Int -> IO String
verificar_mes cont = do 
			putStrLn "\n\t\tMes: \n\t\t\t1 Janeiro\t\t2 Fevereiro\n\t\t\t3 Março\t\t4 Abril\n\t\t\t5 Maio\t\t6 Junho\n\t\t\t7 Julho\t\t8 Agosto\n\t\t\t9 Setembro\t\t10 Outubro\n\t\t\t11 Novembro\t\t12 Dezembro\n\n\t\t\tOpção: "
			op <- getLine
			executar_mes 3 (read op)


executar_mes :: Int -> Int -> IO String 	--opcoes do menu da fila de espera
executar_mes cont 1 = do 
					return "Janeiro"
executar_mes cont 2 = do
					return "Fevereiro"
executar_mes cont 3 = do 
					return "Março"
executar_mes cont 4 = do 
					return "Abril"
executar_mes cont 5 = do 
					return "Maio"
executar_mes cont 6 = do 
					return "Junho"
executar_mes cont 7 = do 
					return "Julho"
executar_mes cont 8 = do 
					return "Agosto"
executar_mes cont 9 = do 
					return "Setembro"
executar_mes cont 10 = do 
					return "Outubro"
executar_mes cont 11 = do 
					return "Novembro"
executar_mes cont 12 = do 
					return "Dezembro"			
executar_mes cont _ = do 
			putStr "\n\t\tTenta de Novo"
			if(cont == 0) then do 
				return "0"
			else do
				verificar_mes (decrementar cont)


verifica_ano :: Int -> IO String     {-Para data-}
verifica_ano cont = do 
			if (cont == 0) then do 	
				putStr "\nOperação Cancelada\nTente de Novo Mais Tarde\nClique Enter\n"
				getChar
				return "0"
			else do
			putStr "\nLimite de Anos Validos: de 1930 a 2003"
			putStr "\nAno : "
			ano <- getLine
			if((read ano) > 1930) then do --Garaante que não Haja Repetição do Numero	
				if((read ano) < 2004) then do
					return ano
				else do
					putStr "\nAno Invalido\n\nPrime Enter e Tente Novamente\n"
					getChar
					verifica_ano (decrementar cont)
			else do
				putStr "\nAno Invalido\n\nPrime Enter e Tente Novamente\n"
				getChar
				verifica_ano (decrementar cont)


teste_de_valor :: Int -> IO Double  -- Garante que o Dinheiro seja sempre positivo
teste_de_valor cont = do 
			if (cont == 0) then do 
				putStr "\nOpção Cancelada\nTente de Novo Mais Tarde\nClique Enter para voltar ao Menu\n"
				getChar
				let cont = 0.0
				return cont
			else do
				putStr "\n\tMontante : "
				a <- getLine
				if((read a) > 0) then do 
					return (read a)
				else do 
					putStr "Só é Aceite Saldo Positivo\nClique Enter e Tenta de Novo\n"
					getChar
					teste_de_valor (decrementar cont)


verifica_nome :: Cliente.Pessoa -> Int -> IO String		--Garante que o Nome  seja único 
verifica_nome lista cont = do 
			let valido =  "aáàãâbcdeéèêfghiíìîjklmnoóòõôpkrstuúùûvwxyzAÁÀÃÂBCDEÉÈÊFGHIÍÌÎJKLMNOÓÒÕÔPQRSTUÚÙÛVWXYZ "

			if (cont == 0) then do 
				putStr "nOperação Cancelada\n"	
				putStr "\nOpção Cancelada\nTente de Novo Mais Tarde\nClique Enter\n"
				getChar
				return "0"
			else do
			putStr "\nNome : "
			nome <- getLine
			if((validar_nome nome valido)==True) then do
				if((Cliente.existe_Nome lista nome) == True) then do --Garaante que não Haja Repetição do Numero	
					putStrLn "\nEste nome ja Existe"
					putStr "\nPressione Enter e Tenta de Novo\n"
					getChar
					verifica_nome lista	(decrementar cont)			
				else do
					return nome
			else do 
				putStr "\nNome Invalido.\n Clique Enter e Tente de novo"
				getChar
				verifica_nome lista	(decrementar cont)

validar_nome :: String -> String -> Bool
validar_nome [] _ = True
validar_nome (x:xs) valido
			|((ver_caracter valido x)==False) = False
			|otherwise = validar_nome xs valido


ver_caracter :: String -> Char -> Bool
ver_caracter [] _ = False
ver_caracter (x:xs) c
			|(c==x) = True
			|otherwise = ver_caracter xs c




teste_de_numero :: Cliente.Pessoa -> Int -> IO Int  --Garante que o Numero da Conta seja Um numero Valido
teste_de_numero lista cont = do
			if (cont == 0) then do 
				putStr "\nOpção Cancelada\nTenta Lembrar o Seu Número e Volte a Tentar de Novo Mais Tarde\nClique Enter para voltar ao Menu\n"
				getChar
				return cont
			else do
				putStr "\n\tNumero da Conta : "
				num <- getLine
				if((Cliente.existe_Num_Conta lista (read num))==True) then do 					
					return (read num)
				else do 
					putStrLn ("\n\tNumero Invalido, Tens "++(show (decrementar cont))++" Jogadas\n\tClique Enter\n")
					getChar
					teste_de_numero lista (decrementar cont)
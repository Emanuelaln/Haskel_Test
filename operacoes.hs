module Operacoes where
--ESTE MODULO CONTEM AS FUNCOES DIREITAS SOBRE A LISTA
import qualified Cliente


deposito :: Cliente.Pessoa -> Int -> Double -> Cliente.Pessoa   --recebe o numero da conta e o valor a depositar
deposito [] _ _ = error "Lista Vazia"
deposito ((ch, no, ni, id, dat, num_conta, valor, num_t):xs) num valor_Dep
		|(num_conta == num) = ((ch, no, ni, id, dat, num_conta, (valor + valor_Dep), num_t):xs)
		|otherwise = deposito xs num valor_Dep


levantamento :: Cliente.Pessoa -> Int -> Double -> Cliente.Pessoa  --recebe o numero da conta e o valor a levantar
levantamento [] _ _ = error "Lista Vazia"
levantamento  ((ch, no, ni, id, dat, num_conta, valor, num_t):xs) num_C valor_Lev
		|(num_conta == num_C) = ((ch, no, ni, id, dat, num_conta, (valor - valor_Lev), num_t):xs)
		|otherwise = levantamento xs num_C valor_Lev


transferencia_desconto :: Cliente.Pessoa -> Cliente.Num_Conta -> Cliente.Valor -> Cliente.Pessoa
transferencia_desconto [] _ _ = error "Lista Vazia"
transferencia_desconto ((ch, no, ni, id, dat, num_conta, valor, num_t):xs) num_C valor_Tr
		|(num_conta==num_C) = ((ch, no, ni, id, dat, num_conta, (valor-valor_Tr), num_t):xs)
		|otherwise = (ch, no, ni, id, dat, num_conta, valor, num_t):(transferencia_desconto xs num_C valor_Tr)


transferencia_aumento :: Cliente.Pessoa -> Cliente.Num_Conta -> Cliente.Valor -> Cliente.Pessoa
transferencia_aumento [] _ _ = error "Lista Vazia"
transferencia_aumento ((ch, no, ni, id, dat, num_conta, valor, num_t):xs) num_C valor_Tr
		|(num_conta==num_C) = ((ch, no, ni, id, dat, num_conta, (valor+valor_Tr), num_t):xs)
		|otherwise = (ch, no, ni, id, dat, num_conta, valor, num_t):(transferencia_aumento xs num_C valor_Tr)


consulta :: Cliente.Pessoa -> Cliente.Num_Conta -> IO Cliente.Valor --recebe o numero da conta
consulta [] _ = error "lista Vazia"
consulta ((ch, no, ni, id, dat, num_conta, valor, num_t):xs) num_C = do
		if (num_conta == num_C) then do
			return valor
		else do 
			consulta xs num_C
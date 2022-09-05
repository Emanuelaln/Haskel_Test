%Factos da Base de Cinhecimento

%bloco(codigo, area, quarteirao)
bloco(1, 30, a).
bloco(2, 60, b).
bloco(3, 90, c).

%coordenada(codigo_do_bloco, latitude, altitude)
coordenada(1, 23, 67).
coordenada(2, 32, 45).
coordenada(3, 46, 45).

%modelo_edificio(codigo, tipo, existe_elevador, existe_rampa)
modelo(1, lower_class, nao, nao).
modelo(2, worker_class, nao, nao). 
modelo(3, midle_class, sim, nao). 
modelo(4, upper_class, sim, sim).  

%edificio(codigo_edificio, código_bloco,código_modelo, numero_de_andar, fracao_1, fracao_2)
edificio(1, 2, 1, 10, a, b).
edificio(2, 1, 3, 9, a, b).
edificio(3, 3, 2, 20, a, b).

%rua(codigo, nome, tipo)
rua(1, vingador, principal).
rua(2, asgard, secundaria).
rua(3, stark, principal).

%edificio_rua(codigo_edificio, codigo_rua)
edificio_rua(1,3).
edificio_rua(2,1).
edificio_rua(3,2).

%edificio_escritor(codigo_edificio, numero_de_escritor)
edificio_escritor(1, 99).
edificio_escritor(2, 63).
edificio_escritor(3, 33).

%edificio_moradia(codigo_edificio, numero_de_apart)
edificio_moradia(4, 66).
edificio_moradia(5, 99).
edificio_moradia(6, 100).

%escritorio(codigo, codigo_edificio, andar, area)
escritorio(1, 1, 2, 20).
escritorio(2, 2, 4, 20).
escritorio(3, 3, 5, 20).

%apartamento(codigo, codigo_edificio, compartimento, fração, andar, numero_de_ordem)
apartamento(1, 4, t4, a, 2, 213).
apartamento(2, 5, t2, b, 5, 512).
apartamento(3, 6, t5, a, 6, 613).

%historico(codigo, codigo_apart, codigo_escr, antigo_dono, novo_dono, motivo, data_da_mudanca)
historico(1, 1, null, joao, pedro, energia, data(1,2, 2019)).
historico(2, null, 2, leo, nascimento, luz, data(6,4, 2019)).
historico(3, 3, null, eric, garcia, energia, data(7,8, 2019)).

%proprietario(codigo, nome, nif).
proprietario(1, joao, 123).
proprietario(2, daniel, 321).
proprietario(3, eric, 456).
proprietario(3, leo, 456).

%contacto(codigo_do_prorietario, email, telefone)
contacto(1, joaogmail.com, 941).
contacto(2, danielgmail.com, 934).
contacto(3, ericgmail.com, 948).

%pessoa_fisica(codigo_do_proprietario, bi)
pessoa_fisica(1, 545).
pessoa_fisica(3, 678).

%pessoa_fisica_apartamento(codigo_pessoa_fisica, código_apart)
pessoa_fisica_apartamento(1, 1).
pessoa_fisica_apartamento(3, 2).

%pessoa_juridica(codigo_do_proprietario)
pessoa_juridica(2).

%pessoa_juridica_escritores(codigo_pessoa_jur, codigo_escr, código_edificio)
pessoa_juridica_escritores(2, 1, 2).

%coordenador_bloco(codigo_proprietario, codigo_bloco)
coordenador_bloco(2,1).

%coordenador_edificio(código_proprietario, código_edificio)
coordenador_edificio(1,1).
coordenador_edificio(2,2).

%parque_de_estacionamento(codigo, codigo_apartamento, codigo_escritorio, area)
parque(1, 1, null, 23).
parque(2, null, 1 ,45).
parque(3, 3,null, 20).

%taxa(codigo, codigo_proprietário, codigo_apartamento, codigo_escritorio, valor)
taxa(1,1, 1, null, 600).
taxa(2,2, null, 1, 400).
taxa(3,3, 3, null, 300).


%Regras
%edificios que possuem elevador
ed_elevador(Cod_ed):-
	edificio(Cod_ed, Cod_bl,Cod_mod, Num_andar, Fra, Frac_2),
	modelo(Cod_mod, Tipo, sim, Rampa).
	
 
%coordenadores de edificios
coord_edificio(Nome, Cod_ed):-
	coordenador_edificio(Cod_prop, Cod_ed),
	proprietario(Cod_prop, Nome, Nif).
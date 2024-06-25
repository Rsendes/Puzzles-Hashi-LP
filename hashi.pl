:- [codigo_comum].

% 96859 Filipe Resendes

%-------------------------------------------------------------------------------
%       extrai_ilhas_linha(N_L, Linha, Ilhas)
%	'Obtem-se as ilhas de uma determinada linha'
%-------------------------------------------------------------------------------
extrai_ilha_linhas_aux(N_L, Linha, ilha(N, (N_L, N_C))) :-
	nth1(N_C, Linha, N),
	member(N, Linha),
	N > 0.

extrai_ilhas_linha(N_L, Linha, Ilhas) :-
	findall(ilha(N, (N_L, N_C)), extrai_ilha_linhas_aux(N_L, Linha,
			ilha(N, (N_L, N_C))), Ilhas_Temp),
	list_to_set(Ilhas_Temp, Ilhas).


%-------------------------------------------------------------------------------
%   	ilhas(Puz, Ilhas)
%	'Obtem-se as Ilhas de um determinado Puzzle'
%-------------------------------------------------------------------------------
ilhas_aux(Puz, ilha(N, (N_L, N_C))) :-
	nth1(N_L, Puz, Linha),
	member(Linha, Puz),
	extrai_ilha_linhas_aux(N_L, Linha, ilha(N, (N_L, N_C))).

ilhas(Puz, Ilhas) :- 
	findall(ilha(N, (N_L, N_C)), ilhas_aux(Puz, ilha(N, (N_L, N_C))), Ilhas_Temp),
	list_to_set(Ilhas_Temp, Ilhas).


%-------------------------------------------------------------------------------
%      	vizinhas(Ilhas, Ilha, Vizinhas)
%	'Obtem-se as vizinhas de uma determinada ilha'
%-------------------------------------------------------------------------------
vizinhas_aux_aux(ilha(N, (N_L, N_C2)), ilha(_, (N_L, N_C1)), ilha(N, (N_L, N_C2))) :-
	N_C1 \== N_C2.

vizinhas_aux_aux(ilha(N, (N_L2, N_C)), ilha(_, (N_L1, N_C)), ilha(N, (N_L2, N_C))) :-
	N_L2 \== N_L1.

vizinhas_aux(Ilhas, Ilha, Vizinha) :-
	member(Membro, Ilhas),
	vizinhas_aux_aux(Membro, Ilha, Vizinha).

% 'Impostora e uma ilha que nao e vizinha por estar apos outra vizinha'
impostora(Ilhas, ilha(_, (X, Y1)), ilha(N, (X, Y2))) :-
	bagof(ilha(N, (X, Y2)), (member(ilha(_, (X, Y3)), Ilhas), Y1 < Y3, Y2 > Y3), _).

impostora(Ilhas, ilha(_, (X, Y1)), ilha(N, (X, Y2))) :-
	bagof(ilha(N, (X, Y2)), (member(ilha(_, (X, Y3)), Ilhas), Y1 > Y3, Y2 < Y3), _).

impostora(Ilhas, ilha(_, (X1, Y)), ilha(N, (X2, Y))) :-
	bagof(ilha(N, (X2, Y)), (member(ilha(_, (X3, Y)), Ilhas), X1 < X3, X2 > X3), _).

impostora(Ilhas, ilha(_, (X1, Y)), ilha(N, (X2, Y))) :-
	bagof(ilha(N, (X2, Y)), (member(ilha(_, (X3, Y)), Ilhas), X1 > X3, X2 < X3), _).

vizinhas(Ilhas, Ilha, Vizinhas) :-
	findall(Vizinha, (vizinhas_aux(Ilhas, Ilha, Vizinha),
			\+ impostora(Ilhas, Ilha, Vizinha)),Vizinhas_Temp),
	list_to_set(Vizinhas_Temp, Vizinhas).


%-------------------------------------------------------------------------------
%		estado(Ilhas, Estado)
%	'Obtem-se o estado de um determinado puzzle'
%-------------------------------------------------------------------------------
estado_aux(Ilhas, [Ilha, Vizinhas, []]) :-
	member(Ilha, Ilhas),
	vizinhas(Ilhas, Ilha, Vizinhas).

estado(Ilhas, Estado) :-
	findall([Ilha, Vizinhas, []],
			estado_aux(Ilhas, [Ilha, Vizinhas, []]), Estado).


%-------------------------------------------------------------------------------
%		posicoes_entre(Pos1, Pos2, Posicoes)
%	'Obtem-se a lista de posicoes entre duas posicoes'
%-------------------------------------------------------------------------------
diferentes(Val1, Val2, Val3) :-
	Val1 =\= Val3,
	Val2 =\= Val3.

posicoes_entre_aux((X, Y1), (X, Y2), (X, Y_P)) :-
	Y1 < Y2,
	between(Y1, Y2, Y_P),
	diferentes(Y1, Y2, Y_P).

posicoes_entre_aux((X, Y1),(X, Y2), (X, Y_P)) :-
	Y1 > Y2,
	between(Y2, Y1, Y_P),
	diferentes(Y1, Y2, Y_P).

posicoes_entre_aux((X1, Y), (X2, Y), (X_P, Y)) :-
	X1 < X2,
	between(X1, X2, X_P),
	diferentes(X1, X2, X_P).

posicoes_entre_aux((X1, Y), (X2, Y), (X_P, Y)) :-
	X1 > X2,
	between(X2, X1, X_P),
	diferentes(X1, X2, X_P).

posicoes_entre(Pos1, Pos2, Posicoes) :-
	bagof(Posicao, posicoes_entre_aux(Pos1, Pos2, Posicao), Posicoes).


%-------------------------------------------------------------------------------
%   	cria_ponte(Pos1, Pos2, Ponte)
%	'Obtem-se uma ponte de duas posicoes'
%-------------------------------------------------------------------------------
cria_ponte((X, Y1), (X, Y2), ponte((X, Y_P1), (X, Y_P2))) :-
	Y1 < Y2,
	Y_P1 is Y1,
	Y_P2 is Y2.

cria_ponte((X, Y1), (X, Y2), ponte((X, Y_P1), (X, Y_P2))) :-
	Y1 > Y2,
	Y_P1 is Y2,
	Y_P2 is Y1.

cria_ponte((X1, Y), (X2, Y), ponte((X_P1, Y), (X_P2, Y))) :-
	X1 < X2,
	X_P1 is X1,
	X_P2 is X2.

cria_ponte((X1, Y), (X2, Y), ponte((X_P1, Y), (X_P2, Y))) :-
	X1 > X2,
	X_P1 is X2,
	X_P2 is X1.


%-------------------------------------------------------------------------------
% 		caminho_livre(Pos1, Pos2, Posicoes, I, Vz)
%	'Avalia se a adicao de uma ponte faz com que duas ilhas deixem de ser vizinhas'
%-------------------------------------------------------------------------------
caminho_livre_aux(_, _, Posicoes, ilha(_, I), ilha(_, Vz)) :-
	posicoes_entre(I, Vz, Posicoes_L),
	member(Membro, Posicoes),
	member(Membro, Posicoes_L).

caminho_livre(_, _, Posicoes, I, Vz) :-
	\+ caminho_livre_aux(_, _, Posicoes, I, Vz).

caminho_livre(Pos1, Pos2, _, ilha(_ ,Pos1), ilha(_, Pos2)).

caminho_livre(Pos1, Pos2, _, ilha(_ , Pos2), ilha(_, Pos1)).


%-------------------------------------------------------------------------------
%		actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, Entrada, Nova_Entrada)
%	'Obtem-se uma nova entrada removendo as ilhas que deixaram de ser vizinhas'
%-------------------------------------------------------------------------------
actualiza_vizinhas_entrada_aux(Pos1, Pos2, Posicoes, [I, Vz1, _], Elem_Vz2) :-
	member(Elem_Vz2, Vz1),
	caminho_livre(Pos1, Pos2, Posicoes, I, Elem_Vz2).

actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, [I, Vz1 , Ponte], [I ,Vz2, Ponte]) :-
	findall(Elem_Vz2 , actualiza_vizinhas_entrada_aux(Pos1, Pos2, Posicoes,
			[I, Vz1, Ponte], Elem_Vz2), Vz2).


%-------------------------------------------------------------------------------
%		actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Novo_Estado)
%	'Obtem-se um novo estado apos adicao de uma ponte'
%-------------------------------------------------------------------------------
actualiza_vizinhas_apos_pontes_aux(Estado, Pos1, Pos2, Nova_Entrada) :-
	member(Entrada, Estado),
	posicoes_entre(Pos1, Pos2, Posicoes),
	actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, Entrada, Nova_Entrada).

actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Novo_Estado) :-
	findall(Nova_Entrada, actualiza_vizinhas_apos_pontes_aux(Estado, 
			Pos1, Pos2, Nova_Entrada), Novo_Estado).


%-------------------------------------------------------------------------------
%		ilhas_terminadas(Estado, Ilhas_term)
%	'Obtem-se a lista de ilhas terminadas de um determinado estado'
%-------------------------------------------------------------------------------
ilhas_terminadas_aux(Estado, ilha(N, Pos)) :-
	member([ilha(N, Pos), _ , Pontes], Estado),
	\+ N = 'X',
	length(Pontes, N).

ilhas_terminadas(Estado, Ilhas_term) :-
	findall(Ilha_Term, ilhas_terminadas_aux(Estado, Ilha_Term), Ilhas_term).


%-------------------------------------------------------------------------------
%		tira_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_Entrada)
%	'Obtem-se uma nova entrada apos remover as ilhas terminadas'
%-------------------------------------------------------------------------------
tira_ilhas_terminadas_entrada_aux(Ilhas_term, [_, Vz, _], Vz_nova) :-
	member(Vz_nova, Vz),
	\+ member(Vz_nova, Ilhas_term).

tira_ilhas_terminadas_entrada(Ilhas_term, [I, Vz, Pontes], [I, Vz_novas, Pontes]) :-
	findall(Vz_nova, tira_ilhas_terminadas_entrada_aux(Ilhas_term,
			[I, Vz, _], Vz_nova), Vz_novas).


%-------------------------------------------------------------------------------
%		tira_ilhas_terminadas(Estado, Ilhas_term, Novo_Estado)
%	'Obtem-se um novo estado apos remover as ilhas terminadas'
%-------------------------------------------------------------------------------
tira_ilhas_terminadas_aux(Estado, Ilhas_term, Nova_Entrada) :-
	member(Entrada, Estado),
	tira_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_Entrada).

tira_ilhas_terminadas(Estado, Ilhas_term, Novo_Estado) :-
	findall(Nova_Entrada, tira_ilhas_terminadas_aux(Estado, Ilhas_term,
			Nova_Entrada), Novo_Estado).


%-------------------------------------------------------------------------------
%		marca_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_entrada)
%	'Obtem-se uma nova entrada caso a ilha pertencer as ilhas terminadas'
%-------------------------------------------------------------------------------
marca_ilhas_terminadas_entrada(Ilhas_term, [I, Vz, Pontes], [I, Vz, Pontes]) :-
	\+ member(I, Ilhas_term).

marca_ilhas_terminadas_entrada(Ilhas_term, [ilha(N, Pos), Vz, Pontes],
		[ilha('X', Pos), Vz, Pontes]) :-
	member(ilha(N, Pos), Ilhas_term).


%-------------------------------------------------------------------------------
%		marca_ilhas_terminadas(Estado, Ilhas_term, Novo_Estado)
% 'Obtem-se um novo estado aplicando o marca_ilhas_terminadas_entrada as suas entradas'
%-------------------------------------------------------------------------------
marca_ilhas_terminadas_aux(Estado, Ilhas_term, Nova_entrada) :-
	member(Entrada, Estado),
	marca_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_entrada).

marca_ilhas_terminadas(Estado, Ilhas_term, Novo_Estado) :-
	findall(Nova_entrada, marca_ilhas_terminadas_aux(Estado, Ilhas_term,
			Nova_entrada), Novo_Estado).


%-------------------------------------------------------------------------------
%		trata_ilhas_terminadas(Estado, Novo_estado)
%	'Obtem-se um novo estado apos aplicar os predicados tira_ilhas_terminadas e
%	 marca_ilhas_terminadas'
%-------------------------------------------------------------------------------
trata_ilhas_terminadas(Estado, Novo_estado) :-
	ilhas_terminadas(Estado, Ilhas_term),
	tira_ilhas_terminadas(Estado, Ilhas_term, Estado_temp),
	marca_ilhas_terminadas(Estado_temp, Ilhas_term, Novo_estado).


%-------------------------------------------------------------------------------
%		junta_pontes(Estado, Num_pontes, Ilha1, Ilha2, Novo_estado)
%	'Obtem-se um novo estado apos adicao de pontes'
%-------------------------------------------------------------------------------
adiciona_pontes_entrada_aux([Ilha1, Vz, _], Ilha1, _, Ponte, [Ilha1, Vz, [Ponte]]).
adiciona_pontes_entrada_aux([Ilha2, Vz, _], _, Ilha2, Ponte, [Ilha2, Vz, [Ponte]]).
adiciona_pontes_entrada_aux([Ilha, Vz, Pontes], Ilha1, Ilha2, _, [Ilha, Vz, Pontes]) :-
	\+ Ilha == Ilha1,
	\+ Ilha == Ilha2.

adiciona_pontes_entrada(Estado, Ilha1, Ilha2, Ponte, Nova_entrada) :-
	member(Entrada, Estado),
	adiciona_pontes_entrada_aux(Entrada, Ilha1, Ilha2, Ponte, Nova_entrada).

adiciona_pontes(Estado, Ilha1, Ilha2, Ponte, Novo_estado) :-
	bagof(Nova_entrada, adiciona_pontes_entrada(Estado, Ilha1, Ilha2, Ponte,
			Nova_entrada), Novo_estado).

junta_pontes(Estado, _, ilha(N1, Pos1), ilha(N2, Pos2), Novo_estado) :-
	cria_ponte(Pos1, Pos2, Ponte),
	adiciona_pontes(Estado, ilha(N1, Pos1), ilha(N2, Pos2), Ponte, Estado_temp1),
	actualiza_vizinhas_apos_pontes(Estado_temp1, Pos1, Pos2, Estado_temp2),
	trata_ilhas_terminadas(Estado_temp2, Novo_estado).

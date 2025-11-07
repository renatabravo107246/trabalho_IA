% Sistema de Representação de Conhecimento e Raciocínio para Tensão Arterial
% Trabalho de Inteligência Artificial

% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
% Definir os dynamic para cada um dos predicados
% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#udhw
:- dynamic (paciente/5).
:- dynamic (consulta/7).
:- dynamic (ta/6).
:- dynamic (excecao/1).
:- op(900, xfy, '::').
:- dynamic ('-'/1).
:- discontiguous (::)/2.
:- discontiguous paciente/5.
:- discontiguous consulta/7.
:- discontiguous excecao/1.
:- discontiguous interdito/1.
:- discontiguous (-)/1.
:- dynamic (paciente/5).
:- dynamic (consulta/7).
:- dynamic (ta/6).
:- dynamic (excecao/1).
:- dynamic ('-'/1).
:- dynamic impreciso/2.

% ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- 
% --- Sistemas de inferência -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
% ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- 


% Extensao do meta-predicado si: Questao, Resposta -> {V,F,D}
si( Questao, verdadeiro):- Questao.
si( Questao, falso):- -Questao.
si( Questao, desconhecido) :- nao(Questao), nao(-Questao).


% Extensao do meta-predicado siC: Questao1, Questao2, Resposta -> {V,F,D}
siC( Q1, Q2, verdadeiro) :- 
	si(Q1, verdadeiro), 
	si(Q2, verdadeiro).
siC( Q1, Q2, falso) :- 
	si(Q1, verdadeiro), 
	si(Q2, falso).
siC( Q1, Q2, desconhecido) :- 
	si( Q1, verdadeiro), 
	si( Q2, desconhecido).
siC( Q1, Q2, falso) :- 
	si( Q1, falso), 
	si( Q2, verdadeiro).
siC( Q1, Q2, falso) :- 
	si( Q1, falso), 
	si( Q2, falso).
siC( Q1, Q2, falso) :- 
	si( Q1, falso), 
	si( Q2, desconhecido).
siC( Q1, Q2, desconhecido) :- 
	si( Q1, desconhecido), 
	si( Q2, verdadeiro).
siC( Q1, Q2, falso) :- 
	si( Q1, desconhecido), 
	si( Q2, falso).
siC( Q1, Q2, desconhecido) :- 
	si( Q1, desconhecido), 
	si( Q2, desconhecido).


% Extensao do meta-predicado siD: Questao1, Questao2, Resposta -> {V,F,D}
siD( Q1, Q2, verdadeiro) :-
	si( Q1, verdadeiro),
	si( Q2, verdadeiro).
siD( Q1, Q2, verdadeiro) :-
	si( Q1, verdadeiro),
	si( Q2, falso).
siD( Q1, Q2, verdadeiro) :-
	si( Q1, verdadeiro),
    si( Q2, desconhecido).
siD( Q1, Q2, verdadeiro) :-
	si( Q1, falso),
	si( Q2, verdadeiro).
siD( Q1, Q2, falso) :-
	si( Q1, falso),
	si( Q2, falso).
siD( Q1, Q2, desconhecido) :-
	si( Q1, falso),
	si( Q2, desconhecido).
siD( Q1, Q2, verdadeiro) :-
	si( Q1, desconhecido),
	si( Q2, verdadeiro).
siD( Q1, Q2, desconhecido) :-
	si( Q1, desconhecido),
	si( Q2, falso).
siD( Q1, Q2, desconhecido) :-
	si( Q1, desconhecido),
	si( Q2, desconhecido).


% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
% --- Predicados Auxiliares -----------------------------------------------------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

% Extensao do predicado comprimento: L, N -> {V,F}
comprimento([],0).
comprimento([_|L],N):-
    comprimento(L,N1),
    N is N1 + 1.


% Extensao do predicado pertence: 
pertence(X, [X|_]).
pertence(N, [_|Tail]) :-
    pertence(N, Tail).


% Extensao do meta-predicado nao: Questao -> {V,F}.
nao(Questao):- Questao, !, fail.
nao(Questao).

% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
% --- Classificacao da Tensao Arterial ----------------------------------------------------------------------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



% Classificação usando limites: Sistólica e Diastólica
classifica_ta(Sist, Dias, 'Hipertensao Estagio 2') :-
    Sist >= 140, Dias >= 90.
classifica_ta(Sist, Dias, 'Hipertensao Estagio 1') :-
    (Sist >= 130, Sist =< 139) ; (Dias >= 80, Dias =< 89).
classifica_ta(Sist, Dias, 'Pre-hipertensao') :-
    Sist >= 120, Sist =< 129.
classifica_ta(Sist, Dias, 'Normal') :-
    Sist < 120, Dias < 80.

% Obter possíveis valores sistólicos para uma consulta (tratando impreciso e excecoes)
sist_valor(ConsultaId, Val) :-
    consulta(ConsultaId, _, _, _, Sist, _, _),
    ( integer(Sist) -> Val = Sist
    ; Sist = impreciso(Inf,Sup), between(Inf,Sup,Val)
    ; Sist = alguem_sist -> fail
    ; Sist = desconhecido, fail
    ).
% também considerar excecoes definidas para a mesma consulta
sist_valor(ConsultaId, Val) :-
    excecao(consulta(ConsultaId, _, _, _, S, _, _)),
    ( integer(S) -> Val = S
    ; S = impreciso(Inf,Sup), between(Inf,Sup,Val)
    ).

% agregador de possíveis valores para sistólica
sist_possiveis(ConsultaId, Lista) :-
    findall(V, sist_valor(ConsultaId,V), L),
    sort(L, Lista).

% Classificação considerando imprecisões: devolve lista de possíveis classificações
classificacao_consulta(ConsultaId, Possiveis) :-
    sist_possiveis(ConsultaId, SistList),
    findall(Cl, ( member(S, SistList), consulta(ConsultaId, _, _, _, _, Dias, _), classifica_ta(S, Dias, Cl) ), L),
    ( L == [] -> Possiveis = [desconhecido] ; sort(L, Possiveis) ).

% Versão que lida com excecoes Múltiplas
classificacao_consulta_aggregate(ConsultaId, PossFinal) :-
    findall(Pos, (
        (excecao(consulta(ConsultaId, D, P, I, S, DI, PU)) ->
            ( integer(S), classifica_ta(S, DI, Cl), Pos = [Cl] ;
              ( S = impreciso(Inf,Sup) -> findall(C, (between(Inf,Sup,SV), classifica_ta(SV, DI, C)), CList), sort(CList, Pos)
              ; Pos = [desconhecido])
        ; (consulta(ConsultaId, D, P, I, S, DI, PU) ->
            ( integer(S) -> (classifica_ta(S, DI, Cl) -> Pos=[Cl] ; Pos=[desconhecido]) ;
              ( S = impreciso(Inf,Sup) -> findall(C3, (between(Inf,Sup,SV2), classifica_ta(SV2, DI, C3)), CList2), sort(CList2, Pos)
                ; Pos=[desconhecido] )
            )
          ; Pos=[desconhecido] )
        )
    ), Lista),
    flatten(Lista, Flat),
    ( Flat == [] -> PossFinal = [desconhecido] ; sort(Flat, PossFinal) ).

% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
% --- Pressuposto Mundo Fechado  --------------------------------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-paciente(IdPac,Nome,DataNasc,Sexo,Morada) :- nao(paciente(IdPac,Nome,DataNasc,Sexo,Morada)),
                                              nao(excecao(paciente(IdPac,Nome,DataNasc,Sexo,Morada))).

-consulta(IdCons, Data, Pac, Idade, Sist, Dias, Puls) :- nao(consulta(IdCons, Data, Pac, Idade, Sist, Dias, Puls)),
                                                         nao(excecao(consulta(IdCons, Data, Pac, Idade, Sist, Dias, Puls))).

-ta(IdTa, Classificacao, SistInf, SistSup, DiasInf, DiasSup) :- nao(ta(IdTa, Classificacao, SistInf, SistSup, DiasInf, DiasSup)),
                                                               nao(excecao(ta(IdTa, Classificacao, SistInf, SistSup, DiasInf, DiasSup))).

% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
% --- Dados ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

% paciente: IdPaciente, Nome, DataNascimento (Dia,Mes,Ano), Sexo (M/F), Morada -> {V,F,D}
% consulta: IdConsulta, Data(Dia,Mes,Ano), IdPaciente, Idade, Sistólica, Diastólica, Pulsação -> {V,F,D}
% ta: IdTA, Classificacao, SistInf, SistSup, DiasInf, DiasSup -> {V,F,D}



% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
% --- Conhecimento perfeito positivo -----------------------------------------------------------------------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

% Extensao do predicado paciente:  IdPaciente, Nome, (Dia,Mes,Ano), Sexo, Morada -> {V,F,D}
paciente(p100, 'Joao Silva', (10,5,1980), M, 'Rua A, 12').
paciente(p200, 'Maria Costa', (1,3,1972), F, 'Rua B, 4').
paciente(p300, 'Rui Pereira', (15,8,1950), M, 'Rua C, 7').
paciente(p400, 'Ana Santos', (12,12,1990), F, 'Rua D, 8').
paciente(p500, 'Carlos Lima', (5,7,1985), M, 'Rua E, 10').

% Extensao do predicado consulta:  IdConsulta, (Dia,Mes,Ano), IdPaciente, Idade, Sistólica, Diastólica, Pulsação -> {V,F,D}
consulta(c100, (15,10,2025), p100, 45, 132, 85, 72).  % Hipertensão Estágio 1
consulta(c101, (20,10,2025), p100, 45, 142, 92, 75).  % Hipertensão Estágio 2
consulta(c200, (1,9,2025),  p200, 53, 118, 78, 70).   % Normal
consulta(c201, (3,9,2025),  p200, 53, 125, 82, 73).   % Pré-hipertensão
consulta(c300, (8,9,2025),  p300, 75, 160, 95, 80).   % Hipertensão Estágio 2
consulta(c400, (10,10,2025), p400, 35, 120, 79, 68).  % Pré-hipertensão
consulta(c500, (12,10,2025), p500, 40, 110, 70, 71).  % Normal

% Extensao do predicado ta:  IdTA, Classificacao, SistInf, SistSup, DiasInf, DiasSup -> {V,F,D}
ta(ta_normal, 'Normal', 0, 119, 0, 79).
ta(ta_pre, 'Pre-Hipertensao', 120, 129, 0, 79).
ta(ta_estg1, 'Hipertensao Estagio 1', 130, 139, 80, 89).
ta(ta_estg2, 'Hipertensao Estagio 2', 140, 999, 90, 999).



% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
% --- Conhecimento perfeito negativo -----------------------------------------------------------------------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

% A paciente 'Ines' nao pertence à base de pacientes (nunca foi registada)
-paciente(p999, 'Ines', (1,1,1990), f, 'Lugar X').

% O paciente p777 não existe, logo não há consultas associadas
-consulta(c777, (5,9,2025), p777, 55, 135, 85, 70).

% A clínica esteve fechada no dia 25/12/2024, logo não houve consultas nesse dia
-consulta(c999, (25,12,2024), p100, 44, 125, 80, 70).

% Não existem classificações TA com valores sistólicos negativos
-ta(ta_erro, 'Valor Invalido', -10, -1, 0, 79).

% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
% ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- 
% --- Evolucao da Base de Conhecimento ---------------------------------------------------------------------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

% Extensao do predicado que permite a evolucao do conhecimento

evolucao(Termo):- findall(Invariante,+Termo::Invariante,Lista),
                    insercao(Termo),
                    validar(Lista).

insercao(Termo):- assert(Termo).
insercao(Termo):- retract(Termo),!,fail.

validar([]).
validar([R|LR]):- R, validar(LR).

% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
% --- Invariantes de Insercao: PACIENTE (seguem o mesmo padrão do IMC) --------------------------------------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


% Nao permitir a insercao de paciente com a idade fora de um intervalo prático (ex.: 0 < idade < 130)
+paciente(_,_,(D,M,A),_,_):: (A >= 1900, A =< 2100). % garante ano plausível

% Nao permitir mes fora do intervalo 1..12
+paciente(_,_,(_,M,_),_,_):: (M>=1, M=<12).

% dias válidos para meses com 31 dias
(+paciente(_,_,(D,M,_),_,_)::(D>=1, D=<31)):- pertence(M,[1,3,5,7,8,10,12]).

% dias válidos para meses com 30 dias
(+paciente(_,_,(D,M,_),_,_)::(D>=1, D=<30)):- pertence(M,[4,6,9,11]).

% fevereiro até 29 (considerando anos bissextos simplificados: <=29)
(+paciente(_,_,(D,M,A),_,_):: (D>=1, D=<29)):- (M==2).

% Garantir unicidade de IdPaciente
+paciente(Id,_,_,_,_):: (findall(Id, paciente(Id,_,_,_,_), S), comprimento(S,N), N == 1).

% Nao permitir inserir conhecimento negativo de paciente se existir positivo
+(-paciente(Id,_,_,_,_)):: (findall(Id, paciente(Id,_,_,_,_), S), comprimento(S,N), N==0).

% Nao permitir inserir paciente se existir conhecimento perfeito negativo para esse Id
+(paciente(Id,_,_,_,_)):: (findall(Id,(-paciente(Id,_,_,_,_)), S), comprimento(S,N), N==0).

% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
% --- Invariantes de Insercao: CONSULTA ------------------------------------------------------------------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


% Idade plausível
+consulta(_,(_,_,_),_,Idade,_,_,_):: (Idade>=0, Idade=<130).

% Mes válido
+consulta(_,(_,M,_),_,_,_,_,_):: (M>=1, M=<12).

% dias válidos (meses 31)
(+consulta(_,(D,M,_),_,_,_,_,_)::(D>=1, D=<31)):- pertence(M,[1,3,5,7,8,10,12]).
% dias válidos (meses 30)
(+consulta(_,(D,M,_),_,_,_,_,_)::(D>=1, D=<30)):- pertence(M,[4,6,9,11]).
% fevereiro simplificado até 29
(+consulta(_,(D,M,A),_,_,_,_,_):: (D>=1, D=<29)):- (M==2).

% Pressões sistólica e diastólica plausíveis (Sist 0..300, Dias 0..200), ou impreciso/desconhecido/interdito
+consulta(_,_,_,_,S,DI,_):: ( (integer(S), S>=0, S=<300) ; S = impreciso(_,_) ; S = desconhecido ; S = alguemX ).
+consulta(_,_,_,_,_,DI,_):: ( (integer(DI), DI>=0, DI=<200) ; DI = desconhecido ; DI = impreciso(_,_) ; DI = alguemX ).

% Unicidade de IdConsulta
+consulta(Id,_,_,_,_,_,_):: (findall(Id, consulta(Id,_,_,_,_,_,_), S), comprimento(S,N), N == 1).

% Consulta deve referenciar paciente existente (ou paciente com campo incerto/interdito para permitir registos)
+consulta(_,_,Pac,_,_,_,_):: ( paciente(Pac,_,_,_,_) ; paciente(Pac,_,_,_,alguem_morada) ).

% Nao permitir inserir conhecimento negativo de uma consulta se existir positivo
+(-consulta(Id,_,_,_,_,_,_)):: (findall(Id, consulta(Id,_,_,_,_,_,_), S), comprimento(S,N), N==0).

% Nao permitir inserir consulta se existir conhecimento perfeito negativo
+(consulta(Id,_,_,_,_,_,_)):: (findall(Id,(-consulta(Id,_,_,_,_,_,_)), S), comprimento(S,N), N==0).

% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
% --- Invariantes de Insercao: TA (tabela classificativa) -------------------------------------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


% Unicidade de Id de TA
+ta(IdTa,_,_,_,_,_):: (findall(IdTa, ta(IdTa,_,_,_,_,_), S), comprimento(S,N), N==1).

% Limites devem ser plausíveis e coerentes (SistInf <= SistSup, DiasInf <= DiasSup)
+ta(_,_,Sinf,Ssup,Dinf,Dsup):: ( integer(Sinf), integer(Ssup), integer(Dinf), integer(Dsup), Sinf =< Ssup, Dinf =< Dsup ).


% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
% --- Involucao da Base de Conhecimento (igual ao modelo) ------------------------------------------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


involucao(Termo):- findall(Invariante, -Termo::Invariante, Lista),
                        remocao(Termo),
                        teste(Lista).

remocao(Termo):-retract(Termo).
remocao(Termo):-assertz(Termo), !, fail.

teste([]).
teste([R|LR]):-R,teste(LR).

% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
% --- Conhecimento Imperfeito: Exemplos (incerto, impreciso, interdito) -----------------------------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
% --- Conhecimento Imperfeito Incerto (Tipo I) -------------------------------------------------------------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

% O paciente Alex SemAbrigo não tem morada fixa conhecida
paciente(555111, 'Alex SemAbrigo', (8,7,1985), M, nenhum).
excecao(paciente(Id,Nome,Data,Sexo,Morada)) :- paciente(Id,Nome,Data,Sexo,nenhum).

% O paciente Marta não recorda o dia exato de nascimento
paciente(666222, 'Marta Oliveira', desconhecido, f, 'Rua do Sol, 10').
excecao(paciente(Id,Nome,Data,Sexo,Morada)) :- paciente(Id,Nome,desconhecido,Sexo,Morada).

% Consulta em que a pulsação não foi registada (valor desconhecido)
consulta(c300, (1,10,2025), p300, 75, 150, 95, desconhecido).
excecao(consulta(IdC, D, P, I, S, DI, Puls)) :- consulta(IdC, D, P, I, S, DI, desconhecido).

% Consulta de um paciente cujo valor diastólico não foi anotado
consulta(c301, (5,10,2025), p200, 53, 132, desconhecido, 74).
excecao(consulta(IdC, D, P, I, S, DI, Puls)) :- consulta(IdC, D, P, I, S, desconhecido, Puls).


% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
% --- Conhecimento Imperfeito Impreciso (Tipo II) ----------------------------------------------------------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

% Paciente com dúvida no ano de nascimento (1959 ou 1960)
excecao(paciente(777888, 'Francisco', (23,10,1959), M, 'Sao Tome')).
excecao(paciente(777888, 'Francisco', (23,10,1960), M, 'Sao Tome')).

% Paciente que sabe apenas que nasceu em novembro, sem lembrar o dia
excecao(paciente(222333, 'Helena', (1,11,1980), f, 'Porto')).
excecao(paciente(222333, 'Helena', (2,11,1980), f, 'Porto')).
excecao(paciente(222333, 'Helena', (3,11,1980), f, 'Porto')).
excecao(paciente(222333, 'Helena', (4,11,1980), f, 'Porto')).
excecao(paciente(222333, 'Helena', (5,11,1980), f, 'Porto')).

% Consulta com valor sistólico aproximado (intervalo impreciso)
consulta(c400, (2,10,2025), p100, 45, impreciso(130,139), 85, 73).

% Consulta com disjunção de valores (duas possibilidades de registo)
excecao(consulta(c401, (3,10,2025), p200, 53, 118, 78, 70)).
excecao(consulta(c401, (3,10,2025), p200, 53, 122, 78, 72)).

% Consulta em que a tensão sistólica é incerta entre dois valores prováveis
excecao(consulta(c402, (4,10,2025), p400, 30, 135, 85, 70)).
excecao(consulta(c402, (4,10,2025), p400, 30, 138, 85, 70)).


% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
% --- Conhecimento Imperfeito Interdito (Tipo III) ---------------------------------------------------------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

% Paciente recusa revelar a morada
paciente(888777, 'Celeb', (5,5,1980), f, alguem_morada).
excecao(paciente(Id,Nome,Data,Sexo,M)) :- paciente(Id,Nome,Data,Sexo,alguem_morada).
interdito(alguem_morada).

% Consulta com valor sistólico interdito (não pode ser revelado)
consulta(c500, (4,10,2025), 888777, 45, alguem_sist, 80, 70).
excecao(consulta(Id, D, P, I, S, DI, Puls)) :- consulta(Id, D, P, I, alguem_sist, DI, Puls).
interdito(alguem_sist).

% Consulta com valor diastólico interdito
consulta(c501, (5,10,2025), 888777, 45, 145, alguem_dias, 72).
excecao(consulta(Id, D, P, I, S, DI, Puls)) :- consulta(Id, D, P, I, S, alguem_dias, Puls).
interdito(alguem_dias).

% Invariantes que impedem inserção de dados no parâmetro interdito
+paciente(Id,Nome,Data,Sexo,M):: 
    (findall(M,(paciente(888777,'Celeb',(5,5,1980),f,M),nao(interdito(M))),S),
     comprimento(S,N), N==0).

+consulta(Id,(_,_,_),P,_,S,_,_):: 
    (findall(S,(consulta(c500,(_,_,_),888777,_,S,_,_),nao(interdito(S))),SList),
     comprimento(SList,N2), N2==0).

+consulta(Id,(_,_,_),P,_,_,D,_):: 
    (findall(D,(consulta(c501,(_,_,_),888777,_,_,D,_),nao(interdito(D))),DList),
     comprimento(DList,N3), N3==0).


% ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- 
% --- Inserir conhecimento imperfeito incerto --------------------------------------------------------------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

inserir_incerto_paciente(Id,Nome,(D,M,A),Sexo,Morada):- evolucao(paciente(Id,Nome,(D,M,A),Sexo,Morada)),
                                                      evolucao(excecao(paciente(Id,Nome,(D,M,A),Sexo,Morada)):- paciente(Id,Nome,(D,M,A),Sexo,Morada)).

inserir_incerto_consulta(IdC,(D,M,A),IdP,Idade,Sist,Dias,Puls):- evolucao(consulta(IdC,(D,M,A),IdP,Idade,Sist,Dias,Puls)),
                                                               evolucao(excecao(consulta(IdC,(D,M,A),IdP,Idade,Sist,Dias,Puls)):- consulta(IdC,(D,M,A),IdP,Idade,Sist,Dias,Puls)).


%----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
%--- Inserir conhecimento impreciso -------------------------------------------------------------------------------------------------------------------------------------
%------------------------------------------------------------------------------------------------------------------------------------

% Inserir uma lista de exceções imprecisas (por exemplo, várias possibilidades de valores)
inserir_conhecimento_impreciso([]).
inserir_conhecimento_impreciso([H | T]) :-
    evolucao(excecao(H)),
    inserir_conhecimento_impreciso(T).

% Inserir conhecimento impreciso para pacientes (várias datas possíveis, por exemplo)
inserir_impreciso_paciente(IdP, Nome, (Dia,Mes,Ano1), Sexo, Morada) :-
    evolucao(excecao(paciente(IdP, Nome, (Dia,Mes,Ano1), Sexo, Morada))).

inserir_impreciso_paciente(IdP, Nome, (Dia,Mes,Ano2), Sexo, Morada) :-
    evolucao(excecao(paciente(IdP, Nome, (Dia,Mes,Ano2), Sexo, Morada))).

% Inserir conhecimento impreciso para consultas (várias tensões possíveis)
inserir_impreciso_consulta(IdC, (Dia,Mes,Ano), IdP, Idade, Sist1, Diast, Puls) :-
    evolucao(excecao(consulta(IdC, (Dia,Mes,Ano), IdP, Idade, Sist1, Diast, Puls))).

inserir_impreciso_consulta(IdC, (Dia,Mes,Ano), IdP, Idade, Sist2, Diast, Puls) :-
    evolucao(excecao(consulta(IdC, (Dia,Mes,Ano), IdP, Idade, Sist2, Diast, Puls))).
%----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
%--- Predicados para transformar/permutar conhecimento (perfeito->incerto, impreciso->perfeito, etc.) ---------------------------------------------------------------------------------------
%----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
% --- Transformar conhecimento perfeito positivo em conhecimento imperfeito incerto ------------------------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

perfeito_incerto_paciente(IdP,Nome,(Dia,Mes,Ano),Sexo,Morada):- 
    involucao(paciente(IdP,Nome,(Dia,Mes,Ano),Sexo,Morada)),
    evolucao(excecao(paciente(Id,Nome,(Dia,Mes,Ano),Sexo,M)) :- paciente(IdP,Nome,(Dia,Mes,Ano),Sexo,Morada)).

perfeito_incerto_consulta(IdC,(Dia,Mes,Ano),IdP,Idade,Sist,Diast,Puls):- 
    involucao(consulta(IdC,(Dia,Mes,Ano),IdP,Idade,Sist,Diast,Puls)),
    evolucao(excecao(consulta(Id,Data,Pac,Idade,Sist,Diast,Puls)) :- consulta(IdC,(Dia,Mes,Ano),IdP,Idade,Sist,Diast,Puls)).


% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
% --- Transformar conhecimento imperfeito incerto em conhecimento perfeito positivo ou Alterar Dados -----------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

alterar_idconsulta(IdC,N_IdC):- 
    involucao(consulta(IdC,(Dia,Mes,Ano),IdP,Idade,Sist,Diast,Puls)),
    evolucao(consulta(N_IdC,(Dia,Mes,Ano),IdP,Idade,Sist,Diast,Puls)).

alterar_idade(IdC,N_Idade):- 
    involucao(consulta(IdC,(Dia,Mes,Ano),IdP,Idade,Sist,Diast,Puls)),
    evolucao(consulta(IdC,(Dia,Mes,Ano),IdP,N_Idade,Sist,Diast,Puls)).

alterar_sistolica(IdC,N_Sist):- 
    involucao(consulta(IdC,(Dia,Mes,Ano),IdP,Idade,Sist,Diast,Puls)),
    evolucao(consulta(IdC,(Dia,Mes,Ano),IdP,Idade,N_Sist,Diast,Puls)).

alterar_diastolica(IdC,N_Diast):- 
    involucao(consulta(IdC,(Dia,Mes,Ano),IdP,Idade,Sist,Diast,Puls)),
    evolucao(consulta(IdC,(Dia,Mes,Ano),IdP,Idade,Sist,N_Diast,Puls)).

alterar_pulsacao(IdC,N_Puls):- 
    involucao(consulta(IdC,(Dia,Mes,Ano),IdP,Idade,Sist,Diast,Puls)),
    evolucao(consulta(IdC,(Dia,Mes,Ano),IdP,Idade,Sist,Diast,N_Puls)).

alterar_morada(IdP,N_Morada):- 
    involucao(paciente(IdP,Nome,(Dia,Mes,Ano),Sexo,Morada)),
    evolucao(paciente(IdP,Nome,(Dia,Mes,Ano),Sexo,N_Morada)).

alterar_sexo(IdP,N_Sexo):- 
    involucao(paciente(IdP,Nome,(Dia,Mes,Ano),Sexo,Morada)),
    evolucao(paciente(IdP,Nome,(Dia,Mes,Ano),N_Sexo,Morada)).


% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
% --- Transformar conhecimento perfeito em conhecimento imperfeito impreciso -------------------------------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

perfeito_impreciso([H|T]):- 
    involucao(H),
    inserir_conhecimento_impreciso([H|T]).


% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
% --- Transformar conhecimento imperfeito impreciso em conhecimento perfeito positivo ---------------------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

impreciso_perfeito_paciente(paciente(IdP,Nome,(Dia,Mes,Ano),Sexo,Morada)):- 
    si((paciente(IdP,Nome,(Dia,Mes,Ano),Sexo,Morada)), desconhecido),
    involucao(excecao(paciente(IdP,Nome,(Dia,Mes,Ano),Sexo,Morada))),
    evolucao(paciente(IdP,Nome,(Dia,Mes,Ano),Sexo,Morada)).

impreciso_perfeito_consulta(consulta(IdC,(Dia,Mes,Ano),IdP,Idade,Sist,Diast,Puls)):- 
    si((consulta(IdC,(Dia,Mes,Ano),IdP,Idade,Sist,Diast,Puls)), desconhecido),
    involucao(excecao(consulta(IdC,(Dia,Mes,Ano),IdP,Idade,Sist,Diast,Puls))),
    evolucao(consulta(IdC,(Dia,Mes,Ano),IdP,Idade,Sist,Diast,Puls)).


% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
% --- Transformar conhecimento perfeito positivo em conhecimento perfeito negativo -------------------------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

pos_neg(IdP,IdC):- 
    involucao(consulta(IdC,(D,M,A),IdP,Idade,Sist,Diast,Puls)),
    involucao(paciente(IdP,Nome,(Dia,Mes,Ano),Sexo,Morada)),
    evolucao(-consulta(IdC,(D,M,A),IdP,Idade,Sist,Diast,Puls)),
    evolucao(-paciente(IdP,Nome,(Dia,Mes,Ano),Sexo,Morada)).


% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
% --- Transformar conhecimento perfeito negativo em conhecimento perfeito positivo -------------------------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

neg_pos(IdP,IdC):- 
    involucao(-consulta(IdC,(D,M,A),IdP,Idade,Sist,Diast,Puls)),
    involucao(-paciente(IdP,Nome,(Dia,Mes,Ano),Sexo,Morada)),
    evolucao(consulta(IdC,(D,M,A),IdP,Idade,Sist,Diast,Puls)),
    evolucao(paciente(IdP,Nome,(Dia,Mes,Ano),Sexo,Morada)).

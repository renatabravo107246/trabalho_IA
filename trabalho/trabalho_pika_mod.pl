% Sistema de Representação de Conhecimento e Raciocínio para Tensão Arterial
% Trabalho de Inteligência Artificial

% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
% Definir os dynamic para cada um dos predicados
% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

:- op(900, xfy, '::').
:- discontiguous (::)/2.

:- if(current_predicate(style_check/1)).
:- style_check(-singleton).
:- endif.

:- discontiguous paciente/5.
:- discontiguous consulta/7.
:- discontiguous excecao/1.
:- discontiguous interdito/1.
:- discontiguous (-)/1.
:- discontiguous impreciso/2.
:- dynamic (paciente/5).
:- dynamic (consulta/7).
:- dynamic (ta/6).
:- dynamic (excecao/1).
:- dynamic ('-'/1).
:- dynamic (impreciso/2).

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
nao(_).

% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
% --- SISTEMA UNIFICADO DE INFERÊNCIA PARA TENSÃO ARTERIAL-------------------------------------------------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

% Sistema principal de inferência para classificação com confiança
inferir_classificacao_ta(ConsultaId, Classificacao, Confianca) :-
    obter_valores_possiveis(ConsultaId, SistPossiveis, DiasPossiveis),
    gerar_classificacoes(SistPossiveis, DiasPossiveis, Classificacoes),
    consolidar_resultados(Classificacoes, Classificacao, Confianca).

% Obter todos os valores possíveis considerando conhecimento imperfeito
obter_valores_possiveis(ConsultaId, SistPossiveis, DiasPossiveis) :-
    findall(S, valor_sistolica_possivel(ConsultaId, S), SistList),
    findall(D, valor_diastolica_possivel(ConsultaId, D), DiasList),
    sort(SistList, SistPossiveis),
    sort(DiasList, DiasPossiveis).

% Valores possíveis para sistólica
valor_sistolica_possivel(ConsultaId, Valor) :-
    consulta(ConsultaId, _, _, _, Sist, _, _),
    obter_valor_concreto(Sist, Valor).
valor_sistolica_possivel(ConsultaId, Valor) :-
    excecao(consulta(ConsultaId, _, _, _, Sist, _, _)),
    obter_valor_concreto(Sist, Valor).

% Valores possíveis para diastólica  
valor_diastolica_possivel(ConsultaId, Valor) :-
    consulta(ConsultaId, _, _, _, _, Dias, _),
    obter_valor_concreto(Dias, Valor).
valor_diastolica_possivel(ConsultaId, Valor) :-
    excecao(consulta(ConsultaId, _, _, _, _, Dias, _)),
    obter_valor_concreto(Dias, Valor).

% Extrair valores concretos de diferentes representações
obter_valor_concreto(Valor, Valor) :- number(Valor).
obter_valor_concreto(impreciso(Min, Max), Valor) :- between(Min, Max, Valor).
obter_valor_concreto(desconhecido, _) :- fail. % Não gera valores para desconhecido
obter_valor_concreto(alguem_sist, _) :- fail.  % Não gera valores para interdito
obter_valor_concreto(alguem_dias, _) :- fail.  % Não gera valores para interdito

% Gerar classificações para todas as combinações possíveis
gerar_classificacoes(SistPossiveis, DiasPossiveis, Classificacoes) :-
    findall(Class, (
        member(Sist, SistPossiveis),
        member(Dias, DiasPossiveis), 
        classifica_ta(Sist, Dias, Class)
    ), Classificacoes).

% Wrapper compatível: classificacao_consulta/2
% Retorna a lista de classificações possíveis para uma consulta.
classificacao_consulta(ConsultaId, Possiveis) :-
    obter_valores_possiveis(ConsultaId, SistPossiveis, DiasPossiveis),
    gerar_classificacoes(SistPossiveis, DiasPossiveis, Classificacoes),
    (   Classificacoes == [] ->
        Possiveis = [desconhecido]
    ;   list_to_set(Classificacoes, Unicas),
        sort(Unicas, Possiveis)
    ).

% Consolidar resultados e calcular confiança
consolidar_resultados(Classificacoes, Classificacao, Confianca) :-
    (   Classificacoes = [] -> 
        Classificacao = desconhecido, 
        Confianca = 0.0
    ;   list_to_set(Classificacoes, Unicas),
        length(Classificacoes, Total),
        (   Unicas = [Unica] -> 
            Classificacao = Unica,
            Confianca = 1.0
        ;   encontrar_classificacao_mais_frequente(Classificacoes, Unicas, Classificacao, Frequencia),
            Confianca is Frequencia / Total
        )
    ).

% Encontrar a classificação mais frequente
encontrar_classificacao_mais_frequente(Classificacoes, Unicas, MelhorClass, MelhorFreq) :-
    maplist(contar_ocorrencias(Classificacoes), Unicas, Frequencias),
    max_list(Frequencias, MelhorFreq),
    nth0(Index, Frequencias, MelhorFreq),
    nth0(Index, Unicas, MelhorClass).

contar_ocorrencias(Lista, Elemento, Count) :-
    include(==(Elemento), Lista, Ocorrencias),
    length(Ocorrencias, Count).

% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
% --- Classificacao da Tensao Arterial ----------------------------------------------------------------------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

% Classificação básica da tensão arterial
classifica_ta(Sist, Dias, 'Hipertensao Estagio 2') :-
    Sist >= 140, Dias >= 90.
classifica_ta(Sist, Dias, 'Hipertensao Estagio 1') :-
    (Sist >= 130, Sist =< 139) ; (Dias >= 80, Dias =< 89).
classifica_ta(Sist, Dias, 'Pre-hipertensao') :-
    Sist >= 120, Sist =< 129.
classifica_ta(Sist, Dias, 'Normal') :-
    Sist < 120, Dias < 80.



% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
% --- SISTEMA DE RELATÓRIOS MÉDICOS DOS PACIENTES ----------------------------------------------------------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

% Relatório completo do paciente
relatorio_completo_paciente(PacienteId) :-
    paciente(PacienteId, Nome, DataNasc, Sexo, Morada),
    consultas_paciente(PacienteId, Consultas),
    estatisticas_paciente(PacienteId, Estatisticas),
    tendencia_paciente(PacienteId, Tendencia),
    alertas_paciente(PacienteId, Alertas),
    
    format('=== RELATÓRIO MÉDICO COMPLETO ===~n~n'),
    format('PACIENTE: ~w (~w)~n', [Nome, PacienteId]),
    format('Data Nascimento: ~w | Sexo: ~w~n', [DataNasc, Sexo]),
    format('Morada: ~w~n~n', [Morada]),
    
    format('--- HISTÓRICO DE CONSULTAS ---~n'),
    exibir_historico_consultas(Consultas),
    nl,
    
    format('--- ESTATÍSTICAS ---~n'),
    exibir_estatisticas(Estatisticas),
    nl,
    
    format('--- TENDÊNCIA ---~n'),
    exibir_tendencia(Tendencia),
    nl,
    
    format('--- ALERTAS E RECOMENDAÇÕES ---~n'),
    exibir_alertas(Alertas).

% Relatório rápido do paciente
relatorio_rapido_paciente(PacienteId) :-
    paciente(PacienteId, Nome, _, _, _),
    ultima_consulta(PacienteId, UltimaId),
    consulta(UltimaId, Data, _, Idade, Sist, Dias, _),
    inferir_classificacao_ta(UltimaId, Classificacao, Confianca),
    tendencia_paciente(PacienteId, Tendencia),
    
    format('=== RELATÓRIO RÁPIDO ===~n'),
    format('Paciente: ~w~n', [Nome]),
    format('Última consulta: ~w (Idade: ~w)~n', [Data, Idade]),
    format('Tensão: ~w/~w~n', [Sist, Dias]),
    format('Classificação: ~w (Confiança: ~2f)~n', [Classificacao, Confianca]),
    format('Tendência: ~w~n', [Tendencia]),
    estatisticas_paciente(PacienteId, Estatisticas),
    format('Comparação com média: ~w~n', [comparar_com_media(Sist, Dias, Estatisticas)]).

% Novo predicado para comparação
comparar_com_media(Sist, Dias, estatisticas(MediaSist, MediaDias, _, _, _)) :-
    (Sist > MediaSist + 10 -> 'ACIMA da média histórica' ;
     Sist < MediaSist - 10 -> 'ABAIXO da média histórica' ;
     'Dentro da média histórica').

% Obter todas as consultas de um paciente
consultas_paciente(PacienteId, Consultas) :-
    findall(consulta(Id, Data, Idade, Sist, Dias, Puls, Class),
            (consulta(Id, Data, PacienteId, Idade, Sist, Dias, Puls),
             inferir_classificacao_ta(Id, Class, _)),
            Consultas).

% Estatísticas do paciente
estatisticas_paciente(PacienteId, estatisticas(MediaSist, MediaDias, MaxSist, MinSist, NumConsultas)) :-
    findall(Sist, (consulta(_, _, PacienteId, _, Sist, _, _), number(Sist)), Sists),
    findall(Dias, (consulta(_, _, PacienteId, _, _, Dias, _), number(Dias)), Diass),
    consultas_paciente(PacienteId, Consultas),
    length(Consultas, NumConsultas),
    (Sists = [] -> MediaSist = desconhecido, MaxSist = desconhecido, MinSist = desconhecido
     ; sum_list(Sists, SistSum), length(Sists, SistLen), MediaSist is SistSum / SistLen,
       max_list(Sists, MaxSist), min_list(Sists, MinSist)
    ),
    (Diass = [] -> MediaDias = desconhecido
     ; sum_list(Diass, DiasSum), length(Diass, DiasLen), MediaDias is DiasSum / DiasLen
    ).

% Determinar tendência do paciente
tendencia_paciente(PacienteId, Tendencia) :-
    consultas_paciente(PacienteId, Consultas),
    (   Consultas = [] -> Tendencia = 'Sem histórico suficiente'
    ;   Consultas = [_] -> Tendencia = 'Aguardando mais dados'
    ;   analisar_evolucao_classificacao(Consultas, Tendencia)
    ).

analisar_evolucao_classificacao(Consultas, Tendencia) :-
    maplist(obter_classificacao, Consultas, Classificacoes),
    (   todos_iguais(Classificacoes) -> Tendencia = 'Estável'
    ;   classificado_como(Classificacoes, 'Hipertensao Estagio 2') -> Tendencia = 'Condição Grave'
    ;   tem_melhora(Classificacoes) -> Tendencia = 'Melhorando' 
    ;   Tendencia = 'Requer atenção'
    ).

obter_classificacao(consulta(_,_,_,_,_,_,Class), Class).

todos_iguais([_]).
todos_iguais([H,H|T]) :- todos_iguais([H|T]).

classificado_como([Class|_], Class).
classificado_como([_|T], Class) :- classificado_como(T, Class).

tem_melhora([Class1, Class2|_]) :-
    ordem_severidade(Class1, N1),
    ordem_severidade(Class2, N2),
    N2 < N1.
tem_melhora([_|T]) :- tem_melhora(T).

ordem_severidade('Normal', 1).
ordem_severidade('Pre-hipertensao', 2).
ordem_severidade('Hipertensao Estagio 1', 3).
ordem_severidade('Hipertensao Estagio 2', 4).
ordem_severidade(desconhecido, 5).

% Sistema de alertas
alertas_paciente(PacienteId, Alertas) :-
    findall(Alerta, condicao_alerta(PacienteId, Alerta), Alertas).

condicao_alerta(PacienteId, 'HIPERTENSÃO PERSISTENTE - Consultar cardiologista') :-
    ultimas_consultas_classificadas(PacienteId, 3, Classificacoes),
    todas_classificadas_como(Classificacoes, 'Hipertensao Estagio 2').

condicao_alerta(PacienteId, 'VALORES ELEVADOS - Monitorizar semanalmente') :-
    ultima_consulta(PacienteId, ConsultaId),
    inferir_classificacao_ta(ConsultaId, 'Hipertensao Estagio 2', _).

condicao_alerta(PacienteId, 'VARIABILIDADE ALTA - Verificar técnica de medição') :-
    estatisticas_paciente(PacienteId, estatisticas(_, _, MaxSist, MinSist, N)),
    N >= 2,
    MaxSist - MinSist > 30.

condicao_alerta(PacienteId, 'POUCAS CONSULTAS - Agendar acompanhamento') :-
    consultas_paciente(PacienteId, Consultas),
    length(Consultas, N),
    N < 2.

% Predicados auxiliares para relatórios
ultima_consulta(PacienteId, ConsultaId) :-
    consulta(ConsultaId, _, PacienteId, _, _, _, _),
    \+ (consulta(OutraId, _, PacienteId, _, _, _, _), OutraId @> ConsultaId).

ultimas_consultas_classificadas(PacienteId, N, Classificacoes) :-
    findall(Class, (
        consulta(Id, _, PacienteId, _, _, _, _),
        inferir_classificacao_ta(Id, Class, _)
    ), Todas),
    reverse(Todas, Reversas),
    primeiros_n(Reversas, N, Classificacoes).

primeiros_n(Lista, N, Primeiros) :-
    length(Primeiros, N),
    append(Primeiros, _, Lista).

todas_classificadas_como(Classificacoes, Class) :-
    forall(member(C, Classificacoes), C == Class).

% Exibição dos relatórios
exibir_historico_consultas(Consultas) :-
    forall(member(consulta(Id, Data, Idade, Sist, Dias, Puls, Class), Consultas),
           format('~w: ~w/~w (Idade: ~w) - ~w~n', [Data, Sist, Dias, Idade, Class])).

exibir_estatisticas(estatisticas(MediaSist, MediaDias, MaxSist, MinSist, NumConsultas)) :-
    format('Consultas realizadas: ~w~n', [NumConsultas]),
    format('Média Sistólica: ~w~n', [MediaSist]),
    format('Média Diastólica: ~w~n', [MediaDias]),
    format('Máxima Sistólica: ~w~n', [MaxSist]),
    format('Mínima Sistólica: ~w~n', [MinSist]).

exibir_tendencia(Tendencia) :-
    format('~w~n', [Tendencia]).

exibir_alertas([]) :-
    format('Nenhum alerta crítico.~n').
exibir_alertas([Alerta|Alertas]) :-
    format('⚠️  ~w~n', [Alerta]),
    exibir_alertas(Alertas).

% Listar pacientes críticos
pacientes_criticos :-
    format('=== PACIENTES CRÍTICOS ===~n'),
    forall(paciente(PacienteId, Nome, _, _, _), (
        (condicao_critica(PacienteId) ->
            format('~w (~w) - REQUER ATENÇÃO IMEDIATA~n', [Nome, PacienteId])
        ; true)
    )).

condicao_critica(PacienteId) :-
    ultima_consulta(PacienteId, ConsultaId),
    inferir_classificacao_ta(ConsultaId, 'Hipertensao Estagio 2', Confianca),
    Confianca > 0.7.

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
paciente(p100, 'Joao Silva', (10,5,1980), m, 'Rua A, 12').
paciente(p200, 'Maria Costa', (1,3,1972), f, 'Rua B, 4').
paciente(p300, 'Rui Pereira', (15,8,1950), m, 'Rua C, 7').
paciente(p400, 'Ana Santos', (12,12,1990), f, 'Rua D, 8').
paciente(p500, 'Carlos Lima', (5,7,1985), m, 'Rua E, 10').

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
% --- Invariantes de Insercao: PACIENTE  --------------------------------------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


% Nao permitir a insercao de paciente com a idade fora de um intervalo prático (ex.: 0 < idade < 130)
% usa variáveis D e M nas invariantes quando são referenciadas múltiplas vezes
+paciente(_,_,(D,M,A),_,_):: (A >= 1900, A =< 2100). % garante ano plausível

% Nao permitir mes fora do intervalo 1..12
+paciente(_,_,(D,M,_),_,_):: (M>=1, M=<12).

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

% Sexo só pode ser masculino ou feminino
+paciente(Id, Nome, Data, Sexo, Morada) ::
    (member(Sexo, [masculino, feminino])).





% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
% --- Invariantes de Insercao: CONSULTA ------------------------------------------------------------------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


% Idade plausível
+consulta(_,(_,_,_),_,Idade,_,_,_):: (Idade>=0, Idade=<130).

% Mes válido
+consulta(_, (D,M,_),_,_,_,_,_):: (M>=1, M=<12).

% dias válidos (meses 31)
(+consulta(_, (D,M,_),_,_,_,_,_)::(D>=1, D=<31)):- pertence(M,[1,3,5,7,8,10,12]).
% dias válidos (meses 30)
(+consulta(_, (D,M,_),_,_,_,_,_)::(D>=1, D=<30)):- pertence(M,[4,6,9,11]).
% fevereiro simplificado até 29
(+consulta(_, (D,M,A),_,_,_,_,_):: (D>=1, D=<29)):- (M==2).

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


% Pressões arteriais devem ser positivas e fisiologicamente válidas
+consulta(_, _, _, _, Sist, Dias, Puls) ::
    (Sist >= 60, Sist =< 250,
     Dias >= 40, Dias =< 150,
     Puls >= 30, Puls =< 200).


% Idade positiva e razoável
+consulta(_, _, _, Idade, _, _, _) ::
    (Idade > 0, Idade =< 120).


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
paciente(555111, 'Alex SemAbrigo', (8,7,1985), m, nenhum).
excecao(paciente(Id,Nome,Data,Sexo,nenhum)) :- paciente(Id,Nome,Data,Sexo,nenhum).

% O paciente Marta não recorda o dia exato de nascimento
paciente(666222, 'Marta Oliveira', desconhecido, f, 'Rua do Sol, 10').
excecao(paciente(Id,Nome,desconhecido,Sexo,Morada)) :- paciente(Id,Nome,desconhecido,Sexo,Morada).

% Consulta em que a pulsação não foi registada (valor desconhecido)
consulta(c300, (1,10,2025), p300, 75, 150, 95, desconhecido).
excecao(consulta(IdC, D, P, I, S, DI, desconhecido)) :- consulta(IdC, D, P, I, S, DI, desconhecido).

% Consulta de um paciente cujo valor diastólico não foi anotado
consulta(c301, (5,10,2025), p200, 53, 132, desconhecido, 74).
excecao(consulta(IdC, D, P, I, S, desconhecido, Puls)) :- consulta(IdC, D, P, I, S, desconhecido, Puls).


% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
% --- Conhecimento Imperfeito Impreciso (Tipo II) ----------------------------------------------------------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

% Paciente com dúvida no ano de nascimento (1959 ou 1960)
excecao(paciente(777888, 'Francisco', (23,10,1959), m, 'Sao Tome')).
excecao(paciente(777888, 'Francisco', (23,10,1960), m, 'Sao Tome')).

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
excecao(paciente(Id,Nome,Data,Sexo,alguem_morada)) :- paciente(Id,Nome,Data,Sexo,alguem_morada).
interdito(alguem_morada).

% Consulta com valor sistólico interdito (não pode ser revelado)
consulta(c500i, (4,10,2025), 888777, 45, alguem_sist, 80, 70).
excecao(consulta(Id, D, P, I, alguem_sist, DI, Puls)) :- consulta(Id, D, P, I, alguem_sist, DI, Puls).
interdito(alguem_sist).

% Consulta com valor diastólico interdito
consulta(c501, (5,10,2025), 888777, 45, 145, alguem_dias, 72).
excecao(consulta(Id, D, P, I, S, alguem_dias, Puls)) :- consulta(Id, D, P, I, S, alguem_dias, Puls).
interdito(alguem_dias).

% Invariantes que impedem inserção de dados no parâmetro interdito
+paciente(Id,Nome,Data,Sexo,M):: 
    (findall(M,(paciente(888777,'Celeb',(5,5,1980),f,M),nao(interdito(M))),S),
     comprimento(S,N), N==0).

+consulta(Id,(_,_,_),P,_,S,_,_):: 
    (findall(S,(consulta(c500i,(_,_,_),888777,_,S,_,_),nao(interdito(S))),SList),
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



% Substitui completamente os dados do paciente Id por novos valores (respeita invariantes)
editar_paciente(Id, NovoNome, (ND,NM,NA), NovoSexo, NovaMorada) :-
    paciente(Id, Nome, (D,M,A), Sexo, Morada),        % encontra registo actual
    involucao(paciente(Id, Nome, (D,M,A), Sexo, Morada)),
    evolucao(paciente(Id, NovoNome, (ND,NM,NA), NovoSexo, NovaMorada)).





% --------------------------------------------------------------------
% EDIÇÃO PARCIAL DE FACTOS (atualiza apenas um campo mantendo os outros)
% Predicados adicionados:
% - editar_paciente_parcial(Id, Campo, NovoValor)
% - editar_consulta_parcial(IdConsulta, Campo, NovoValor)
% - editar_ta_parcial(IdTA, Campo, NovoValor)
% Campos aceites (strings/átomos indicativos):
% paciente: nome, data, sexo, morada
% consulta: data, idade, sistolica, diastolica, pulsacao, paciente
% ta: classificacao, sist_inf, sist_sup, dias_inf, dias_sup
% --------------------------------------------------------------------

% editar_paciente_parcial(Id, Campo, NovoValor)
editar_paciente_parcial(Id, nome, NovoNome) :-
    paciente(Id, Nome, Data, Sexo, Morada),
    involucao(paciente(Id, Nome, Data, Sexo, Morada)),
    evolucao(paciente(Id, NovoNome, Data, Sexo, Morada)).

editar_paciente_parcial(Id, data, (ND,NM,NA)) :-
    paciente(Id, Nome, Data, Sexo, Morada),
    involucao(paciente(Id, Nome, Data, Sexo, Morada)),
    evolucao(paciente(Id, Nome, (ND,NM,NA), Sexo, Morada)).

editar_paciente_parcial(Id, sexo, NovoSexo) :-
    paciente(Id, Nome, Data, Sexo, Morada),
    involucao(paciente(Id, Nome, Data, Sexo, Morada)),
    evolucao(paciente(Id, Nome, Data, NovoSexo, Morada)).

editar_paciente_parcial(Id, morada, NovaMorada) :-
    paciente(Id, Nome, Data, Sexo, Morada),
    involucao(paciente(Id, Nome, Data, Sexo, Morada)),
    evolucao(paciente(Id, Nome, Data, Sexo, NovaMorada)).

% fallback para campo desconhecido
editar_paciente_parcial(_, Campo, _) :-
    \+ member(Campo, [nome,data,sexo,morada]),
    format('Campo inválido para paciente: ~w~n', [Campo]), !, fail.

% --------------------------------------------------------------------
% editar_consulta_parcial(IdConsulta, Campo, NovoValor)
% Campos: data, idade, sistolica, diastolica, pulsacao, paciente
editar_consulta_parcial(IdC, data, (ND,NM,NA)) :-
    consulta(IdC, Data, IdP, Idade, Sist, Dias, Puls),
    involucao(consulta(IdC, Data, IdP, Idade, Sist, Dias, Puls)),
    evolucao(consulta(IdC, (ND,NM,NA), IdP, Idade, Sist, Dias, Puls)).

editar_consulta_parcial(IdC, idade, NovaIdade) :-
    consulta(IdC, Data, IdP, Idade, Sist, Dias, Puls),
    involucao(consulta(IdC, Data, IdP, Idade, Sist, Dias, Puls)),
    evolucao(consulta(IdC, Data, IdP, NovaIdade, Sist, Dias, Puls)).

editar_consulta_parcial(IdC, sistolica, NovaSist) :-
    consulta(IdC, Data, IdP, Idade, Sist, Dias, Puls),
    involucao(consulta(IdC, Data, IdP, Idade, Sist, Dias, Puls)),
    evolucao(consulta(IdC, Data, IdP, Idade, NovaSist, Dias, Puls)).

editar_consulta_parcial(IdC, diastolica, NovaDias) :-
    consulta(IdC, Data, IdP, Idade, Sist, Dias, Puls),
    involucao(consulta(IdC, Data, IdP, Idade, Sist, Dias, Puls)),
    evolucao(consulta(IdC, Data, IdP, Idade, Sist, NovaDias, Puls)).

editar_consulta_parcial(IdC, pulsacao, NovaPuls) :-
    consulta(IdC, Data, IdP, Idade, Sist, Dias, Puls),
    involucao(consulta(IdC, Data, IdP, Idade, Sist, Dias, Puls)),
    evolucao(consulta(IdC, Data, IdP, Idade, Sist, Dias, NovaPuls)).

editar_consulta_parcial(IdC, paciente, NovoPaciente) :-
    consulta(IdC, Data, IdP, Idade, Sist, Dias, Puls),
    involucao(consulta(IdC, Data, IdP, Idade, Sist, Dias, Puls)),
    evolucao(consulta(IdC, Data, NovoPaciente, Idade, Sist, Dias, Puls)).

editar_consulta_parcial(_, Campo, _) :-
    \+ member(Campo, [data,idade,sistolica,diastolica,pulsacao,paciente]),
    format('Campo inválido para consulta: ~w~n', [Campo]), !, fail.

% --------------------------------------------------------------------
% editar_ta_parcial(IdTA, Campo, NovoValor)
% Campos: classificacao, sist_inf, sist_sup, dias_inf, dias_sup
editar_ta_parcial(IdTA, classificacao, NovaClass) :-
    ta(IdTA, Class, SI, SS, DI, DS),
    involucao(ta(IdTA, Class, SI, SS, DI, DS)),
    evolucao(ta(IdTA, NovaClass, SI, SS, DI, DS)).

editar_ta_parcial(IdTA, sist_inf, NovoSi) :-
    ta(IdTA, Class, SI, SS, DI, DS),
    involucao(ta(IdTA, Class, SI, SS, DI, DS)),
    evolucao(ta(IdTA, Class, NovoSi, SS, DI, DS)).

editar_ta_parcial(IdTA, sist_sup, NovoSs) :-
    ta(IdTA, Class, SI, SS, DI, DS),
    involucao(ta(IdTA, Class, SI, SS, DI, DS)),
    evolucao(ta(IdTA, Class, SI, NovoSs, DI, DS)).

editar_ta_parcial(IdTA, dias_inf, NovoDi) :-
    ta(IdTA, Class, SI, SS, DI, DS),
    involucao(ta(IdTA, Class, SI, SS, DI, DS)),
    evolucao(ta(IdTA, Class, SI, SS, NovoDi, DS)).

editar_ta_parcial(IdTA, dias_sup, NovoDs) :-
    ta(IdTA, Class, SI, SS, DI, DS),
    involucao(ta(IdTA, Class, SI, SS, DI, DS)),
    evolucao(ta(IdTA, Class, SI, SS, DI, NovoDs)).

editar_ta_parcial(_, Campo, _) :-
    \+ member(Campo, [classificacao,sist_inf,sist_sup,dias_inf,dias_sup]),
    format('Campo inválido para ta: ~w~n', [Campo]), !, fail.

% --------------------------------------------------------------------
% Utilitários: editar sem falha (tenta e repõe em caso de erro para evitar perda total)
% editar_paciente_parcial_safe/3 - tenta editar e repõe valor antigo em caso de falha
editar_paciente_parcial_safe(Id, Campo, NovoValor) :-
    paciente(Id, Nome, Data, Sexo, Morada),
    (   editar_paciente_parcial(Id, Campo, NovoValor)
    ->  true
    ;   % Em caso de falha, repõe o original (se tiver sido removido)
        (   \+ paciente(Id, Nome, Data, Sexo, Morada) -> assertz(paciente(Id, Nome, Data, Sexo, Morada)) ; true ),
        fail ).

editar_consulta_parcial_safe(IdC, Campo, NovoValor) :-
    consulta(IdC, Data, IdP, Idade, Sist, Dias, Puls),
    (   editar_consulta_parcial(IdC, Campo, NovoValor)
    ->  true
    ;   ( \+ consulta(IdC, Data, IdP, Idade, Sist, Dias, Puls) -> assertz(consulta(IdC, Data, IdP, Idade, Sist, Dias, Puls)) ; true ),
        fail ).

editar_ta_parcial_safe(IdTA, Campo, NovoValor) :-
    ta(IdTA, Class, SI, SS, DI, DS),
    (   editar_ta_parcial(IdTA, Campo, NovoValor)
    ->  true
    ;   ( \+ ta(IdTA, Class, SI, SS, DI, DS) -> assertz(ta(IdTA, Class, SI, SS, DI, DS)) ; true ),
        fail ).

% --------------------------------------------------------------------
% Exemplo de uso (comentado):
% ?- editar_paciente_parcial(p100, morada, 'Nova Rua, 99').
% ?- editar_consulta_parcial(c100, sistolica, 135).
% ?- 
 % nota: campo errado 'syst_sup' dará erro
% --------------------------------------------------------------------

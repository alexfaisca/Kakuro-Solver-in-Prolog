/* Alexandre Faisca Coelho 100120 */
:- [codigo_comum]. 

/* soma_membros(L, Soma), em que L e uma lista de inteiros e Soma e um
inteiro, significa que Some e a soma dos elementos de L */

soma_membros([], 0).
soma_membros([El|R], Soma) :-
    soma_membros(R, Soma_Resto),
    Soma is El + Soma_Resto.


/* combinacoes_soma(N, Els, Soma, Combs), em que N e um inteiro, Els e
uma lista de inteiros, e Soma e um inteiro, significa que Combs e a
lista ordenada cujos elementos sao as combinacoes N a N, dos elementos
de Els cuja soma e Soma */

combinacoes_soma(N, Els, Soma, Combs):-
    findall(Comb, (combinacao(N, Els, Comb), soma_membros(Comb, Soma)), Combs).


/* permutacoes_soma(N, Els, Soma, Perms), em que N e um inteiro, Els
e uma lista de inteiros, e Soma e um inteiro, significa que Perms e a
lista ordenada cujos elementos sao as permutacoes das combinacoes N a
N, dos elementos de Els cuja soma e Soma */

permutacoes_soma(N, Els, Soma, Perms) :-
    combinacoes_soma(N,Els,Soma,Combs), /* Obter combinacoes soma */
    /* Obter permutacoes das combinacoes soma */
    findall(Perm, (member(L, Combs), permutation(L, Perm)), Perms_Unsorted),
    sort(Perms_Unsorted, Perms). /* sort permutacoes soma */



/* Representacao interna: espaco(Soma, Lista), Soma e um inteiro nao negativo e
Lista uma lista cujos membros sao variaveis ou inteiros entre 1 e 10 */

/* Seletores tipo abstrato de dados espaco */
/* espaco_soma(Esp, S), em que Esp e um espaco significa que S e soma de Esp */
espaco_soma(espaco(S, _), S).

/* espaco_list(Esp, Lst) em que Esp e espaco significa que Lst e lista de Esp*/
espaco_list(espaco(_, Lst), Lst).

/* Identificadores tipo abstrato de dados espaco*/
/* eh_espaco(Esp) true se Esp e espaco, false caso contrario */
eh_espaco(espaco(S,Lst)) :- S >= 0, length(Lst, N), N > 0, 
    forall(member(X, Lst), ((integer(X), X > 0, X < 10); var(X))).

/* espacos_iguais(E1, E2) em que E1 e E2 sao espacos, true se forem o mesmo 
espaco e false caso contrario */
espacos_iguais(E1,E2) :-
    eh_espaco(E1), eh_espaco(E2), espaco_soma(E1, S1), espaco_soma(E2, S2),
    S1 == S2, espaco_list(E1, L1), espaco_list(E2, L2), L1 == L2.


/* espaco_fila(Fila, Esp, H_V), em que Fila e uma fila (linha ou coluna)
de um puzzle e H_V e um dos atomos h ou v, conforme se trate de uma fila
horizontal ou vertical respectivamente, significa que Esp e um espaco de
Fila */

/* Percorrer fila ate encontrar inicio de um espaco */
espaco_fila([P|R], espaco(Soma, Vars), H_V) :-
    /* Verificar se e potencial inicio de um espaco */
    P = [V,H], integer(V), integer(H),
    /* Obter soma */
    sel_H_V(V,H,H_V, Soma),
    /* Obter variaveis e confirmar existencia da espaco */
    obter_vars(R,Vars), Vars \= [].
espaco_fila([_|R], Esp, H_V) :- nonvar(R), espaco_fila(R, Esp, H_V).


/* obter_vars(Lst, Vars), em que Lst e uma lista, significa que Vars e um lista
contendo a sequencia de membros variaveis no inicio da lista Lst */

obter_vars(Lst,Vars) :-
    append(Vars,Dep,Lst), 
    forall(member(V,Vars),var(V)),
    (Dep = [] ; (Dep = [P|_], nonvar(P))).


/* sel_H_V(V,H, V_H, R), em que V, H sao inteiros, V_H um atomo h ou v conforme se
trate de uma linha ou coluna e R igual a V se for coluna e a H se for linha */

sel_H_V(V,_,v, V).
sel_H_V(_,H,h,H).


/* espacos_fila(H_V, Fila, Espacos), em que Fila e uma fila (linha ou coluna) de
uma grelha e e H_V e um dos atomos h ou v, significa que Espacos e a lista de todos os
espacos de Fila, da esquerda para a direita */

espacos_fila(H_V, Fila, Espacos) :-
    (bagof(Esp, espaco_fila(Fila, Esp, H_V), Espacos) -> true ; Espacos = []).


/* espacos_puzzle(Puzzle, Espacos), em que Puzzle e um puzzle, significa que
Espacos e a lista de espacos de Puzzle */

espacos_puzzle(Puzzle, Espacos) :-
    espacos_puzzle_aux(Puzzle, Horizontais, h), /* Obter espacos horizontais */
    mat_transposta(Puzzle, Puzzle_T), /* Obter lista de filas verticais */
    espacos_puzzle_aux(Puzzle_T, Verticais, v), /* Obter espacos verticais */
    append(Horizontais, Verticais, Espacos). /* Juntar listas dos espacos */


/* espacos_puzzle_aux(Filas, Espacos, Dir) em que Filas e uma lista de filas, e
Dir e a direcao das filas, significa que Espacos e a lista dos espacos das
filas de Filas */

espacos_puzzle_aux(Filas, Espacos, Dir) :-
    /* Obter lista das listas de espacos de cada fila de Filas */
    (bagof(Esps, Fila^(member(Fila, Filas), espacos_fila(Dir, Fila, Esps)),
    Esps_List) -> true ; Esps_List = []),
    append(Esps_List, Espacos). /* Juntar os espacos numa lista unica */


/* espacos_com_posicoes_comuns(Espacos, Esp, Esps_com), em que Espacos e uma 
lista de espacos e Esp e um espaco, significa que Esps_com e a lista de espacos
com variaveis em comum com Esp, exceptuando Esp */

espacos_com_posicoes_comuns(Espacos, Esp, Esps_com) :- 
    espaco_list(Esp, Lst), /* Obter lista de Esp */
    /* Obter espacos cuja lista tenha membros em comum com a lista de Esp */
    (bagof(Target, Lst_T^Int^(member(Target,Espacos), \+espacos_iguais(Target,
    Esp), espaco_list(Target,Lst_T), intersecaoeq(Lst_T, Lst, Int), Int \= []) 
    ,Esps_com) -> true ; Esps_com = []).


/* intersecaoeq(L1, L2, Int), em que L1, L2 sao listas significa que Int contem
os elementos que sao comuns a L1 e a L2, nao unificando variaveis */

intersecaoeq([], _, []) :- !.
intersecaoeq(_, [], []) :- !.
intersecaoeq([P|R],L2,[P|R_Int]) :- membereq(P,L2), intersecaoeq(R,L2,R_Int),!.
intersecaoeq([P|R],L2,R_Int) :- \+ membereq(P,L2), intersecaoeq(R,L2,R_Int).


/*membereq(El, L) em que L e lista, true se L contem El, false caso contrario*/

membereq(_,[]) :- false, !.
membereq(El,[P|_]) :- El == P, !. /* Compara termos sem os unificar */
membereq(El,[_|R]) :- membereq(El,R).


/* nth1eq(I, L, El), em que L e lista, I e inteiro significa que El e o I-esimo
elemento de El (variaveis nao sao unificadas) */

nth1eq(I,L,El) :- nth1eq_aux(I,L,El,1).


/* nth1eq_aux(I,L,El,N) funcao auxiliar de nth1eq, N representa o indice do
primeiro elemento de L na lista originalmente passada por nth1eq */

nth1eq_aux(_,[],_,_) :- false, !. /* Lista vazia nao tem elementos */
nth1eq_aux(N,[P|_],El,N) :- El == P, !.
nth1eq_aux(I,[P|R],El,N) :- \+(P == El), N1 is N + 1, nth1eq_aux(I,R,El,N1).


/* permutacoes_soma_espacos(Espacos, Perms_soma), Espacos e lista de espacos,
significa que Perms_soma e lista de listas de 2 elementos, em que o elemento 1
e um espaco de Espacos e o 2 a lista ordenada de permutacoes cuja soma e igual
a soma do espaco */

permutacoes_soma_espacos(Espacos, Perms_soma) :- lista_entre(1, 9, Num_Pos),
    (bagof([Esp, Perms], Sum^Vars^Ln^(member(Esp,Espacos),espaco_soma(Esp, Sum)
    ,espaco_list(Esp, Vars), length(Vars, Ln), permutacoes_soma(Ln,
    Num_Pos, Sum, Perms)), Perms_soma) -> true ; Perms_soma = []), !.


/* lista_entre(A, B, Lst), em que A e B sao inteiros significa que Lst e a
lista ordenada dos inteiros entre A e B, inclusive. */

lista_entre(A,B,Lst) :- findall(N, between(A,B,N), Lst).


/* permutacoes_espaco(Esp, Perms, Perms_soma), em que Esp e um espaco Perms e a
lista de permutacoes de Esp e Perms_soma e a lista de permutacoes dos espacos*/

permutacoes_espaco(Esp, Perms, Perms_soma) :-
    member([Esp,Perms], Perms_soma).


/* permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma) em que Perm e uma
permutacao, Esp e um espaco, Espacos e uma lista de espacos, e Perms_soma e uma
lista de listas como descrito em "permutacoes_soma_espacos", significa que Perm
e uma permutacao possivel para o espaco Esp */

permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma) :-
    permutacoes_espaco(E, Perms, Perms_soma), /*Obter permutacoes do espaco*/
    espacos_iguais(Esp, E),
    member(Perm, Perms), /* Permutacao possivel e membro de Perms */
    espaco_list(Esp, Vars), /* Obter lista do espaco */
    unificavel(Vars, Perm), /* Perm e unificavel com lista de variaveis Vars */
    /* Obter espacos com posicoes comuns a Esp e verificar validade de Perm */
    espacos_com_posicoes_comuns(Espacos, Esp, Esps_com), 
    perm_compativel(Esp, Perm, Esps_com, Perms_soma).

/* perm_compativel(Lista_Esp, Perm, Esps_com, Perms_soma), Esp e um espaco Perm
e uma permutacao, Esps_com e a lista de espacos com posicoes comuns com Esp e 
Perms_soma a lista de permutacoes dos espacos true se Perm e permutacao
compativel com os espacos de Esps_com e false caso contrario */

perm_compativel(Esp, Perm, Esps_com, Perms_soma) :-
    espaco_list(Esp,Lista_Esp), /* Obter lista de Esp que seria subsituida */
    /* Verificar validade da substituicao da lista por Perm em Esps_com */
    forall((member(Esp_c, Esps_com), permutacoes_espaco(E,Perms_C,Perms_soma),
    espacos_iguais(Esp_c,E), espaco_list(Esp_c, Lista_C), 
    simula_substituicao(Lista_Esp,Lista_C, Perm, Sim)), member(Sim, Perms_C)).


/* simula_substituicao(Lst1, Lst2, Lst3, Lst_Sim), em que Lst1, Lst2 e Lst3 sao
listas, significa que Lst_Sim e o resultado de substituir em Lst2 as variaveis 
comuns com Lst1, pelos valores que resultariam de uma unificacao com Lst3 */

simula_substituicao(_,[],_,[]) :- !. /* Caso terminal */
simula_substituicao(Lst1,[P|R],Lst3, [P_Sim|Lst_Sim_R]) :-
    is_list(P),
    /* Se termo P e lista, simular substituicao nos membros de P */
    simula_substituicao(Lst1, P, Lst3, P_Sim),
    /* Simular substituicao em R */
    simula_substituicao(Lst1, R, Lst3, Lst_Sim_R), !.
simula_substituicao(Lst1,[P|R],Lst3,[P|R_Sim]) :- 
    /* Se termo P nao e var ou nao pertence a Lst1 entao e membro de Lst_Sim */
    (nonvar(P);\+membereq(P,Lst1)), simula_substituicao(Lst1,R,Lst3,R_Sim), !.

simula_substituicao(Lst1,[P|R],Lst3,[Q|Lst_Sim_R]) :-
    var(P), /* Se elemento P e variavel nao deve ser unificado em Lst_Sim */
    nth1eq(I,Lst1, P), /* Obter indice I de P em Lst1 */
    nth1(I,Lst3, Q), /* Obter elemento I de Lst3 que substitui P em Lst_Sim*/
    simula_substituicao(Lst1,R,Lst3,Lst_Sim_R).


/* permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp, Perms_poss), Esp e um
espaco, Espacos e uma lista de espacos, e Perms_soma e uma lista de listas como
descrito em "permutacoes_soma_espacos", significa que Perms_poss e a lista cujo
elemento 1 e a lista de variaveis de Esp e o elemento 2 e lista de permutacoes
possiveis ordenada para o espaco Esp */

permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp, [V, P]) :-
    espaco_list(Esp, V), /* Obter lista do espaco */
    /* Obter as permutacoes possiveis para o espaco */
    bagof(Perm, permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma), P).


/* permutacoes_possiveis_espacos(Espacos, Perms_poss_esps), em que Espacos e 
uma lista de espacos, significa que Perms_poss_esps e a lista de permutacoes 
possiveis como descrito em "permutacoes_soma_espacos" */

permutacoes_possiveis_espacos(Espacos, Perms_poss_esps) :-
    permutacoes_soma_espacos(Espacos, Perms_soma), /* Obter permutacoes soma */
    /* Obter permutacoes possiveis de entre as permutacoes da soma */
    (bagof(Perms_poss, Esp^(member(Esp, Espacos), permutacoes_possiveis_espaco(
    Espacos, Perms_soma, Esp, Perms_poss)), Perms_poss_esps) -> 
    true ; Perms_poss_esps = []), !.


/* numeros_comuns(Lst_Perms, Numeros_comuns), em que Lst_Perms e uma lista de
permutacoes, significa que Numeros_comuns e uma lista de pares (pos, numero),
significando que todas listas de Lst_Perms tem o numero numero na posicao pos*/

numeros_comuns(Lst_Perms, Numeros_comuns) :-
    /* Obter lista do espaco e lista de permutacoes */
    Lst_Perms=[P1|R_Perms],
    /* Obter todos os pares (Index, Val) que se repetem em Lst_Perms */
    (bagof((Index, Val), Index^(nth1(Index, P1, Val),
    forall(member(Perm, R_Perms), nth1(Index, Perm, Val))), Numeros_comuns) ->
    true ; Numeros_comuns = []).

/* atribui_comuns(Perms_Possiveis), em que Perms_Possiveis e uma lista de
permutacoes possiveis, actualiza esta lista atribuindo a cada espaco numeros
comuns a todas as permutacoes possiveis para esse espaco */

atribui_comuns([]). /* Caso terminal */
atribui_comuns([[Vars,Perms_poss]|R_Perms_poss]) :-
    /*Encontrar nos. comuns do primeiro elemento de Perms_Possiveis e atribuir */
    numeros_comuns(Perms_poss,Numeros_comuns), /*Obter nos. comuns do espaco*/
    unifica_vars_ind(Vars,Numeros_comuns), /*Atribuir numeros comuns do espaco*/
    atribui_comuns(R_Perms_poss). /* Repetir para o elemento seguinte */


/* unifica_vars_ind(Vars, Valores) em que Vars e uma lista com variaveis e
Valores uma lista de pares (pos, valor) contendo a posicao pos e o termo valor
com o qual a variavel na posicao pos de Vars deve ser unificada */

unifica_vars_ind(_,[]). 
unifica_vars_ind(Vars, [(Index,Val)|R_Valores]) :-
    nth1(Index,Vars,Val), /* Unificar Val com variavel na posicao Index */
    unifica_vars_ind(Vars,R_Valores), !. /* Repetir para o elemento seguinte */


/* retira_impossiveis(Perms_Possiveis, Novas_Perms_Possiveis), em que 
Perms_Possiveis e uma lista de permutacoes possiveis, significa que 
Novas_Perms_Possiveis e o resultado de tirar permutacoes impossiveis de 
Perms_Possiveis */

retira_impossiveis([],[]) :- !. /* Caso terminal */
retira_impossiveis([[Vars, Perms]|R_Perms_Possiveis], 
[[Vars, N_Perms]|N_R_Perms_Possiveis]) :-
    /* Retirar permutacoes impossiveis do primeiro elemento da lista */
    (bagof(Perm, (member(Perm,Perms), unificavel(Vars,Perm)), N_Perms) -> true;
    N_Perms = []),
    retira_impossiveis(R_Perms_Possiveis, N_R_Perms_Possiveis). /* Repetir */


/* unificavel(Lst1, Lst2) em que Lst1, Lst2 sao listas, true se listas tem o mesmo
tamanho e os seus elementos sao unificaveis dois a dois */

unificavel([],[]).
unificavel([P1|R1], [P2|R2]) :- /* Caso em que elemento e lista */
    is_list(P1),
    unificavel(P1,P2), /* Verificar se listas sao unificaveis */
    unificavel(R1,R2), !. /* Verificar se o resto e unificavel */
unificavel([P1|R1],[P2|R2]) :-
    (var(P1) ; var(P2) ), /* Variaveis sao sempre unificaveis */
    unificavel(R1,R2), !.
unificavel([P|R1],[P|R2]) :- nonvar(P), unificavel(R1,R2).


/* simplifica(Perms_Possiveis, Novas_Perms_Possiveis) em que Perms_Possiveis e 
lista de permutacoes possiveis significa que Novas_Perms_Possiveis e resultado 
de simplificar Perms_Possiveis */

simplifica(Perms_Possiveis, Novas_Perms_Possiveis) :-
    simplifica_aux([], Perms_Possiveis, Novas_Perms_Possiveis), !.

/* simplifica_aux(Perms, N_Perms, Novas_Perms), fincao auxiliar de simplifica/2
mantem registo de Perms e N_Perms resultado da aplicacao a Perms de um ciclo de
simplificacao de modo a comparar as duas, termina quando estas sao iguais */

/* Verificar se Perms foi alterada */
simplifica_aux(Perms, N_Perms, N_Perms) :- Perms = N_Perms, !.
simplifica_aux(_, Perms, Novas_Perms) :- /*Se ha alteracao executa novo ciclo*/
    atribui_comuns(Perms), /* Atribuir numeros comuns */
    retira_impossiveis(Perms, N_Perms), /* Retirar perms impossiveis */
    simplifica_aux(Perms, N_Perms, Novas_Perms), !. /* Repetir */


/* inicializa(Puzzle, Perms_Possiveis), em que Puzzle e um puzzle significa que
Perms_Possiveis e a lista simplificada de permutacoes de Puzzle */

inicializa(Puzzle, Perms_Possiveis) :-
    /* Obter lista de espacos de Puzzle */
    espacos_puzzle(Puzzle, Espacos),
    /* Obter lista de permutacoes possiveis para Puzzle */
    permutacoes_possiveis_espacos(Espacos, Perms_Poss_Espacos),
    /* Simplificar lista de permutacoes possiveis para Puzzle */
    simplifica(Perms_Poss_Espacos, Perms_Possiveis), !.


/* espacos_por_resolver(Perms_Possiveis, Perms_Espacos), em que Perms_Possiveis
e a lista de permutacoe possiveis significa que Perms_Espacos e a lista de 
permutacoes dos espacos que tem mais de uma permutacao possivel */

espacos_por_resolver(Perms_Possiveis, Perms_Espacos) :-
    bagof(Perms, V^L_Perms^N^(member(Perms,Perms_Possiveis), Perms=[V,L_Perms],
    length(L_Perms, N), N > 1), Perms_Espacos), !.
espacos_por_resolver(_,[]). /* Se bagof falha nao ha espacos por resolver */


/* escolhe_perm_list(Lst_Perms, Perms) em que Lst_Perms e lista de permutacoes
possiveis para conjunto de espacos significa que Perms e a lista de permutacoes
escolhida segundo os criterios dados */

escolhe_perm_list(Lst_Perms, Perms) :-
    /* Obter lista dos tamanhos e indices das listas de permutacoes */
    bagof([Size,I], L^P^(nth1(I,Lst_Perms,[L,P]),length(P,Size)),Size_Ind_Lst),
    /* Ordenar as listas, listas com tamanhos menores surgirao primeiro */
    sort(Size_Ind_Lst, S_I_Sorted),
    member([_,Index],S_I_Sorted), /* Obter indice da lista de permutacoes */
    nth1(Index, Lst_Perms, Perms). /* Obter Index-esimo elemento de Lst_Perms */


/* escolhe_menos_alternativas(Perms_Possiveis, Escolha), em que Perms_Possiveis
e uma lista de permutacoes possiveis significa que Escolha e o elemento desta
lista escolhido segundo os criterios dados */

escolhe_menos_alternativas(Perms_Possiveis, Escolha) :-
    /* Obter lista de espacos com mais de uma permutacao possivel */
    espacos_por_resolver(Perms_Possiveis, Perms_Elegiveis),
    /* Escolher primeiro dos espacos com menor numero de permutacoes */
    escolhe_perm_list(Perms_Elegiveis, Escolha).


/* experimenta_perm(Escolha, Perms_Possiveis, Novas_Perms_Possiveis) em que 
Perms_Possiveis e a lista de permutacoes possiveis Escolha e um dos seus 
elementos e Novas_Perms_Possiveis e o resultado de aplicar o algoritmo dado 
para experimentar permutacoes */

experimenta_perm(Escolha, Perms_Possiveis, Novas_Perms_Possiveis) :-
    Escolha = [Lst, Lst_Perms], member(Perm, Lst_Perms), /* Obter permutacao */
    /* Substituir Escolha por [Perm, [Perm]] */
    select(Escolha, Perms_Possiveis, [Perm,[Perm]],N_Perms_Possiveis),
    /*Simular unificacao das variaveis de Lst com os termos homologos de Perm*/
    simula_substituicao(Lst, N_Perms_Possiveis, Perm, Novas_Perms_Possiveis).

    
/* resolve_aux(Perms_Possiveis, Novas_Perms_Possiveis) em que Perms_Possiveis e
uma lista de permutacoes possiveis significa que Novas_Perms_Possiveis e o
resultado de aplicar o algoritmo de resolucao de listas de permutacoes
possiveis dado */

resolve_aux(Perms_Possiveis, Novas_Perms_Possiveis) :-
    /* Escolher um espaco segundo os criterios dados */
    escolhe_menos_alternativas(Perms_Possiveis, Escolha),
    /* Experimentar atribuir uma permutacao ao espaco escolhido */
    experimenta_perm(Escolha, Perms_Possiveis, Perms_Possiveis_Escolha),
    /* Simplificar lista das permutacoes possiveis obtida */
    simplifica(Perms_Possiveis_Escolha, Perms_Possiveis_Mod),
    /* Se estiver resolvido parar, senao repetir */
    (resolvido(Perms_Possiveis_Mod) -> Novas_Perms_Possiveis =
    Perms_Possiveis_Mod, ! ;
    resolve_aux(Perms_Possiveis_Mod, Novas_Perms_Possiveis)). 


/* resolvido(Perms_Possiveis) em que Perms_Possiveis e uma lista de permutacoes
de permutacoes possiveis true se puzzle esta resolvido, false caso contrario */

resolvido(Perms_Possiveis) :- /* Verificar se nao existem veriaveis na lista */
    forall((member([Lst,_], Perms_Possiveis), member(X, Lst)), nonvar(X)).


/* resolve(Puz) em que Puz e um puzzle, resolve Puz */

resolve(Puz) :-
    /* Inicializar Puz */
    inicializa(Puz, Perms_Possiveis),
    /* Verificar se esta resolvido, se sim parar, caso contrario resolver a 
    lista de permutacoes possiveis e unificar variaveis com resultado */
    (resolvido(Perms_Possiveis) -> true ;
    resolve_aux(Perms_Possiveis, Perms_Resolvidas),
    bagof(Lst, I^R^(nth1(I,Perms_Possiveis,[Lst|R])), Espacos_Lst),
    bagof(Val, I^R^(nth1(I,Perms_Resolvidas,[Val|R])), Val_Lst),
    Espacos_Lst = Val_Lst, !).
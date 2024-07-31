:- dynamic detailed_mode_disabled/0.
:- dynamic debug_moves/0.
:- ensure_loaded('files.pl').

% --------------------- EXPLICATIE CODIFICARE STARI --------------------------
% State = [(X, Y, Type)]
% X, Y - coordonatele patratului
% Type - tile || fragile || scope || block || oswitch** || xswitch**
% ** - pentru etapa 2
%   Am codificat astfel spatiul de stari drept o lista de tupluri, fiecare
% tuplu reprezentand un patrat de pe harta. Fiecare patrat are coordonatele
% sale si tipul sau. Tipul poate fi tile, fragile, scope sau block.
%   Coordonata (0, 0) este coltul din stanga/sus, chiar dacă nu există un
% tile acolo.
% ----------------------------------------------------------------------------

% empty_state/1
% empty_state(-SNew)
% Construiește o stare goală (fără nicio informație), care va fi dată
% primului apel set/4
empty_state(S) :-
    S = [].

% set_tile/3
% set_tile(+S, +Pos, -SNew)
% Construiește starea SNew, care conține aceleași informații ca și S
% și în plus faptul că la poziția Pos se află o pătrățică normală.

% --------------------- EXPLICATIE SET_TILE_1 --------------------------------
% In cazul in care exista deja o celula, la coordonatele date ca parametru,
% marcata drept block, se va sterge aceasta celula.
% ----------------------------------------------------------------------------
set_tile(S, (X, Y), SNew) :-
    select((X, Y, block), S, SRest),
    SNew = SRest.

% --------------------- EXPLICATIE SET_TILE_2 --------------------------------
% In cazul in care nu exista o celula la coordonatele date ca parametru
% se va adauga o celula noua cu tipul tile.
% ----------------------------------------------------------------------------
set_tile(S, (X, Y), SNew) :-
    \+ member((X, Y, _), S),
    SNew = [(X, Y, tile) | S].

% set_blank/3
% set_blank(+S, +Pos, -SNew)
% Construiește starea SNew, care conține aceleași informații ca și S.
% Va fi apelat de tester doar pentru pozițiile fără pătrățele de la
% coordonate unde pentru același x și y mai mare, sau pentru același y
% și x mai mare, există pătrățele. Puteți să nu faceți nimic în acest
% predicat - depinde de cum vă reprezentați intern starea.
% --------------------- EXPLICATIE SET_BLANK --------------------------------
% Daca se gaseste o celula de tip tile la coordonatele date ca paramentru 
% se sterge celula respectiva, altfel se intoarce starea initiala.
% ----------------------------------------------------------------------------
set_blank(S, (X, Y), SNew) :-
    (select((X, Y, tile), S, SRest) -> SNew = SRest ; SNew = S).

% set_target/3
% set_target(+S, +Pos, -SNew)
% Construiește starea SNew, care conține aceleași informații ca și S
% și în plus faptul că la poziția Pos se află gaura (scopul).
set_target(S, (X, Y), SNew) :-
    SNew = [(X, Y, target) | S].

% set_fragile/3
% set_fragile(+S, +Pos, -SNew)
% Construiește starea SNew, care conține aceleași informații ca și S
% și în plus faptul că la poziția Pos se o pătrățică fragilă, pe care
% blocul nu poate sta în picioare.
set_fragile(S, (X, Y), SNew) :-
    SNew = [(X, Y, fragile) | S].

% set_block_initial/3
% set_block_initial(+S, +Pos, -SNew)
% Construiește starea SNew, care conține aceleași informații ca și S
% și în plus faptul că la poziția Pos se află inițial blocul, plasat în
% picioare.
% --------------------- EXPLICATIE SET_BLOCK_INITIAL --------------------------
% Se adauga blocul in picioare la coordonatele date ca parametru. De asemenea,
% se adauga o celula tile in locul unde se afla blocul in starea initiala.
% ----------------------------------------------------------------------------
set_block_initial(S, (X, Y), SNew) :-
    S1 = [(X, Y, block) | S],
    SNew = [(X, Y, tile) | S1].

% set_block/3
% set_block(+S, +Pos, -SNew)
% Folosit in mutarea blocului, construiește starea SNew, care conține
% aceleași informații ca și S și în plus faptul că la poziția Pos se
% află blocul.
% --------------------- EXPLICATIE SET_BLOCK --------------------------
% Se adauga blocul la coordonatele date ca parametru. Daca la aceste
% coordonate exista deja un bloc, atunci nu se va adauga altul si se va
% intoarce starea initiala.
% ---------------------------------------------------------------------
set_block(S, (X, Y), SNew) :-
    (\+ get_cell(S, (X, Y), block) ->
        SNew = [(X, Y, block) | S] ;
        SNew = S
    ).

% get_b_pos/2
% get_b_pos(+S, -BlockPos)
% Obtine pozitia sau pozitiile blocului (în funcție de dacă blocul este
% în picioare sau culcat, ca (X, Y) sau ca [(X1, Y1), (X2, Y2)])
% ----------------------- EXPLICATIE GET_B_POS -------------------------------
% Cauta in lista de stari S toate pozitiile unde se afla blocul.
% Intoarce o lista cu aceste pozitii(Result).
% Daca lista Result contine un singur element, atunci BlockPos este
% acest element. Altfel, BlockPos este lista Result.
% Result = [SinglePos] incearca sa lege SinglePos la singurul element din
% lista Result. Daca reuseste atunci lista are un singur element si
% altfel are mai multe.
% ----------------------------------------------------------------------------
get_b_pos(S, BlockPos) :-
    findall((X, Y), member((X, Y, block), S), Result),
    (Result = [SinglePos] -> BlockPos = SinglePos ; BlockPos = Result).

% get_bounds/5
% get_bounds(+S, -Xmin, -Xmax, -Ymin, -Ymax).
% Obtine coordonatele limită de pe hartă la care exită celule.
% --------------------- EXPLICATIE GET_BOUNDS --------------------------
% Cauta in lista de stari S toate coordonatele X si Y ale celulelor
% si plaseaza aceste coordonate in liste separate.
% Mai apoi se calculeaza minimul si maximul din fiecare lista.
% Se obtin astfel coordonatele limita ale hartii.
% ----------------------------------------------------------------------
get_bounds(S, Xmin, Xmax, Ymin, Ymax) :-
    findall(X, member((X, _, _), S), Xs),
    findall(Y, member((_, Y, _), S), Ys),
    min_list(Xs, Xmin),
    max_list(Xs, Xmax),
    min_list(Ys, Ymin),
    max_list(Ys, Ymax).

% get_cell/3
% get_cell(S, Pos, Type).
% Leagă Type la tipul pătrățelei de la poziția Pos. Type trebuie legat
% la:
% tile - pentru o pătrățică obișnuită.
% fragile - pentru o pătrățică fragilă.
% target - pentru scop (gaura).
% oswitch - pentru switch de tip o.
% xswitch - pentru switch de tip x.
%
% Dacă la poziția respectivă nu se află nimic, sau este în afara
% limitelor date de get_bounds, predicatul întoarce false.
% --------------------- EXPLICATIE GET_CELL --------------------------
% Se obtin coordonatele limita ale hartii cu get_bounds.
% Se verifica daca coordonatele date ca parametru se afla in interiorul
% hartii.
% Daca da, se cauta in lista de stari S o celula cu coordonatele date
% ca parametru si se intoarce tipul acesteia.
% Daca predicatul member() nu gaseste o celula cu coordonatele date
% ca parametru, atunci se intoarce false.
% ----------------------------------------------------------------------
get_cell(S, (X, Y), What) :- 
    get_bounds(S, Xmin, Xmax, Ymin, Ymax),
    X >= Xmin, X =< Xmax, Y >= Ymin, Y =< Ymax,
    member((X, Y, What, _, _), S);
    member((X, Y, What), S).

% move/3
% move(S, Move, SNext)
% Calculează în SNext starea care rezultă din realizarea mutării Move în
% starea S.
% Mutarea este una dintre d, u, l, r.
% Întoarce false dacă mutarea duce la căderea blocului în vid (nu dacă
% blocul a ajuns la scop).
move(S, Move, SNext) :-
    get_b_pos(S, BlockPos),
    
    (BlockPos = (X, Y) ->
        % Blocul este in picioare

        % Se calculeaza vecinii blocului
        neighbor((X, Y), Move, (XN1, YN1)),
        neighbor2((X, Y), Move, (XN2, YN2)),

        % Se verifica daca blocurile vecine sunt valide
        (get_cell(S, (XN1, YN1), _), get_cell(S, (XN2, YN2), _) ->
            % Se muta blocul in starea urmatoare si sterge
            % blocul din starea curenta
            set_block(S, (XN1, YN1), S1),
            set_block(S1, (XN2, YN2), S2),
            set_tile(S2, (X, Y), SNext) ; false
        ) ;

        % Blocul este culcat
        BlockPos = [(X1, Y1), (X2, Y2)],

        % Se calculeaza vecinii blocurilor
        neighbor((X1, Y1), Move, (XN1, YN1)),
        neighbor((X2, Y2), Move, (XN2, YN2)),

        % Se verifica daca blocurile vecine sunt valide
        (get_cell(S, (XN1, YN1), _), get_cell(S, (XN2, YN2), _) ->
            % Se muta blocurile in starea urmatoare si se sterg
            % blocurile din starea curenta
            set_block(S, (XN1, YN1), S1),
            set_block(S1, (XN2, YN2), S2),
            set_tile(S2, (X1, Y1), S3),
            set_tile(S3, (X2, Y2), SNext) ; false
        )
    ).

% is_final/1
% is_final(S)
% Întoarce adevărat dacă în starea S blocul este în picioare, pe aceeași
% poziție cu gaura (scopul).
% --------------------- EXPLICATIE IS_FINAL --------------------------
% Se obtine pozitia blocului cu get_b_pos.
% Se verifica daca exista o singura pozitie in lista de pozitii a blocului
% si daca aceasta pozitie este o tinta.
% ----------------------------------------------------------------------
is_final(S) :- 
    get_b_pos(S, BlockPos),
    (BlockPos = (X, Y) ->
        get_cell(S, (X, Y), target) ; false
    ).

%%%%%%%%%% Etapa 2

%% TODO
% set_switch/6
% set_switch(+S, +Pos, +Switch, +Func, +Positions, -SNew)
% Leagă starea SNew la o stare cu aceleași informații ca și S, și în
% plus un switch la poziția Pos, cu parametrii dați.
%
% Switch: oswitch sau xswitch.
% Func: switch, uponly sau dnonly.
% Positions: pozițiile podului.
set_switch(S, (X, Y), Switch, Func, Positions, SNew) :-
    % Se adauga switch-ul la coordonatele date ca parametru
    SNew = [(X, Y, Switch, Func, Positions) | S].

%% TODO
% solve/2
% solve(+S, -Moves)
% Solve găsește o soluție pentru problema din starea S. Soluția este
% reprezentată ca secvența de mutări Moves.
%
% Pentru a fi soluție, mutările din Moves trebuie sa ducă blocul în
% picioare pe poziția scop (target).
solve(_, _) :- false.









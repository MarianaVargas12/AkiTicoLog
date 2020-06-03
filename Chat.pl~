

%Otros archivos
:-[basedatos,bnf].
:- use_module(library(random)).
 :- dynamic     estatura/1,
                residencia/1,
                edad/1,
                profesion/1,
                color/1,
                largo/1,
                tipo/1,
                genero/1,
                estadocivil/1.



% -------------------------------Inicio-----------------------------------
% Inicia la conversacion
inicio:-
    saludo,
    bienvenida,
    preparada,
    adivinar.

%Da la bienvenida al usuario
saludo:-
    oraciones(saludar,Lista),
    escoger_aleatorio(Lista,Saludo),
    habla(aki),
    imprimirconsola(Saludo),
    habla(usuario),
    leer(_).
bienvenida:-
    habla(aki),
    write('Le voy a explicar lo que vamos a jugar, usted va a pensar en uno de estos personajes:\n
    Hanna Gabriel
    Toledo
    Alex Badilla
    Melissa Mora
    Karina Ramos
    Edgar Silva
    Johana Solano
    Ignacio santos
    Frankiln Chang Diaz
    Ines Sanchez
    Natalia Carvajal
    Carlos Alvarado
    Keylor Navas
    Shirley Cruz
    Hernan Jimenez
    yo le voy a hacer preguntas para intentar adivinar en quien esta pensando, solo responde en minuscula y entre comillas simples y con un punto al final
    .Es muy facil.').

adivinar:-
    repeat,
    habla(aki),
    comenzarJuego,!.

comenzarJuego:-
    asserta(estatura(_)),
    asserta(residencia(_)),
    asserta(edad(_)),
    asserta(profesion(_)),
    asserta(color(_)),
    asserta(largo(_)),
    asserta(tipo(_)),
    asserta(genero(_)),
    asserta(estadocivil(_)),
    preguntas,!.




% ---------------------------Lista paracomenzar-------------------------
%Pregunta si ya penso en la persona
preparada:-
    habla(aki),
    write('necesito que responda la siguiente pregunta con si o no\n'),
    write('Ya escogio un famositico?\n'),
    leer(Respuesta),
    preparada(Respuesta),!.


%Verifica si la persona esta lista
preparada(S):-
  afirmativo(S), !,
  habla(aki),
  write('Yaaaaay, comencemos\n').


% verifica si la persona aun no esta lista
preparada(Entrada):-
  negativo(Entrada), !,
  habla(aki),
  write('ok! Tomese su tiempo, me avisa cuando esta listo para adivinar escribiendo si\n'),
  leer(Respuesta),
  preparada(Respuesta),!.

% salida si detecta un problema
preparada(Respuesta):-
  member('salir',Respuesta), !.

% No respondio con si o no
preparada(_):-
  habla(aki),
  write('No respondiste con SI o NO, intentalo de nuevo\n'),
  habla(usuario),
  leer(Respuesta),
  preparada(Respuesta).

%--------------------------Preguntas--------------------------------
preguntas:-
    caracteristicas(Lista),
    adivinar(Lista),!.

adivinar(Lista):-
    escoger_aleatorio(Lista,Caract),
    preguntas(Caract,ListaP),
    escoger_aleatorio(ListaP,Pregunta),
    imprimirconsola(Pregunta),
    habla(usuario),
    leer(S),
    length(S,Cant),
    length(Lista,M),
    F is M-1,
    verificar(Cant,F,S,Lista),!.


%--------------------------Verificacion------------------------------


%sigue preguntando porque faltan datos
verificar(_,0,_,_):-
    estatura(Estatura),
    residencia(Residencia),
    edad(Edad),
    profesion(Profesion),
    color(Color),
    largo(Largo),
    tipo(Tipo),
    genero(Genero),
    estadocivil(Estadocivil),
    buscar(X,Estatura,Residencia,Edad,Profesion,Color,Largo,Tipo,Genero,Estadocivil),!,
    retract(estatura(Estatura)),
    retract(residencia(Residencia)),
    retract(edad(Edad)),
    retract(profesion(Profesion)),
    retract(color(Color)),
    retract(largo(Largo)),
    retract(tipo(Tipo)),
    retract(genero(Genero)),
    retract(estadocivil(Estadocivil)),
    habla(aki),
    write('Su personaje es '),
    write(X),
    write('?'),
    habla(usuario),
    leer(Respuesta),
    final(Respuesta),!.

verificar(0,_,_,ListaC):-
    adivinar(ListaC),!.

verificar(Cant,_,Caracteristicas,ListaC):-
    nElemento(Caracteristicas,Cant,Elemento),
    veri_caract(Elemento,ListaC,Nueva),!,
    eliminar(Elemento,Caracteristicas,Nuev_Carac),
    length(Nueva,F),
    length(Nuev_Carac,K),
    verificar(K,F,Nuev_Carac,Nueva),!.



%Encuentra la estatura
veri_caract(Elemento_buscar,ListaC,Nueva):-
    caracteristicas_bd(estatura,X),
    buscList(Elemento_buscar,X),!,
    retract(estatura(_)),
    asserta(estatura(Elemento_buscar)),
    eliminar(estatura,ListaC,Nueva).
%Encuentra donde nacio
veri_caract(Elemento_buscar,ListaC,Nueva):-
    caracteristicas_bd(residencia,X),
    buscList(Elemento_buscar,X),!,
    retract(residencia(_)),
    asserta(residencia(Elemento_buscar)),
    eliminar(residencia,ListaC,Nueva).
%Encuentra la edad
veri_caract(Elemento_buscar,ListaC,Nueva):-
    caracteristicas_bd(edad,X),
    buscList(Elemento_buscar,X),!,
    retract(edad(_)),
    asserta(edad(Elemento_buscar)),
    eliminar(edad,ListaC,Nueva).
%Encuentra la profesion
veri_caract(Elemento_buscar,ListaC,Nueva):-
    caracteristicas_bd(profesion,X),
    buscList(Elemento_buscar,X),!,
    retract(profesion(_)),
    asserta(profesion(Elemento_buscar)),
    eliminar(profesion,ListaC,Nueva).
%Encuentra encuentra el color de pelo
veri_caract(Elemento_buscar,ListaC,Nueva):-
    caracteristicas_bd(color,X),
    buscList(Elemento_buscar,X),!,
    retract(color(_)),
    asserta(color(Elemento_buscar)),
    eliminar(color,ListaC,Nueva).
%Encuentra el largo de pelo
veri_caract(Elemento_buscar,ListaC,Nueva):-
    caracteristicas_bd(largo,X),
    buscList(Elemento_buscar,X),!,
    retract(largo(_)),
    asserta(largo(Elemento_buscar)),
    eliminar(largo,ListaC,Nueva).
%Encuentra el tipo de pelo
veri_caract(Elemento_buscar,ListaC,Nueva):-
    caracteristicas_bd(tipo,X),
    buscList(Elemento_buscar,X),!,
    retract(tipo(_)),
    asserta(tipo(Elemento_buscar)),
    eliminar(tipo,ListaC,Nueva).
%Encuentra el genero
veri_caract(Elemento_buscar,ListaC,Nueva):-
    caracteristicas_bd(genero,X),
    buscList(Elemento_buscar,X),!,
    retract(genero(_)),
    asserta(genero(Elemento_buscar)),
    eliminar(genero,ListaC,Nueva).
%Encuentra el estado civil
veri_caract(Elemento_buscar,ListaC,Nueva):-
    caracteristicas_bd(estadocivil,X),
    buscList(Elemento_buscar,X),!,
    retract(estadocivil(_)),
    asserta(estadocivil(Elemento_buscar)),
    eliminar(estadocivil,ListaC,Nueva).

%no sabe la respuesta
veri_caract(Elemento_buscar,_,_):-
   nosabe(P),
   member(Elemento_buscar,P),!.

%si lo que dijo no lo entiende
veri_caract(_,ListaC,_):-
    excepcion(ListaC),!.



%Excepcion uno no sabe
excepcion(ListaC):-
    oraciones(incompresion,ListaP),
    escoger_aleatorio(ListaP,Pregunta),
    habla(aki),
    imprimirconsola(Pregunta),
    habla(usuario),
    leer(Respuesta),
    length(Respuesta,Cant),
    length(ListaC,M),
    print(M),
    verificar(Cant,M,Respuesta,ListaC),!.

%-----------------------------Finalizar------------------------------
final(Respuesta):-
    afirmativo(Respuesta),!,
    oraciones(acerto,S),
    escoger_aleatorio(S,Pregunta),
    habla(aki),
    imprimirconsola(Pregunta),
    write('quiere jugar de nuevo?'),
    habla(usuario),
    leer(Resp_lei),
    jugarNuevo(Resp_lei),!.

final(Respuesta):-
    negativo(Respuesta),!,
    oraciones(fallar,S),
    escoger_aleatorio(S,Pregunta),
    habla(aki),
    imprimirconsola(Pregunta),
    write('quiere jugar de nuevo?'),
    habla(usuario),
    leer(Resp_lei),
    jugarNuevo(Resp_lei),!.
jugarNuevo(Respuesta):-
    afirmativo(Respuesta),!,
    habla(aki),
    write('juguemos'),
    comenzarJuego.
jugarNuevo(Respuesta):-
    negativo(Respuesta),!,
    oraciones(despedida,P),
    escoger_aleatorio(P,Pregunta),
    habla(aki),
    imprimirconsola(Pregunta).

imprimirconsola([]):- nl.
imprimirconsola([H|T]):-
    write(H),
    write(' '),
    imprimirconsola(T).

%--------------------------------Extras-------------------------------
%Revisa si la respuesta fue si o no

afirmativo(S):-
  member('si',S),!.

negativo(S):-
  member('no',S),!.

primer_elemento([X|Cola], X,Cola).

%Dice quien habla para que el usuario no se pierda
habla(aki):-
    nombre_Aki(X), write(X), write(': '), flush_output.
habla(usuario):-
    usuario(X), write(X), write(': '), flush_output.
nombre_Aki('Aki').
usuario('tu').

%Escoge aleatoriamente un elemento de una lista
escoger_aleatorio(Lista, Elemento):-
    length(Lista, Longitud),
    Mayor is Longitud + 1,
    random(1, Mayor, Rand),
    nElemento(Lista, Rand, Elemento).

%Funcion utilizada para tomar el dato del numero en una lista
nElemento([Cabeza|_], 1, Cabeza).
nElemento([_|Cola], N, Elemento):-
    nElemento(Cola, N1, Elemento),
    N is N1 + 1.

%Funcion que elimina un elemento de una Lista

eliminar(Elemento, [Elemento|Resultado], Resultado).

eliminar(Elemento, [Y|Ys], [Y|Zs]):-
          eliminar(Elemento, Ys, Zs).


%--------------------------Buscar-------------------------


% funciones para buscar personajes que coincidan con el
% parametro ingresado por el usuario.
estatura(ESTATURA,X):- personaje(X,ESTATURA,_,_,_,_,_,_,_,_).
lugNac(LUGAR,X):- personaje(X,_,LUGAR,_,_,_,_,_,_,_).
edad(EDAD,X):- personaje(X,_,_,EDAD,_,_,_,_,_,_).
profesion(PROFESION,X):- personaje(X,_,_,_,A,_,_,_,_,_),buscList(PROFESION,A).
colorPelo(COLOR,X):- personaje(X,_,_,_,_,COLOR,_,_,_,_).
longPelo(LONGITUD,X):- personaje(X,_,_,_,_,_,LONGITUD,_,_,_).
tipPelo(TIPO,X):- personaje(X,_,_,_,_,_,_,TIPO,_,_).
genero(GENERO,X):- personaje(X,_,_,_,_,_,_,_,GENERO,_).
estCiv(CIVIL,X):- personaje(X,_,_,_,_,_,_,_,_,CIVIL).

%funcion para buscar en una lista un elemento
buscList(X,[X|_]).
buscList(X,[_|Y]):- buscList(X,Y).

% funcion que busca un personaje que coincida los mismos parametros
buscar(X,Est,Lug,Ed,Prof,ColP,LongP,TipP,Gene,Civ):-
    estatura(Est,X),
    lugNac(Lug,X),
    edad(Ed,X),
    profesion(Prof,X),
    colorPelo(ColP,X),
    longPelo(LongP,X),
    tipPelo(TipP,X),
    genero(Gene,X),
    estCiv(Civ,X).

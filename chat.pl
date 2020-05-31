

%Otros archivos
:-[basedatos].
:- use_module(library(random)).


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
    write('Le voy a explicar lo que vamos a jugar, usted va a pensar en uno de estos personajes:
    yo le voy a hacer preguntas para intentar adivinar en quien esta pensando. Es muy facil.').

adivinar:-
    repeat,
    habla(aki).

% ---------------------------Lista paracomenzar-------------------------
%Pregunta si ya penso en la persona
preparada:-
    habla(aki),
    write('necesito que responda la siguiente pregunta con si o no\n'),
    write('Ya escogio un famositico?'),
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
    adivinar(Lista).

adivinar(Lista):-
    escoger_aleatorio(Lista,Caract),
    eliminar(Caract,Lista,Resultado),
    preguntas(Caract,ListaP),
    escoger_aleatorio(ListaP,Pregunta),
    imprimirconsola(Pregunta),
    habla(usuario),
    leer(S),
    adivinar(S,Resultado).

adivinar(Caracteristicas,ListaC):-
    length(Caracteristicas,Cant),
    length(ListaC,M),
    verificar(Cant,M,Caracteristicas,ListaC).

%--------------------------Verificacion------------------------------
verificar(Cant,0,Caracteristicas,ListaC):-
    .


verificar(0,M,Caracteristicas,ListaC):-
    .


verificar(Cant,M,Caracteristicas,ListaC):-
    nElemento(Caracteristicas,Cant,Elemento),
    veri_caract(Elemento,ListaC,M,Nueva),
    eliminar(Elemento,Caracteristicas,Nuev_Carac),
    length(Nueva,F),
    length(Nuev_Carac,K),
    verificar(F,K,Nuev_Carac,Nueva).

%Cuando no es ninguna de las caracteristicas
veri_caract(Elemento_buscar,ListaC,0,Nueva):-
    excepciones().

%Encuentra el dato en la caracteristica
veri_caract(Elemento_buscar,ListaC,M,Nueva):-
    nElemento(ListaC,M,Caracteristica),
    caracteristicas_bd(Caracteristica,X),
    member(Elemento_buscar,X),
    %verificar dato (Mario)
    eliminar(Caracteristica,ListaC,Nueva).

%Cuando no ha encontrado el dato en la primera caracteristica
veri_caract(Elemento_buscar,ListaC,M,Nueva):-
    Nuevo is M-1,
    veri_caract(Elemento_buscar,ListaC,Nuevo,Nueva).

veri_caract(Elemento_buscar,ListaC,M,Nueva):-
    nosabe(P),
    member(Elemento_buscar,P).


%Excepcion uno no sabe
excepcion(Caracteristicas,ListaC)

%--------------------------------Extras-------------------------------
%Revisa si la respuesta fue si o no
afirmativo(S):-
  member('si',S).

negativo(S):-
  member('no',S).

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



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
    habla(aki),
    .

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




%--------------------------------Extras-------------------------------
%Revisa si la respuesta fue si o no
afirmativo(S):-
  member('si',S).

negativo(S):-
  member('no',S).

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



%Otros archivos
:-[BaseDatos].

% -------------------------------Inicio-----------------------------------
% Inicia la conversacion
inicio:-
    bienvenida,
    preparada,
    adivinar.

%Da la bienvenida al usuario
bienvenida:-
    oraciones(bienvenida,Lista),
    escoger_aleatorio(Lista,Saludo),
    habla(aki),
    imprimirconsola(Saludo),
    habla(usuario),
    leer(_).
adivinar:-
    repeat,
    habla(bot),
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
  habla(bot),
  write('No respondiste con SI o NO, intentalo de nuevo'),
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




%Otros archivos
:-[BaseDatos].
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
    write('Le voy a explicar lo que vamos a jugar, usted va a pensar en uno de estos personajes:
    yo le voy a hacer preguntas para intentar adivinar en quien esta pensando. Es muy facil.').

adivinar:-
    repeat,
    habla(aki),
    comenzarJuego.
comenzarJuego:-
    retract(estatura(_)),
    retract(residencia(_)),
    retract(edad(_)),
    retract(profesion(_)),
    retract(color(_)),
    retract(largo(_)),
    retract(tipo(_)),
    retract(genero(_)),
    retract(estadocivil(_)),
    assert(estatura(_)),
    assert(residencia(_)),
    assert(edad(_)),
    assert(profesion(_)),
    assert(color(_)),
    assert(largo(_)),
    assert(tipo(_)),
    assert(genero(_)),
    assert(estadocivil(_)),
    preguntas.




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
%Finalizacion, busca y adivina el personaje
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
    buscar(X,Estatura,Residencia,Edad,Profesion,Color,Largo,Tipo,Genero,Estadocivil),
    habla(aki),
    write('Su personaje es'),
    write(X),
    write('?'),
    habla(usuario),
    leer(Respuesta),
    final(Respuesta).

%sigue preguntando porque faltan datos
verificar(0,_,_,ListaC):-
    adivinar(ListaC).


verificar(Cant,_,Caracteristicas,ListaC):-
    nElemento(Caracteristicas,Cant,Elemento),
    veri_caract(Elemento,ListaC,Nueva),
    eliminar(Elemento,Caracteristicas,Nuev_Carac),
    length(Nueva,F),
    length(Nuev_Carac,K),
    verificar(F,K,Nuev_Carac,Nueva).

%Encuentra la estatura
veri_caract(Elemento_buscar,ListaC,Nueva):-
    caracteristicas_bd(estatura,X),
    member(Elemento_buscar,X),
    retract(estatura(_)),
    assert(estatura(Elemento_buscar)),
    %verificar dato (Mario)
    eliminar(estatura,ListaC,Nueva).
%Encuentra donde nacio
veri_caract(Elemento_buscar,ListaC,Nueva):-
    caracteristicas_bd(residencia,X),
    member(Elemento_buscar,X),
    retract(residencia(_)),
    assert(residencia(Elemento_buscar)),
    %verificar dato (Mario)
    eliminar(residencia,ListaC,Nueva).
%Encuentra la edad
veri_caract(Elemento_buscar,ListaC,Nueva):-
    caracteristicas_bd(edad,X),
    member(Elemento_buscar,X),
    retract(edad(_)),
    assert(edad(Elemento_buscar)),
    %verificar dato (Mario)
    eliminar(edad,ListaC,Nueva).
%Encuentra la profesion
veri_caract(Elemento_buscar,ListaC,Nueva):-
    caracteristicas_bd(profesion,X),
    member(Elemento_buscar,X),
    retract(profesion(_)),
    assert(profesion(Elemento_buscar)),
    %verificar dato (Mario)
    eliminar(profesion,ListaC,Nueva).
%Encuentra encuentra el color de pelo
veri_caract(Elemento_buscar,ListaC,Nueva):-
    caracteristicas_bd(color,X),
    member(Elemento_buscar,X),
    retract(color(_)),
    assert(color(Elemento_buscar)),
    %verificar dato (Mario)
    eliminar(color,ListaC,Nueva).
%Encuentra el largo de pelo
veri_caract(Elemento_buscar,ListaC,Nueva):-
    caracteristicas_bd(largo,X),
    member(Elemento_buscar,X),
    retract(largo(_)),
    assert(largo(Elemento_buscar)),
    %verificar dato (Mario)
    eliminar(largo,ListaC,Nueva).
%Encuentra el tipo de pelo
veri_caract(Elemento_buscar,ListaC,Nueva):-
    caracteristicas_bd(tipo,X),
    member(Elemento_buscar,X),
    retract(tipo(_)),
    assert(tipo(Elemento_buscar)),
    %verificar dato (Mario)
    eliminar(tipo,ListaC,Nueva).
%Encuentra el genero
veri_caract(Elemento_buscar,ListaC,Nueva):-
    caracteristicas_bd(genero,X),
    member(Elemento_buscar,X),
    retract(genero(_)),
    assert(genero(Elemento_buscar)),
    %verificar dato (Mario)
    eliminar(genero,ListaC,Nueva).
%Encuentra el estado civil
veri_caract(Elemento_buscar,ListaC,Nueva):-
    caracteristicas_bd(estadocivil,X),
    member(Elemento_buscar,X),
    retract(estadocivil(_)),
    assert(estadocivil(Elemento_buscar)),
    eliminar(estadocivil,ListaC,Nueva).

%no sabe la respuesta
veri_caract(Elemento_buscar,_,_):-
    nosabe(P),
    member(Elemento_buscar,P).

%si lo que dijo no lo entiende
veri_caract(_,_,_):-
    excepcion.

%Excepcion uno no sabe
excepcion:-
    oraciones(incompresion,ListaP),
    escoger_aleatorio(ListaP,Pregunta),
    habla(aki),
    imprimirconsola(Pregunta),
    habla(usuario),
    leer(Respuesta),
    adivinar(Respuesta).

%-----------------------------Finalizar------------------------------
final(Respuesta):-
    afirmativo(Respuesta),!,
    oraciones(acerto,S),
    escoger_aleatorio(S,Pregunta),
    habla(aki),
    imprimirconsola(Pregunta),
    write('quiere jugar de nuevo?'),
    habla(usuario),
    leer(Respuesta),
    jugarNuevo(Respuesta).

final(Respuesta):-
    negativo(Respuesta),!,
    oraciones(fallar,S),
    escoger_aleatorio(S,Pregunta),
    habla(aki),
    imprimirconsola(Pregunta),
    write('quiere jugar de nuevo?'),
    habla(usuario),
    leer(Respuesta),
    jugarNuevo(Respuesta).
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



%Otros archivos
:-[BaseDatos].

% -------------------------------Inicio-----------------------------------
% Inicia la conversacion
inicio:-
    bienvenida,
    preparada,
    adivinar.

bienvenida:-
    oraciones(bienvenida,Lista),
    escoger_aleatorio(Lista,Saludo),
    imprimir_usuario(aki),
    imprimirconsola(Saludo),
    imprimir_usuario(usuario),
    leer(_).

preparada:-
    preguntas(lista,Respuesta),
    escoger_aleatorio(Respuestas,Pregunta),
    imprimir_consola(aki),
    imprimirconsola(Pregunta),
    imprimir_consola(usuario),
    leer(Respuesta),
    obtener_respuesta(Respuesta).




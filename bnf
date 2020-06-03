:-[basedatos].
leer(Res):-read(X),
    atomic_list_concat(List," ",X),
    oracion(List,[]),
   analizar(List,Res).

%Funcion que comprueba las caracteristicas ingresadas por el usuario
analizar(List,Res):-caracteristicas_bd(estatura,A),
    caracteristicas_bd(residencia,B), caracteristicas_bd(edad,C),
    caracteristicas_bd(profesion,D),caracteristicas_bd(color,E),
    caracteristicas_bd(largo,F),caracteristicas_bd(tipo,G),
    caracteristicas_bd(genero,H),caracteristicas_bd(estadocivil,I),
    union(A,B,J),union(J,C,K),union(K,D,L),union(L,E,M),union(M,F,N),
    union(N,G,O),union(O,H,P),union(P,I,Q),
    union(Q,[si,'no se','no lo se','no','hola'],R),
    print(R),intersection(R,List,Res).

%Identifica las diferentes formas de realizar una oracion
oracion(S0,S):- sintagma_nominal(S0,S1),
sintagma_verbal(S1,S).
oracion(S0,S):- pronombre(S0,S1),
sintagma_verbal(S1,S).
oracion(S0,S):- sintagma_verbal(S0,S).

sintagma_nominal(S0,S):- articulo(S0,S1),
nombre(S1,S).

sintagma_nominal(S0,S):- articulo(S0,S1),
sustantivo(S1,S).

sintagma_nominal(S0,S):- articulo(S0,S1),
adjetivo(S1,S).

sintagma_nominal(S0,S):- pronombre(S0,S1),
nombre(S1,S).


sintagma_verbal(S0,S):- verbo(S0,S).
sintagma_verbal(S0,S):- verbo(S0,S1),
sintagma_nominal(S1,S).
sintagma_verbal(S0,S):- verbo(S0,S1),
sustantivo(S1,S).
sintagma_verbal(S0,S):- verbo(S0,S1),
adjetivo(S1,S).
sintagma_verbal(S0,S):- verbo(S0,S1),
adjetivo(S1,S2),sustantivo(S2,S).
sintagma_verbal(S0,S):- verbo(S0,S1),articulo(S1,S2),
sustantivo(S2,S).
sintagma_verbal(S0,S):- verbo(S0,S1),articulo(S1,S2),
sustantivo(S2,S).


%Articulo
articulo([el|S],S).
articulo([la|S],S).
articulo([los|S],S).
articulo([las|S],S).

articulo([este|S],S).
articulo([estos|S],S).
articulo([esta|S],S).
articulo([estas|S],S).

articulo([un|S],S).
articulo([una|S],S).
articulo([unos|S],S).
articulo([unas|S],S).
articulo([del|S],S).
articulo([en|S],S).


%Verbos
verbo([vive|S],S).
verbo([vivía|S],S).
verbo([tiene|S],S).
verbo([tenía|S],S).
verbo([nació|S],S).
verbo([habla|S],S).
verbo([juega|S],S).
verbo([jugaba|S],S).
verbo([escribe|S],S).
verbo([escribía|S],S).
verbo([estudia|S],S).
verbo([estudiaba|S],S).
verbo([es|S],S).
verbo(['es de'|S],S).
verbo([mide|S],S).

%Pronombre
pronombre([mi|S],S).
pronombre([él|S],S).
pronombre([el|S],S).
pronombre([ella|S],S).
pronombre([ellos|S],S).
pronombre([ellas|S],S).

%Nombres
nombre([amiga|S],S).
nombre([mujer|S],S).
nombre([muchacha|S],S).
nombre([amigo|S],S).
nombre([hombre|S],S).
nombre([muchacho|S],S).
nombre([personaje|S],S).

%Sustantivos
%Profesion
sustantivo([boxeador|S],S).
sustantivo([cantante|S],S).
sustantivo([maquillista|S],S).
sustantivo([modelo|S],S).
sustantivo([presentador|S],S).
sustantivo([periodista|S],S).
sustantivo([modelo|S],S).
sustantivo([abogado|S],S).
sustantivo([astronauta|S],S).
sustantivo([fisico|S],S).
sustantivo(["ingeniero mecanico"|S],S).
sustantivo([politologo|S],S).
sustantivo([politico|S],S).
sustantivo([presidente|S],S).
sustantivo([escritor|S],S).
sustantivo([portero|S],S).
sustantivo([deportista|S],S).
sustantivo([futbolista|S],S).
sustantivo(["director de cine"|S],S).
sustantivo([actor|S],S).
sustantivo([comediante|S],S).
sustantivo(["miss costa rica"|S],S).
sustantivo([anos|S],S).
sustantivo([años|S],S).
sustantivo([cm|S],S).
sustantivo([centimetros|S],S).
sustantivo([hombre|S],S).
sustantivo([mujer|S],S).

%Lugar
sustantivo([heredia|S],S).
sustantivo([alajuela|S],S).
sustantivo(['san jose'|S],S).
sustantivo([espana|S],S).
sustantivo([españa|S],S).
sustantivo([guanacaste|S],S).

%Adjetivos
%Edad
adjetivo(['37'|S],S).
adjetivo(['39'|S],S).
adjetivo(['27'|S],S).
adjetivo(['32'|S],S).
adjetivo(['26'|S],S).
adjetivo(['52'|S],S).
adjetivo(['29'|S],S).
adjetivo(['58'|S],S).
adjetivo(['69'|S],S).
adjetivo(['88'|S],S).
adjetivo(['33'|S],S).
adjetivo(['34'|S],S).
adjetivo(['40'|S],S).
%colores
adjetivo([negro|S],S).
adjetivo(['no tiene'|S],S).
adjetivo([rubio|S],S).
adjetivo([castano|S],S).
adjetivo([cafe|S],S).
%Pelo
adjetivo([largo|S],S).
adjetivo([corto|S],S).
adjetivo([dreads|S],S).
adjetivo([calvo|S],S).
adjetivo([lacio|S],S).
adjetivo([colochos|S],S).
%Estado Civil
adjetivo([soltero|S],S).
adjetivo([soltera|S],S).
adjetivo([casado|S],S).
adjetivo([casada|S],S).

%Altura
adjetivo(['170'|S],S).
adjetivo(['175'|S],S).
adjetivo(['173'|S],S).
adjetivo(['177'|S],S).
adjetivo(['180'|S],S).
adjetivo(['174'|S],S).
adjetivo(['185'|S],S).
adjetivo(['165'|S],S).
adjetivo(['178'|S],S).
adjetivo(['163'|S],S).
adjetivo(['160'|S],S).




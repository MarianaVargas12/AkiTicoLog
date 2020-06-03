%personaje(Nombre, Estatura en cm, lugar de nacimiento, edad,
% profesion(es), color de pelo,longitud del pelo,tipo de
% pelo,genero,estado civil(solter@/casad@))
personaje('Hanna Gabriel','170','alajuela','37',['boxeador'],'negro','largo','dreads','mujer','casado').
personaje('Toledo','178','londres','39',['cantante'],'no tiene','no tiene','calvo','hombre','casado').
personaje('Alex Badilla','175','san ramon','27',['maquillista','cantante'],'rubio','corto','lacio','hombre','soltero').
personaje('Melissa Mora','170','san ramon','32',['modelo','cantante'],'negro','largo','lacio','mujer','casado').
personaje('Karina Ramos','173','heredia','26',['modelo'],'castano','largo','lacio','mujer','soltero').
personaje('Edgar Silva','173','guanacaste','52',['presentador','periodista'],'negro','corto','lacio','hombre','casado').
personaje('Johanna Solano','177','san jose','29',['modelo','miss costa rica','triatlona'],'negro','largo','lacio','mujer','soltero').
personaje('Ignacio Santos','180','cuba','58',['periodista','presentador','abogado'],'negro','corto','lacio','hombre','casado').
personaje('Franklin Chang Diaz','178','san jose','69',['atronauta','fisico','ingeniero mecanico'],'negro','corto','lacio','hombre','casado').
personaje('Ines Sanchez','170','cuba','88',['periodista','presentador'],'negro','corto','colocho','mujer','soltero').
personaje('Natalia Carvajal','173','san jose','29',['modelo'],'cafe','largo','lacio','mujer','soltero').
personaje('Carlos Alvarado','174','san jose','40',['politico','politologo','presidente','escritor','periodista'],'negro','corto','lacio','hombre','casado').
personaje('Keylor Navas','185','san jose','33',['portero','deportista','futbolista'],'negro','corto','lacio','hombre','casado').
personaje('Shirley Cruz','165','san jose','34',['futbolista'],'negro','largo','lacio','mujer','soltero').
personaje('Hernan Jimenez','178','san jose','40',['director de cine','actor','comediante'],'negro','corto','colocho','hombre','soltero').


%********************Preguntas***************************
%preguntas que se le pueden hacer al usuario
preguntas(estatura,[
          ['Cuanto mide su personaje?'],
          ['Su personaje es alto? Cuanto mide?'],
          ['De fijo es alto o pequeño, cuanto mide?'],
          ['Me podria decir la estatura de esa persona?']]).
preguntas(residencia,[
          ['Donde vive esa persona?'],
          ['Ya seee que vive en costa rica, pero en que lugar?'],
          ['Me podria decir donde vive esa persona?'],
          ['Voy a sonar un poco stalker, pero donde vive esa persona?'],
          ['Una pregunta, donde vive esa persona?']]).
preguntas(edad,[
          ['Cuantos años tiene?'],
          ['Tendra mas de 50 años? Cuantos?'],
          ['Tendra menos de 50 años? Cuantos?'],
          ['Me podria decir la edad de la persona?']]).
pregunta(profesion,[
          ['Otra pregunta, Cual es su profesion?'],
          ['A que se dedica?'],
          ['Ya se que es famoso, pero a que se dedica?'],
          ['Sabe cual es su profesion?']]).
preguntas(color,[
          ['Que color de pelo tiene?'],
          ['Me puede decir el color de pelo?'],
          ['Sabe que color de pelo tiene?'],
          ['Ayy ya se fijo tiene el color de pelo..... no, mejor me lo dice usted']]).
preguntas(largo,[
          ['Que tan largo tiene el cabello?'],
          ['Cree que el pelo es corto o largo?'],
          ['Como diria que es el largo del cabello?'],
          ['El pelo es largo o pequeño?']]).
preguntas(tipo,[
          ['Como tiene el pelo?'],
          ['Sabe me gustaria tener colochos, pero lo importante como es el pelo de su persoanje?'],
          ['De fijo tiene el pelo bonito, como es?'],
          ['De que estilo tiene el pelo?'],
          ['Le gusta el pero de su personaje? como es?']]).
pregunta(genero,[
          ['Cual es el genero?'],
          ['Me podria decir el genero?'],
          ['Primero ocupo saber cual es su genero?'],
          ['Sera que me puede decir el genero?']]).
pregunta(estadocivil,[
          ['Solo por chismear, cual es la relacion amorosa de su perosnaje?'],
          ['Ya que estamos haciendo preguntas, cual es la situacion amorosa de su personaje?'],
          ['Una muy importante, como está en el amor?'],
          ['Y como está la situacion amorosa de esa persona?']]).

%****************************Frases*********************************
%Oraciones que se le pueden decir al usuario depende del caso
oraciones(despedida,[
          ['Ayy :(, bye'],
          ['Fue un gusto hablar con usted'],
          ['Que le vaya bien'],
          ['Te cuidas'],
          ['Vuelva pronto, paso muy sola'],
          ['Adios'],
          ['Espero verla pronto']]).

oraciones(saludar,[
          ['Hola, es un gusto tenerlo aca'],
          ['Buenas, que dicha que vino'],
          ['Heeeey, hola'],
          ['Hola, es un gusto'],
          ['Hi, ya queria que viniera alguien a jugar conmigo'],
          ['Holis, soy aki'],
          ['Holiiii, soy akiticolog pero me puede llamar aki']]).
oraciones(fallar,[
          ['Lo siento soy muy mala en esto, voy a mejorar'],
          ['mmm como que hay algo raro'],
          ['Ooops, nunca pierdo'],
          ['Eso no es posible, yo no pierdo'],
          ['No me gusta perder, tiene que haber algo mal']]).
oraciones(adivinar,[
          ['Sera que su personaje es'],
          ['Su personaje es'],
          ['Verdad que su personaje es'],
          ['El nombre de su personaje es']]).
oraciones(acerto,[['Soy demasiado buena en esto'],
          ['Uff soy demasiado buena'],
          ['Me esta gustando esto de adivinar'],
          ['Algun dia me voy a saber todos los famosos del mundo']]).
oraciones(incompresion,[
          ['Lo siento, no entiendo lo que dice'],
          ['No comprendo, me lo puede decir diferente por favor'],
          ['Mmm no entiendo su idioma'],
          ['Intentalo de nuevo'],
          ['Amigo, no comprendo'],
          ['Piense bien lo que me dice']]).
oraciones(miestado,[
          ['en mi casa, pero bien'],
          ['di ahi vamos'],
          ['Muy bien por dica'],
          ['Excelente'],
          ['No tan bien, pero jugemos']]).
oraciones(aleatorias,[
          ['Que tal si me pregunta mas tarde'],
          ['Mmmm no lo se'],
          ['No puedo responder a eso'],
          ['Que esta  hablando?'],
          ['No lo creo, pero no estoy segura']]).

oraciones(preguntas_aleatorias,[]).

%**********************Caracteristicas***************************
%caracteristicas presentes en los personajes
caracteristicas([
    estatura,
    residencia,
    edad,
    profesion,
    color,
    largo,
    tipo,
    genero,
    estadocivil]).

caracteristicas_bd(estatura,[
                   '170',
                   '175',
                   '173',
                   '177',
                   '180',
                   '174',
                   '185',
                   '165',
                   '178']).
caracteristicas_bd(residencia,[
                   'heredia',
                   'alajuela',
                   'san jose',
                   'españa',
                   'guanacaste']).
caracteristicas_bd(edad,[
                   '37',
                   '39',
                   '27',
                   '32',
                   '26',
                   '52',
                   '29',
                   '58',
                   '69',
                   '88',
                   '33',
                   '34',
                   '40']).
caracteristicas_bd(profesion,[
                   'boxeador',
                   'cantante',
                   'maquillista',
                   'modelo',
                   'presentador',
                   'periodista',
                   'miss costa rica',
                   'abogado',
                   'astronauta',
                   'fisico',
                   'ingeniero mecanico',
                   'politico',
                   'politologo',
                   'presidente',
                   'escritor',
                   'portero',
                   'deportista',
                   'futbolista',
                   'director de cine',
                   'actor',
                   'comediante']).
caracteristicas_bd(color,[
                   'negro',
                   'no tiene',
                   'rubio',
                   'castano',
                   'cafe']).
caracteristicas_bd(largo,[
                   'largo',
                   'no tiene',
                   'corto']).
caracteristicas_bd(tipo,[
                   'dreads',
                   'calvo',
                   'lacio',
                   'colochos']).
caracteristicas_bd(genero,[
                   'hombre',
                   'mujer']).
caracteristicas_bd(estadocivil,[
                   'soltero',
                   'casado',
                   'soltera',
                   'casada']).


nosabe(['no se','nose','no tengo idea','ni idea','quien sabe','no me acuerdo']).

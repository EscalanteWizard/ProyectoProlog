:- discontiguous menu/0.                % sentencia discontiguous se utiliza para evitar que el compilador arroje mensajes de advertencia al compilar
:- discontiguous procesar_opcion/1.
%=======================================================================================================================
procesarOpcionPersonas(1):-
    writeln(""),nl,
    writeln("**********Agregar nueva persona**********").

procesarOpcionPersonas(2):-
    writeln(""),nl,
    writeln("**********Mostrando todas las personas de la base de conocimientos**********").

procesarOpcionPersonas(_):-
    writeln(""),nl,
    writeln("Opcion invalida, por favor seleccione una opcion valida"),nl,
    procesar_opcion(1).

% Predicado para procesar la opcion de gestion de personas
procesar_opcion(1) :-
    writeln(""),nl,
    writeln("**********Gestion de personas**********"),
    writeln("1. Agregar nueva persona"),
    writeln("2. Mostrar todas las personas"),
    read(OpcionPersonas),
    procesarOpcionPersonas(OpcionPersonas).
    menu.
%=======================================================================================================================
procesar_opcion(2) :-
    writeln(""),nl,
    writeln("**********Gestion de proyectos**********"),
    menu.
%=======================================================================================================================
procesar_opcion(3) :-
    writeln(""),nl,
    writeln("**********Gestion de tareas**********"),
    menu.
%=======================================================================================================================
procesar_opcion(4) :-
    writeln(""),nl,
    writeln("**********Buscar tareas libres**********"),
    menu.
%=======================================================================================================================
procesar_opcion(5) :-
    writeln(""),nl,
    writeln("**********Recomendar persona**********"),
    menu.
%=======================================================================================================================
procesar_opcion(6) :-
    writeln(""),nl,
    writeln("**********Asignar tarea**********"),
    menu.
%=======================================================================================================================
procesar_opcion(7) :-
    writeln(""),nl,
    writeln("**********Cerrar tarea**********"),
    menu.
%=======================================================================================================================
procesar_opcion(8) :-
    writeln(""),nl,
    writeln("**********Estadisticas**********"),
    menu.
%=======================================================================================================================
procesar_opcion(0) :-
    writeln("¡Hasta luego!"),
    halt.
procesar_opcion(_) :-
    writeln("Opcion invalida. Por favor, selecciona una opcion valida."),nl,
    menu.
%=======================================================================================================================
% Predicado para mostrar el menú principal y procesar la opción seleccionada
menu :-
    writeln(""),nl,
    writeln("**********Menu general**********"),
    writeln("Seleccione una opcion:"),
    writeln("1. Gestionar personas"),
    writeln("2. Gestionar proyectos"),
    writeln("3. Gestionar tareas"),
    writeln("4. Buscar tareas libres"),
    writeln("5. Recomendar persona"),
    writeln("6. Asignar tarea"),
    writeln("7. Cerrar tarea"),
    writeln("8. Estadisticas"),
    writeln("0. Salir"),
    read(Opcion),
    procesar_opcion(Opcion).
%=======================================================================================================================
% Predicado principal para iniciar el programa
inicio :-
    menu.

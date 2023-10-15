% Predicado para mostrar el menú y procesar la opción seleccionada
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

% Predicado para procesar la opción seleccionada
procesar_opcion(1) :-
    writeln("Has seleccionado la opción de gestionar personas."),
    % Aquí puedes agregar la lógica correspondiente a la opción de gestionar personas
    menu.
procesar_opcion(2) :-
    writeln("Has seleccionado la opción de gestionar proyectos."),
    % Aquí puedes agregar la lógica correspondiente a la opción de gestionar proyectos
    menu.
procesar_opcion(3) :-
    writeln("Has seleccionado la opción de gestionar tareas."),
    % Aquí puedes agregar la lógica correspondiente a la opción de gestionar tareas
    menu.
procesar_opcion(4) :-
    writeln("Has seleccionado la opción de buscar tareas libres."),
    % Aquí puedes agregar la lógica correspondiente a la opción de buscar tareas libres
    menu.
procesar_opcion(5) :-
    writeln("Has seleccionado la opción de recomendar persona."),
    % Aquí puedes agregar la lógica correspondiente a la opción de recomendar persona
    menu.
procesar_opcion(6) :-
    writeln("Has seleccionado la opción de asignar tarea."),
    % Aquí puedes agregar la lógica correspondiente a la opción de asignar tarea
    menu.
procesar_opcion(7) :-
    writeln("Has seleccionado la opción de cerrar tarea."),
    % Aquí puedes agregar la lógica correspondiente a la opción de cerrar tarea
    menu.
procesar_opcion(8) :-
    writeln("Has seleccionado la opción de estadísticas."),
    % Aquí puedes agregar la lógica correspondiente a la opción de estadísticas
    menu.
procesar_opcion(0) :-
    writeln("¡Hasta luego!"),
    halt.
procesar_opcion(_) :-
    writeln("Opcion invalida. Por favor, selecciona una opcion valida."),
    menu.

% Predicado principal para iniciar el programa
inicio :-
    menu.

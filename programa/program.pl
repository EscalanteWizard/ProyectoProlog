%Lista de hechos dinamicos que se agregan a la base de conocimientos
:- dynamic(proyecto/5).
:- dynamic(tarea/7).
:- dynamic(persona/5).

%Verifica que la lista de tares recibida sea parte d elas tareas permitidas del programa
%Entradas: Una lista de tareas
%Salidas: true si todas las tareas son validas, false si no
%Restricciones: La lsita no debe estar vacia
tareas(['requerimientos', 'disenio', 'desarrollo', 'qa', 'fullstack', 'frontend', 'backend', 'administracion']).

%Predicado para cargar los hechos en la base de conocimientos de prolog a iniciar el programa llamando al menu principal
%Entradas: Las rutas de todos los archivos que contienen los datos a cargarse en la backend
%Salidas: Carga la base de conocimientos con los datos de todos los documentos de actualizarArchivoDeTexto
%Restricciones: Las rutas deben ser validas
inicio :-
    cargarBaseConocimientos('D:\\WockSpace\\Lenguajes2023\\ProyectoProlog\\programa\\personas.txt'),
    cargarBaseConocimientos('D:\\WockSpace\\Lenguajes2023\\ProyectoProlog\\programa\\proyectos.txt'),
    cargarBaseConocimientos('D:\\WockSpace\\Lenguajes2023\\ProyectoProlog\\programa\\tareas.txt'),
    menu.

%Predicado para leer el contenido del archivo indicado en la ruta
%Entradas: La ruta de un archivo txt
%Salidas: Retorna el contenido del archivo en texto
%Restricciones: Las ruta debe ser valida
cargarBaseConocimientos(RutaArchivo) :-
    see(RutaArchivo),
    readLines,
    seen.

%Predicado que lee el contenido de un archivo y lo agrega a la base de conocimientos
%Entradas: El buffer de lectura
%Salidas: Agrega el contenido del archivo a la base de conocimientos
%Restricciones: El archivo debe contener hechos validos de prolog
readLines :-
    repeat,
    readLine(Linea),
    (Linea == end_of_file, !;
    asserta(Linea),
    fail).

%Predicado que lee cada linea de un buffer de texto
%Entradas: El buffer de lectura
%Salidas: El contenido de una linea del archivo del buffer
%Restricciones: Las ruta debe ser un string
readLine(Linea) :-
    read(Linea).

%Despliega el menu principal del programa
%Entradas: El input or teclado que el usuario indique
%Salidas: Segun que seleccion haga el usuario el menu desplegara las funcionalidades que sean necesarias
%Restricciones: La base de conocimeintos debe estar cargada
menu:-
    writeln('\n'),
    writeln('*********** Menu *************'),
    writeln('1. Agregar persona'),
    writeln('2. Mostrar personas'),
    writeln('3. Agregar proyecto'),
    writeln('4. Mostrar proyectos'),
    writeln('5. Agregar tarea'),
    writeln('6. Mostrar tareas'),
    writeln('7. Mostrar tareas pendientes'),
    writeln('8. Mostrar tareas por trabajador'),
    writeln('9. Asignar tarea'),
    writeln('10. Ver estatus financiero de los proyectos'),
    writeln('11. Ver cantidad de tareas por personas'),
    writeln('12. Salir'),
    write('Ingrese la opcion que desea: '),
    read_line_to_string(user_input, Opcion), nl, procesarOpcion(Opcion).

%Manejo de los input por teclado que haga el usuario
%Entradas: Un sting 
%Salidas: Reenvia el flujo del programa a la funcion que el usuario indique
%Restricciones: La seleccion del usuario debe ser valida
procesarOpcion(Opcion):- string_lower(Opcion, "1"), agregarEmpleado, menu.
procesarOpcion(Opcion):- string_lower(Opcion, "2"), mostrarEmpleados, menu.
procesarOpcion(Opcion):- string_lower(Opcion, "3"), addProyecto, menu.
procesarOpcion(Opcion):- string_lower(Opcion, "4"), showProyecto, menu.
procesarOpcion(Opcion):- string_lower(Opcion, "5"), addTarea, menu.
procesarOpcion(Opcion):- string_lower(Opcion, "6"), showTareas, menu.
procesarOpcion(Opcion):- string_lower(Opcion, "7"), showPendientes, menu.
procesarOpcion(Opcion):- string_lower(Opcion, "8"), buscarTareaTrabajador, menu.
procesarOpcion(Opcion):- string_lower(Opcion, "9"), asignarTarea, menu.
procesarOpcion(Opcion):- string_lower(Opcion, "10"), estatusProyectos,menu.
procesarOpcion(Opcion):- string_lower(Opcion, "11"), tareasPorTrabajador,menu.
procesarOpcion(Opcion):- string_lower(Opcion, "12").
procesarOpcion(_):- writeln('Ingrese una opcion valida.'), nl, menu.

%Agregar un nuevo empleado a la base de conocimientos
%Entradas: Nombre,Posicion, costo por tarea y rating del empleado
%Salidas: Crea el nuevo hecho empleado y lo agrega a la base de conocimientos de prolog
%Restricciones: No puede haber varios empleados con el mismo nombre
agregarEmpleado:-
    write('Nombre: '), read_line_to_string(user_input, Nombre), nl,
    write('Posicion: '), read_line_to_string(user_input, Posicion), nl,
    write('Costo por tarea: '), read_line_to_string(user_input, CostoPorTarea), nl,
    write('Rating: '), read_line_to_string(user_input, Rating), nl,
    agregarNuevoEmpleado(Nombre, Posicion, CostoPorTarea, Rating).

agregarNuevoEmpleado(Nombre, Posicion, CostoPorTarea, Rating):-
    (persona(Nombre, _, _, _, _) ->
        writeln('Ya existe una persona con este nombre'), nl
    ;   (not(validarNum(CostoPorTarea)) ->
            writeln('El costo por tarea debe ser numérico'), nl
        ;   (not(validarNum(Rating)) ->
                writeln('El rating debe ser numérico'), nl
            ;   (atom_number(CostoPorTarea, Num), Num > 0 ->
                    (atom_number(Rating, RatingNum), RatingNum > 0 ->
                        % Agregar el empleado si pasa todas las verificaciones
                        addListaTareasTrabajador(Nombre, Posicion, CostoPorTarea, Rating, [])
                    ;   writeln('El rating debe ser un número válido'), nl
                    )
                ;   writeln('El costo por tarea debe ser un número válido'), nl
                )
            )
        )
    ).

%Agrega la lista de tareas que realiza el trabajador
%Entradas: La lista de labores que el trabajador realiza
%Salidas: Crea la lista de tareas y las agrega al empleado
%Restricciones: Las tareas deben ser tareas validas del programa
addListaTareasTrabajador(Nombre, Posicion, Costo, Rating, Tareas) :-
    writeln('Ingrese la lista de tareas que realiza e trabajador (ingrese "*" para terminar): '),
    agregarTareas(Nombre, Posicion, Costo, Rating, Tareas).

agregarTareas(Nombre, Posicion, Costo, Rating, Tareas) :-
    read_line_to_string(user_input, InputTarea),
    (InputTarea = "*" ->
        (Tareas = [] ->
            writeln('Debe ingresar al menos una tarea'), nl
        ;   asserta(persona(Nombre, Posicion, Costo, Rating, Tareas)),
            actualizarArchivoPersonas(Nombre, Posicion, Costo, Rating, Tareas),
            writeln('El usuario ha sido agregado')
        )
    ;   (member(InputTarea, Tareas) ->
            writeln('Esta tarea ya ha sido agregada')
        ;   append(Tareas, [InputTarea], NuevasTareas),
            agregarTareas(Nombre, Posicion, Costo, Rating, NuevasTareas)
        )
    ).

%Actualiza el archivo personas.txt con los datos de nuevo empleado
%Entradas: Los datos del nuevo empleado a ser agregado a la base de conocimientos
%Salidas: Agrega el nuevo empleado en la base de conocimientos
%Restricciones: La ruta del archivo debe ser valida
actualizarArchivoPersonas(Nombre, Cargo, CostoPorTarea, Rating, ListaTareas):-
    open('D:\\WockSpace\\Lenguajes2023\\ProyectoProlog\\programa\\personas.txt', append, Stream),
    writeq(Stream, persona(Nombre, Cargo, CostoPorTarea, Rating, ListaTareas)), write(Stream, '.\n'),
    close(Stream).

%Muestra todos los empleados en la base de conocimentos 
%Entradas: ninguna
%Salidas: Muestra de forma ordenada y con formato la lista de todos los empleados en la base de conocimeintos
%Restricciones: La BC no debe estar vacia
mostrarEmpleados :-
    persona(Nombre, Posicion, CostoPorTarea, Rating, _),
    write('Nombre: '), writeln(Nombre),
    write('Posicion: '), writeln(Posicion),
    write('Costo por tarea: '), writeln(CostoPorTarea),
    write('Rating: '), writeln(Rating),
    writeln('\nTareas:\n'),
    mostrarTrabajosDelTrabajador(Nombre),
    nl, fail.
mostrarEmpleados :- true.

%Muestra la lista de labores que realiza el empleado indicado como argumento 
%Entradas: El nombre de un empleado
%Salidas: Muestra la lista de las labores asignadas al empleado
%Restricciones: La lista de labores del empleado indicado no debe estar vacia
mostrarTrabajosDelTrabajador(Empleado):-
    tarea(Tarea, Proyecto, _, _, Empleado, _, _),
    write('Tarea: '), writeln(Tarea),
    write('Proyecto: '), writeln(Proyecto),
    nl, fail.


%Agrega el nuevo proyecto a la base de conocimientos
%Entradas: El usuario indica los datos del nuevo proyecto
%Salidas: Agrega el nuevo proyecto a la base de conocimientos
%Restricciones: No pueden existir dos proyectos con el mismo nombre
addProyecto:-
    write('Nombre del proyecto: '), read_line_to_string(user_input, Nombre), nl,
    write('Nombre de la empresa: '), read_line_to_string(user_input, Empresa), nl,
    write('Presupuesto: '), read_line_to_string(user_input, Presupuesto), nl,
    write('Fecha de inicio: '), read_line_to_string(user_input, FechaInicio), nl,
    write('Fecha final: '), read_line_to_string(user_input, FechaFin), nl,
    agregarProyecto(Nombre, Empresa, Presupuesto, FechaInicio, FechaFin).

agregarProyecto(Nombre, Empresa, Presupuesto, FechaInicio, FechaFin):-
    (   proyecto(Nombre, _, _, _, _) ->
        writeln('Ya existe un proyecto con este nombre'), nl
    ;   not(validarNum(Presupuesto)) ->
        writeln('El presupuesto debe ser numerico'), nl
    ;   asserta(proyecto(Nombre, Empresa, Presupuesto, FechaInicio, FechaFin)),
        actualizarArchivoProyectos(Nombre, Empresa, Presupuesto, FechaInicio, FechaFin),
        writeln('Proyecto agregado con exito!!'), nl
    ).

%Actualiza el archivo proyectos.txt con los datos de nuevo proyecto
%Entradas: Los datos del nuevo proyecto a ser agregado a la base de conocimientos
%Salidas: Agrega el nuevo proyecto en la base de conocimientos
%Restricciones: La ruta del archivo debe ser valida
actualizarArchivoProyectos(Nombre, Empresa, Presupuesto, FechaInicio, FechaFin):-
    open('D:\\WockSpace\\Lenguajes2023\\ProyectoProlog\\programa\\proyectos.txt', append, Stream),
    writeq(Stream, proyecto(Nombre, Empresa, Presupuesto, FechaInicio, FechaFin)), write(Stream, '.\n'),
    close(Stream).

%Muestra todos los proyectos en la base de conocimeintos
%Entradas: La base de conocimientos
%Salidas: Muestra de forma ordenada los datos de todos los proyectos en la base de conocimientos
%Restricciones: La BC de conocimeintos debe contener proyectos
showProyecto :-
    proyecto(Nombre, Empresa, Presupuesto, FechaInicio, FechaFin),
    write('Nombre: '), writeln(Nombre),
    write('Empresa: '), writeln(Empresa),
    write('Presupuesto: '), writeln(Presupuesto),
    write('Fecha inicio: '), writeln(FechaInicio),
    write('Fecha final: '), writeln(FechaFin),
    write('GastosTotales total: '), 
    sumarCostosTareas(Nombre, GastosTotales), write(GastosTotales), nl,
    nl, fail.
showProyecto :- true.

%Agrega las tareas a la base de conocimientos de prolog
%Entradas: Los datos de la nueva tarea a ser agregada
%Salidas: Agrega la nueva entrada a la base de conocimientos
%Restricciones: El tipo de la tarea debe ser un tipo valido y debe exitir al menos un pryecto en la BC
addTarea :- getNumProyts(NumProyectos), NumProyectos = 0, writeln('Debe existir al menos un proyecto').
addTarea :- writeln('Nombre: '), read_line_to_string(user_input, Nombre), writeln('Proyecto: '), read_line_to_string(user_input, Proyecto), writeln('Tipo: '), read_line_to_string(user_input, Tarea), writeln('Fecha inicio: '), read_line_to_string(user_input, Fecha), validarTarea(Nombre, Proyecto, Tarea, Fecha), actualizarArchivoTareas(Nombre, Proyecto, Tarea, "pendiente", "sin asignar", Fecha, "sin asignar"), asserta(tarea(Nombre, Proyecto, Tarea, "pendiente", "sin asignar", Fecha, "sin asignar")), writeln('Tarea agregada.').

validarTarea(_, _, Tarea, _) :- string_lower(Tarea, Tarea_lower), tareas(Lista), maplist(string_lower, Lista, Lista_lower), not(member(Tarea_lower, Lista_lower)), writeln('Debe ingresar un tipo de tarea vaido'), !, fail.
validarTarea(_, Proyecto, _, _) :- not(validarEmpresa(Proyecto)), writeln('Proyecto invalido'), !, fail.
validarTarea(Nombre, _, _, _) :- validarExistenciaTarea(Nombre), writeln('Nombre invalido'), !, fail.
validarTarea(_, _, _, _).


%Actualiza el archivo de tareas
%Entradas: La ruta del archivo de tareas y los datos de la nueva tarea a agregarEmpleado
%Salidas: Agrega la nueva tarea al archivo de tareas
%Restricciones: La ruta debe ser valida
actualizarArchivoTareas(Nombre, Proyecto, Tipo, Estado, Encargado, FechaInicio, Fecha_final):-
    open('D:\\WockSpace\\Lenguajes2023\\ProyectoProlog\\programa\\tareas.txt', append, Stream),
    writeq(Stream, tarea(Nombre, Proyecto, Tipo, Estado, Encargado, FechaInicio, Fecha_final)), write(Stream, '.\n'),
    close(Stream).

%Muestra todas las tareas del sistema
%Entradas: La BC
%Salidas: todas las tareas de la BC
%Restriciones: La BC debe estar cargada
showTareas :-
    tarea(Nombre, Proyecto, Tipo, Estado, Encargado, FechaInicio, FechaCierre),
    write('Nombre: '), writeln(Nombre),
    write('Proyecto: '), writeln(Proyecto),
    write('Tipo: '), writeln(Tipo),
    write('Estado: '), writeln(Estado),
    write('Responsable: '), writeln(Encargado),
    write('Fecha inicio: '), writeln(FechaInicio),
    write('Fecha fin: '), writeln(FechaCierre),
    write('\n'),
    fail.
showTareas :- true.

%Asigna una tarea a un trabajador
%Entradas: Nombre de encargado, proyecto y nombre de tarea
%Salidas: Asigna la tarea al encargado indicado
%Restricciones: La BC debe contener proyectos, tareas y empleados
asignarTarea:- 
    write('Nombre encargado: '), read_line_to_string(user_input, Encargado), nl,
    write('Nombre proyecto: '), read_line_to_string(user_input, Proyecto), nl,
    write('Nombre tarea: '), read_line_to_string(user_input, Tarea), nl,
    asignarTareaAux(Proyecto, Tarea, Encargado).

asignarTareaAux(Proyecto, Tarea, Encargado) :- 
    (not(proyecto(Proyecto, _, _, _, _)) ->
        writeln('Ese nombre de proyecto no existe'), nl
    ; not(tarea(Tarea, _, _, _, _, _, _)) ->
        writeln('La tarea debe ser de los tipos validos'), nl
    ; not(persona(Encargado, _, _, _, _)) ->
        writeln('No existe una persona con ese nombre'), nl
    ; not(tarea(Tarea, Proyecto, _, _, _, _, _)) ->
        writeln('El proyecto no cuenta con tareas del tipo especificado'), nl
    ; tarea(Tarea, Proyecto, _, _, Encargado, _, _) ->
        writeln('La tarea no se encuentra disponible para ser asignada'), nl
    ; retract(tarea(Tarea, Proyecto, Tipo, _, _, FechaInicio, Fecha_final)),
        asserta(tarea(Tarea, Proyecto, Tipo, 'activa', Encargado, FechaInicio, Fecha_final)), 
        actualizarTareas,
        writeln('La tarea ha sido asignada'), nl
    ).

%Actualiza el documentos txt con los datos de las tareas para asignar el nuevo encargado
%Entradas: Los nuevos datos de la tarea 
%Salidas: cambia el encargado de "sin asignar" al nombre del encargado
%Restricciones: El encargado, proyecto y tarea deben estar debidamente identificados en la BC
actualizarTareas :-
    findall(tarea(Tarea, Proyecto, Tipo, Estado, Cargo, FechaInicio, Fecha_final), tarea(Tarea, Proyecto, Tipo, Estado, Cargo, FechaInicio, Fecha_final), Tareas),
    actualizarArchivoDeTexto(Tareas, 'D:\\WockSpace\\Lenguajes2023\\ProyectoProlog\\programa\\tareas.txt').

actualizarArchivoDeTexto(Tareas, FilePath) :-
    actualizarContenidoArchivo(FilePath),
    open(FilePath, append, Stream),
    maplist([Hecho]>>writeq(Stream, Hecho), Tareas),
    write(Stream, '.\n'),
    close(Stream).

actualizarContenidoArchivo(FilePath) :-
    open(FilePath, write, Stream), 
    write(Stream, ''),   
    close(Stream).

%Muestra las tareas con estado pendiente
%Entradas: la BC
%Salidas: Muestra todas las tareas en estado pendiente
%Restricciones: la bas de conocimientos debe estar cargada
showPendientes() :-
    tarea(Nombre, Proyecto, Tipo, "Pendiente", Encargado, FechaInicio, FechaCierre),
    write('Nombre: '), writeln(Nombre),
    write('Proyecto: '), writeln(Proyecto),
    write('Tipo: '), writeln(Tipo),
    write('Estado: Pendiente '),
    write('Encargado: '), writeln(Encargado),
    write('Fecha inicio: '), writeln(FechaInicio),
    write('Fecha finalizacion: '), writeln(FechaCierre),
    write('\n'),
    fail.
showPendientes(_) :- true.

%Muestra las tareas que un trabajador puede realizar segun su lista de tareas
%Entradas: El nombre del trabajador
%Salidas: Muestra la lista de tareas que el trabajador puede realizar
%Restricciones: Deben existir trabajadores, proyectos y tareas en la BC
buscarTareaTrabajador :-
    writeln('\n'),
    writeln('Nombre del trabajador a buscar: '),
    read_line_to_string(user_input, Nombre),
    buscarTareaTrabajadorAux(Nombre).
buscarTareaTrabajadorAux(Nombre) :- persona(Nombre, _, _, _, Lista), showTareasPorLaboresDeTrabajador(Lista).
buscarTareaTrabajadorAux(Nombre) :- not(verificarTrabajador(Nombre)), writeln("\nTrabajador no valido.\n"), !.

%Muestra la lista de tareas que el trabajador realiza
%Entradas: La lista de tareas del trabajador
%Salidas: Muestra los datos de las tareas
%Restricciones: La lista de tareas no debe estar vacia
showTareasPorLaboresDeTrabajador(Tareas) :-
    tarea(Nombre, Proyecto, Tipo, Estado, "sin asignar", FechaInicio, FechaCierre),
    member(Tipo, Tareas),
    write('Nombre: '), writeln(Nombre),
    write('Proyecto: '), writeln(Proyecto),
    write('Tipo de tareas: '), writeln(Tipo),
    write('Estado: '), writeln(Estado),
    write('Encargado: sin asignar'),
    write('Fecha de inicio: '), writeln(FechaInicio),
    write('Fecha de finalizacion: '), writeln(FechaCierre),
    write('\n'),
    fail.
showTareasPorLaboresDeTrabajador(_) :- true.

%Muestra el estado de los proyectos, si esta dentro, fuera del presupuesto o en la raya
%Entradas: Nombre del proyecto y su presupuesto
%Salidas: Muestra todos los proyectos e imprime un mensaje mostrando su estado
%Restricciones: La base de conocimientos debe contener proyectos
estatusProyectos:- 
    writeln("\n******Estatus financiero de los proyectos*****\n"),
    proyecto(NombreProyecto, _, Presupuesto, _, _), 
    sumarCostosTareas(NombreProyecto, GastosTotales),
    atom_number(Presupuesto, Num),
    (GastosTotales > Num ->
        write('\nProyecto: '), write(NombreProyecto), write(' sobrepasa el presupuesto'), nl
    ; GastosTotales < Num ->
        write('\nProyecto: '), write(NombreProyecto), write(' esta dentro del presupuesto'), nl
    ; 
        write('\nProyecto: '), write(NombreProyecto), write(' quedo en tablas'), nl
    ),
    fail.
estatusProyectos :- true.

%Retorna el numero de proyectos existentes en la BC
%Entradas: la cantidad a unificar
%Salidas: El numero de proyectos existentes en la BC
%Restricciones: ninguna
getNumProyts(Cantidad) :-
    findall(_, proyecto(_,_,_,_,_), Proyectos),
    length(Proyectos, Cantidad).

%Valida el nombre del trabajador
%Entradas: El nombre del trabajador
%Salidas: True si el trabajador existe, false si no
%Restricciones: El nombre debe ser un string
verificarTrabajador(Nombre) :-
    persona(Nombre, _, _, _, _), !.

%Valida el nombre de la empresa
%Entradas: El nombre de la empresa
%Salidas: True si la empresa existe, false si no
%Restricciones: El nombre debe ser un string
validarEmpresa(Nombre) :-
    proyecto(Nombre, _, _, _, _), !.

%Valida el nombre de la tarea
%Entradas: El nombre de la tarea
%Salidas: True si la tarea existe, false si no
%Restricciones: El nombre debe ser un string
validarExistenciaTarea(Nombre) :-
    tarea(Nombre, _, _, _, _, _, _), !.

%Muestra el numero de tareas que cada trabajador tiene asignadas
%EntradaS: El usuario debe indicar el nombre del trabajador
%Salidas: Muestra la cantidad de tareas asignadas al trabajador
%Restricciones: El nombre del trabajador debe ser valido
tareasPorTrabajador:- 
    writeln("\n******Cantidad de tareas por persona*****\n"),
    persona(Nombre, _, _, _, _),
    findall(_,
        (tarea(_, _, _, _, Nombre, _, _)),
        Tareas
    ), length(Tareas, Cantidad), write(Nombre),
    write(' trabaja o trabajo en '), write(Cantidad), write(' tareas'), nl, fail.
tareasPorTrabajador :- true.

%Verifica que un string sea numerico
%Entradas: Un string
%Salidas: true si el string es un numero, false si no
%Restricciones: el argumento debe ser un string de un numero
validarNum(String) :-
    atom_string(Atom, String),
    atom_number(Atom, _).

%Calcula el costo de todas las tareas relacionadas con el nombre de proyecto indicado
%Entradas: El nombre de un proyecto y el costo a ser unificado
%Salidas: Retorna el costo total de las tareas asignadas al proyecto
%Restricciones: Ninguna
sumarCostosTareas(NombreProyecto, CostoTotal) :-
    findall(Pago, (
        tarea(_, NombreProyecto, _, _, Encargado, _, _),
        persona(Encargado, _, PagoStr, _, _),
        atom_number(PagoStr, Pago)
    ), Pagos),
    sum_list(Pagos, CostoTotal).
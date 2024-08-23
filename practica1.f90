MODULE InventarioMod
    IMPLICIT NONE
    ! Definición del tipo de datos para el inventario
    TYPE :: Inventario
        CHARACTER(LEN=100) :: nombre ! Nombre del equipo
        CHARACTER(LEN=100) :: ubicacion! Ubicación del equipo
        INTEGER :: cantidad! Cantidad disponible del equipo
        Real :: precio_unitario! Precio unitario del equipo
    END TYPE Inventario
    INTEGER, PARAMETER :: MAX_INVENTARIO = 300 ! Número máximo de equipos en el inventario
END MODULE InventarioMod
PROGRAM practica1
    USE InventarioMod  ! Uso del módulo InventarioMod para manejar inventarios
    TYPE(Inventario), DIMENSION(:), ALLOCATABLE :: inventarios  ! Array de inventarios que se puede redimensionar
    CHARACTER(LEN=100) :: filename  ! Nombre del archivo a procesar
    INTEGER :: num_inventario  ! Número actual de inventarios
    INTEGER :: opcion  ! Opción seleccionada en el menú
    INTEGER :: contador  ! Contador de elementos en el inventario
    ! Inicialización
    num_inventario = MAX_INVENTARIO  ! Inicializa el número máximo de inventarios
    ALLOCATE(inventarios(num_inventario))  ! Reserva memoria para el array de inventarios
    contador = 0  ! Inicializa el contador de inventarios a 0
    DO ! Menú principal
        PRINT *, ""  ! Imprime una línea en blanco para separar las secciones
        PRINT *, "----------------------------------------------------"  ! Imprime una línea de separación
        PRINT *, "Practica 1 - Lenguajes formales y de programacion"  ! Imprime el título del programa
        PRINT *, "----------------------------------------------------"  ! Imprime una línea de separación
        PRINT *, "# Sistema de Inventario"  ! Imprime el título de la sección del sistema de inventario
        PRINT *, ""  ! Imprime una línea en blanco para separar los elementos del menú
        PRINT *, "1. Cargar Inventario Inicial"  ! Opción para cargar el inventario inicial
        PRINT *, "2. Cargar Instrucciones de Movimiento"  ! Opción para cargar instrucciones de movimiento
        PRINT *, "3. Crear Informe de Inventario"  ! Opción para crear un informe de inventario
        PRINT *, "4. Salir"  ! Opción para salir del programa
        PRINT *, ""  ! Imprime una línea en blanco
        PRINT *, "Ingrese una opcion:"  ! Solicita al usuario que ingrese una opción
        READ *, opcion  ! Lee la opción ingresada por el usuario
        SELECT CASE (opcion)
            CASE (1)
                PRINT *, 'Escriba la Ruta'  ! Solicita al usuario la ruta del archivo de inventario
                READ *, filename  ! Lee el nombre del archivo a procesar
                CALL analizador(filename, .FALSE., contador)  ! Llama a la subrutina analizador para procesar el archivo .inv
                PRINT *, 'Inventario cargado'  ! Mensaje de confirmación de carga de inventario
            CASE (2)
                PRINT *, 'Escriba la Ruta del archivo .mov'  ! Solicita al usuario la ruta del archivo de movimientos
                READ *, filename  ! Lee el nombre del archivo a procesar
                CALL analizador(filename, .TRUE., contador)  ! Llama a la subrutina analizador para procesar el archivo .mov
                PRINT *, 'Movimientos procesados'  ! Mensaje de confirmación de procesamiento de movimientos
            CASE (3)
                PRINT *, 'Creando Informe de Inventario'  ! Mensaje para crear el informe de inventario
                CALL generar_informe(inventarios, contador)  ! Llama a la subrutina generar_informe para crear el informe
            CASE (4)
                PRINT *, 'Salir'  ! Mensaje para salir del programa
                EXIT  ! Sale del programa
            CASE DEFAULT
                PRINT *, 'Opcion invalida'  ! Mensaje para opciones no válidas
        END SELECT
    END DO 
CONTAINS
SUBROUTINE parse_line(line, inventario1)
    USE InventarioMod, ONLY: Inventario  ! Importa el tipo Inventario del módulo InventarioMod
    TYPE(Inventario), INTENT(OUT) :: inventario1  ! Parámetro de salida para el inventario a completar
    CHARACTER(LEN=*), INTENT(IN) :: line  ! Línea de entrada que se debe procesar
    INTEGER :: start, end_pos  ! Variables para las posiciones de los separadores
    CHARACTER(LEN=300) :: field(4)  ! Array para almacenar los campos extraídos
    INTEGER :: temp_int  ! Variable temporal para almacenar la cantidad
    REAL :: temp_real  ! Variable temporal para almacenar el precio unitario
    INTEGER :: ios  ! Variable para manejar errores de lectura
    CHARACTER(LEN=200) :: temp_line  ! Variable temporal para almacenar la línea procesada
    ! Asignar la línea leída a una variable temporal
    temp_line = TRIM(line)  ! Elimina espacios en blanco al principio y al final de la línea
    start = 1  ! Inicializa la posición de inicio para la extracción de campos
    ! Extraer nombre
    end_pos = INDEX(temp_line, ";")  ! Encuentra la posición del primer separador ";"
    IF (end_pos > 0) THEN
        field(1) = TRIM(temp_line(start:end_pos-1))  ! Extrae el nombre del equipo
        temp_line = TRIM(temp_line(end_pos+1:))  ! Actualiza temp_line eliminando el nombre extraído
    ELSE
        PRINT *, 'Error: Linea no tiene el primer separador ; esperado'  ! Mensaje de error si falta el separador
        RETURN  ! Sale de la subrutina en caso de error
    END IF
    ! Extraer cantidad
    end_pos = INDEX(temp_line, ';')  ! Encuentra la posición del segundo separador ";"
    IF (end_pos > 0) THEN
        field(2) = TRIM(temp_line(start:end_pos-1))  ! Extrae la cantidad
        temp_line = TRIM(temp_line(end_pos+1:))  ! Actualiza temp_line eliminando la cantidad extraída
    ELSE
        PRINT *, 'Error: Linea no tiene el segundo separador ; esperado'  ! Mensaje de error si falta el separador
        RETURN  ! Sale de la subrutina en caso de error
    END IF
    ! Extraer precio unitario
    end_pos = INDEX(temp_line, ';')  ! Encuentra la posición del tercer separador ";"
    IF (end_pos > 0) THEN
        field(3) = TRIM(temp_line(start:end_pos-1))  ! Extrae el precio unitario
        temp_line = TRIM(temp_line(end_pos+1:))  ! Actualiza temp_line eliminando el precio unitario extraído
    ELSE
        PRINT *, 'Error: Linea no tiene el tercer separador ; esperado'  ! Mensaje de error si falta el separador
        RETURN  ! Sale de la subrutina en caso de error
    END IF
    ! Extraer ubicación
    field(4) = TRIM(temp_line)  ! Asigna la ubicación restante
    ! Asignar valores a los campos del inventario
    inventario1%nombre = field(1)  ! Asigna el nombre al campo correspondiente del inventario
    READ(field(2), *, IOSTAT=ios) temp_int  ! Lee la cantidad del campo y almacena en temp_int
    IF (ios /= 0) THEN
        PRINT *, 'Error en el formato de cantidad: ', TRIM(field(2))  ! Mensaje de error si el formato de cantidad es incorrecto
        RETURN  ! Sale de la subrutina en caso de error
    END IF
    inventario1%cantidad = temp_int  ! Asigna la cantidad al campo correspondiente del inventario
    READ(field(3), *, IOSTAT=ios) temp_real  ! Lee el precio unitario del campo y almacena en temp_real
    IF (ios /= 0) THEN
        PRINT *, 'Error en el formato de precio unitario: ', TRIM(field(3))  ! Mensaje de error si el formato de precio unitario es incorrecto
        RETURN  ! Sale de la subrutina en caso de error
    END IF
    inventario1%precio_unitario = temp_real  ! Asigna el precio unitario al campo correspondiente del inventario
    inventario1%ubicacion = field(4)  ! Asigna la ubicación al campo correspondiente del inventario
END SUBROUTINE parse_line
SUBROUTINE analizador(archivo, es_mov, contador)
    USE InventarioMod, ONLY: Inventario  ! Importa el tipo Inventario del módulo InventarioMod
    CHARACTER(LEN=100), INTENT(IN) :: archivo  ! Nombre del archivo que se va a procesar
    LOGICAL, INTENT(IN) :: es_mov  ! Indicador de si el archivo es de tipo .mov (TRUE) o .inv (FALSE)
    CHARACTER(LEN=200) :: line  ! Línea leída del archivo
    INTEGER :: ios  ! Variable para manejar errores de entrada/salida
    CHARACTER(LEN=100) :: comando, datos, nombre, ubicacion  ! Variables para almacenar partes de la línea
    INTEGER :: start, end_pos, cantidad  ! Variables para manipulación de cadenas y cantidad
    INTEGER, INTENT(INOUT) :: contador  ! Contador que lleva el número de inventarios procesados
    ! Abrir el archivo para lectura
    OPEN(UNIT=10, FILE=archivo, STATUS='OLD', ACTION='READ', IOSTAT=ios)  ! Abre el archivo en modo lectura
    IF (ios /= 0) THEN
        PRINT *, 'Error al abrir el archivo para lectura'  ! Mensaje de error si no se puede abrir el archivo
        STOP  ! Termina la ejecución del programa
    END IF
    ! Leer el archivo línea por línea
    DO
        READ(10, '(A)', IOSTAT=ios) line  ! Lee una línea del archivo
        IF (ios /= 0) EXIT  ! Sale del bucle si hay un error al leer la línea (fin de archivo)
        ! Separar la línea en comando y datos
        start = 1  ! Inicializa la posición de inicio para la separación
        end_pos = SCAN(line(start:), ' ')  ! Encuentra la posición del primer espacio
        IF (end_pos == 0) THEN
            comando = TRIM(line(start:))  ! Si no hay espacio, toda la línea es el comando
        ELSE
            comando = TRIM(line(start:start+end_pos-1))  ! Extrae el comando hasta el primer espacio
            datos = TRIM(line(start+end_pos:))  ! Extrae los datos restantes después del espacio
        END IF
        ! Verifica que contador no exceda los límites
        IF (contador >= MAX_INVENTARIO) THEN
            PRINT *, 'Limite de inventarios alcanzado'  ! Mensaje de error si se alcanza el límite
            EXIT  ! Sale del bucle
        END IF
        IF (.NOT. es_mov) THEN
            ! Procesar archivo .inv
            CALL parse_line(datos, inventarios(contador+1))  ! Llama a parse_line para procesar los datos del archivo .inv
            contador = contador + 1  ! Incrementa el contador de inventarios
        ELSE
            ! Procesar archivo .mov
            IF (comando == 'agregar_stock') THEN
                CALL extraer_datos_mov(datos, nombre, cantidad, ubicacion)  ! Extrae los datos de la línea
                CALL agregar_stock(inventarios, contador, nombre, cantidad, ubicacion)  ! Llama a agregar_stock para actualizar el inventario

            ELSE IF (comando == 'eliminar_equipo') THEN
                CALL extraer_datos_mov(datos, nombre, cantidad, ubicacion)  ! Extrae los datos de la línea
                CALL eliminar_equipo(inventarios, contador, nombre, cantidad, ubicacion)  ! Llama a eliminar_equipo para actualizar el inventario
            END IF
        END IF
    END DO
    CLOSE(UNIT=10)  ! Cierra el archivo después de procesar
END SUBROUTINE analizador
SUBROUTINE extraer_datos_mov(datos, nombre, cantidad, ubicacion)
    CHARACTER(LEN=*), INTENT(IN) :: datos  ! Cadena de entrada que contiene los datos a extraer
    CHARACTER(LEN=100), INTENT(OUT) :: nombre, ubicacion  ! Variables de salida para el nombre y la ubicación
    INTEGER, INTENT(OUT) :: cantidad  ! Variable de salida para la cantidad
    INTEGER :: end_pos, ios  ! Variables para manejar la posición de los separadores y errores de entrada/salida
    CHARACTER(LEN=200) :: datos_temp  ! Variable temporal para manipular los datos
    CHARACTER(LEN=20) :: cantidad_str  ! Variable para almacenar la cadena de la cantidad
    ! Copiar datos a una variable temporal para manipulación
    datos_temp = datos  ! Asigna la cadena de entrada a la variable temporal
    ! Extraer nombre
    end_pos = INDEX(datos_temp, ';')  ! Encuentra la posición del primer separador ';'
    IF (end_pos > 0) THEN
        nombre = TRIM(datos_temp(1:end_pos-1))  ! Extrae el nombre antes del primer separador
        datos_temp = TRIM(datos_temp(end_pos+1:))  ! Actualiza datos_temp para eliminar la parte procesada
    ELSE
        PRINT *, 'Error: Datos no tienen el primer separador ; esperado'  ! Mensaje de error si no se encuentra el primer separador
        RETURN  ! Sale de la subrutina
    END IF
    ! Extraer cantidad
    end_pos = INDEX(datos_temp, ';')  ! Encuentra la posición del segundo separador ';'
    IF (end_pos > 0) THEN
        cantidad_str = TRIM(datos_temp(1:end_pos-1))  ! Extrae la cadena de la cantidad antes del segundo separador
        datos_temp = TRIM(datos_temp(end_pos+1:))  ! Actualiza datos_temp para eliminar la parte procesada

        ! Convertir la cadena a entero
        READ(cantidad_str, *, IOSTAT=ios) cantidad  ! Convierte la cadena de cantidad a entero
        IF (ios /= 0) THEN
            PRINT *, 'Error en el formato de cantidad'  ! Mensaje de error si la conversión falla
            RETURN  ! Sale de la subrutina
        END IF
    ELSE
        PRINT *, 'Error: Datos no tienen el segundo separador ; esperado'  ! Mensaje de error si no se encuentra el segundo separador
        RETURN  ! Sale de la subrutina
    END IF
    ! Extraer ubicación
    ubicacion = TRIM(datos_temp)  ! La parte restante de datos_temp se asigna a la ubicación
END SUBROUTINE extraer_datos_mov
SUBROUTINE generar_informe(inventarios, num_inventario)
    USE InventarioMod  ! Importa el módulo que define el tipo Inventario
    IMPLICIT NONE  ! Desactiva la declaración implícita de variables
    TYPE(Inventario), DIMENSION(:), INTENT(IN) :: inventarios  ! Arreglo de inventarios pasado como argumento de entrada
    INTEGER, INTENT(IN) :: num_inventario  ! Número de inventarios a procesar, pasado como argumento de entrada
    INTEGER :: i  ! Variable de índice para el bucle
    CHARACTER(LEN=100) :: line  ! Variable para almacenar una línea de texto formateado
    CHARACTER(LEN=50) :: nombre_format  ! Variable para almacenar el nombre del equipo formateado
    REAL :: valor_total  ! Variable para calcular el valor total de cada inventario
    ! Abrir el archivo 'informe.txt' en modo de reemplazo
    OPEN(UNIT=10, FILE='informe.txt', STATUS='REPLACE')
    ! Escribir encabezado en el archivo
    WRITE(10, '(A)') "---------------------------------------------------------------------------------------"
    WRITE(10, '(A)') "Informe de Inventario:"
    WRITE(10, '(A)') "---------------------------------------------------------------------------------------"
    WRITE(10, '(A)') "Equipo              Cantidad       Precio Unitario      Valor Total       Ubicación"
    WRITE(10, '(A)') "---------------------------------------------------------------------------------------"
    ! Bucle para procesar cada inventario
    DO i = 1, num_inventario
        ! Formatear el nombre del equipo correctamente
        nombre_format = TRIM(ADJUSTL(inventarios(i)%nombre))  ! Ajustar y eliminar espacios en blanco alrededor del nombre
        ! Calcular el valor total del inventario
        valor_total = inventarios(i)%cantidad * inventarios(i)%precio_unitario  ! Multiplicar cantidad por precio unitario
        ! Formatear la línea de información del inventario
        WRITE(line, '(A15, I10, F19.2, F21.2, A17)') nombre_format, &  ! Formato de salida para el nombre del equipo
                                              inventarios(i)%cantidad,    &  ! Formato de salida para la cantidad
                                              inventarios(i)%precio_unitario, &  ! Formato de salida para el precio unitario
                                              valor_total, &  ! Formato de salida para el valor total
                                              TRIM(ADJUSTL(inventarios(i)%ubicacion))  ! Formato de salida para la ubicación
        ! Escribir la línea formateada en el archivo
        WRITE(10, '(A)') TRIM(line)  ! Escribir la línea en el archivo
    END DO
    CLOSE(10)  ! Cerrar el archivo
END SUBROUTINE generar_informe
SUBROUTINE agregar_stock(inventarios, contador, nombre, cantidad, ubicacion)
    USE InventarioMod  ! Importa el módulo que define el tipo Inventario
    TYPE(Inventario), DIMENSION(:), INTENT(INOUT) :: inventarios  ! Arreglo de inventarios, pasado como argumento de entrada y salida
    INTEGER, INTENT(INOUT) :: contador  ! Contador del número de inventarios, pasado como argumento de entrada y salida
    CHARACTER(LEN=*), INTENT(IN) :: nombre, ubicacion  ! Nombre del equipo y ubicación, pasados como argumentos de entrada
    INTEGER, INTENT(IN) :: cantidad  ! Cantidad a agregar, pasada como argumento de entrada
    LOGICAL :: encontrado  ! Variable para indicar si se encontró el equipo
    INTEGER :: i  ! Variable de índice para el bucle
    encontrado = .FALSE.  ! Inicializa la variable encontrado como falso
    ! Bucle para buscar el equipo en el inventario
    DO i = 1, contador
        ! Compara el nombre y la ubicación del inventario actual con los proporcionados
        IF (TRIM(inventarios(i)%nombre) == TRIM(nombre) .AND. TRIM(inventarios(i)%ubicacion) == TRIM(ubicacion)) THEN
            ! Si se encuentra, actualiza la cantidad del equipo
            inventarios(i)%cantidad = inventarios(i)%cantidad + cantidad
            encontrado = .TRUE.  ! Marca como encontrado
            EXIT  ! Sale del bucle
        END IF
    END DO
    ! Si el equipo no fue encontrado, muestra un mensaje de error
    IF (.NOT. encontrado) THEN
        PRINT *, 'Error: El equipo no existe en la ubicacion especificada'
    END IF
END SUBROUTINE agregar_stock
SUBROUTINE eliminar_equipo(inventarios, contador, nombre, cantidad, ubicacion)
    USE InventarioMod  ! Importa el módulo que define el tipo Inventario
    TYPE(Inventario), DIMENSION(:), INTENT(INOUT) :: inventarios  ! Arreglo de inventarios, pasado como argumento de entrada y salida
    INTEGER, INTENT(INOUT) :: contador  ! Contador del número de inventarios, pasado como argumento de entrada y salida
    CHARACTER(LEN=*), INTENT(IN) :: nombre, ubicacion  ! Nombre del equipo y ubicación, pasados como argumentos de entrada
    INTEGER, INTENT(IN) :: cantidad  ! Cantidad a eliminar, pasada como argumento de entrada
    LOGICAL :: encontrado  ! Variable para indicar si se encontró el equipo
    INTEGER :: i  ! Variable de índice para el bucle
    encontrado = .FALSE.  ! Inicializa la variable encontrado como falso
    ! Bucle para buscar el equipo en el inventario
    DO i = 1, contador
        ! Compara el nombre y la ubicación del inventario actual con los proporcionados
        IF (TRIM(inventarios(i)%nombre) == TRIM(nombre) .AND. TRIM(inventarios(i)%ubicacion) == TRIM(ubicacion)) THEN
            ! Si se encuentra, verifica si la cantidad a eliminar es menor o igual a la cantidad existente
            IF (inventarios(i)%cantidad >= cantidad) THEN
                ! Si la cantidad es suficiente, actualiza la cantidad del equipo
                inventarios(i)%cantidad = inventarios(i)%cantidad - cantidad
                encontrado = .TRUE.  ! Marca como encontrado
            ELSE
                ! Si la cantidad a eliminar es mayor que la existente, muestra un mensaje de error
                PRINT *, 'Error: Cantidad a eliminar mayor que la existente en la ubicacion'
            END IF
            EXIT  ! Sale del bucle
        END IF
    END DO
    ! Si el equipo no fue encontrado, muestra un mensaje de error
    IF (.NOT. encontrado) THEN
        PRINT *, 'Error: El equipo no existe en la ubicacion especificada'
    END IF
END SUBROUTINE eliminar_equipo
END PROGRAM practica1
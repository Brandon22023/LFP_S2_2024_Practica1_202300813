MODULE InventarioMod
    IMPLICIT NONE

    TYPE :: Inventario
        CHARACTER(LEN=50) :: nombre
        CHARACTER(LEN=50) :: ubicacion
        INTEGER :: cantidad
        Real :: precio_unitario
    END TYPE Inventario

    INTEGER, PARAMETER :: MAX_INVENTARIO = 300

END MODULE InventarioMod


PROGRAM practica1
    USE InventarioMod
    TYPE(Inventario), DIMENSION(:), ALLOCATABLE :: inventarios
    CHARACTER(LEN=100) :: filename
    INTEGER :: num_inventario
    INTEGER :: opcion
    INTEGER :: contador

    ! Inicialización
    num_inventario = MAX_INVENTARIO
    ALLOCATE(inventarios(num_inventario))
    contador = 0

    DO
        print *, ""
        print *, "----------------------------------------------------"
        print *, "Practica 1 - Lenguajes formales y de programacion"
        print *, "----------------------------------------------------"
        print *, "# Sistema de Inventario"
        print *, ""
        print *, "1. Cargar Inventario Inicial"
        print *, "2. Cargar Instrucciones de Movimiento"
        print *, "3. Crear Informe de Inventario"
        print *, "4. Salir"
        print *, ""
        print *, "Ingrese una opcion:"
        read *, opcion

        SELECT CASE (opcion)
            CASE (1)
                 PRINT *, 'Escriba la Ruta'
                READ *, filename
                CALL analizador(filename, .FALSE., contador)
                print *, 'Inventario cargado'
            CASE (2)
                PRINT *, 'Escriba la Ruta del archivo .mov'
                READ *, filename
                CALL analizador(filename, .TRUE., contador)  ! Procesar archivo .mov
                PRINT *, 'Movimientos procesados'
            CASE (3)
                PRINT *, 'Creando Informe de Inventario'
                CALL generar_informe(inventarios, contador)
            CASE (4)
                PRINT *, 'Salir'
                EXIT
            CASE DEFAULT
                PRINT *, 'Opcion invalida'

        END SELECT
    


    END DO 

CONTAINS

SUBROUTINE parse_line(line, inventario1)
    USE InventarioMod, ONLY: Inventario
    TYPE(Inventario), INTENT(OUT) :: inventario1
    CHARACTER(LEN=*), INTENT(IN) :: line
    INTEGER :: i
    CHARACTER(LEN=200) :: temp_line
    INTEGER :: start, end_pos
    CHARACTER(LEN=50) :: field(4)
    INTEGER :: temp_int
    REAL :: temp_real
    INTEGER :: ios

    temp_line = line
    start = 1

    ! Extraer nombre
    end_pos = INDEX(temp_line, ';')
    IF (end_pos > 0) THEN
        field(1) = TRIM(temp_line(1:end_pos-1))
        temp_line = temp_line(end_pos+1:)
    ELSE
        PRINT *, 'Error: Línea no tiene el primer separador ; esperado'
        RETURN
    END IF

    ! Extraer cantidad
    end_pos = INDEX(temp_line, ';')
    IF (end_pos > 0) THEN
        field(2) = TRIM(temp_line(1:end_pos-1))
        temp_line = temp_line(end_pos+1:)
    ELSE
        PRINT *, 'Error: Línea no tiene el segundo separador ; esperado'
        RETURN
    END IF

    ! Extraer precio unitario
    end_pos = INDEX(temp_line, ';')
    IF (end_pos > 0) THEN
        field(3) = TRIM(temp_line(1:end_pos-1))
        temp_line = temp_line(end_pos+1:)
    ELSE
        PRINT *, 'Error: Línea no tiene el tercer separador ; esperado'
        RETURN
    END IF

    ! Extraer ubicación
    field(4) = TRIM(temp_line)

    ! Asignar valores a los campos del inventario
    inventario1%nombre = field(1)
    READ(field(2), *, IOSTAT=ios) temp_int
    IF (ios /= 0) THEN
        PRINT *, 'Error en el formato de cantidad: ', TRIM(field(2))
        RETURN
    END IF
    inventario1%cantidad = temp_int

    READ(field(3), *, IOSTAT=ios) temp_real
    IF (ios /= 0) THEN
        PRINT *, 'Error en el formato de precio unitario: ', TRIM(field(3))
        RETURN
    END IF


    inventario1%precio_unitario = temp_real

    inventario1%ubicacion = field(4)
END SUBROUTINE parse_line


SUBROUTINE analizador(archivo, es_mov, contador)
    USE InventarioMod, ONLY: Inventario
    CHARACTER(LEN=100), INTENT(IN) :: archivo
    LOGICAL, INTENT(IN) :: es_mov
    CHARACTER(LEN=100) :: line
    INTEGER :: ios 
    CHARACTER(LEN=100) :: comando
    CHARACTER(LEN=100) :: datos
    CHARACTER(LEN=50) :: nombre, ubicacion
    INTEGER :: cantidad, start, end_pos
    INTEGER :: contador

    OPEN(UNIT=10, FILE=archivo, STATUS='OLD', ACTION='READ', IOSTAT=ios)
    IF (ios /= 0 ) THEN
        PRINT *, 'Error al abrir el archivo para lectura'
        STOP 
    END IF

    contador = 0
    ! Asegúrate de que el archivo esté en la posición correcta antes de comenzar a leer.
    REWIND(10)  ! Reinicia el puntero del archivo al principio
    DO    
        READ(10, '(A)', IOSTAT=ios) line
        IF (ios /= 0 ) EXIT
       
        start = 1
        end_pos = SCAN(line(start:), ' ')
        IF (end_pos == 0) THEN
            comando = TRIM(line(start:))
        ELSE 
            comando = TRIM(line(start:start+end_pos-2))
            datos = TRIM(line(start+end_pos:))
        END IF
        
        ! Verifica que contador no exceda los límites
        IF (contador >= MAX_INVENTARIO) THEN
            PRINT *, 'Límite de inventarios alcanzado'
            EXIT
        END IF

        IF (es_mov) THEN
            ! Procesar archivo .mov
            SELECT CASE (comando)
                CASE ('agregar_stock')
                    CALL parse_line(datos, inventarios(contador))
                    CALL agregar_stock(inventarios, contador, inventarios(contador)%nombre, inventarios(contador)%cantidad, inventarios(contador)%ubicacion)
                CASE ('eliminar_equipo')
                    CALL parse_line(datos, inventarios(contador))
                    CALL eliminar_equipo(inventarios, contador, inventarios(contador)%nombre, inventarios(contador)%cantidad, inventarios(contador)%ubicacion)
                CASE DEFAULT
                    PRINT *, 'Comando inválido en archivo .mov'
            END SELECT
        ELSE
            ! Procesar archivo .inv
            SELECT CASE (comando)
                CASE ('crear_equipo')
                    CALL parse_line(datos, inventarios(contador))
                    contador = contador + 1
                CASE DEFAULT
                    PRINT *, 'Comando inválido en archivo .inv'
            END SELECT
        END IF

    END DO
    CLOSE(UNIT=10)

END SUBROUTINE analizador

SUBROUTINE generar_informe(inventarios, num_inventario)
    USE InventarioMod, ONLY: Inventario
    TYPE(Inventario), DIMENSION(num_inventario), INTENT(IN) :: inventarios
    INTEGER, INTENT(IN) :: num_inventario
    INTEGER :: i
    REAL :: valor_total
    INTEGER :: ios

    ! Abrir el archivo de informe
    OPEN(UNIT=11, FILE='informe.txt', STATUS='REPLACE', ACTION='WRITE', IOSTAT=ios)
    IF (ios /= 0) THEN
        PRINT *, 'Error al abrir el archivo de informe'
        STOP
    END IF

    ! Encabezado
    WRITE(11, '(A)') 'Informe de Inventario:'
    WRITE(11, '(A)') '-----------------------------------------------------------------------------------------------'
    WRITE(11, '(A)') '      Equipo        Cantidad       Precio Unitario      Valor Total          Ubicación'
    WRITE(11, '(A)') '-----------------------------------------------------------------------------------------------'

    ! Impresión de datos del inventario
    DO i = 1, num_inventario
        valor_total = inventarios(i)%cantidad * inventarios(i)%precio_unitario

        ! Escribir los datos con el formato adecuado
        WRITE(11, '(A20, I10, F12.2, F12.2, A20)') &
                TRIM(inventarios(i)%nombre), &
                inventarios(i)%cantidad, &
                inventarios(i)%precio_unitario, &
                valor_total, &
                TRIM(inventarios(i)%ubicacion)
    END DO

    ! Cierre del archivo de informe
    CLOSE(UNIT=11)
END SUBROUTINE generar_informe



SUBROUTINE agregar_stock(inventarios, contador, nombre, cantidad, ubicacion)
    USE InventarioMod
    TYPE(Inventario), DIMENSION(:), INTENT(INOUT) :: inventarios
    INTEGER, INTENT(IN) :: contador
    CHARACTER(LEN=*), INTENT(IN) :: nombre, ubicacion
    INTEGER, INTENT(IN) :: cantidad
    INTEGER :: i
    LOGICAL :: encontrado

    encontrado = .FALSE.
    DO i = 1, contador
        IF (TRIM(inventarios(i)%nombre) == TRIM(nombre) .AND. TRIM(inventarios(i)%ubicacion) == TRIM(ubicacion)) THEN
            inventarios(i)%cantidad = inventarios(i)%cantidad + cantidad
            encontrado = .TRUE.
            EXIT
        END IF
    END DO

    IF (.NOT. encontrado) THEN
        PRINT *, 'Error: El equipo no existe en la ubicación especificada'
    END IF
END SUBROUTINE agregar_stock

SUBROUTINE eliminar_equipo(inventarios, contador, nombre, cantidad, ubicacion)
    USE InventarioMod
    TYPE(Inventario), DIMENSION(:), INTENT(INOUT) :: inventarios
    INTEGER, INTENT(IN) :: contador
    CHARACTER(LEN=*), INTENT(IN) :: nombre, ubicacion
    INTEGER, INTENT(IN) :: cantidad
    INTEGER :: i
    LOGICAL :: encontrado

    encontrado = .FALSE.
    DO i = 1, contador
        IF (TRIM(inventarios(i)%nombre) == TRIM(nombre) .AND. TRIM(inventarios(i)%ubicacion) == TRIM(ubicacion)) THEN
            IF (inventarios(i)%cantidad >= cantidad) THEN
                inventarios(i)%cantidad = inventarios(i)%cantidad - cantidad
                encontrado = .TRUE.
            ELSE
                PRINT *, 'Error: Cantidad a eliminar mayor que la existente en la ubicación'
            END IF
            EXIT
        END IF
    END DO

    IF (.NOT. encontrado) THEN
        PRINT *, 'Error: El equipo no existe en la ubicación especificada'
    END IF
END SUBROUTINE eliminar_equipo


END PROGRAM practica1
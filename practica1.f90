MODULE InventarioMod
    IMPLICIT NONE

    TYPE :: Inventario
        CHARACTER(LEN=100) :: nombre
        CHARACTER(LEN=100) :: ubicacion
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

                ! Imprimir los valores almacenados en inventarios
                DO i = 1, contador
                    PRINT *, 'Inventario', i, ':'
                    PRINT *, 'Nombre:', TRIM(inventarios(i)%nombre)
                    PRINT *, 'Cantidad:', inventarios(i)%cantidad
                    PRINT *, 'Precio Unitario:', inventarios(i)%precio_unitario
                    PRINT *, 'Ubicacion:', TRIM(inventarios(i)%ubicacion)
                    PRINT *, '--------------------------'
                END DO
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
    INTEGER :: start, end_pos
    CHARACTER(LEN=300) :: field(4)
    INTEGER :: temp_int
    REAL :: temp_real
    INTEGER :: ios
    CHARACTER(LEN=200) :: temp_line

    ! Asignar la línea leída a una variable temporal
    temp_line = TRIM(line)
    start = 1

    ! Extraer nombre
    end_pos = INDEX(temp_line, ';')
    IF (end_pos > 0) THEN
        field(1) = TRIM(temp_line(start:end_pos-1))
        PRINT *, 'Nombre extraído:', field(1)
        temp_line = TRIM(temp_line(end_pos+1:))
    ELSE
        PRINT *, 'Error: Línea no tiene el primer separador ; esperado'
        RETURN
    END IF

    ! Extraer cantidad
    end_pos = INDEX(temp_line, ';')
    IF (end_pos > 0) THEN
        field(2) = TRIM(temp_line(start:end_pos-1))
        PRINT *, 'Cantidad extraída:', field(2)
        temp_line = TRIM(temp_line(end_pos+1:))
    ELSE
        PRINT *, 'Error: Línea no tiene el segundo separador ; esperado'
        RETURN
    END IF

    ! Extraer precio unitario
    end_pos = INDEX(temp_line, ';')
    IF (end_pos > 0) THEN
        field(3) = TRIM(temp_line(start:end_pos-1))
        PRINT *, 'Precio Unitario extraído:', field(3)
        temp_line = TRIM(temp_line(end_pos+1:))
    ELSE
        PRINT *, 'Error: Línea no tiene el tercer separador ; esperado'
        RETURN
    END IF

    ! Extraer ubicación
    field(4) = TRIM(temp_line)
    PRINT *, 'Ubicación extraída:', field(4)

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
    CHARACTER(LEN=200) :: line  
    INTEGER :: ios
    CHARACTER(LEN=100) :: comando, datos, nombre, ubicacion
    INTEGER :: start, end_pos, cantidad
    INTEGER, INTENT(INOUT) :: contador

    ! Abrir el archivo para lectura
    OPEN(UNIT=10, FILE=archivo, STATUS='OLD', ACTION='READ', IOSTAT=ios)
    IF (ios /= 0) THEN
        PRINT *, 'Error al abrir el archivo para lectura'
        STOP 
    END IF

    ! Leer el archivo línea por línea
    DO
        READ(10, '(A)', IOSTAT=ios) line
        IF (ios /= 0) EXIT
        PRINT *, "Línea leída:", TRIM(line)

        ! Separar la línea en comando y datos
        start = 1
        end_pos = SCAN(line(start:), ' ')
        IF (end_pos == 0) THEN
            comando = TRIM(line(start:))
        ELSE
            comando = TRIM(line(start:start+end_pos-2))
            datos = TRIM(line(start+end_pos+1:))
        END IF

        ! Verifica que contador no exceda los límites
        IF (contador >= MAX_INVENTARIO) THEN
            PRINT *, 'Límite de inventarios alcanzado'
            EXIT
        END IF

        IF (.NOT. es_mov) THEN
            ! Procesar archivo .inv
            CALL parse_line(datos, inventarios(contador+1))
            contador = contador + 1

        ELSE
            ! Procesar archivo .mov
            IF (comando == 'agregar_stock') THEN
                CALL extraer_datos_mov(datos, nombre, cantidad, ubicacion)
                CALL agregar_stock(inventarios, contador, nombre, cantidad, ubicacion)

            ELSE IF (comando == 'eliminar_equipo') THEN
                CALL extraer_datos_mov(datos, nombre, cantidad, ubicacion)
                CALL eliminar_equipo(inventarios, contador, nombre, cantidad, ubicacion)
            END IF
        END IF
    END DO
    CLOSE(UNIT=10)
END SUBROUTINE analizador
SUBROUTINE extraer_datos_mov(datos, nombre, cantidad, ubicacion)
    CHARACTER(LEN=*), INTENT(IN) :: datos
    CHARACTER(LEN=100), INTENT(OUT) :: nombre, ubicacion
    INTEGER, INTENT(OUT) :: cantidad
    INTEGER :: end_pos, ios
    CHARACTER(LEN=200) :: datos_temp
    CHARACTER(LEN=20) :: cantidad_str

    ! Copiar datos a una variable temporal para manipulación
    datos_temp = datos

    ! Extraer nombre
    end_pos = INDEX(datos_temp, ';')
    IF (end_pos > 0) THEN
        nombre = TRIM(datos_temp(1:end_pos-1))
        datos_temp = TRIM(datos_temp(end_pos+1:))
    ELSE
        PRINT *, 'Error: Datos no tienen el primer separador ; esperado'
        RETURN
    END IF

    ! Extraer cantidad
    end_pos = INDEX(datos_temp, ';')
    IF (end_pos > 0) THEN
        cantidad_str = TRIM(datos_temp(1:end_pos-1))
        datos_temp = TRIM(datos_temp(end_pos+1:))

        ! Convertir la cadena a entero
        READ(cantidad_str, *, IOSTAT=ios) cantidad
        IF (ios /= 0) THEN
            PRINT *, 'Error en el formato de cantidad'
            RETURN
        END IF
    ELSE
        PRINT *, 'Error: Datos no tienen el segundo separador ; esperado'
        RETURN
    END IF

    ! Extraer ubicación
    ubicacion = TRIM(datos_temp)
END SUBROUTINE extraer_datos_mov
SUBROUTINE generar_informe(inventarios, num_inventario)
    USE InventarioMod
    IMPLICIT NONE
    TYPE(Inventario), DIMENSION(:), INTENT(IN) :: inventarios
    INTEGER, INTENT(IN) :: num_inventario
    INTEGER :: i
    CHARACTER(LEN=100) :: line
    CHARACTER(LEN=50) :: nombre_format
    REAL :: valor_total
    OPEN(UNIT=10, FILE='informe.txt', STATUS='REPLACE')

    WRITE(10, '(A)') "----------------------------------------------------"
    WRITE(10, '(A)') "Informe de Inventario:"
    WRITE(10, '(A)') "----------------------------------------------------"
    WRITE(10, '(A)') "Equipo           Cantidad  Precio Unitario  Valor Total   Ubicación"
    WRITE(10, '(A)') "----------------------------------------------------"

    DO i = 1, num_inventario
        ! Formatear el nombre del equipo correctamente
        nombre_format = TRIM(ADJUSTL(inventarios(i)%nombre))

        ! Calcular el valor total
        valor_total = inventarios(i)%cantidad * inventarios(i)%precio_unitario

       WRITE(line, '(A15, I10, F10.2, F10.2, A15)') nombre_format, &
                                             inventarios(i)%cantidad,    &
                                             inventarios(i)%precio_unitario, &
                                             valor_total, &
                                             TRIM(ADJUSTL(inventarios(i)%ubicacion))
        WRITE(10, '(A)') TRIM(line)
    END DO

    CLOSE(10)
END SUBROUTINE generar_informe


SUBROUTINE agregar_stock(inventarios, contador, nombre, cantidad, ubicacion)
    USE InventarioMod
    TYPE(Inventario), DIMENSION(:), INTENT(INOUT) :: inventarios
    INTEGER, INTENT(INOUT) :: contador
    CHARACTER(LEN=*), INTENT(IN) :: nombre, ubicacion
    INTEGER, INTENT(IN) :: cantidad
    LOGICAL :: encontrado
    INTEGER :: i

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
    INTEGER, INTENT(INOUT) :: contador
    CHARACTER(LEN=*), INTENT(IN) :: nombre, ubicacion
    INTEGER, INTENT(IN) :: cantidad
    LOGICAL :: encontrado
    INTEGER :: i

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
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
                CALL analizador(filename)
                print *, 'inventario cargado'
            CASE (2)
                PRINT *, 'Escriba la Ruta'
                READ *, filename
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


    SUBROUTINE analizador(archivo)
        USE InventarioMod, ONLY: Inventario
        CHARACTER(LEN=100) :: archivo
        CHARACTER(LEN=100) :: line
        INTEGER :: ios 
        CHARACTER(LEN=100) :: comando
        CHARACTER(LEN=100) :: datos
        INTEGER :: i, start, end_pos
        INTEGER :: contador


        OPEN(UNIT=10, File=archivo, STATUS='OLD', ACTION='READ', IOSTAT=ios)
        IF (ios /= 0 ) THEN
            PRINT *, 'Error al abrir el archivo para lectura'
            STOP 
        END IF
        contador = 0
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

            SELECT CASE (comando)
                CASE ('crear_equipo')
                    PRINT *, ""
                    print *, "-----------------------------------"
                    PRINT *, 'Crear Equipo'
                    CALL parse_line(datos, inventarios(contador))
                    PRINT *, 'equipo: ', TRIM(inventarios(contador)%nombre)
                    PRINT *, 'cantidad: ', inventarios(contador)%cantidad
                    PRINT *, 'precio unitario: ', inventarios(contador)%precio_unitario
                    PRINT *, 'ubicacion: ', TRIM(inventarios(contador)%ubicacion)
                    print *, "-----------------------------------"
                    
                    contador = contador + 1

                
                CASE ('agregar_stock')
                    PRINT *, 'Agregar Stock'
                    ! CALL parse_line(datos)
                CASE ('eliminar_stock')
                    PRINT *, 'Eliminar Stock'
                    print *, datos
                CASE DEFAULT
                    PRINT *, 'Comando invalido'
            END SELECT


           

        END DO
        CLOSE(UNIT=10)

    END SUBROUTINE analizador

SUBROUTINE generar_informe(inventarios, num_inventario)
    USE InventarioMod, ONLY: Inventario
    TYPE(Inventario), DIMENSION(num_inventario), INTENT(IN) :: inventarios
    INTEGER, INTENT(IN) :: num_inventario
    INTEGER :: i
    REAL :: valor_total
    CHARACTER(LEN=100) :: filename
    INTEGER :: ios
    OPEN(UNIT=11, FILE='informe.txt', STATUS='REPLACE', ACTION='WRITE', IOSTAT=ios)
    IF (ios /= 0) THEN
        PRINT *, 'Error al abrir el archivo de informe'
        STOP
    END IF

    WRITE(11, *) 'Equipo', 'Cantidad', 'Precio Unitario', 'Valor Total', 'Ubicacion'
    DO i = 1, num_inventario
        valor_total = inventarios(i)%cantidad * inventarios(i)%precio_unitario
        WRITE(11, '(A, I5, F10.2, F10.2, A)') inventarios(i)%nombre, inventarios(i)%cantidad, inventarios(i)%precio_unitario, valor_total, inventarios(i)%ubicacion
    END DO

    CLOSE(UNIT=11)
END SUBROUTINE generar_informe


END PROGRAM practica1
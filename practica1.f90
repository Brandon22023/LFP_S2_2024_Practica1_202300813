program practica
    implicit none
    integer :: opcion
    do
    print *, ""
    print *, "----------------------------------------------------"
    print *, "Practica 1 - Lenguajes formales y de programacion"
    print *, "----------------------------------------------------"
    print *, "#sistema de inventario"
    print *, ""
    print *, "1. Cargar Investario Inicial"
    print *, "2. Cargar Instrucciones de Movimiento"
    print *, "3. Crear Informe de Inventario"
    print *, "4. Salir"
    print *, ""
    print *, "Ingrese una opcion:"
    print *, ""
    read *, opcion
    select case(opcion)
        case(1)
        case(2)
        case(3)
        case(4)
            exit  
        case default
            print *, "Opcion no valida, favor de elegir una opcion valida"
    end select
    end do
end program practica
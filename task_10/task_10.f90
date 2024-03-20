program task_10
    implicit none
    integer :: n, m, id
    character (len = 20) :: arquive
    logical :: arquive_exist
    real, dimension(:,:), allocatable :: x

    print*, "Leitura e escrita em arquivo de dados:"
    print*, "-----Usar no cabeçalho ## ... *------"
    print*, "-----Definindo número de colunas-----"
    print*, ""

    arquive = "force.dat"
    print*, arquive

    inquire(file = arquive, exist = arquive_exist)

        if(arquive_exist) then
            id = 12
            
            call lines(n, m, id, arquive)

            allocate (x(n,m))

            print 12, n, m 

            call matrix(n, m, id, x)

            call max_values(n, m, x)
        else
            print*, "Arquivo não encontrado, não será criado"
        end if
    
    12 format("Linha = ",I3," ;Coluna = ",I3)
end program task_10

subroutine matrix(n, m, id, x)
    implicit none
    integer :: i, j, n, m, id
    real, dimension(n,m) :: x

    read(id,*) ((x(i,j), j = 1, m), i = 1, n)
    write(*, 15) ((x(i,j), j = 1, m), i = 1, n)

    15 format(3(F12.3))
end subroutine matrix

subroutine max_values(n, m, x)
    implicit none
    integer :: j, n, m
    real, dimension(n, m) :: x
    real, dimension(m) :: max 
    
        do j = 1, m
            max(j) = maxval(x(1 : n, j))
        end do
    
    print*, "Máximos:"
    write(*,"(3(F12.3))") max 
    12 format(3(F12.3))
end subroutine max_values

subroutine lines(n, m, id, arquive)
    implicit none
    integer, parameter :: max_col = 30
    integer :: j, n, m, stat, id
    character (len = 3), dimension(1, max_col) :: x
    character (len = 20) :: arquive

    open(id, file = arquive, form = "formatted")

    n = 0
    read(id, *)
        do 
            read(id, * , iostat = stat)
            if (stat /= 0) exit 
            n = n + 1
        end do
    rewind(id)

    read(id, * , iostat = stat) (x(1, j), j = 1, max_col)
    m = 1
        
        do 
            if (x(1, m) == '*') exit 
            m = m + 1
        end do
    rewind(id)
    read (id, *)

    return
end subroutine lines
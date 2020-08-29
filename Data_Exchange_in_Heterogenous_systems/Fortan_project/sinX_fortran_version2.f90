program sinX_fortran
    implicit none

    double precision, dimension(100) :: theta, sin_theta 
    integer:: i

    open(1, file = 'theta_values.dat', status = 'old')
    open(2, file = 'sin_theta_values.csv', status='new')  

    do i = 1,100  
        read(1,*) theta(i)
        ! sin_theta(i) = theta(i) + 1

        ! version 1
        ! sin_theta(i) = SineApprox(theta(i) , 3 ) 
        
        !version 2   
        sin_theta(i) = SineApprox(theta(i)) 
     end do 
     
     close(1) 
     
     do i = 1,100  
        write(2,*) i,",",sin_theta(i)
     end do

    close(2)

contains
    function SineApprox(x) result(func)
        implicit none
        integer, parameter :: wp = selected_real_kind(15,300)
        real(wp), parameter :: pi = atan2(0._wp,-1._wp)
        real(wp), parameter :: ten = 1.e-15_wp
     
        integer :: N, i, j
        real(wp) :: serie, x, xrad
        real(wp) :: xtest, senx, func, fat
     
        ! -- Initialization
        ! write(*,*) "Choose an angle in degrees"
        ! read(*,*) x
     
        senx = 0._wp
        xrad = (x*pi)/180._wp
        xtest = (x*pi)/180_wp
     
        func = sin(xtest)
     
        ! -- Main loop
        i = 0
        serie = huge(serie)
        do while (abs(serie) .ge. ten)
     
           ! -- Compute factorial
           fat = 1._wp
           do j=1,2*i+1
              fat = fat*j
           enddo
     
           ! -- Evaluate i-th term in series
           N = (2*i)+1
           serie = (((-1)**i)*(xrad**N))/fat
           senx = senx + serie
           i = i + 1
     
           write(*,*) 'Completed iteration ', i
           write(*,*) 'serie, senx: ', serie, senx
        enddo
     
        write(*,*) 'Completed while loop'
     
        write(*,*) 'senx: ', senx
        write (*,*) 'i: ', i
        write(*,*) 'func: ', func
     
     end function 


end program sinX_fortran
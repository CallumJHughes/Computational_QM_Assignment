program main
  implicit none
  integer, parameter :: dp=selected_real_kind(15,300)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!! DEFINE OBJECTS !!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  integer :: l, n
  real (kind=dp) :: Energy, Potential, radius,  delta, R0

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!! MAIN PROGRAMME !!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !Energy = 8
  radius = 1.0
  R0 = 2.0
  n = 1

  print *, R(radius,R0,n)

  !print *, Chi(Energy,radius)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!! FUNCTIONS AND SUBROUTINES !!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
contains
  function Chi(E,r)
    !!! Function to calculate the value for Chi (What is chi) !!!
    real (kind=dp) :: Chi, E, r

    Chi = sin(sqrt(2*E)*r)
  end function

  function R(radius,R0,n)
    !!! Function to to define trial wavefunction !!!
    real (kind=dp) :: R, R0,  radius
    integer :: n

    if (radius .LE. R0) then
      R = 1 - (radius/R0)**n
    else
      R = 0
    end if
  end function

end program

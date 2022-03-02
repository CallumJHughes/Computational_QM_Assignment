program main
  implicit none
  integer, parameter :: dp=selected_real_kind(15,300)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!! DEFINE OBJECTS !!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  integer :: l, n
  real (kind=dp) :: Energy, Potential, radius,  delta

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!! MAIN PROGRAMME !!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !Energy = 8
  radius = 1.0
  n = 1

  print *, R(radius,n)

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

  function R(radius,n)
    !!! Function to to define trial wavefunction !!!
    real (kind=dp), parameter :: R0 = 3.0
    real (kind=dp) :: R, radius
    integer :: n

    if (radius .LE. R0) then
      R = 1 - (radius/R0)**n
    else
      R = 0
    end if
  end function

end program

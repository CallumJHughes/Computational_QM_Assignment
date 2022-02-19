program main
  implicit none
  integer, parameter :: dp=selected_real_kind(15,300)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!! DEFINE OBJECTS !!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  integer :: l
  real (kind=dp) :: Energy, Potential, radius,  delta

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!! MAIN PROGRAMME !!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  Energy = 8
  radius = 2

  print *, Chi(Energy,radius)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!! FUNCTIONS AND SUBROUTINES !!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
contains
  function Chi(E,r)
    !!! Function to calculate the value for Chi (What is chi) !!!
    real (kind=dp) :: Chi, E, r

    Chi = sin(sqrt(2*E)*r)
  end function

end program

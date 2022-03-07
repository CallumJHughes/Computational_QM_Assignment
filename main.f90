program main
  implicit none
  integer, parameter :: dp=selected_real_kind(15,300)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!! DEFINE OBJECTS !!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  integer :: angMomentum, n
  real (kind=dp) :: energy, Potential, radius,  deltaL, R0
  real (kind=dp) :: pi = atan(1.0) * 4.0

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!! MAIN PROGRAMME !!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  energy = 8
  radius = 1.0
  R0 = 2.0
  n = 1
  angMomentum = 1

  print *, R(radius,R0,n)

  call CalculatePhaseShift

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!! FUNCTIONS AND SUBROUTINES !!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
contains
  function Chi(R,radius)
    !!! Function to calculate the value for Chi (What is chi) !!!
    real (kind=dp) :: Chi, radius, R

    Chi = R * radius
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

  function Wavenumber(energy)
    !!! Function to calculate wavenumber k !!!
    real (kind=dp) :: Wavenumber, energy

    wavenumber = sqrt(energy * 2)
  end function

  subroutine CalculatePhaseShift
    !!! Suboroutine to calculate the phase shift !!!
    deltaL = asin(radius*R(radius,R0,n)) + (angMomentum*(pi/2)) - (Wavenumber(energy)*radius)

    print *, 'Phase shift is: ', deltaL
  end subroutine

end program

!!!!!!!!!!!!!
!!! NOTES !!!
!!!!!!!!!!!!!
! – Chi used in CalculatePhaseShift might not be correct; rest should be okay

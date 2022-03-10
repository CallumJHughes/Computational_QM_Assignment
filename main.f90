program main
  implicit none
  integer, parameter :: dp=selected_real_kind(15,300)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!! DEFINE OBJECTS !!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  integer :: angMomentum, n, intenergy
  real (kind=dp) :: energy, Potential, radius,  deltaL, R0, totalPsi, sigma
  real (kind=dp) :: pi = atan(1.0) * 4.0

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!! MAIN PROGRAMME !!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !energy = 8 ! Energy of incoming wave
  radius = 1.0
  R0 = 2.0
  n = 1
  angMomentum = 1 ! Defines value for angular momentum of wave

  print *, R(radius,R0,n)

  print *, 'Bob' ! Test

  do intenergy=0,2
    print *, intenergy
    energy = real(intenergy) / 10 ! Converts integer energy value to real value and a 1/10th
    print *, energy
    call CalculatePhaseShift
    call CalcTotCrossSection
  end do

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!! FUNCTIONS !!!!!!!!!!!!!!
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
    !!! Function to calculate wavenumber k of outgoing wave !!!
    !!! This is also wavenumber of incoming wave since we assume an elastic collision !!!
    real (kind=dp) :: Wavenumber, energy

    wavenumber = sqrt(energy * 2)
  end function

  function Psi(Chi,radius)
    !!! Calculate wavefunction for current radial function (chi) of current angular momentum !!!
    real (kind=dp) :: Psi, Chi, radius

    Psi = Chi(R,radius) / radius
  end function

  function AngMomentumMax(angMomentum)
    !!! Calculates maximum angular momentum value !!!
    !!! Any particles with higher angular momentum will just pass through unaffected so are ignored !!!
    !!! (Solve quadratic lmax**2 + lmax - (krmax)**2 = 0???)
    integer :: AngMomentumMax, angMomentum

    AngMomentumMax = angMomentum ! Just for testing
  end function

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!! SUBROUTINES !!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine CalculatePhaseShift
    !!! Suboroutine to calculate the phase shift !!!
    deltaL = asin(radius*R(radius,R0,n)) + (angMomentum*(pi/2)) - (Wavenumber(energy)*radius)

    print *, 'Phase shift is: ', deltaL
  end subroutine

  subroutine CalculateTotalWavefunction
    !!! Calculates total wavefunction by numerically chi over all possible angular momentums !!!
    !!! This gives solution to Schroedinger Equation !!!
    !!! Constants including Normalisation constants are absorbed and set to 1 !!!
    do angMomentum = 0, AngMomentumMax(angMomentum)
      totalPsi = totalPsi + Psi(Chi,radius)
    end do
  end subroutine

  subroutine CalcTotCrossSection
    !!! Subroutine to calculate the total scattering cross section !!!
    sigma = 0

    do angMomentum = 0, angMomentumMax(angMomentum)
      sigma = sigma + ((2*angMomentum + 1)*(sin(deltaL))**2)
    end do

    sigma = sigma * ((4*pi)/(Wavenumber(energy))**2)
  end subroutine

end program

!!!!!!!!!!!!!
!!! NOTES !!!
!!!!!!!!!!!!!
! – Chi used in CalculatePhaseShift might not be correct; rest should be okay
! – Use subroutine to check if lmax blah blah is approx. krmax using while loop

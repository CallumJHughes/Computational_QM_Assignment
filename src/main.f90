program main
!======================================!
! Written by Callum Hughes, 19/02/2022 !
!======================================!

  use constants
  use parameters
  implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!! DEFINE OBJECTS !!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  integer :: l, lmax, Estep, step
  real (kind=dp) :: r, Chi, Z, Veff, r1, r2, sigma, E
  real (kind=dp), dimension(:), allocatable :: rData, ChiData, VData, VeffData, ChiUnscatData, DeltaData

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!! MAIN PROGRAMME !!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  call CalcMaxAngMom
  print *, 'Maximum Angular Momentum:', lmax
  allocate(DeltaData(0:lmax)) ! Allocates DeltaData to start at 0, since l starts at 0

  do Estep = 1, 10
    E = real(Estep) / 10
    do l = 0, lmax
      print *, '----------------------------------------------'
      print *, 'Current Energy:', E
      print *, 'Current Angular Momentum:', l
      call AllocateArrays
      call Solve
      call WriteData
      call FindR1
      call FindR2
      call DeallocateArrays
      DeltaData(l) = PhaseShift(r1,r2,pi)
      print *, 'Phase Shift:', PhaseShift(r1,r2,pi)
    end do
    call CalcCrossSection
  end do

contains
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!! FUNCTIONS !!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  function V(r)
    !!! Function to define the potential !!!
    real (kind=dp) :: V, r

    if (r < Rv) then
      V = V0
    else
      V = 0
    end if
  end function

  function f(r,E)
    !!! Function to be integrated !!!
    real (kind=dp) :: f, r, E

    f = -2.0*(E-V(r)-(l*(l+1)/(2*(r**2))))
  end function

  function ChiUnscat(E,r)
    !!! Function to calulate the wave of the unscattered particle !!!
    real (kind=dp) :: ChiUnscat, E, r

    ChiUnscat = sin(sqrt(2*E)*r)
  end function

  function PhaseShift(r1,r2,pi)
    !!! Function to calculate the phase shift between unscattered and scattered particles !!!
    !!! Returns value between -pi/2 < r < pi/2 as required !!!
    real (kind=dp) :: PhaseShift, r1, r2, pi

    if ((r2-r1) .GT. (pi/2)) then
      PhaseShift = (r2 - r1) - (pi/2)
    else if ((r2-r1) .LT. -(pi/2)) then
      PhaseShift = (r2 - r1) + (pi/2)
    else
      PhaseShift = r2 - r1
    end if
  end function

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!! SUBROUTINES !!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine CalcMaxAngMom
    !!! Subroutine to calculate the maximum angular momentum that will 'feel' the potential !!!
    integer :: l

    do l = 0, 20 ! Loops across large number of angular momentums
      if (int(sqrt(real(l)*(real(l)+1))) .EQ. (int(sqrt(2*E)*Rv))) then ! Tests value of l against k*rmax
        lmax = l ! Assigns value of l if maximum angular momentum is found
      else
        continue
      end if
    end do
  end subroutine

  subroutine AllocateArrays
    !!! Subroutine to allocate most of the arrays to their needed sizes !!!
    allocate(rData(nint(Rmax/h)))
    allocate(ChiData(nint(Rmax/h)))
    allocate(VData(nint(Rmax/h)))
    allocate(VeffData(nint(Rmax/h)))
    allocate(ChiUnscatData(nint(Rmax/h)))
  end subroutine

  subroutine Solve
    !!! Subroutine to perform integration steps using Euler's method !!!
    real (kind=dp) :: rnew, Chinew, Znew

    ! Boundary conditions
    r = h
    Chi = r**(l+1)
    Z = (l+1)*(r**l)
    Veff = V(r)+(l*(l+1)/(2*(r**2)))

    ! Assigns boundary conditions to first element of their respective arrays
    rData(1) = r
    ChiData(1) = Chi
    VData(1) = V(r)
    VeffData(1) = Veff

    step = 1 ! Reset step value

    ! Integration
    do while (r < Rmax)
      ChiUnscatData(step) = ChiUnscat(E,r)
      rnew=r + h
      Chinew=Chi+Z*h
      Znew = Z + f(r,E)*Chi*h
      r = rnew
      Chi = Chinew
      Z = Znew
      Veff=V(r)+(l*(l+1)/(2*(r**2)))

      rData(step) = r
      ChiData(step) = Chi
      VData(step) = V(r)
      VeffData(step) = Veff
      
      step = step + 1
    end do
  end subroutine

  subroutine WriteData
    !!! Subroutine to write the necessary data to external data files !!!
    character (len=50) :: Elabel

    write(Elabel,*) Estep

    open(unit=1, file="ChiData"//trim(adjustl(Elabel))//".dat")
    open(unit=2, file="VData"//trim(adjustl(Elabel))//".dat")
    open(unit=3, file="VeffData"//trim(adjustl(Elabel))//".dat")
    open(unit=4, file="ChiUnscatData"//trim(adjustl(Elabel))//".dat")

    do step = 1, nint(Rmax/h)
      write(1,*) Rdata(step), ChiData(step)
      write(2,*) Rdata(step), VData(step)
      write(3,*) Rdata(step), VeffData(step)
      write(4,*) Rdata(step), ChiUnscatData(step)
    end do

    close(1)
    close(2)
    close(3)
    close(4)
  end subroutine

  subroutine FindR1
    !!! Subroutine to find the r value for when the scattered wave cross the x-axis !!!
    logical :: check
    integer :: step
    real (kind=dp) :: x, y, a, b

    r1 = 0

    do step = int((Rmax-10)/h), int((Rmax/h)-1)
      a = 1.0
      b = ChiData(step) ! Data from scattered wave
      x = sign(a,b) ! Returns a with sign of b

      b = ChiData(step+1) ! Next data point from scattered wave
      y = sign(a,b) ! Returns a with sign of b

      if (x .LT. 0 .AND. y .GT. 0) then ! Checks if there is a sign difference between x and y (-x and +y)
        if (r1 .EQ. 0) then ! Checks if r1 has been allocated a value yet
          r1 = step * h
          print *, 'r1: ', r1
        else
          continue
        end if
      else if (x .GT. 0 .AND. y .LT. 0) then ! Checks if there is a sign difference between x and y (+x and -y)
        if (r1 .EQ. 0) then ! Checks if r1 has been allocated a value yet
          r1 = step * h
          print *, 'r1: ', r1
        else
          continue
        end if
      else
        continue
      end if
    end do
  end subroutine

  subroutine FindR2
    !!! Subroutine to find the r value for when the unscattered wave cross the x-axis !!!
    logical :: check
    integer :: step
    real (kind=dp) :: x, y, a, b

    r2 = 0

    do step = nint((Rmax-5)/h), nint((Rmax/h)-1) ! 
      a = 1.0
      b = ChiUnscatData(step) ! Data from scattered wave
      x = sign(a,b) ! Returns a with sign of b

      b = ChiUnscatData(step+1) ! Next data point from scattered wave
      y = sign(a,b) ! Returns a with sign of b

      if (x .LT. 0 .AND. y .GT. 0) then ! Checks if there is a sign difference between x and y (-x and +y)
        if (r2 .EQ. 0) then ! Checks if r1 has been allocated a value yet
          r2 = step * h
          print *, 'r2: ', r2
        else
          continue
        end if
      else if (x .GT. 0 .AND. y .LT. 0) then ! Checks if there is a sign difference between x and y (+x and -y)
        if (r2 .EQ. 0) then ! Checks if r1 has been allocated a value yet
          r2 = step * h
          print *, 'r2: ', r2
        else
          continue
        end if
      else
        continue
      end if
    end do
  end subroutine

  subroutine CalcCrossSection
    !!! Subroutine to Calculate the total scattering cross-section for a given energy !!!
    sigma = 0 ! Initial value

    ! Finds sigma by integrating across all possible angular momentums
    do l = 0, lmax
      sigma = sigma + ((2*l)+1)*(sin(DeltaData(l))**2)
    end do

    sigma = sigma * ((4*pi)/(2*E)) ! Multiplies integral by coefficient
    print *, '----------------------------------------------'
    print *, 'Total Scattering Cross-Section for Energy of',E,':', sigma
  end subroutine

  subroutine DeallocateArrays
    !!! Subroutine to deallocate arrays as needed for next step in loop !!!
    deallocate(rData)
    deallocate(ChiData)
    deallocate(VData)
    deallocate(VeffData)
    deallocate(ChiUnscatData)
  end subroutine
end program main

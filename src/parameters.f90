module parameters
	use constants
	implicit none

	real(kind=dp), parameter :: h  = 0.001 ! Step size for integration
	real(kind=dp), parameter :: V0 = -1.0  ! Depth of potential well
	real(kind=dp), parameter :: Rv = 2.0   ! Width of potential

	integer, parameter :: Rmax = 40  ! Maximum value for integration
end module

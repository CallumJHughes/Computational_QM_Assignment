module parameters
	use constants
	implicit none

	real(kind=dp) :: h, V0, E, Rv
	integer       :: Rmax, step

	h    = 0.001 ! Step size for integration
	Rmax = 40    ! Maximum value for integration
	V0   = -1.0  ! Depth of potential well
	step = 1     ! Number of steps used in certain parameters
	Rv   = 2.0   ! Width of potential
	E    = 1.0   ! Energy value used to calculate maximum angular momentum

end module

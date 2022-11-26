module constants
	implicit none

	integer, parameter       :: dp=selected_real_kind(15,300) ! Defines double precision
	real(kind=dp), parameter :: pi = 4*atan(1.0_dp) ! Defines value of pi
end module

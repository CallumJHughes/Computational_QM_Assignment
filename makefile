# Define directory
src = src
bin = bin

# Define variables
const = $(src)/constants.f90
params = $(src)/parameters.f90
main = $(src)/main.f90

# Compiler
f90comp = gfortran

# Flags
flagsFast = -O3 -march=native
flagsDebug = -Wall -fcheck=all
flags = $(flagsFast) # Uses flags to optimise performance

running: $(bin) compile exec clean

# Make bin directory
$(bin):
	mkdir $(bin)

# Compile programs
compile:
	$(f90comp) -c $(const) -o $(bin)/c.o $(flags)
	$(f90comp) -c $(params) -o $(bin)/p.o $(flags)
	#$(f90comp) -c $(main) $(flags)

# Execute
exec:
	$(f90comp) $(bin)/c.o $(bin)/p.o $(main) -o $(bin)/main $(flags)

# Clean
clean:
	rm -f *.mod
	rm -f $(bin)/*.o

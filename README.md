# Computational Quantum Mechanics Assignment

## Run Instructions

*This program was written, compiled, and run on MacOS Monterey Version 12.2.1 in a 'zsh' shell. Although bash was not tested, 'zsh' is built from 'bash' so it should still work.*

To compile, the following line needs to be run in the terminal:

'''
$ make
'''

In order to run the program, the script 'run.sh' needs to be executed. This is done by first moving into the 'bin' directory, then executing the following line :

'''
$ ./run.sh
'''

The output data files are found in the 'data' directory. It should be noted that with each run the data files will be overwritten and so it is up to the user to save these files in another directory. 

## Output

The program 'main' will print the following to the standard output.

*Only the first section for the first energy is shown below. This output is shown for each given energy.*

'''
 Maximum Angular Momentum:           2
 ----------------------------------------------
 Current Energy:  0.10000000149011612
 Current Angular Momentum:           0
 r1:    30.217001435230486
 r2:    35.124001668300480
 Phase Shift:   3.3362038625637069
 ----------------------------------------------
 Current Energy:  0.10000000149011612
 Current Angular Momentum:           1
 r1:    35.647001693141647
 r2:    35.124001668300480
 Phase Shift: -0.52300002484116703
 ----------------------------------------------
 Current Energy:  0.10000000149011612
 Current Angular Momentum:           2
 r1:    34.681001647259109
 r2:    35.124001668300480
 Phase Shift:  0.44300002104137093
 ----------------------------------------------
 Total Scattering Cross-Section for Energy of  0.10000000149011612      :   107.10032811549108
'''

At the top, the maximum angular momentum allowed for the current system is showed. Within each mini-section, the current energy, angular momentum, r1, r2, and phase
shift is written to the standard output. r1 represents the radius at which the scattered wave crosses the x-axis and r2 represents the radius at which the unscattered 
wave crosses the x-axis. The total scattering cross-section of the scattering for a given energy is also written to the standard output.

Several files are also created, e.g. 'ChiData2.cat', where the name represents from which array the data was taken from, alonside the energy at which the data was 
collected. For 'ChiData2.dat', the data was collected from the array 'ChiData' at an energy of '0.2' (divide the number by 10 to get the energy of that particle).

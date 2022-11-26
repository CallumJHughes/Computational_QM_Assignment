#!/bin/bash

# clear data directory
rm ../data/*.dat

# Run main program
./main

# Move data files to data directory
mv *.dat ../data

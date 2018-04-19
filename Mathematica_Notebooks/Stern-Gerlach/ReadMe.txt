Stern-Gerlach Experiment Notebooks
==================================

All notebooks are for Mathematica v.6 or later.

Magnet Field Calculator.nb
--------------------------
Lets you calculate the calibrated magnet field strength from the magnet current. Uses an interpolation of selected points on the magnet calibration curve plot shown in the lab notes for experiment 33.

Zmax Calculator.nb
------------------
Calculates the ideal Zmax, given a magnetic field strength and oven temperature. Uses the formulas derived in the experiment 33 lab notes appendices A and B.

Z Distribution Calculator.nb
----------------------------
Calculates the Z distribution function given a value for Zmax and a 0-field beam width. This notebook must find the file 'ConvolvedZdistribution.dat' in the same directory from which it was loaded into Mathematica. Uses the formulas derived in the experiment 33 lab notes appendix B.

ConvolvedZdistribution.dat
--------------------------
Compressed data file used by 'Z Distribution Calculator.nb'. Make sure you copy this file into the same directory to which you copy 'Z Distribution Calculator.nb'.
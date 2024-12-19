# TvGP code


This repository contains code to support the paper “Tensor-variate
Gaussian process regression for efficient emulation of complex systems:
comparing regressor and covariance structures in outer product and
parallel partial emulators” by Semochkina[^1], Jackson[^2] and
Woods[^1].

The code is structured as follows:

- each of the folders “influenza_model” and “environmental_model”
  contains code to generate example results from the paper for the
  respective simulators;
- within each of these folders, there is an “\*\_emulation.R” script
  that runs the emulation and produces the results;
- the “code” folder in the parent directory contains common functions;
- the “code” folders for each model contain specific functions used in
  the emulation scripts;
- the “data” folders contain the data used in the emulation scripts.

[^1]: University of Southampton, Southampton, UK

[^2]: Durham University, Durham, UK

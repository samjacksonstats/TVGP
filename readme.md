# TvGP code


This repository contains code to support the paper “ensor-variate
Gaussian process regression for efficient emulation of complex systems:
comparing regressor and covariance structures in outer product and
parallel partial emulators” by Semochkina[^1], Jackson[^2] and
Woods[^3].

The code is structured as follows:

- each of the folders “influenza_model” and “environmental_model”
  contains code to generate example results from the paper for the
  respective simulators.
- within each of these folders, there is an “\*\_emulation.R” script
  that runs the emulation and produces the results.
- the “code” folders contain the functions (and data) used in the
  emulation scripts.

[^1]: University of Southampton, Southampton, UK

[^2]: Durham University, Durham, UK

[^3]: University of Southampton, Southampton, UK
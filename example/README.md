## READ ME

After unzipping *example.zip,* open the *example* directory. The *example* directory contains a single file *example.R*.

Installing the `dcme` package requires first installing the `mlr3superlearner` package from GitHub. Installing an R package from GitHub first requires installing the `devtools` package from CRAN. Lines 1-6 of *example.R* install `devtools`, and then subsequently installs `mlr3superlearner` and then `dcme`.

Running lines 8-21 of *example.R* will generate simulated data that can be used to estimate double-complier mediated effects. The object `n` controls the sample size of the simulated data.

Running line 23 of *example.R* will, using the simulated data, estimate the above mediated effects. For information on changing the call to `dcme()` from it's default parameters, run `?dcme` in the console.

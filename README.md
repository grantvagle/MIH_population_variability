# MIH_population_variability


## Files

`sims_functions_file.R`
This file contains all of the functions required for running the simulations, including all three scenarios (Main sims, non-consecutive sampling, and random fixed relationship). Note, many of the functions are shared among simulation scenarios, and this is noted in comments in the file.


`sims_running_file.R` 
This file loads necessary packages, sources functions from the `sims_functions_file.R`, creates a data frame of sites with the desired relationships, and sets parameter values. Two sets of parameters are present. First, uncommented is a small parameter set for test runs of the code. Second, commented, is the full parameter set used for publication (this takes a long time to run). Then follows the code for executing the simulations, for the main sims, non-consecutive sampling sims, and random fixed relationship sims, in order. Additional or manipulated parameters for the different simulation scenarios are present in their respective sections. After executing the simulations, results from each are saved in a `.csv` file.


`sims_plotting_file.R`
This file loads the results `.csv` files, manipulates the data using the `dplyr` package, and uses `ggplot2` to create plots. Detected slopes, proportion significant, and median $R^2$ plots are produced for each of the three simulation scenarios.


### System requirements

These simulations require [R version 3.5](https://www.R-project.org/) or greater. Additionally, the following packages are required: [dplyr](https://CRAN.R-project.org/package=dplyr), [reshape2](http://www.jstatsoft.org/v21/i12/), [mobsim](https://CRAN.R-project.org/package=mobsim), as well as [ggplot2](https://ggplot2.tidyverse.org) for plotting (full citations below).

While it is possible to run these simulations on a laptop or desktop, the compute resources and time to completion make the use of a cluster computer more convenient. Expect several hours of compute time for 1000 simulations of each parameter set. I only suggest executing test runs (~10 simulations per parameter set) on a laptop, which should take 30 seconds to a few minutes to complete.

#### Basic running instructions
After installing all necessary packages (above), ensure that your working directory is set to the folder containing all three `.R` scripts. Open `sims_running_file.R` and run the script line-by-line. Results will be saved as ` .csv` files in the working directory. The `sims_running_file.R` script is set up with a small parameter set and only 10 simulations per set for ease of computing. The full parameter set run for publication is present in commented form. 

To visualize results, open `sims_plotting_file.R` and run line-by-line, viewing the plots. This script reads from the `.csv` results files created by `sims_running_file.R`. 


## Literature Cited
May, F. (2017). mobsim: Spatial Simulation and Scale-Dependent Analysis of Biodiversity Changes. R package version 0.1.0. https://CRAN.R-project.org/package=mobsim

R Core Team (2018). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. https://www.R-project.org/.

Wickham, H. (2007). Reshaping Data with the reshape Package. Journal of Statistical Software, 21(12), 1-20. http://www.jstatsoft.org/v21/i12/.

Wickham, H. (2016). ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York.

Wickham, H., R. François, L. Henry and K. Müller (2019). dplyr: A Grammar of Data Manipulation. R package version 0.8.0.1. https://CRAN.R-project.org/package=dplyr


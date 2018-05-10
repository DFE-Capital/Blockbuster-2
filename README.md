 This package represents the most current version of the Blockbuster model. Code optimisation resulted in a completely rewritten code base so it is appropriate to initiate a new github page here. The original version can be found at: (https://github.com/DFE-Capital/Blockbuster)

  # Blockbuster2

*This is a prototype package being developed in anticipation of analysing the [Condition Data Collection](https://www.gov.uk/guidance/condition-data-collection-programme-information-and-guidance) and is subject to constant development*

This R package allows you to simulate the deterioration of School buildings through time using a Discrete Time Markov Chain. R packages are an ideal way to package and distribute R code and data for re-use by others.

Data on the condition of the school estate were collected during the Property Data Survey Programme (PDSP) of 2012-2014. Approximately 2.7 million rows of data were collected. This provides the initial state of the School Estate at timestep zero. The deterioration of the School Estate is then modelled by using deterioration rates associated with each construction element / sub-element / construction-type. Cost of repairs for these building components are estimated. An investment profile for rebuilding and maintaining the School Estate can also be input to mitigate this entropic spiral.

The PDS data is not included with this package. A small simulated dataset is provided.

The model works with the following parameters

---------------------------------------------
Data inputs               Description 
------------------------- ----------------------------------------------------
Input data for time zero  A data frame with one row per construction element

Deterioration rates       Estimated probabilities of deterioration within one
                          year for each component at each condition grade

Repair costs              Repair costs of repairing components at condition
                          grades B, C, D, and E

Rebuild costs             Cost in pounds per square metre used to estimate cost
                          of rebuilding individual buildings
                          
Repair funds              A vector containing the budget for repairs each year

Rebuild funds             A vector containing the budget for rebuilding each year

Inflation                 A vector containing the inflation to be applied to all
                          repair and rebuild costs each year
                          
--------------------------------------------------------------------------

  ## Installing the package

  The package can be installed with the `devtools` package with `devtools::install_github('DFE-Capital/Blockbuster-2')`.

  If you cannot use this function (due to firewalls for instance) you can download the package as a `.zip` file from the main repository page, and run `devtools::install_local('path_to_zip_file')`. You may need to install [Rtools](https://cran.r-project.org/bin/windows/Rtools/) to facilitate this.

  ## Using the package

  ## From an R session

  Launch an R session as normal and run the following (again setting the arguments as required):

  `library(Blockbuster2)`
  
  The beauty of package development is that the business knowledge is enshrined in the package, with the code and documentation in one place. To familiarise yourself after install you can work through the blockbuster vignette which will provide typical uses. The blockbuster-methodology vignette provides in depth detail about how the model works.

  You can also use the extensive R help which explains the input arguments and output values of all functions by prefixing a function with a *?*.
  
  ```
  ?Blockbuster
  ```
  
  ## Example
  
  The following code will run the Blockbuster model on the simulated data over a
  10 year period and store the output in an object called `results`:
  
  ```
  results <- Blockbuster(simulated_elements, forecast.horizon = 10)
  ```
  Note that by default the Blockbuster Deterioration Model automatically saves yearly states into files called `blockbuster_output_element_*.rds` and `blockbuster_output_block_*.rds`within an `output` sub-folder of the current working directory that the function will create if it does not already exist.  The complete output is saved as `blockbuster_output_output.rds`.  This behaviour is customisable.  Saving of outputs can be turned off with `save = FALSE` passed as an argument. Alternatively, the path and filename can be adjusted using the `filelabel` and `path` arguments.  When using large datasets and forecasting over long periods it is recommended to always use `save = TRUE`, to avoid losing progress should R run out of memory to hold the complete output.
  
  The element level output is a copy of the input, but with updated grade probabilities and repair totals.  Note that the the cost of repairing an element at
  a given grade is given by the `*.repair.total` column.  The `*.repair.cost` column is the unit cost of repairing that component type.
  
  We recommend using the functions `pull_Element_Data`, `pull_Block_Data`, `load_Element_Data` and `load_Block_Data` to put the output of a Blockbuster Deterioration Model run into a friendlier format for
  analysis.
  
  Passing numeric vectors to the `rebuild.money` and `repair.money` arguments allows control of the amount of money available for rebuilding and repairing each year respectively.
  
  ## Project goal

  To simulate the deterioration of the School Estate into the future under various maintenance and rebuilding spending policies. This will provide a data driven approach to improving the management of the [School Estate potenitally saving public money](https://www.nao.org.uk/report/capital-funding-for-schools/). The model's predictions should be evaluated with the next [Condition Data Collection](https://www.gov.uk/guidance/condition-data-collection-programme-information-and-guidance) due in 2017 and beyond. This package will also provide the skills and learnings necessary to extend this package functionality for the CDC data.

## Improvements

* If you find any bugs raise an issue on the [Github package page](https://github.com/DFE-Capital/Blockbuster-2).
* Any suggestions or improvments to the decision making process within the model would also be welcome.
* Package development is iterative and dependent on user feedback.

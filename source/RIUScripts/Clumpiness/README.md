# Clumpiness analysis

This part contians the scripts for Clumpiness analysis

## Prerequisites
* R (>= 3.3.3)
* data.table (>= 1.10.4-3)
* dplyr (>= 0.7.1)
* ggplot2 (>= 2.2.1)
* lubridate (>= 1.7.1)

## List of scripts

* [[clumpiness.R](./clumpiness.R)] - Script to compute the clumpiness measure and the corresponding critical value, and some visualizations.
* [\*-driver.R] - Script to run the LTV analysis.

## TODO List

1. Add comments.
2. Remove unused comments.
3. Improve the \*-driver.R scripts
4. etc.

## References
* [Yao Zhang, Eric T. Bradlow, and Dylan S. Small (2013) New measures of clumpiness for incidence data. Journal of Applied Statistics 40(11): 2533-2548.](http://dx.doi.org/10.1080/02664763.2013.818627)
* [Yao Zhang, Eric T. Bradlow, and Dylan S. Small (2015) Predicting Customer Value Using Clumpiness: From RFM to RFMC. Marketing Science 34(2): 195-208.](http://dx.doi.org/10.1287/mksc.2014.0873)
# Customer Lifetime Value (LTV) analysis

This part contians the scripts for LTV analysis

## Prerequisites
* R (>= 3.3.3)
* parallel
* BTYD (>= 2.4)
* BTYDplus (>= 1.0.1)
* coda (>= 0.19-1)
* data.table (>= 1.10.4-3)
* dplyr (>= 0.7.1)
* ggplot2 (>= 2.2.1)
* lubridate (>= 1.7.1)
* optparse (>= 1.4.4)

## List of scripts

* [[BTYDmodels.R](./BTYDmodels.R)] - Script to fit the Buy Till You Die (BTYD) models and some visualizations.
* [[findLTV.R](./findLTV.R)] - Script to compute the LTV.
* [[MonetaryValue.R](./MonetaryValue.R)] - Script to model the spending amount per transaction for each customer.
* [[BTYD-trainer.R](./BTYD-trainer.R)] - Script to train the BTYD model.
* [[LTV-driver.R](./LTV-driver.R)] - Script to run the LTV analysis.
* [\*-trainer.R] - Depreciated and replaced by [BTYD-trainer.R](./BTYD-trainer.R).
* [\*-driver.R] - Depreciated and replaced by [LTV-driver.R](./LTV-driver.R).

## TODO List

1. Add comments.
2. Remove unused comments.
3. Improve the [LTV-driver.R](./LTV-driver.R) by reading trained model via rds file generated from BTYD-trainer.R.
4. Improve the [BTYD-trainer.R](./BTYD-trainer.R).
5. etc.

## References
* [PNBD] - [David C. Schmittlein, Donald G. Morrison, and Richard Colombo (1987) Counting Your Customers: Who-Are They and What Will They Do Next?. Marketing Science 33(1): 1-24.](https://doi.org/10.1287/mnsc.33.1.1)
* [BG/NBD] - [Peter S. Fader, Bruce G. S. Hardie, and Ka Lok Lee (2005) “Counting Your Customers” the Easy Way: An Alternative to the Pareto/NBD Model. Marketing Science 24(2): 275-284.](https://doi.org/10.1287/mksc.1040.0098)
* [BG/BB] - [Fader PS, Hardie BGS, Shang J (2010) Customer-base analysis in a discrete-time noncontractual setting. Marketing Sci. 29(6): 1086–1108.](http://dx.doi.org/10.1287/mksc.1100.0580)
* [PGGG] - [Michael Platzer and Thomas Reutterer (2016) Ticking Away the Moments: Timing Regularity Helps to Better Predict Customer Activity. Marketing Science 35(5): 779-799.](https://doi.org/10.1287/mksc.2015.0963)
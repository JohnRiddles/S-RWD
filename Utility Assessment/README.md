# README

This directory contains code used to assess data utility for the synthetic data sets based on both Synthea and N3C. This also includes a directory containing an R package, synthassess, which implements a variety of utility and other quality measures. The primary evaluations involve comparing distributions between the source and synthetic data sets. Specific comparisons include the following.

- Number of unique values for different variables each data set.
- Comparison of missingness rates
- Kolmogorov-Smirnov statistics for comparison of continuous values
- Hellinger distance for comparison of categorical values
- Average difference in correlation coefficients
- Average difference in Cramer's V
- Confidence interval overlaps
- U-Statistic (a global utility measure that measures multivariate associations)

The scripts for utility assessment require a configuration file that specifies whether a variable is categorical, continuous, or should not be evaluated.
Examples of such configuration files are included in both the Synthea and N3C directories.

The synthassess directory contains an R package with functions that can be applied in general situations.
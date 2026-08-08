# README

This directory contains three subdirectories. 

## Use Case Analysis Data Preparation

The subdirectory 'Use-Case-Analysis-Data-Preparation' is used to prepare data for the use-case analysis. The programs in that subdirectory are structured for use in the Palantir Foundry platform used for N3C. They are specifically structured as a [Python transform](https://www.palantir.com/docs/foundry/transforms-python/transforms), which is a set of Python programs that specify a sequence of transformation steps that take a collection of input files and produce a set of output files and intermediate files. The programs take as input a set of OMOP tables and some user-defined configuration files that identify concept sets or metadata used for defining transformations. There is a subdirectory named 'datasets' that defines transforms for the original data, and a subdirectory named 'synthetic_datasets' that defines transforms for the synthetic data.

## Concept Sets and Config Files

The subdirectory 'Concept-Sets-and-Config-Files' primarily contains CSV files which list or define concept sets used in the analysis. It also contains one small configuration file that is used in a data transformation step. The only concept sets that are explicitly defined in these files (i.e., by listing specific concept IDs for a concept set) are those that are not already defined in the N3C table 'concept_set_members'.

## Use Case Data Analysis

The subdirectory 'Use-Case-Data-Analysis' contains R scripts used to complete final data preparation steps and conduct the use case analysis. It contains the following scripts:

- 'palantir-foundry-helper-functions.R': A set of R functions used to simplify data access, especially for large datasets stored as collections of Parquet files.

- 'prepare-use-case-analysis-data.R': A sequency of data subsetting and transformation steps that build upon the Python transform described earlier. The result is a dataset in Palantir Foundry that contains exactly the set of cases and variables needed for all analyses. It results in a dataset named 'use_case_analysis_data'.

- 'prepare-synthetic-use-case-analysis-data.R': A copy of the previous script, but adjusted to work for the synthetic datasets. It results in a dataset named 'synthetic_use_case_analysis_data'.

- 'conduct-use-case-analysis.R': This script conducts the use case analysis, separately for the original data and for the synthetic data. It then combines the analysis results from both datasets into combined summaries.
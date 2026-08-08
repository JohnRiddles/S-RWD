# README

This repository contains code used for the S-RWD: Synthetic Data for Real-World Data Project.  It is organized into four directories.

## Data Generation

This directory contains scripts that were used to generated synthetic data sets with two different source data sets and run in two different environments. One was used to generate data within the Oak Ridge National Laboratory's Frontier supercomputer using Synthea data. The other generates data based on N3C and is run within the N3C Enclave.

The model used was CEHR-GPT. https://github.com/knatarajan-lab/cehrgpt. A copy of the CEHR-GPT source code, as well as source code for two interdependent packages, CEHR-BERT and CEHRBERT-Data, are also included. Having the package source locally available was necessary for generation using N3C for a variety of reasons. One reason is that the package would not work with N3C out of the box, so the package source was patched at run-time, which requires having the source code directly available. 

The original CEHR-GPT repository is available here: https://github.com/knatarajan-lab/cehrbert_data
CEHR-BERT: https://github.com/cumc-dbmi/cehrbert
CEHRBERT-data: https://github.com/knatarajan-lab/cehrbert_data

## Risk Assessment

This directory contains code used to assess disclosure risk of the synthetic data sets based on both Synthea and N3C. Synthea itself is public, so should not have high disclosure risk in the source data, let alone the synthetic data, but it was used to test functionality. 

## Use Case Analysis

This directory contains code used to perform a use case analysis based on an N3C COPD cohort.

## Utility Assessment

This directory contains code used to assess data utility for the synthetic data sets based on both Synthea and N3C. This also includes a directory containing an R package, synthassess, which implements a variety of utility measures.

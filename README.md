# Missing Data techniques for Reject Inference

**Type:** Master's Thesis

**Author:** Radoslav Evtimov

**1st Examiner:** Prof. Dr. Stefan Lessmann  

**2nd Examiner:** Prof. Dr. Benjamin Fabian 

Idea behind the main output of the thesis:

![results](/Process_flow.jpg)

## Table of Content

- [Summary](#summary)
- [Working with the repo](#Working-with-the-repo)
    - [Dependencies](#Dependencies)
    - [Setup](#Setup)
- [Reproducing results](#Reproducing-results)
- [Results](#Results)
- [Project structure](-Project-structure)

## Summary

**Keywords**: Reject Inference, Missing Data, Credit Risk, MNAR, Acceptance loop (give at least 5 keywords / phrases).

**Full text**: Full text can be found in different formats in the "paper" folder.   

## Working with the repo

### Dependencies

For the results on the Master Thesis R 4.1.2 was used.
There is dependence on the "miceMNAR" package that is currently not available on CRAN. It has to be manually downloaded and installed from https://cran.r-project.org/src/contrib/Archive/miceMNAR/. 

### Setup

1. Clone this repository

2. Set up your working directory in R to be the one with all files of this repo

## Reproducing results

To recreate results, there are two options: 

- Run the predefined files for
  - 2 variables drawn from the Multivariate Gaussian Distribution
  Run files acceptance_loop_4.R to get the results corresponding to combination 4
  - 3 variables drawn from the Multivariate Gaussian Distribution
  Run file acceptance_loop_2.R to get the results corresponding to combination 2
  Run file acceptance_loop_3.R to get the results corresponding to combination 3

All dependecies will be automatically flagged - if any packages are missing from the environment, the console will return this as dependency. If they are available, they will be added to the environment.

Output: 
All files with results from the different combinations will be automatically stored in the working directory. 


## Results

The "plot" folder contains the plots shown in the paper. 
The actual results can be recreated by following the .R files in the "src" folder. 

## Project structure


```bash
├── README.md
├── paper
    ├── master_thesis_Evtimov_Radoslav.pdf          -- master Thesis in pdf format
    ├── master_thesis_Evtimov_Radoslav.tex          -- master Thesis in Latex format
    ├── references_master_thesis.bib                -- references in bibtex format
├── params                                          -- stores parameters  
├── plots                                           -- stores image file
└── src
    ├── acceptance_loop_2.R                         -- code for acceptance loop for combination 2
    ├── acceptance_loop_3.R                         -- code for acceptance loop for combination 3
    ├── acceptance_loop_4.R                         -- code for acceptance loop for combination 4
    ├── acceptance_loop_adjustables_4_vars.R        -- code for acceptance loop with adjustable combinations for 4 features
    ├── acceptance_loop_adjustables_5_vars.R        -- code for acceptance loop with adjustable combinations for 5 features
    ├── mnar_simulation_study.R                     -- code for MNAR simulation study
    ├── al_4_var_functions.R                        -- functions for the acceptance loop
    ├── al_5_var_functions.R                        -- functions for the acceptance loop
    ├── binaryPPMA_functions.R                      -- functions for the PPMM
    └── visualisations_master_thesis.R              -- visualisation script used                 
```

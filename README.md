# Title

**Type:** Master's Thesis

**Author:** Radoslav Evtimov

**1st Examiner:** Prof. Dr. Stefan Lessmann  

**2nd Examiner:** Prof. Dr. Benjamin Fabian 

[Insert here a figure explaining your approach or main results]

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

**Full text**: Link will be included shortly.  

## Working with the repo

### Dependencies

For the results on the Master Thesis R 4.1.2 was used.

Does a repository have information on dependencies or instructions on how to set up the environment?

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

Does a repository contain a table/plot of main results and a script to reproduce those results?

## Project structure

(Here is an example from SMART_HOME_N_ENERGY, [Appliance Level Load Prediction](https://github.com/Humboldt-WI/dissertations/tree/main/SMART_HOME_N_ENERGY/Appliance%20Level%20Load%20Prediction) dissertation)

```bash
├── README.md
├── params                                          -- stores output csv file  
├── plots                                           -- stores image files
└── src
    ├── prepare_source_data.ipynb                   -- preprocesses data
    ├── data_preparation.ipynb                      -- preparing datasets
    ├── model_tuning.ipynb                          -- tuning functions
    └── run_experiment.ipynb                        -- run experiments 
    └── plots                                       -- plotting functions                 
```

# lifecourse-edu-cog-CHARLS
This repo contains all the code necessary to replicate data construction and analyses in the paper: 

Wu Y, Zhou Y, Rojas-Saunero LP, Chen R, Gross AL, Nianogo R, Ritz BR, Mayeda ER. Effect of hypothetical education interventions on late-life memory function and change among middle-aged and older adults in China

## Table of contents
1. [Project Descriptions](#proj_desc)
2. [Scripts Descriptions](#script_desc)

## Project Descriptions <a name="proj_desc"></a>
Limited research exists regarding education in different life stage and late-life cognitive health. Existing evidence and theroy suggest that completing as much education as possible early in life rather than adulthood may shape cognitive outcomes differently. We used nationally representative data and a target trial framework to investigate the joint effect of early life educational level and participation in adulthood education programs on memory function and change among Chinese middle-aged and older adults.

* Dataset: China Health and Retirement Longitudinal Study (CHARLS), a nationally representative longitudinal study of Chinese adults aged 45 and older. We used data from waves 1 to 5 (2011-2020).
* Exposure: Educational attainment in early life (no formal education, primary school, middle school, college) and participation in adult education programs (yes/no).
* Outcome: Episodic memory function and change measured by immediate word recall scores and delayed word recall scores from years 2011-2020. 
* Method: G-computation with time-varying exposures and confounders using generalized estimating equations (GEE).


## Scripts Descriptions <a name="script_desc"></a>
*R scripts for data construction, G computation function, bootstrapping, cluster computing submission and generating tables and figures were in the `scripts` folder. The R scripts are named in the order they should be run. Functions created by the authors were sourced where needed.*

* **1.dataset_construction.R**: Cleans the data and creates the necessary variable for the analysis.
* **2. multiple_imputation.R**: Performs multiple imputation to handle missing data using multiple imputation (predictive mean matching).
* **2.1_analytic_dataset.R**: Creates the final analytic dataset after multiple imputation (for main and sensitivity analyses).
* **3.0_GEE_time_varying_g_comp_function_cat_exp.R**: Contains the function for G-computation with time-varying exposures and confounders using GEE.
* **3.1_main_sens_bootstrap_cluster.R**: Calculates point estimates and performs bootstrapping for main and sensitivity analyses on UCLA Hoffman cluster.
* **main_sens_gee_tv_g_comp_submission.sh**: Cluster submission script for main and sensitivity analyses.
* **3.2_sens2_restricted_sample_bootstrap_cluster.R**: Calculates point estimates and performs bootstrapping for sensitivity analysis 2 (restricted sample) on UCLA Hoffman cluster.
* **sens2_restricted_sample_gee_tv_g_comp_submission.sh**: Cluster submission script for sensitivity analysis 2 (restricted sample).
* **4_cluster_results.R**: Combines results from all bootstrap samples and calculates estimates and confidence intervals. Create figures and tables for modelling results.
* **9_tables.R**: Creates summary tables for the manuscript.
* **functions.R**: Contains all the custom helper functions used in the analysis (except for g comp function which is in script 3.0).

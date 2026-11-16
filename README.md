# lifecourse-edu-cog-CHARLS
This repo contains all the code necessary to replicate data construction and analyses in the paper: 

Wu Y, Zhou Y, Rojas-Saunero LP, Chen R, Pescador Jimenez M, Gross AL, Nianogo RA, Ritz BR, Mayeda ER. The joint effect of early-life and adult education on memory function and change among middle-aged and older adults in China. Alzheimer's & Dementia. 2026 (in press).

## Table of contents
1. [Project Descriptions](#proj_desc)
2. [Scripts Descriptions](#script_desc)

## Project Descriptions <a name="proj_desc"></a>
Evidence on life-stage-specific education and late-life cognitive health
remains limited. Completing formal education early in life, rather than
through adult education programs later, may shape late-life cognition
differently. China's middle-aged and older adult population, with low
early-life education levels and nationwide adult education programs launched
from 1978 onward, offers a unique setting to study this question. Guided by
the target trial emulation framework, we estimated the joint effect of
early-life educational attainment and adult education program participation
on memory function and change among Chinese adults aged 45 years and older.

* **Data**: China Health and Retirement Longitudinal Study (CHARLS), a
  nationally representative longitudinal survey of Chinese adults aged 45 years
  and older and their spouses. We used study waves 1 to 5 (2011, 2013, 2015,
  2018, 2020) and the 2014 life history interview. The analytic sample
  included 23,599 participants aged 45 years or older at their baseline
  (first) interview (weighted n = 551,750,672).
* **Exposures**:
  * Early-life educational attainment: no formal education, primary school,
    secondary school (middle school, high school, or vocational school), or
    college.
  * Adult education program participation (yes/no), including university
    extension, night school, self-study college exams, correspondence courses,
    literacy programs, and accelerated courses.
* **Outcomes**: Episodic memory function and change, measured by 10-word
  immediate and delayed word recall scores (each 0 to 10) over 9 years of
  follow-up. Scores from 2018 and 2020 (HCAP-based administration) were
  harmonized to earlier waves using weighted equipercentile equating.
  [Wu et al. J Alzheimers Dis Rep 2024](https://pmc.ncbi.nlm.nih.gov/articles/PMC11863736/)
* **Confounders**:
  * Early-life: baseline age, sex/gender, birth order (and its interaction
    with sex/gender), region, rurality, and hukou status of first childhood
    residence, parental educational attainment, and parental occupation.
  * Early adulthood (closest to age 30): number of children, marital status,
    rural residence, occupation status, and hukou status.
* **Methods**:
  * Multiple imputation by chained equations (predictive mean matching;
    20 imputed datasets, 60 iterations each) for missing covariates,
    exposures, and baseline memory scores.
  * Stabilized inverse probability of censoring weights (truncated at the
    99th percentile) for loss to follow-up due to death and non-response.
  * Parametric g-computation to handle time-varying confounding, with
    survey-weighted generalized estimating equations (GEE) for memory scores
    (follow-up time modeled with a restricted cubic spline, plus a practice
    effect indicator) and a final marginal structural model.
  * Survey weights applied at every step; 95% CIs from 1,000 bootstrap
    samples; estimates pooled across imputations using Rubin's rules.
* **Sensitivity analyses**:
  1. Analyses without survey weights.
  2. Analyses restricted to participants enrolled in 2011 (n = 16,972).


## Scripts <a name="script_desc"></a>

All R scripts are in the `scripts` folder and are numbered in the order they
should be run. Custom functions are sourced within scripts where needed.

| Script | Description |
|---|---|
| `1.dataset_construction.R` | Cleans the raw data and constructs exposure, outcome, and covariate variables. |
| `2.multiple_imputation.R` | Performs multiple imputation by chained equations using predictive mean matching. |
| `2.1_analytic_dataset.R` | Creates the final analytic datasets from the imputed data for the main and sensitivity analyses. |
| `3.0_GEE_time_varying_g_comp_function_cat_exp.R` | Defines the g-computation function for categorical exposures with time-varying confounding using GEE. |
| `3.1_main_sens_bootstrap_cluster.R` | Computes point estimates and bootstrap samples for the main analysis and sensitivity analysis 1 (no survey weights) on the UCLA Hoffman2 cluster. |
| `main_sens_gee_tv_g_comp_submission.sh` | Hoffman2 job submission script for `3.1_main_sens_bootstrap_cluster.R`. |
| `3.2_sens2_restricted_sample_bootstrap_cluster.R` | Computes point estimates and bootstrap samples for sensitivity analysis 2 (participants enrolled in 2011) on the UCLA Hoffman2 cluster. |
| `sens2_restricted_sample_gee_tv_g_comp_submission.sh` | Hoffman2 job submission script for `3.2_sens2_restricted_sample_bootstrap_cluster.R`. |
| `4_cluster_results.R` | Combines bootstrap results, pools estimates across imputations, computes 95% CIs, and generates result figures and tables (Figures 3 to 5, Tables S7 to S12, Figures S8 to S9). |
| `9_tables.R` | Creates descriptive tables for the manuscript (Table 1, Table S6). |
| `functions.R` | Custom helper functions used across scripts (the g-computation function is in script 3.0). |

## Software and computing <a name="software"></a>

All data construction and analyses were conducted in R v4.4.1. Bootstrap
analyses were run on the Hoffman2 Shared Cluster provided by the UCLA
Institute for Digital Research and Education's Research Technology Group.

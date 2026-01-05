This repository contains code for the paper "Sampling for computational efficiency when conducting analyses in big data," In Press at the _Epidemiology_.
 

CODE DESCRIPTION

Step 1. Generate the simulated data using churn_1_data.R

Step 2. Run the analyses

  - Estimate rate using crude approaches: churn_2_crude-rate.R 
  - Estimate risk using crude approaches: churn_2_crude-risk.R
  - Estimate risk and rate using weights (not including Z): churn_2_wt.R
  - Estimate risk and rate using weights (including Z): churn_2_wt-z.R
  - Estimate risk and rate using multiple imputation (not including Z): churn_2_mi.R
  - Estimate risk and rate using multiple imputation (including Z): churn_2_mi-z.R
  - Estimate crude risk allowing for interval censoring: churn_2_ic.R

Step 3. Combine results across approaches and scenarios using churn_3_stack.R

Step 4. Summarize results in tables and figures
  - Build Table 1: churn_4_table.R
  - Build figures for crude results: churn_4_fig-unadj.R
  - Build figures for adjusted results: churn_4_fig-adj.R
  

Rosenfeld, Bryn; Imai, Kosuke; Shapiro, Jacob, 2015, "Replication Data for: An Empirical Validation Study of Popular Survey Methodologies for Sensitive Questions,” The American Journal of Political Science. Forthcoming.

Data:

The replication archive includes 3 data files. These are:
1. ms_counts by county_final.RData, data from the Mississippi Secretary of State’s voter history file on the population of 2011 General Election voters;
2. ms-g11-county-vota-data.RData, official county-level election results from the 2011 Mississippi General Election (sourced from county recapitulation reports)
3. ms_replication_data.RData, original survey data collected by the authors. This data file was removed from Dataverse in October 2020 due to (concerns of reidentification of respondents). The scripts assume you have access to this data file. If you are interested in these data, please obtain an IRB approval that states the anonymity protection of survey respondents and the contact one of the authors. 

Documentation:
1. this readme.txt file
2. validate-codebook.pdf, a codebook for the three datasets listed above.

Code:

The following four script files reproduce all of the results in the paper (and intermediate analysis):
1. validate-replication-code.R, replicates all results in the published paper and supplemental appendix. This R script should be run first. Other scripts run inside of it. Note that many of the simulations are computationally intensive and will take some time to run. The saved model objects are also included in the archive and can be loaded for use in subsequent analysis if the user does not wish to replicate the actual MCMC draws. 
2. validate-replication-code-data-setup-weighting.R is a supporting R script, which recodes survey variables and computes survey weights to create the Analysis Datasets
3. endorse-comp-iters.R, supporting R script used to replicate the results for the endorsement condition in Section 4.5 (Efficiency Comparison)
4. endorse-comp-iters.slurm, supporting slurm script used to replicate the results for the endorsement condition in Section 4.5 (Efficiency Comparison)      

Models:

All MCMC simulations used in the analysis are also included in the archive. They are:
1. endorse_out_all (1-4), endorsement model with intercept only (4 parallel simulations from overdispersed starting values);
2. endorse_out_all_cov (1-4), endorsement model with voter file covariates;
3. endorse_out_all_rf (1-4), endorsement mixed effects model with individual-level voter file covariates and county-level random intercepts;
4. endorse_out_survey_covs (1-4), endorsement model with survey-measured covariates;  
5. list_out_rf (1-3), list mixed effects model with individual-level voter file covariates and county-level random intercepts;
6. rr_out_rf, random response mixed effects model with individual-level voter file covariates and county-level random intercepts;
7. replication-endorse-comp.zip, a zip file containing simulation results for the endorsement condition in Section 4.5 (Efficiency Comparison, note: the MCMC simulations for this analysis are 7.5 GB and are available on request from the authors) 

Software:

All data analyses in this article were carried out using R version 3.1.2.

The following R package versions were used in the analysis and are included in the replication archive:
endorse_1.4.1
list_7.1
rr_1.3

Along with these supporting packages:
arm_1.7-07
coda_0.17-1
lme4_1.1-7
MASS_7.3-39

For compatibility, all of these packages can be installed from source. Note that once you have saved these packages locally, you will need to insert the correct file path in order to install them in the R script “validate-replication-code.R” 

# **Master's Thesis "Statistical Modelling of Genetic Background of Haemoglobin Deferral in Blood Donation"**


**Host institution:** Finnish Red Cross Blood Service

**Author:** Krista Karttunen

**Study Institution:** Aalto University (School of Science)

**Program:** Master's Programme in Life Science Technologies

**Major:** Bioinformatics and Digital Health

---
**Codes modified from Toivonen et al. the https://github.com/FRCBS/anemia_and_hb_deferral_prediction/**

---

## Description of files
| File Name       | Description                                      | 
|-----------------|--------------------------------------------------|
| `common.R`   | Contains common functions and data descriptions used in the other code files. |
| `collecting_new_snps.Rmd`   | Codes to combine the results of multiple GWAS result tables into a single table. |
| `summarise_data.Rmd`  | Contains the data exploration and initial data preprocessing.         |
| `training_models.Rmd`    | Data creation of the model specific train, test and validate data sets. Model training, diagnostics and effect size plots.         | 
| `testing_models.Rmd` | Creationg the ROC and PR curves and AUC and AUPR measures.       |
| `pretty_forest_plots.Rmd` | Codes for creating effect size plots are slo here separately from the model training.      |

# Improved decision making for water lead testing in U.S. child care facilities using machine-learned Bayesian networks

This repository provides all the data and code needed to recreate the Bayesian Network (BN) models used to predict building-wide water lead risk in child care facilities in the publication below:

Mulhern, R. E.; Kondash, A.; Norman, E.; Johnson, J.; Levine, K.; McWilliams, A.; Napier, M.; Weber, F.; Stella, L.; Wood, E.; Lee Pow Jackson, C.; Colley, S.; Cajka, J.; MacDonald Gibson, J.; Redmon, J. H. "Improved Decision Making for Water Lead Testing in U.S. Child Care Facilities Using Machine-Learned Bayesian Networks."
<i>Environmental Science and Technology.</i> Under review. 

### Data
The data set for this work is based on first-draw water lead sampling from over 4,000 child care centers in North Carolina, collected by the Clean Water for Carolina Kids program: https://www.cleanwaterforcarolinakids.org/. The data set is deidentified and provides eight binary target variables (whether the maximum water lead concentration for each facility exceeded 1, 5, 10, or 15 ppb and whether the 90th percentile lead concentration for each facility exceeded 1, 5, 10, or 15 ppb) as well as compiled predictor variables for machine learning. 

The data set to reproduce the analysis and data dictionary are located in the folder: <i>childcare_lead_BNmodels/data</i>

### Software
All models were built in RStudio which can be downloaded here: https://posit.co/download/rstudio-desktop/

Additional software packages that will need to be downloaded and installed from CRAN include:
<ul> 
  <li>dplyr - used for data wrangling</li>
  <li>tidyverse - used for data wrangling</li>
  <li>ggplot2 - used for plotting</li>
  <li>ggrepel - used for plotting labels</li>
  <li>gRain - used for plotting BN networks</li>
  <li>visNetwork - used for plotting BN network structures</li>
  <li>Rgraphviz - used for plotting BN network structures</li>
  <li>bnlearn - used for learning Bayesian network structures</li>
  <li>ForestDisc - used for random forest discretizations of continuous variables</li>
  <li>ROCR - used to evaluate performance using ROC curve</li>
  <li>purrr - used to compile ROC values from nested lists</li>
  <li>caret - used to generate the confusion matrix</li>
  <li>mice - used to handle missing data</li>
</ul>

### R scripts
All scripts below are located in the folder: <i>childcare_lead_BNmodels/scripts</i>



The scripts are meant to be run in the following order, since each script generates various outputs that the subsequent scripts utilize for data analysis and visualization. 


### Questions
Questions about the code in this repository should be directed to Riley E. Mulhern: rmulhern@rti.org

# Improved decision making for water lead testing in U.S. child care facilities using machine-learned Bayesian networks
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7477787.svg)](https://doi.org/10.5281/zenodo.7477787)

Last updated: March 6, 2023

This repository provides all the data and code needed to recreate the Bayesian Network (BN) models used to predict building-wide water lead risk in child care facilities in the publication below:

Mulhern, R. E.; Kondash, A.; Norman, E.; Johnson, J.; Levine, K.; McWilliams, A.; Napier, M.; Weber, F.; Stella, L.; Wood, E.; Lee Pow Jackson, C.; Colley, S.; Cajka, J.; MacDonald Gibson, J.; Redmon, J. H. "Improved Decision Making for Water Lead Testing in U.S. Child Care Facilities Using Machine-Learned Bayesian Networks."
<i>Environmental Science and Technology.</i> Under review. 

### Data
The data set for this work is based on first-draw water lead sampling from over 4,000 child care centers in North Carolina, collected by the Clean Water for Carolina Kids program: https://www.cleanwaterforcarolinakids.org/. The data set is deidentified and provides eight binary target variables as well as compiled predictor variables for machine learning. The target variables are defined as follows:
<ul> 
  <li><b>maxabove1</b> - Whether the maximum first-draw lead concentration at each facility exceeded 1 ppb.</li>
  <li><b>maxabove5</b> - Whether the maximum first-draw lead concentration at each facility exceeded 5 ppb.</li>
  <li><b>maxabove10</b> - Whether the maximum first-draw lead concentration at each facility exceeded 10 ppb.</li>
  <li><b>maxabove15</b> - Whether the maximum first-draw lead concentration at each facility exceeded 15 ppb.</li>
  <li><b>perc90above1</b> - Whether the 90th percentile first-draw lead concentration at each facility exceeded 1 ppb.</li>
  <li><b>perc90above5</b> - Whether the 90th percentile first-draw lead concentration at each facility exceeded 5 ppb.</li>
  <li><b>perc90above10</b> - Whether the 90th percentile first-draw lead concentration at each facility exceeded 10 ppb.</li>
  <li><b>perc90above15</b> - Whether the 90th percentile first-draw lead concentration at each facility exceeded 15 ppb.</li>
</ul>

The data set to reproduce the analysis and data dictionary are located in the folder: <i>childcare_lead_BNmodels/data</i>

### Software
All models were built in RStudio which can be downloaded here: https://posit.co/download/rstudio-desktop/

Additional software packages that will need to be downloaded and installed from CRAN include:
<ul> 
  <li><code>dplyr</code> - used for data wrangling</li>
  <li><code>tidyverse</code> - used for data wrangling</li>
  <li><code>ggplot2</code> - used for plotting</li>
  <li><code>ggrepel</code> - used for plotting labels</li>
  <li><code>gRain</code> - used for plotting BN networks</li>
  <li><code>visNetwork</code> - used for plotting BN network structures</li>
  <li><code>Rgraphviz</code> - used for plotting BN network structures</li>
  <li><code>bnlearn</code> - used for learning Bayesian network structures</li>
       <ul>
      <li>To estimate conditional probabilities of the network with missing data, the latest release of <code>bnlearn</code> may need to be downloaded here: https://www.bnlearn.com/releases/bnlearn_4.9-20230207.tar.gz</li>
      </ul>
  <li><code>ForestDisc</code> - used for random forest discretizations of continuous variables</li>
  <li><code>ROCR</code> - used to evaluate performance using ROC curve</li>
  <li><code>purrr</code> - used to compile ROC values from nested lists</li>
  <li><code>caret</code> - used to generate the confusion matrix</li>
  <li><code>mice</code> - used to handle missing data</li>
</ul>

### R scripts
All scripts below are located in the folder: <i>childcare_lead_BNmodels/scripts</i>

The main code required to build a model for each target is <b>Mulhern_et_al_BN_model_script_as_published.R</b> The target node must be manually set by the user. This script will allow the user to visualize and save the outputs for a single model at a time. 

If only a single target is of interest, then no other scripts are necessary. (This script may also serve as a template for other open source machine learning applications using Bayesian networks by replicating the basic pre-processing and machine learning steps shown in the script, including: defining numerical and categorical variables, splitting the data set into training and test sets, discretizing continuous variables, learning the network structure, selecting significant predictor nodes, and assessing the model's performance.)

In order to summarize the outputs of all eight models shown in the cited manuscript, the above script must be run iteratively eight times for each target node. The subsequent scripts then summarize the outputs of all eight models. These additional scripts are described below and <b>should be run in the following order</b> since the outputs of some are used as the inputs to others:
<ul> 
  <li><b>improvement_summary.R</b> - This script generates Figures 4 and 6 in the manuscript to compare the F-scores, sensitivity improvement, and sampling reduction metrics achieved by the BN models compared to the various alternative heuristics. </li>
  <li><b>performance_summary.R</b> - This script generates Figure 2 and Figure S9 in the manuscript and an overall summary table of the performance metrics of all eight models.</li>
  <li><b>sigvars_summary_all_models.R</b> - This script generates Figure 3 in the manuscript to visualize the frequency of variables selected across all eight models.</li>
  <li><b>network_structure_summary.R</b> - This script generates clean versions of the network structures for all eight nodes. Interactive versions of the outputs can be seen at: https://www.cleanwaterforcarolinakids.org/publications/bn_models</li>
  <li><b>tornado_chart_summary.R</b> - This script generates the tornado plots in Figures S11 through S15 in the manuscript Supporting Information. These plots help visualize the effect of important variables on water lead risk across all models where they were selected.</li>
</ul>

### Questions
Questions about the code in this repository should be directed to Riley E. Mulhern: rmulhern@rti.org

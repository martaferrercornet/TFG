#-------------------------------------------------------------------------------
# CORE CODE
#-------------------------------------------------------------------------------

# The following code is the one that ask the user to introduce the type of analysis
# and the input data.

# Then, according to this choice, the code automatically runs all the other scripts
# to provide the results of the analysis


rm(list=ls()); graphics.off(); cat("\014")

# the libraries are loaded
library(plyr)
library(cocor)
library(png)
library(ggplot2)
library(lubridate)
library(plot.matrix)
library(tidyverse)
library(stats)
library(nlme)
library(xlsx)

# working directory
path = "~/Enginyeria Biomèdica/4t curs/1r semestre/Treball de Fi de Grau/core"
setwd(path)

# path where the results of the analyses are saved
results_path = "~/Enginyeria Biomèdica/4t curs/1r semestre/Treball de Fi de Grau/core/results"

# the following code is able to run other scripts, depending on the analysis that
# is needed
status_focus = T; status_analysis = T
while ((status_focus==T)& (status_analysis==T)){
  focus = readline(prompt="Select: (FA/MD/Cortical and Subcortical) ")
  if ((focus=="FA") | (focus=="MD") | (focus=="Cortical and Subcortical")) {
    status_focus = F
  } else {
    message("Please, select FA, MD or Cortical and Subcortical.")
  }
  if (status_focus==F) {
    while (status_analysis==T) {
      analysis = readline(prompt="Select: (Longitudinal/Sociodemographic/Sex) ")
      if ((analysis=="Longitudinal") | (analysis=="Sociodemographic") | (analysis=="Sex")){
        status_analysis = F
      } else {
        message("Please, select a valid option: Longitudinal, Sociodemographic or Sex")
      }
    }
  }
}
if (status_focus==F & status_analysis==F) {
  # PATIENT SELECTION
  source("01_PATIENT_SELECTION.R")
  
  # DEMOGRAPHIC TABLE
  source("02_DEMOGRAPHIC_TABLE.R")
  
  # DEMOGRAPHIC DESCRIPTION
  source("02_DEMOGRAPHIC_DESCRIPTION.R")
  
  # ANALYSIS AND PLOTS
  if (analysis=="Longitudinal") {
    source("METRICS OF CHANGE.R")
  } else if (analysis=="Sex") {
    n=1
    source("ANOVA ANALYSIS AND PLOTS.R")
  } else if (analysis=="Sociodemographic") {
    n=2
    source("ANOVA ANALYSIS AND PLOTS.R")
    source("03_PLOT_MATRIX.R")
    source("code_spc_imd.R")
  }
  source("03_PLOT_MATRIX.R")
}

# ------------------------------------------------------------------------------
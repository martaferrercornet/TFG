#-------------------------------------------------------------------------------
# SAMPLE DESCRIPTION: DEMOGRAPHIC TABLE
#-------------------------------------------------------------------------------

# In order to describe the sample, this code creates a table which includes the 
# main demographics of the participants.

source("functions.R")

# The following demographic and clinic variables are taken from the dictionary
prov_variables = data.frame(variable=c("Age at scan 1","Age at scan 2","Sex",
                                       "Ethnicity",
                                       "Systolic blood pressure rep1",
                                       "Systolic blood pressure rep2",
                                       "Diastolic blood pressure rep1",
                                       "Diastolic blood pressure rep2",
                                       "Diagnosed diabetes",
                                       "Weight","Waist circumference",
                                       "Hip circumference","BMI",
                                       "Date at scan 1","Date at scan 2",
                                       "Index of Multiple Deprivation"),
                            code = c("f.21003.2.0","f.21003.3.0","f.31.0.0",
                                     "f.21000.0.0","f.4080.3.0","f.4080.3.1",
                                     "f.4079.3.0","f.4079.3.1","f.2443.3.0",
                                     "f.12143.3.0","f.48.3.0","f.49.3.0",
                                     "f.21001.3.0","f.53.2.0","f.53.3.0","f.26410.0.0"))


def_variables = data.frame(variables=c("Age at scan 1","Age at scan 2",
                                       "Sex","Years between scans",
                                       "Ethnicity (white/non-white)",
                                       "Systolic blood pressure",
                                       "Diastolic blood pressure",
                                       "Diagnosed diabetes (yes/not-yes)",
                                       "Weight","Waist/Hip ratio","BMI",
                                       "Index of Multiple Deprivation"))

# A table is built with the data from all the participants.
prov_table = data.frame()
for (i in 1:length(prov_variables[,1])) {
  colname = prov_variables[i,2]
  column = bd_study[[colname]]
  prov_table[1:length(column),i]= column
}
colnames(prov_table)=prov_variables[,1]

# The following variables need further computation and are not directy found in
# the dictionary.
idx_not_computed = !(def_variables$variables %in% prov_variables$variable)
to_be_computed_variables =  data.frame(def_variables[idx_not_computed,1])

# The rest of the variables are divided in two groups. 
# 1. Tool variables: used to compute other variables
# 2. Definitive variables: already available for analysis.
computed_variables = data.frame()
tool_variables = data.frame()
c = 0; m = 0;
for (i in 1:length(prov_variables[,1])) {
  name = prov_variables[i,1]
  if (name %in% def_variables[,1]) {
    c=c+1
    computed_variables[c,1] = name
    computed_variables[c,2] = i
  }
  else {
    m = m+1
    tool_variables[m,1]=name
  }
}

demographic_table = prov_table[,computed_variables[,2]]

# Computation of the required variables 
for (i in 1:length(to_be_computed_variables[,1])) {
  len_table = length(demographic_table[1,])
  variable = to_be_computed_variables[i,1]
  
  if (variable=="Years between scans") {
    date1 = prov_table[,which(colnames(prov_table)=="Date at scan 1")]
    date2 = prov_table[,which(colnames(prov_table)=="Date at scan 2")]
    years = interval(date1,date2) %/% months(1)/12
    demographic_table[,len_table+1] = years
    colnames(demographic_table)[len_table+1]=variable
  }
  
  if ((variable=="Systolic blood pressure") | 
      (variable=="Diastolic blood pressure")) {
    name_measure1 = paste(variable,"rep1")
    name_measure2 = paste(variable,"rep2")
    measure1 = prov_table[,which(colnames(prov_table)==name_measure1)]
    measure2 = prov_table[,which(colnames(prov_table)==name_measure2)]
    mean_measure = apply(as.matrix(measure1,measure2),1,mean)
    demographic_table[,len_table+1] = mean_measure
    colnames(demographic_table)[len_table+1]=variable
  }
  
  if (variable=="Ethnicity (white/non-white)") {
    white_ethnics = c("British","Irish","Any other white background")
    non_white_ethnics = c("African","Any other Asian background","Caribbean",
                          "Indian","Irish","Other ethnic group","Pakistani")
    ethnics_column = prov_table[,which(colnames(prov_table)=="Ethnicity")]
    idx_white = ethnics_column %in% white_ethnics 
    idx_non_white = ethnics_column %in% non_white_ethnics
    ethnicity = matrix(NA,ncol=1,nrow=length(ethnics_column))
    ethnicity[idx_white,1]="White"
    ethnicity[idx_non_white,1]="Non-white"
    demographic_table[,len_table+1] = as.character(ethnicity)
    colnames(demographic_table)[len_table+1]=variable
  }
  
  if (variable=="Waist/Hip ratio") {
    waist = prov_table[,which(colnames(prov_table)=="Waist circumference")]
    hip = prov_table[,which(colnames(prov_table)=="Hip circumference")]
    ratio = waist/hip
    demographic_table[,len_table+1] = ratio
    colnames(demographic_table)[len_table+1]=variable
    
  }
  
  if (variable=="Diagnosed diabetes (yes/not-yes)") {
    diabetes_column = prov_table[,which(colnames(prov_table)=="Diagnosed diabetes")]
    not_yes = c("Do not know","Prefer not to answer","No")
    idx_yes = which(diabetes_column=="Yes")
    idx_not_yes = diabetes_column %in% not_yes
    diab = matrix(NA,ncol=1,nrow=length(ethnics_column))
    diab[idx_yes,1]="Yes"
    diab[idx_not_yes,1]="Not-yes"
    demographic_table[,len_table+1] = as.character(diab)
    colnames(demographic_table)[len_table+1]=variable
  }
}

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# SAMPLE DESCRIPTION: DEMOGRAPHIC ANALYSIS
#-------------------------------------------------------------------------------

# This code creates a table which provides a statistical summary of the demographic
# data of the participants.

source("functions.R")

figure = matrix(data = NA,nrow=length(colnames(demographic_table))+1,ncol=(length(table(bd_study$f.group))+1))
colnames(figure) = c(names(table(bd_study$f.group)),"Puncorr")
group_vector = names(table(bd_study$f.group))

for (i in 1:length(group_vector)) {
  figure[1,i] = table(bd_study$f.group)[i]
}


# The following data is analysed
rownames(figure) = c("Number of participants",
                     "Age at scan 1 (mean ± s.d. (range))",
                     "Age at scan 2 (mean ± s.d. (range))","Sex (female/male)",
                     "Weight (kg)","BMI (kg/m^2)",
                     "Index of Multiple Deprivation (a.u.) (mean ± s.d. (range))",
                     "Years between scans 1 and 2 (mean ± s.d. (range))",
                     "Ethnicity (Non-white/white)",
                     "Systolic blood pressure (mmHg)",
                     "Diastolic blood pressure (mmHg)",
                     "Diagnosed diabetes",
                     "Waist/hip ratio")


# The mean, the standard deviation and some percentages are computed
idx_row = data.frame(c(2,3,7,8,NA,NA),c(4,9,NA,NA,NA,NA),c(5,6,10,11,13,NA),c(12,NA,NA,NA,NA,NA))
functions = c("mean_sd_range","percentages","mean_sd","diabetes")
for (i in 1:length(idx_row[1,])) { # 
  idx_column = idx_row[,i]
  idx_column = idx_column[!is.na(idx_column)]
  func = get(functions[i])
  
  for (j in 1:length(idx_column)) {
    idx = idx_column[j]
    variable = colnames(demographic_table)[idx-1]
    for (k in 1:length(group_vector)) {
      group = group_vector[k]
      figure[idx,k] = func(variable,group,demographic_table)
    }
  }
}

# The p_values are also computed.
p_values = data.frame(variables = colnames(demographic_table),
                      p_values=rep(NA,length(colnames(demographic_table))))

for (i in 1:length(colnames(demographic_table))) {
  variable = colnames(demographic_table)[i]
  column = demographic_table[,which(colnames(demographic_table)==variable)]
  if (class(column)=="character") {
    p_values[i,2] = homogeneity_of_proportions(variable,demographic_table)
  } else {
    p_values[i,2] = homogeneity_of_means(variable,demographic_table)
  }
}

figure[2:length(figure[,1]),length(figure[1,])] = round(p_values[,2],3)

# The table is saved in the results directory
setwd(results_path)

if (analysis!="Sociodemographic") {
  idx_IMD = which(rownames(figure)=="Index of Multiple Deprivation (a.u.) (mean ± s.d. (range))")
  figure = figure[-idx_IMD,]
  filename = "UK_demographic.csv"
} else if (analysis=="Sociodemographic") {
  filename = "ENG_demographic.csv"
}

write.table(figure,filename,sep=",",col.names=NA,row.names=T)
setwd(path)
#-------------------------------------------------------------------------------
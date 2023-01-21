#-------------------------------------------------------------------------------
# LONGITUDINAL ANALYSIS
#-------------------------------------------------------------------------------

# The following code is used in the longitudinal analysis to identify the MRI-
# derived measures that change significantly different between the HC and the 
# COVID group according to the different metrics of change and the Group-Age 
# interaction of an LME model

source("functions.R")


p_metrics = data.frame(variables = colnames(model_table)[6:length(model_table)])
p_metrics_corr = data.frame(variables = colnames(model_table)[6:length(model_table)])

metrics = matrix(NA,ncol=length(p_metrics[,1]),nrow=length(bd_study[,1]))
colnames(metrics) = p_metrics[,1]

p_model = data.frame(variables = colnames(model_table)[6:length(model_table)])
mean_sd_table = data.frame()

col_odd_even <- seq_len(ncol(volume_table)) %% 2 

# Firstly, the measures are compared in the first and in the second scan with 
# the purpose of identifying baseline difference.

# On the one hand, the p-value (uncorrected and adjusted using the false discovery
# rate method) is computed as well as the mean and the standard deviation.

c = 0
for (i in 1:length(p_metrics[,1])) {
  for (j in 1:2) {
    if (j==1) {
      metric="Before"
      idx_odd_even=1
    } else if (j==2) {
      metric="After"
      idx_odd_even=0
    }
    k = seq(1 + (j+1)%%2,length(p_metrics[,1])*2,2)
    variable = colnames(volume_table)[k[i]]
    p = homogeneity_of_means(variable,volume_table)
    p_metrics[i,1+j] = p
    if (p<0.05) {
      c=c+1
      mean_sd_table[c,1] = gsub("  bis","",variable)
      mean_sd_table[c,2] = metric
      mean_sd_table[c,3] = mean_sd(variable,"Case",volume_table[,col_odd_even==idx_odd_even])  
      mean_sd_table[c,4] = mean_sd(variable,"Control",volume_table[,col_odd_even==idx_odd_even])
      mean_sd_table[c,5] = round(p,digits=3)
      
      variable_cases = cases(variable,volume_table)
      variable_controls = controls(variable,volume_table)
      boxplots_metrics_of_change(variable_cases,variable_controls)
    }
  }
}

# The adjusted p-value is computed
p_metrics_corr[,2] = p.adjust(p_metrics[,2],"BH")
p_metrics_corr[,3] = p.adjust(p_metrics[,3],"BH")


# Then, the process is repeated to assess the following metrics of change
metrics_of_change = c("Before","After","Difference","Rate_of_change","Percent_change","SPC","SPC_year")
years = demographic_table[,which(colnames(demographic_table)=="Years between scans")]
for (i in 3:length(metrics_of_change)) { 
  func = get(metrics_of_change[i])
  metric = gsub("_"," ",metrics_of_change[i])
  for (j in 1:length(colnames(metrics))) {
    k = (j*2)-1
    v1 = volume_table[,k]
    v2 = volume_table[,k+1]
    metrics[,j] = func(v1,v2,years)
    variable = colnames(metrics)[j]
    p=homogeneity_of_means(variable,metrics)
    p_metrics[j,i+1] = p
    if (p<0.05) {
      c=c+1
      mean_sd_table[c,1] = variable
      mean_sd_table[c,2] = metric
      mean_sd_table[c,3] = mean_sd(variable,"Case",metrics)  
      mean_sd_table[c,4] = mean_sd(variable,"Control",metrics)
      mean_sd_table[c,5] = round(p,digits=3)
      
      idx_cases = bd_study$f.group=="Case"
      idx_controls = bd_study$f.group=="Control"
      variable_cases = metrics[idx_cases,j]
      variable_controls = metrics[idx_controls,j]
      boxplots_metrics_of_change(variable_cases,variable_controls)
    }
  }
  p_metrics_corr[,i+1] = p.adjust(p_metrics[,i+1],"BH")
  metrics = matrix(NA,ncol=length(p_metrics[,1]),nrow=length(bd_study[,1]))
  colnames(metrics) = p_metrics[,1]
}

colnames(p_metrics_corr)=c("Variables",gsub("_"," ",metrics_of_change))
p_metrics_corr[,1]=trim(p_metrics_corr[,1])

if (length(mean_sd_table)>0) {
  mean_sd_table[,1]=trim(mean_sd_table[,1])
  for (i in 2:length(p_metrics[1,])) {
    column_name = colnames(p_metrics_corr)[i]
    if (column_name %in% mean_sd_table[,2]) {
      idx_row_metric = which(mean_sd_table[,2]==column_name)
      variable_list = mean_sd_table[idx_row_metric,1]
      for (variable in variable_list) {
        variable = trim(variable)
        idx_row = which(p_metrics_corr[,1]==variable)
        idx_col = which(colnames(p_metrics_corr)==column_name)
        p_corr = p_metrics_corr[idx_row,idx_col]
        idx_row_variable = which(mean_sd_table[,1]==variable)
        idx_row = intersect(idx_row_variable,idx_row_metric)
        mean_sd_table[idx_row,6]=round(p_corr,3) 
      }
    }
  }
  
  colnames(mean_sd_table)=c("Variable","Analysis","Case (M±SD)","Control (M±SD)","Puncorr","Pcorr")
  
}



# The LME is built
colnames(model_table) = make.names(colnames(model_table))
model_table$Group = as.factor(model_table$Group)
model_table$Sex = as.factor(model_table$Sex)
model_table$eid = as.factor(model_table$eid)
model_table$IMD = as.factor(model_table$IMD)
for (i in c(2,6:length(model_table[1,]))){
  model_table[,i]=as.numeric(model_table[,i])
}


for (i in 1:length(p_model[,1])) { 
  variable = colnames(model_table)[i+5]
  frml = as.formula(paste0(variable," ~ Group + Age + Sex + Group*Age"))
  tryCatch(
    expr = {
      model = lme(frml, random =~Age|eid,data = model_table,na.action=na.exclude,
                  control=lmeControl(msMaxIter=100,opt='optim'))
    },
    error = function(e) {
    }
  )
  summary = summary(model)
  
  # From the model, we extract the uncorrected p-value for the Group-Age interaction
  p = summary$tTable[5,5]
  p_model[i,2]=p
  if (p<0.05) {
    print(variable)
    print(summary)
    # PLOT
    variable=trim(variable)
    if (focus=="Cortical and Subcortical") {
      ylab = bquote(.(gsub("\\."," ",variable))~~~(mm^3))
    } else if (focus=="MD") {
      ylab = bquote(.(gsub("\\."," ",variable))~~~(mm^2)/2)
    } else {
      ylab = gsub("\\."," ",variable)
    }
    title = gsub("\\."," ",variable)
    title = gsub("  "," ",title)
    
    filename = paste0("LME_",metric,"_",tolower(variable),".png")
    mypath = paste0(results_path,"/",toupper(analysis),"/",focus,"/LME/",filename)
    png(file=mypath, res = 300, units = "mm", width=418, height = 210)
    
    plot=ggplot(model_table, aes(x = Age, y = model_table[[variable]],color = Group)) +
      geom_point(aes(color = Group,alpha=0.0003)) + geom_line(aes(group=eid, color=Group,alpha=0.0003)) +
      xlab("Age (years)") + ylab(ylab) + guides(alpha = "none") +
      ggtitle(title)+ theme(plot.title = element_text(hjust = 0.5,size = 30),text = element_text(size = 18)) +
      geom_smooth(method="lm",aes(fill=Group))
    
    print(plot)
    dev.off()
  }
  print(paste(i,"/",length(p_metrics[,1])))
}

p_metrics[,length(metrics_of_change)+2]=p_model[,2]
p_metrics_corr[,length(metrics_of_change)+2] = p.adjust(p_metrics[,length(metrics_of_change)+2],"BH")

colnames(p_metrics) = c("Variables",metrics_of_change,"LME")
colnames(p_metrics_corr) = colnames(p_metrics)
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# SOCIODEMOGRAPHIC ANALYSIS: IMD CORRELATION
#-------------------------------------------------------------------------------

# In this script the Pearson's correlation coefficient is computed for the IMD
# against the SPC per year in both groups. Then, it is assessed if both correlation
# coefficients are significantly different. Finally, the scatterplot is saved in 
# the cases when there is a significant difference.


source("functions.R")

# We create a table to facilitate the analysis.
m=2
IMD = bd_study$f.26410.0.0

model_table_socio = model_table[,1:4]
model_table_socio = cbind(model_table_socio,IMD)
model_table_socio$Age = as.numeric(model_table_socio$Age)


idx = which(colnames(demographic_table)=="Years between scans")
years = demographic_table[,idx]


len = length(colnames(model_table))-5
for (j in 1:len) {
  k = (j*2)-1
  v1 = volume_table[,k]
  v2 = volume_table[,k+1]
  column = data.frame(SPC_year(v1,v2,years))
  print(paste("SPC per year",colnames(volume_table)[k]))
  model_table_socio = cbind(model_table_socio,column)
  len = length(colnames(model_table_socio))
  colnames(model_table_socio)[len]= colnames(column)=paste("SPC per year",colnames(volume_table)[k])
}

len = length(model_table_socio[,1])/2
model_table_socio = model_table_socio[1:len,]
colnames(model_table_socio) = make.names(colnames(model_table_socio))


len = length(colnames(model_table))
p_metrics=as.data.frame(data.frame(colnames(model_table)[6:len]))
p_metrics_corr = as.data.frame(p_metrics)


# We compute and compare the correlation coefficients.
len = length(colnames(model_table_socio))-5
for (i in 1:len) { 
  variable = colnames(model_table_socio)[i+5]
  corr_control_summary = cor.test(model_table_socio[model_table_socio$Group=="Control",]$IMD,model_table_socio[model_table_socio$Group=="Control",variable],method = "pearson",exact=FALSE)
  corr_case_summary = cor.test(model_table_socio[model_table_socio$Group=="Case",]$IMD,model_table_socio[model_table_socio$Group=="Case",variable],method = "pearson",exact=FALSE)
  corr_control = corr_control_summary$estimate
  corr_case = corr_case_summary$estimate
  
  test = cocor.indep.groups(r1.jk=corr_case, r2.hm=corr_control, n1=552, n2=559, alternative="two.sided", alpha=0.05, conf.level=0.95, null.value=0)
  p = test @fisher1925$p.value
  p_metrics[i,2] = p

  if (p<0.05) {
    print(corr_control_summary)
    print(corr_case_summary)
    print(test)
    # We plot the significant coefficients.
    title = gsub("\\."," ",variable)
    title = gsub("SPC per year ","",title)
    plot=ggplot(model_table_socio, mapping= aes(x=IMD,y=model_table_socio[[variable]],color=Group,alpha=0.03))+
      geom_point() + 
      labs(x="IMD (a.u.)",y="SPC per year") + ggtitle(title) +
      theme(plot.title = element_text(hjust = 0.5,size = 30),
            text = element_text(size = 35)) +
      scale_color_manual(values = c("Case" = "#F8766D",
                                    "Control"="#008FC4")) +
      guides(alpha = "none")
    filename = paste0("scatterplot_",tolower(gsub("\\."," ",variable)),".png")
    mypath = file.path(paste0(results_path,"/",toupper(analysis),"/",focus,"/CORRELATION/",filename))
    png(file=mypath, res = 300, units = "mm", width=418, height = 210)
    print(plot)
    dev.off()
  }
  print(paste(i,"/",length(colnames(model_table_socio)[i+5])))
}

# We adjust the uncorrected p-values.
colnames(p_metrics)= c("Variables","p-value")
p_metrics_corr[,2] = p.adjust(p_metrics[,2],"BH")
#-------------------------------------------------------------------------------
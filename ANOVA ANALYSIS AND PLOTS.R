#-------------------------------------------------------------------------------
# SEX AND SOCIODEMOGRAPHIC ANALYSIS: ANOVA AND POST-HOC ANALYSIS
#-------------------------------------------------------------------------------

# In this code, an ANOVA analysis and the corresponding post-hoc analysis is 
# implemented to analyse the effect of categorical variables such us the sex and
# the sociodemographic quartile of the participants in the change of the different 
# MRI-derived measures calculated by means of the symmetrized Percent Change per
# Year



source("functions.R")

mean_sd_list = data.frame()
conf_interval = data.frame()

# A table is generated to facilitate the analysis
m=1
IMD = bd_study$f.26410.0.0

model_table_socio = model_table[,1:4]
model_table_socio = cbind(model_table_socio,IMD)

idx = which(colnames(demographic_table)=="Years between scans")
years = demographic_table[,idx]

len = length(colnames(model_table))-5
for (j in 1:len) {
  l = (j*2)-1
  v1 = volume_table[,l]
  v2 = volume_table[,l+1]
  column = data.frame(SPC_year(v1,v2,years))
  model_table_socio = cbind(model_table_socio,column)
  len = length(colnames(model_table_socio))
  colnames(model_table_socio)[len]= colnames(column)=paste("SPC per year",colnames(volume_table)[l])
}

len = length(model_table_socio[,1])/2
model_table_socio = model_table_socio[1:len,]
colnames(model_table_socio) = make.names(colnames(model_table_socio))

IMD_cat = model_table[,5]
model_table_socio = cbind(model_table_socio,IMD_cat)


len = length(colnames(model_table))
p_metrics=as.data.frame(data.frame(colnames(model_table)[6:len]))
p_metrics_corr = as.data.frame(p_metrics)


c=0
d = 0
for (i in 1:length(p_metrics[,1])) { 
  variable = colnames(model_table_socio)[i+5]
  
  # The subjects are divided into the groups
  if (n==1) {
    interaction_column = "GroupSex"
    model_table_socio[[interaction_column]] = interaction(model_table_socio$Group,model_table_socio$Sex)
    frml = as.formula(paste0(variable," ~ Group + Sex + Group*Sex"))
  } else if (n==2) {
    interaction_column = "GroupIMD_cat"
    model_table_socio[[interaction_column]] = interaction(model_table_socio$Group,model_table_socio$IMD_cat)
    frml = as.formula(paste0(variable," ~ Group + IMD_cat + Group*IMD_cat"))
  }
  
  # The anova test is run to extract the p-values
  model = homogeneity_of_more_than_two_means(variable,model_table_socio)
  summary = summary(model)
  p = summary[[1]][3,5]
  p_metrics[i,2]=p
  
  # The variables which have a p-value lower than the significance level are plotted
  # in boxplots.
  if (p < 0.05) {
    print(variable)
    print(summary)
    d=d+1
    mean_sd_list[d,1]=variable
    if (n==1) {
      mean_sd_list[d,2]=mean_sd(variable,"Case.Male",model_table_socio)
      mean_sd_list[d,3]=mean_sd(variable,"Control.Male",model_table_socio)
      mean_sd_list[d,4]=mean_sd(variable,"Case.Female",model_table_socio)
      mean_sd_list[d,5]=mean_sd(variable,"Control.Female",model_table_socio)
      colnames(mean_sd_list)=c("Variables","Case.Male","Control.Male","Case.Female","Control.Female")
    } else if (n==2) {
      mean_sd_list[d,2]=mean_sd(variable,"Case.1",model_table_socio)
      mean_sd_list[d,3]=mean_sd(variable,"Control.1",model_table_socio)
      mean_sd_list[d,4]=mean_sd(variable,"Case.2",model_table_socio)
      mean_sd_list[d,5]=mean_sd(variable,"Control.2",model_table_socio)
      mean_sd_list[d,6]=mean_sd(variable,"Case.3",model_table_socio)
      mean_sd_list[d,7]=mean_sd(variable,"Control.3",model_table_socio)
      mean_sd_list[d,8]=mean_sd(variable,"Case.4",model_table_socio)
      mean_sd_list[d,9]=mean_sd(variable,"Control.4",model_table_socio)
      colnames(mean_sd_list)=c("Variables","Case.1","Control.1","Case.2","Control.2","Case.3","Control.3","Case.4","Control.4")
    }
    
    
    if (n==1) {
      plot=ggplot(aes(y = as.numeric(.data[[variable]]), x = GroupSex), data = model_table_socio) + 
        geom_boxplot() + xlab("") +ylab("SPC per year") + 
        ggtitle(toupper(gsub("SPC per year ","",gsub("\\."," ",variable)))) + 
        theme(plot.title = element_text(hjust = 0.5,size = 30),
              text = element_text(size = 35))
    } else if (n==2) {
      plot=ggplot(aes(y = as.numeric(.data[[variable]]), x = GroupIMD_cat), data = model_table_socio) + 
        geom_boxplot() + xlab("") +ylab("SPC per year") + 
        ggtitle(toupper(gsub("SPC per year ","",gsub("\\."," ",variable)))) + 
        theme(plot.title = element_text(hjust = 0.5,size = 30),
              text = element_text(size = 35))
    }
    
    # The boxplots are saved
    filename = paste0("boxplot_",tolower(variable),".png")
    mypath = file.path(paste0(results_path,"/",toupper(analysis),"/",focus,"/BOXPLOTS/",filename))
    png(file=mypath, res = 300, units = "mm", width=418, height = 210)
    print(plot)
    dev.off()
    
    
    # The post-hoc analysis is run
    post_hoc = stats::TukeyHSD(model)
    if (n==1) {
      post_hoc=TukeyHSD(model, "Group:Sex", ordered = TRUE)
      summary = post_hoc$`Group:Sex`
    } else if (n==2) {
      post_hoc=TukeyHSD(model, "Group:IMD_cat", ordered = TRUE)
      summary = post_hoc$`Group:IMD_cat`
    }

    
    # From the post-hoc analysis, we keep the adjusted p-value and the 95% Confidence
    # of Interval of the pairs that are significantly different.
    
    p_adj = summary[,4]
    for (j in 1:length(p_adj)) {
      p = p_adj[j]
      if (p<0.05) {
        c=c+1
        variable = gsub("\\."," ",variable)
        variable = gsub("SPC per year ","",variable)
        conf_interval[c,1]=variable
        conf_interval[c,2]=rownames(summary)[j]
        conf_interval[c,3]=paste0("(",formatC(summary[j,2],format="e",3),",",formatC(summary[j,3],format="e",3),")")
        conf_interval[c,4]=round(summary[j,4],5)
        colnames(conf_interval)=c("Variables","","95% CI","Padj")
      }
    }
    
    # We also plot and save a summary of the post-hoc analysis
    # title = paste("95% family-wise confidence level: \n",variable)
    # 
    # str(post_hoc)
    # plot=ggiraphExtra::ggHSD(post_hoc) + 
    #   ggtitle(title) +
    #   theme(axis.text.y = element_text(size = 8, angle = 0,hjust=1),
    #         plot.title = element_text(hjust = 0.5),
    #         axis.text.x = element_text(size = 8))
    # filename = paste0("TukeyHSD_",tolower(variable),".png")
    # mypath = paste0(results_path,"/",toupper(analysis),"/",focus,"/POST-HOC COMPARISONS/",filename)
    # png(file=mypath, res = 300, units = "mm", width=418, height = 210)
    # print(plot)
    # dev.off()
  }
}
#-------------------------------------------------------------------------------
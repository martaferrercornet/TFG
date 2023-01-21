#-------------------------------------------------------------------------------
# FUNCTIONS
#-------------------------------------------------------------------------------

# In this section, different functions are defined. This functions are used in
# the other scripts

#-------------------------------------------------------------------------------
# The trim function is used to eliminate spaces at the beggining and at the end
# of the character
trim <- function( x ) {
  gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}

#-------------------------------------------------------------------------------

# This two functions are able to identify the measures from COVID patients and HC
# from a database
cases = function(variable,table) {
  column = table[,which(colnames(table)==variable)]
  idx = bd_study$f.group=="Case"
  return(column[idx])
}

controls = function(variable,table) {
  column = table[,which(colnames(table)==variable)]
  idx = bd_study$f.group=="Control"
  return(column[idx])
}

#-------------------------------------------------------------------------------

# The following functions are used to compute the mean, the standard deviation and
# other statistical indicators

mean_sd_range = function(variable,group,main_table) {
  column = main_table[,which(colnames(main_table)==variable)]
  if (group=="Case") {
    column_group = cases(variable,main_table)
  }
  else if (group=="Control") {
    column_group =controls(variable,main_table)
  }
  
  mean = round(mean(column_group,na.rm=T),digits=1)
  sd = round(sd(column_group,na.rm=T),digits=1)
  min = round(min(column_group,na.rm=T),digits=1)
  max = round(max(column_group,na.rm=T),digits=1)
  
  text = paste0(as.character(mean)," ± ",as.character(sd)," (",
                as.character(min)," - ",as.character(max),")")
  return(text)
}   

mean_sd = function(variable,group,main_table) {
  
  if (group=="Case") {
    column_group = cases(variable,main_table)
  }
  else if (group=="Control") {
    column_group = controls(variable,main_table)
  } else {
    idx = which(main_table[[interaction_column]]==group)
    column_group = main_table[[variable]][idx]
  }
  
  
  
  if (identical(main_table,demographic_table)) {
    mean = round(mean(column_group,na.rm=T),digits=1)
    sd = round(sd(column_group,na.rm=T),digits=1)
  } else {
    if (mean(column_group,na.rm=T)<0.001) {
      mean = formatC(mean(column_group,na.rm=T), format = "e", digits = 2)
    } else {
      mean = round(mean(column_group,na.rm=T),digits=3)
    }
    if (sd(column_group,na.rm=T)<0.009) {
      sd = formatC(sd(column_group,na.rm=T), format = "e", digits = 2)
    } else {
      sd = round(sd(column_group,na.rm=T),digits=3)
    }
  }
  
  
  text = paste0(as.character(mean)," ± ",as.character(sd))
  return(text)
} 

percentages = function(variable,group,main_table) {
  column = main_table[,which(colnames(main_table)==variable)]
  tab = table(column,bd_study$f.group)
  tab = addmargins(tab)
  if (group=="Case") {
    column_group = cases(variable,main_table)
    idx_col = which(colnames(tab)==group)
  }
  else if (group=="Control") {
    column_group = controls(variable,main_table)
    idx_col = which(colnames(tab)==group)
  } 
  classe1 = tab[1,idx_col]
  classe2 = tab[2,idx_col]
  total = tab[3, idx_col]
  
  perc1 = round(classe1*100/total,1)
  perc2 = round(classe2*100/total,1) 
  
  text = paste0(as.character(classe1)," (",
                as.character(perc1),"%) / ",as.character(classe2)," (",as.character(perc2),"%)")
  return(text)
}

diabetes = function(variable,group,main_table) {
  column = main_table[,which(colnames(main_table)==variable)]
  if (group=="Case") {
    column_group = cases(variable,main_table)
  }
  else if (group=="Control") {
    column_group = controls(variable,main_table)
  } 
  table = table(column_group)
  table = addmargins(table)
  yes = table[2]
  total = table[3]
  perc = round(yes*100/total,1)
  
  text = paste0(as.character(yes)," (",as.character(perc),"%)")
  return(text)
}

#-------------------------------------------------------------------------------

# The different metrics of change are defined

Difference = function(v1,v2,years) {
  return(v2-v1)
}

Rate_of_change = function(v1,v2,years) {
  return((v2-v1)/years)
}

Percent_change = function(v1,v2,years) {
  return((v2-v1)*100/v1)
}

SPC = function(v1,v2,years) {
  return((v2 - v1)/((v2 + v1)/2))
}

SPC_year = function(v1,v2,years) {
  return(((v2 - v1)/((v2 + v1)/2))/years)
}

#-------------------------------------------------------------------------------

# The following functions are used to compute different statistical tests. This
# functions study the data to identify if they meet the applicability conditions
# of the tests. Based on this, the algorithm applies the indicated test.

homogeneity_of_means = function(variable,table) {
  groups = tolower(names(table(bd_study$f.group)))
  for (i in 1:length(groups)) {
    groups[i] = paste0(groups[i],"s")
  }
  variable_1_name = get(groups[1])
  variable_2_name = get(groups[2])
  variable_1 = variable_1_name(variable,table)
  variable_2 = variable_2_name(variable,table)
  
  
  inf = sum(is.infinite(variable_1))
  if (inf!=0) {
    variable_1[is.infinite(variable_1)]=max(variable_1[is.finite(variable_1)])
  }
  inf = sum(is.infinite(variable_2))
  if (inf!=0) {
    variable_2[is.infinite(variable_2)]=max(variable_2[is.finite(variable_2)])
  }
  
  p_normal_1 = shapiro.test(variable_1)$p.value
  p_normal_2 = shapiro.test(variable_2)$p.value
  n_1 = sum(!is.na(variable_1))
  n_2 =  sum(!is.na(variable_2))
  # si es compleixen les applicability conditions
  if ( ((p_normal_1>0.05) & (p_normal_2>0.05)) |
       (((n_1>30) & (n_2>30)))) {
    # mirar si tenen variància semblant
    p_var = var.test(variable_1,variable_2)$p.value
    if (p_var<0.05) {
      var_value = F
    } else {
      var_value = T
    }
    # t-test per mirar si la feature és significativament diferent
    p_test = t.test(variable_1,variable_2,var.equal=var_value)$p.value
    
    # si no es compleixen les applicability conditions
  } else {
    p_test = wilcox.test(variable_1,variable_2,exact = FALSE)$p.value
  }
  return(p_test)
}


homogeneity_of_proportions = function(variable,table) {
  column = table[,which(colnames(table)==variable)]
  tab = table(column,bd_study$f.group)
  tab_expected = chisq.test(tab)$expected
  vector_expected = as.numeric(as.matrix(tab_expected))
  # si les applicability conditions no es compleixen
  if (any(vector_expected<5)) {
    p_test = fisher.test(tab)$p.value
    # si les applicability conditions sí es compleixen
  } else {
    p_test = chisq.test(tab)$p.value
  }
  return(p_test)
}

homogeneity_of_more_than_two_means = function(variable,table) {
  p_normal=data.frame()
  p_variance=data.frame()
  len_categories=data.frame()
  categories = names(table(table[[interaction_column]]))
  for (i in 1:length(categories)) {
    idx = which(table[[interaction_column]]==categories[i])
    assign(as.character(categories[i]),as.numeric(table[[variable]][idx]))
    p_normal[i,1]=shapiro.test(get(categories[i]))$p.value
    len_categories[i,1]=length(get(categories[i]))
  }
  
  if (all(len_categories>30)) {
    if(all(p_normal>0.05)) {
      for (i in 1:length(categories)) {
        p_variance=bartlett.test(table[[variable]] ~ table[[interaction_column]],data=table)$p.value
      }
    } else {
      for (i in 1:length(categories)) {
        p_variance=fligner.test(table[[variable]] ~ table[[interaction_column]],data=table)$p.value
      }
    }
    if (all(p_variance>0.05)) {
      model = aov(frml,data=model_table_socio)
    } else {
      model = aov(frml,data=model_table_socio)
      message("Not equal variances.")
    }
  } else {
    message("Applicability conditions for ANOVA test are not meet.")
  }
  return(model)
}


#-------------------------------------------------------------------------------

# Function to plot and save boxplots
boxplots_metrics_of_change = function(variable_cases,variable_controls) {
  data = data.frame(Group=c(rep("SARS-CoV-2",length(variable_cases)),
                            rep("Controls",length(variable_controls))),
                    Measure=c(variable_cases,variable_controls))
  if (grepl("(\\s)$",variable)) {
    variable = stringr::str_trim(variable,side="right")
  } else if (grepl("bis",variable)) {
    variable = str_squish(variable)
  }
  variable=gsub("bis","",variable)
  if ((metric=="Before") | (metric=="After")) {
    title=paste0(variable," (",metric,")")
    if (focus=="Cortical and Subcortical") {
      ylab = bquote(.(gsub("\\."," ",variable))~~~~(mm^3))
    } else if (focus=="MD") {
      ylab = bquote(.(gsub("\\."," ",variable))~~~~(mm^2/s))
    } else {
      ylab = gsub("\\."," ",variable)
    }
  } else {
    if (metric=="Percent_change") {
      ylab=gsub("_"," ",metric)
      ylab=paste(ylab,"(%)")
    }
    ylab=gsub("_"," ",metric)
    title=toupper(gsub("bis","",variable))
  }
  
  plot=ggplot(data,aes(x=Group,y=Measure)) + geom_boxplot() + 
    ggtitle(title) + theme(plot.title = element_text(hjust = 0.5)) +
    xlab("") + ylab(ylab) + theme(text = element_text(size = 35),
                                  plot.title = element_text(size = 30))
  filename = paste0("boxplot_",metric,"_",tolower(variable),".png")
  mypath = file.path(paste0(results_path,"/",toupper(analysis),"/",focus,"/BOXPLOTS/",filename))
  png(file=mypath, res = 300, units = "mm", width=418, height = 210)
  print(plot)
  dev.off()
}
#-------------------------------------------------------------------------------

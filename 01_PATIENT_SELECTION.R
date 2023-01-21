#-------------------------------------------------------------------------------
# DATA LOADING AND PARTICIPANT SELECTION
#-------------------------------------------------------------------------------

# In the following code, the data from the UK Biobank is imported and a selection
# process is performed in order to choose the subjects which are suitable for the
# analysis.


# The file containing the R session with the full database is loaded.
# This step can take some minutes.

# load("bd.RData")

# Then, only the participants of the COVID-19 repeat imaging study are
# selected, and labelled as HC or COVID.

# c = 0
# f.group = data.frame()
# for (i in 1:length(bd$f.41000.3.0)) {
#   cell = bd$f.41000.3.0[i]
#   if (is.na(cell)==FALSE) {
#     last_digit = cell - round(cell, -1)
#     if (last_digit == 1) {
#       c = c+1
#       f.group[c,1]=i
#       f.group[c,2]="Case"
#     } else if (last_digit == 0) {
#       c = c+1
#       f.group[c,1]=i
#       f.group[c,2]="Control"
#     }
#   }
# }
# colnames(f.group)=c("Index","Group")

# bd_study = bd[f.group[,1],]
# bd_study = cbind(bd_study,f.group[,2])
# colnames(bd_study)[which(colnames(bd_study)=="f.group[, 2]")]= 'f.group'
# write.table(bd_study,"bd.csv",sep=",")

bd_study = read.table("bd.csv",header=TRUE,sep=",")

# A table with all the measures is loaded
volumes = read.table("volume_columns_1.csv",header=F,sep=";")

volume_table = data.frame()
for (i in 1:length(volumes[,1])) {
  colname = volumes[i,2]
  column = bd_study[[colname]]
  volume_table[1:length(column),i]=column
}
colnames(volume_table)=volumes[,1]

for (i in seq(2,length(colnames(volume_table)),2)) {
  colnames(volume_table)[i]=paste(volumes[i,1],"bis")
}

# averaging of the measures that were calculated for the left and right hemisphere
new_volume_table = data.frame()
colnames=character()
for (i in 1:length(colnames(volume_table))) {
  title = colnames(volume_table)[i]
  len = length(colnames(new_volume_table))
  if  (!grepl("bis",title) & grepl("(left)",title)) {
    left = volume_table[,i]
    left_bis = volume_table[,i+1]
    right = volume_table[,i+2]
    right_bis = volume_table[,i+3]
    name = substring(title,1,tail(unlist(gregexpr('l', title)), n=1)-2)
    name_bis = paste0(name," bis")
    mean = (left+right)/2
    mean_bis = (left_bis+right_bis)/2
    colnames = c(colnames,name)
    colnames = c(colnames,name_bis)
    new_volume_table[,len+1]=mean
    new_volume_table[,len+2]=mean_bis
  } 
  else if ( !grepl("(left)",title) & !grepl("(right)",title) ) {
    new_volume_table[1:length(volume_table[,1]),len+1]=volume_table[,i]
    name = colnames(volume_table)[i]
    colnames = c(colnames,name)
  } 
}

colnames(new_volume_table)=colnames
volume_table = new_volume_table
volumes = colnames(volume_table)


# A table with a friendly structure to build models is also created. This table 
# includes the MRI-derived measures and also personal data (eid, age, sex, group and IMD).
# Each row of the table is devoted to one MRI scan from a participant.

model_table = matrix(NA,ncol=length(volumes)/2,
                     nrow=length(bd_study[,1])*2)
colnames(model_table) = volumes[seq(1,length(volumes),2)]

for (i in 1:length(model_table[1,])) {
  j = (i*2)-1
  pre_column = volume_table[,j]
  post_column = volume_table[,j+1]
  column = c(pre_column,post_column)
  model_table[,i] = column
}

eid_age_group_sex_IMD = matrix(NA,ncol=5,nrow=length(model_table[,1]))
eid_age_group_sex_IMD[,1] = c(bd_study$f.eid,bd_study$f.eid)
eid_age_group_sex_IMD[,2] = c(bd_study$f.21003.2.0,bd_study$f.21003.3.0)
eid_age_group_sex_IMD[,3] = c(bd_study$f.group,bd_study$f.group)
eid_age_group_sex_IMD[,4] = c(bd_study$f.31.0.0,bd_study$f.31.0.0)

IMD_england = bd_study$f.26410.0.0

breaks = quantile(IMD_england, na.rm=T, c(0.25,0.5,0.75,1))
IMD = cut(IMD_england, breaks = c(0,breaks),labels=c(0,1,2,3))

eid_age_group_sex_IMD[,5] = c(IMD,IMD)
colnames(eid_age_group_sex_IMD) = c("eid","Age","Group","Sex","IMD")

model_table = cbind(eid_age_group_sex_IMD,model_table)
model_table = as.data.frame(model_table)


if (analysis=="Sociodemographic") {
  ind_eng = !is.na(model_table$IMD[1:length(bd_study[,1])])
  eid_eng = model_table$eid[ind_eng]
  model_table = model_table[model_table$eid %in% eid_eng,]
  volume_table = volume_table[ind_eng,]
  bd_study = bd_study[ind_eng,]
}


# Patients with pathologies listed in the neuro_columns_1.csv file are identified
# and removed from the dataset

neuro_columns = read.table(file="neuro_columns_1.csv",header = F, sep = ";",quote = "")

cancer_codes = data.frame(cancer = c("Brain cancer / primary malignant brain tumour",
                                     "meningeal cancer / malignant meningioma",
                                     "Spinal cord or cranial nerve cancer",
                                     "Retinoblastoma",
                                     "Peripheral nerve/automic nerve cancer",
                                     "Eye and/or adnexal cancer"),
                          code = c(1032,1031,1033,1075,1029,1030))

mental_health_codes = data.frame(mental_health_problem = c("Social anxiety or social phobia",
                                                           "Schizophrenia",
                                                           "Any other type of psychosis or psychotic illness",
                                                           "A personality disorder",
                                                           "Any other phobia (eg disabling fear of heights or spiders)",
                                                           "Panic attacks",
                                                           "Obsessive compulsive disorder (OCD)",
                                                           "Mania, hypomania, bipolar or manic-depression",
                                                           "Depression",
                                                           "Bulimia nervosa",
                                                           "Psychological over-eating or binge-eating",
                                                           "Autism, Asperger's or autistic spectrum disorder",
                                                           "Anxiety, nerves or generalized anxiety disorder",
                                                           "Anorexia nervosa",
                                                           "Agoraphobia",
                                                           "Attention deficit or attention deficit and hyperactivity disorder (ADD/ADHD)"),
                                 code = c(1,2,3,4,5,6,7,10,11,12,13,14,15,16,17,18))

eliminated_patients = data.frame()
c = 0
for (i in 1:length(bd_study[,1])) {
  for (j in 1:length(neuro_columns[,1])) {
    colname = neuro_columns[j,2]
    cell = bd_study[[colname]][i]
    if (is.null(cell)==T) {
      stop(paste("The program is not prepare to remove patients with",neuro_columns[j,1]))
    }
    else if(is.na(cell)==F) {
      if (grepl("-",cell) & ((grepl("20",cell))|(grepl("19",cell))) ) {
        c = c+1
        eliminated_patients[c,1] = bd_study$f.eid[i]
        eliminated_patients[c,2] = neuro_columns[j,1]
        eliminated_patients[c,3] = bd_study[i,length(bd_study[1,])]
      }
      else if (grepl("f.20001",colname)) {
        if (cell %in% cancer_codes[,2]) {
          c = c+1
          idx_row = which(cancer_codes[,2]==as.character(cell))
          cancer_group = cancer_codes[idx_row,1]
          eliminated_patients[c,1] = bd_study$f.eid[i]
          eliminated_patients[c,2] = cancer_group
          eliminated_patients[c,3] = bd_study[i,length(bd_study[1,])]
        }
      }
      else if (grepl("f.20544",colname)) {
        if (cell %in% mental_health_codes[,1]) {
          c = c+1
          idx_row = which(mental_health_codes[,1]==as.character(cell))
          mental_disorder = mental_health_codes[idx_row,1]
          eliminated_patients[c,1] = bd_study$f.eid[i]
          eliminated_patients[c,2] = mental_disorder
          eliminated_patients[c,3] = bd_study[i,length(bd_study[1,])]
        }
      }
      else if ((colname=="f.20483.0.0") | (colname=="f.20401.0.0")) {
        if (as.character(cell)=="Yes") {
          c = c+1
          eliminated_patients[c,1] = bd_study$f.eid[i]
          eliminated_patients[c,2] = neuro_columns[j,1]
          eliminated_patients[c,3] = bd_study[i,length(bd_study[1,])]
        }
      }
    } 
  }
}

colnames(eliminated_patients) = c("eid","Reason","Group")
eid_remove = data.frame(unique(eliminated_patients[,1]))

# Participants which do not have the data from at least one of the scans are found
# and removed from the dataset.

len = length(eid_remove[,1])
c = 0
for (i in 1:length(model_table[,1])) {
  cells = model_table[i,6:length(model_table[1,])]
  
  if ( (sum(is.na(cells))) >= 54) {
    c = c+1
    eid_remove[c+len,1] = model_table$eid[i]
  }
}


# In this step, all the identified participants in the previous sections are excluded
# from all the tables and databases

eid_remove = unique(eid_remove)

model_table = model_table[!(model_table$eid %in% eid_remove[,1]),]
volume_table = volume_table[!(bd_study$f.eid %in% eid_remove[,1]),]
bd_study = bd_study[!(bd_study$f.eid %in% eid_remove[,1]),]

# According to the input introduced by the user, the program selects the variables
# that were chosen, and excludes the others.

if (focus=="FA") {
  volumes = colnames[119:172]
} else if (focus=="MD") {
  volumes = colnames[173:226]
} else if (focus=="Cortical and Subcortical") {
  volumes = colnames[1:118]
}

model_table = model_table[,(colnames(model_table) %in% c("eid","Age","Group","Sex","IMD",volumes ))]
volume_table = volume_table[,(colnames(volume_table) %in% volumes),]


# A table is saved with the information of the neurological and/or psychiatric
# patients.

setwd(results_path)

if (analysis=="Sociodemographic") {
  filename = "ENG_neuropsychiatric.csv"
} else {
  filename = "UK_neuropsychiatric.csv"
}

write.table(eliminated_patients,filename,sep=",",col.names=T,row.names=F)

setwd(path)
# ------------------------------------------------------------------------------
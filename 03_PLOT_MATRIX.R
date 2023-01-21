#-------------------------------------------------------------------------------
# GRAPHICAL REPRESENTATION
#-------------------------------------------------------------------------------

# The following code graphically represents the computed p-values in a colorful
# way to rapidly identify the significant changes

row.names(p_metrics) = p_metrics[,1]
p_metrics[1] <- NULL

row.names(p_metrics_corr) = p_metrics_corr[,1]
p_metrics_corr[1] <- NULL

# The column and row names are adapted to make them fit into the plot
rownames(p_metrics) = gsub("\\."," ",rownames(p_metrics))
rownames(p_metrics) = gsub("Volume of ","",rownames(p_metrics))
rownames(p_metrics) = gsub("grey matter","GM",rownames(p_metrics))
rownames(p_metrics) = gsub("white matter","WM",rownames(p_metrics))
rownames(p_metrics) = gsub(" for head size","",rownames(p_metrics))
rownames(p_metrics) = gsub("grey","GM",rownames(p_metrics))
rownames(p_metrics) = gsub("_"," ",rownames(p_metrics))
rownames(p_metrics) = gsub("Total volume of ","",rownames(p_metrics))
rownames(p_metrics) = gsub("formerly Supplementary Motor Cortex","",rownames(p_metrics))
rownames(p_metrics) = gsub("\\() ","",rownames(p_metrics))
rownames(p_metrics) = gsub("cerebrospinal fluid","CSF",rownames(p_metrics))
rownames(p_metrics) = gsub("Mean ","",rownames(p_metrics))
rownames(p_metrics) = gsub("on FA skeleton","",rownames(p_metrics))
colnames(p_metrics) = gsub("_"," ",colnames(p_metrics))
colnames(p_metrics) = gsub("Before","Before *",colnames(p_metrics))
colnames(p_metrics) = gsub("After","After *",colnames(p_metrics))
for (i in 1:length(rownames(p_metrics))) {
  substr(rownames(p_metrics),1,1) = str_to_title(substr(rownames(p_metrics),1,1)) 
}

p_metrics = t(p_metrics)
p_metrics = as.matrix(p_metrics)

p_metrics_corr = t(p_metrics_corr)
p_metrics_corr = as.matrix(p_metrics_corr)

# We define the filenames to save the plot

if (class(n)=="numeric") {
  if ((m==2)&(n==2)) {
    filename = paste0("matrix_",tolower(analysis)," (continuous)_",tolower(focus),".png")  
  } else if (m==1) {
    filename = paste0("matrix_",tolower(analysis),"_(quantiles)_",tolower(focus),".png")
  }
} else {
  filename = paste0("matrix_",tolower(analysis),"_",tolower(focus),".png")
}
mypath = paste0(results_path,"/",toupper(analysis),"/",focus,"/",filename)

# We define the titles of the plots
if (analysis=="Sex") {
  title = paste(toupper(focus),"SEX EFFECT ANALYSIS")
} else if (analysis=="Sociodemographic") {
  if (m==2) {
    title = paste(toupper(focus),"SOCIODEMOGRAPHIC EFFECT ANALYSIS (continuous IMD)")
  } else if (m==1) {
    title = paste(toupper(focus),"SOCIODEMOGRAPHIC EFFECT ANALYSIS (IMD quartiles)")
  }
} else if (analysis=="Longitudinal") {
  title=paste(toupper(focus),toupper(analysis),"ANALYSIS")
}

print(title)

# We do the plot

if (length(rownames(p_metrics))==1) {
  png(file=mypath, res = 300, units = "mm", width=418, height = 140)
  rownames(p_metrics)="p-value"
  par(mar = c(2,4,18,1)) # par(mar = c(17,6,5,2))
  brk=200
  col_mat = plot(p_metrics, axis.col=list(side=3, las=2,cex.axis=0.8),
                 axis.row = list(side=2,las=2,cex.axis=0.9), breaks=brk, 
                 col=colorspace::diverge_hsv(brk),key=list(side=1,font=1,cex.axis=0.75), 
                 fmt.key="%.2f",polygon.key=NULL,
                 xlab="",ylab="",main="")
  title(title, adj = 0.5, line = 17)
} else {
  png(file=mypath, res = 300, units = "mm", width=418, height = 210)
  par(mar = c(17, 6, 3, 3))
  brk=100
  col_mat = plot(p_metrics, axis.col=list(side=1, las=2,cex.axis=0.75),
                 axis.row = list(side=2,las=2,cex.axis=0.75), breaks=brk, 
                 col=colorspace::diverge_hsv(brk),key=list(side=4,font=1,cex.axis=0.75), 
                 fmt.key="%.2f",polygon.key=NULL,
                 xlab="",ylab="",main=title) 
}

asterisk = readPNG(paste0(path,"/Asterisk.png"))
square = readPNG(paste0(path,"/square.png"))

for (i in 1:length(col_mat$cell.polygon)) {
  args = col_mat$cell.polygon[[i]]
  if (p_metrics[i]<0.05) {
    rasterImage(asterisk, args$x[1]+0.1, args$y[1]+0.1, args$x[3]-0.1, args$y[2]-0.1)
  } 
  if ((analysis=="Longitudinal") | (analysis=="Sociodemographic" & m==2)) {
    if (p_metrics_corr[i]<0.05) {
      rasterImage(square, args$x[1]+0.1, args$y[1]+0.1, args$x[3]-0.1, args$y[2]-0.1)
    }
  }
}

dev.off()
#-------------------------------------------------------------------------------

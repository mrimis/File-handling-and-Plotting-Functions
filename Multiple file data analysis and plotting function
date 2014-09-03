#Function for returning list of features that user wants to use in his plotting
feature_list <- function(...) {
      input_list <- list(...)
      return(input_list)
  }
  
  
#Function for extracting user selected features data from feature table of multiple files 
#ftr_extr_multiple_file(path_in="<path of folder of feature data to be extracted>",feature_list("<name of features separated by comma>")
ftr_extr_multiple_file<- function(path_in,FUN=feature_list(...)){
  ftr_list=unlist(FUN)
all_files = list.files(path_in)
table_ix = grep(pattern=".txt", x=all_files)
table_names = all_files[table_ix]
dta_list=list()
for(i in table_names){
  data <-  read.table(paste(path_in,i, sep=""), header=TRUE,sep='')
  idx=c()
  for (i in 1:length(ftr_list)){
    idx[i]=grep(ftr_list[i], colnames(data))
    dta = data[idx]
    }
  dta_list[[length(dta_list)+1]] <- dta
}
return(dta_list)
}

#Function for plotting correlation plot for all the extracted image features from multiple files
#corr_ALLftr_mult_file_plot(path_in="<path of folder of feature data to be analysed>",path_out="<path of folder for plot image to be exported >")
corr_ALLftr_mult_file_plot<- function(path_in,path_out){
  all_files = list.files(path_in)
  table_ix = grep(pattern=".txt", x=all_files)
  table_names = all_files[table_ix]
  dta_list=list()
  for(i in table_names)
  {
    dta <-  read.table(paste(path_in,i, sep=""), header=TRUE,sep='')
    dta_ext=dta[c(4:ncol(dta))]
    dta_final=abs(cor(dta_ext)) # get correlations
    png(filename=paste(path_out,"scatterplot_clust_",i,"_all_feature.png",sep=""))
    corrplot(dta_final, method="circle",order="hclust",hclust.method="average" )
    dev.off()
  }
}


#Function for extracting user selected features data from feature table of multiple files 
#ftr_extr_multiple_file(path_in="<path of folder of feature data to be extracted>",feature_list("<name of features separated by comma>")
ftr_extr_multiple_file<- function(path_in=path_in_list(...),FUN=feature_list(...)){
  ftr_list=unlist(FUN)
  path_in_lst=unlist(path_in)
  for (i in 1:length(path_in_lst)){
	all_files = list.files(path_in_lst[i])
	table_ix = grep(pattern=".txt", x=all_files)
	table_names = all_files[table_ix]
	dta_list=list()
	for(j in table_names){
		data <-  read.table(paste(path_in,j, sep=""), header=TRUE,sep='')
		idx=c()
		for (k in 1:length(ftr_list)){
			idx[k]=grep(ftr_list[k], colnames(data))
			dta = data[idx]
		}
	dta_list[[length(dta_list)+1]] <- dta
	}
}
return(dta_list)
}

#Function for plotting kernel density of  one   feature selected by user for multiple files
#density_single_ftr_mult_file_plot(path_in="<path of feature data to be analysed>",path_out="<path of folder for plot image to be exported >",feature_list("<name of the feature user want to plot>"))
density_single_ftr_mult_file_plot <- function(path_in,path_out,FUN=feature_list(...)){
  all_files = list.files(path_in)
	table_ix = grep(pattern=".txt", x=all_files)
	table_names = all_files[table_ix]
	a <- ftr_extr_multiple_file(path_in,FUN)
  feature_name=colnames(a[[1]])
  for(i in 1:length(a))
  {
  dta <- sapply(a[[i]], as.numeric)
  d <- density(dta)# returns the density data
  png(filename=paste(path_out,feature_name,"_",table_names[i],"_Kernal_Density.png",sep=""))
  plot(d, xlab=feature_name,main=paste(feature_name,"- Kernal Density plot",sep=" "))
  polygon(d, col="red", border="blue")
  dev.off()
}
}


#Function for plotting density variation of selected features extracted from multiple file
library(ggplot2)
density_var_day_plot<- function(path_in=path_in_list(...),path_out,feature_name){
  ftr_list=unlist(c("day",feature_name))
  path_in_lst=unlist(path_in)
  dta_list=list()
  for (i in 1:length(path_in_lst)){
    all_files = list.files(path_in_lst[i])
    table_ix = grep(pattern=".txt", x=all_files)
    table_names = all_files[table_ix]
    for(j in 1:length(table_names)){
      data <-  read.table(paste(path_in_lst[i],table_names[j], sep=""), header=TRUE,sep='')
      idx=c()
      for (k in 1:length(ftr_list)){
        idx[k]=grep(ftr_list[k], colnames(data))
        dta = data[idx]
      }
      dta_list[[length(dta_list)+1]] <- dta
    }
  }
  final_list=do.call(rbind.data.frame, dta_list)
  Days=as.character(final_list$day)
feature=as.integer(as.matrix(final_list[2]))
title <- paste(ftr_list[2]," Kernal density variation w.r.t days",sep="")
#png(filename=paste(path_out,ftr_list[2],"_Kernal_Density.png",sep=""))
ggplot(final_list, aes(x=feature)) + geom_density(aes(group=Days, colour=Days))+ labs(title = title)
#dev.off()
}


#Function for Detailed Correlation analysis of selected features 
#The output gives text files containing correlation matrix and table containing pair-wise correlation and p-values along with plot showing number of analysis(histogram for each features + scatter plot with linear fitting + correlation significance)  for the selected features 
#detail_corr_analysis_mult_file_plot(path_in="<path of feature data to be analysed>",path_out="<path of folder for plot image to be exported >",feature_list("<name of features user want to plot>"))
detail_corr_analysis_mult_file_plot <- function(path_in,path_out,FUN=feature_list(...)){
  a <- ftr_extr_multiple_file(path_in,FUN)
  all_files = list.files(path_in)
	table_ix = grep(pattern=".txt", x=all_files)
	table_names = all_files[table_ix]
  for(i in 1:length(a))
  {
# correlation matrix 
result_df=cor_mat(a[[i]])
write.table(result_df, paste(path_out,"corr_mat",table_names[i],".txt",sep=""),sep="\t",row.names = TRUE, col.names=TRUE)

#Detail dataframe object containing correlation and p-value for each pair of features
result_df2=corr_pval_square_matrix(cor_mat(a[[i]]))

#Saving the  Detail table showing correlation and p-value for each pair of features
write.table(result_df2, paste(path_out,"corr_final_results",table_names[i],".txt",sep=""),sep="\t ",row.names = FALSE, col.names=TRUE)

 
# Save the plot
png(filename=paste(path_out,"detailed_corr_plot",table_names[i],".png",sep=""))

chart.Correlation(a[[i]])
dev.off()
}
}

#Function for histogram plot showing frequency of particular feature with the normal curve for multiple files
#hist_multiple_file_plot(path_in="<path of folder of feature data to be analysed>",path_out="<path of folder for plot image to be exported >",feature_list("<name of single feature user want to plot>"))
hist_multiple_file_plot <- function(path_in,path_out,FUN=feature_list(...)){
	all_files = list.files(path_in)
	table_ix = grep(pattern=".txt", x=all_files)
	table_names = all_files[table_ix]
	a <- ftr_extr_multiple_file(path_in,FUN)
	feature_name=colnames(a[[1]])
	for (i in 1:length(a))
	{	
		dta <- sapply(a[[i]], as.numeric)
		png(filename=paste(path_out,feature_name,"_",table_names[i],"_histogram_with_curve.png",sep=""))
		h=hist(dta,breaks=10, col="red",xlab=feature_name, main=paste(feature_name,"- histogram with normal curve",sep=" "))
		xfit<-seq(min(dta),max(dta),length=100)
		yfit<-dnorm(xfit,mean=mean(dta),sd=sd(dta))
		yfit <- yfit*diff(h$mids[1:2])*length(dta)
		lines(xfit, yfit, col="blue", lwd=2) 
		dev.off()
	}
}

#Function for returning list of parameters to be extracted from data table list
parameter_list <- function(...) {
      input_list <- list(...)
      return(input_list)
  }


#Function for returning list of path of folders containing different days feature data table that user wants to use in his plotting
path_in_list <- function(...) {
      input_list <- list(...)
      return(input_list)
  }


#Function for plotting scatter plot plot for all the selected image features from multiple files
#scatter_clust_mult_plot(path_in="<path of folder of feature data to be analysed>",feature_list("<list of features >"))
scatter_clust_mult_plot<- function(path_in,path_out,FUN=feature_list(...)){
	all_files = list.files(path_in)
	table_ix = grep(pattern=".txt", x=all_files)
	table_names = all_files[table_ix]
	a <- ftr_extr_multiple_file(path_in,FUN)
	for (i in 1:length(a))
	{
	dta=abs(cor(a[[i]])) # get correlations
	png(filename=paste(path_out,table_names[i],"_features_clust_scatterplot.png",sep=""))
	corrplot(dta, method="circle",order="hclust",hclust.method="average" )
	dev.off()
	}
}


#Function for correlation plot with pink to yellow colour shows highest to lowest correlation for user selected features
#Requires "gclus" package
#scatter_corr_plot_mult_file(path_in="<path of feature data to be analysed>",path_out="<path of folder for plot image to be exported >",feature_list("<name of the features user want to plot>"))
scatter_corr_plot_mult_file <- function(path_in,path_out,FUN=feature_list(...)){
  a <- ftr_extr_multiple_file(path_in,FUN)
  all_files = list.files(path_in)
	table_ix = grep(pattern=".txt", x=all_files)
	table_names = all_files[table_ix]
  for(i in 1:length(a))
  {
  dta.r=abs(cor(a[[i]])) # get correlations
  dta.col <- dmat.color(dta.r) # get colors
  # reorder variables so those with highest correlation
  # are closest to the diagonal
  dta.o <- order.single(dta.r)
  png(filename=paste(path_out,"scatterplot_corr",table_names[i],".png",sep=""))
  cpairs(a[[i]], dta.o, panel.colors=dta.col, gap=.5,
         main="Variables Ordered and Colored by Correlation" )
  dev.off()
}
}

#Function for simple scatter plot between two user selected features from the list of extracted features for multiple feature data files
#scatter_plot_mult_file (path_in="<path of folder containing feature data files to be analysed>",path_out="<path of folder for plot image to be exported >",feature_list("<name of the two features user want to plot>"))
scatter_plot_mult_file <- function(path_in,path_out,FUN=feature_list(...)){
  all_files = list.files(path_in)
	table_ix = grep(pattern=".txt", x=all_files)
	table_names = all_files[table_ix]
	a <- ftr_extr_multiple_file(path_in,FUN)
  feature_name=colnames(a[[1]])
  for(i in 1:length(a))
  {
  png(filename=paste(path_out,feature_name[1],"_",feature_name[2],"_",table_names[i],"_scatterplot.png",sep=""))
  plot(a[[i]],xlab=feature_name[1],ylab=feature_name[2],main=paste(feature_name[1],"_",feature_name[2],"- Scatterplot",sep=" "))
  dev.off()
}
}

#Function for 3 Dimensional scatterplot for any 3 features selected by the user from the features table of multiple file 
#Requires "scatterplot3d" package
#path_in="<path of folder of feature data to be analysed>",path_out="<path of folder for plot image to be exported >",feature_list("<name of three features user want to plot>"))
scatterplot_3D_mult_file<- function(path_in,path_out,FUN=feature_list(...)){
  a <- ftr_extr_multiple_file(path_in,FUN)
  all_files = list.files(path_in)
	table_ix = grep(pattern=".txt", x=all_files)
	table_names = all_files[table_ix]
  for(i in 1:length(a))
  {
  png(filename=paste(path_out,"scatter_",table_names[i],"_plot.png",sep=""))
  scatterplot3d(a[[i]],main="3 D scatter plot for selected features" )
  dev.off()
}
}


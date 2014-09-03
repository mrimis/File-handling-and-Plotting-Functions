#Function for Correlation square matrix generation
cor_mat <- function (X, dfr = nrow(X) - 2) {
mat<- cor(X, use="pairwise.complete.obs")
above <- row(mat) < col(mat)
mat2 <- mat[above]^2
Fstat <- mat2 * dfr/(1 - mat2)
mat[above] <- 1 - pf(Fstat, 1, dfr)
mat[row(mat) == col(mat)] <- NA
return(mat)
}

#Function for returning list of features that user wants to use in his plotting
feature_list <- function(...) {
      input_list <- list(...)
      return(input_list)
  }
  

#Function for extracting  features of single feature data matrix
ftr_extr<- function(path_in,FUN=feature_list(...)){
  ftr_list=unlist(FUN)
  data=read.table(path_in,header=TRUE, sep='') 
  idx=c()
  for (i in 1:length(ftr_list)){
  idx[i]=grep(ftr_list[i], colnames(data))
  dta = data[idx]
  }
  return(dta)
}  


#Function for plotting correlation plot for all the extracted image features
#corr_ALLftr_clust_plot(path_in="<path of feature data to be analysed>",path_out="<path of folder for plot image to be exported >")
corr_ALLftr_clust_plot<- function(path_in,path_out){
  data=read.table(path_in,header=TRUE, sep='') 
  dta=abs(cor(data)) # get correlations
  png(filename=paste(path_out,"all_feature_clust_scatterplot.png",sep=""))
  corrplot(dta, method="circle",order="hclust",hclust.method="average" )
  dev.off()
}


#Function for correlation plot with hierarchical clustering for user selected features
#Requires "corrplot" package
#corr_clust_plot(path_in="<path of feature data to be analysed>",path_out="<path of folder for plot image to be exported >",feature_list("<list containing name of features user want to analyse>"))
corr_clust_plot<- function(path_in,path_out,FUN=feature_list(...)){
  a <- ftr_extr(path_in,FUN)
  dta=abs(cor(a)) # get correlations
  png(filename=paste(path_out,"corr_clust_plot.png",sep=""))
  corrplot(dta, method="circle",order="hclust",hclust.method="average" )
  dev.off()
}


#Function for generating correlation Pvalues in the form of square matrix
corr_pval_square_matrix <- function(m) {
if( (class(m) != "matrix") | (nrow(m) != ncol(m))) stop("Must be a square matrix.")
if(!identical(rownames(m), colnames(m))) stop("Row and column names must be equal.")
ut <- upper.tri(m)
mat=data.frame(i = rownames(m)[row(m)[ut]],j = rownames(m)[col(m)[ut]],cor=t(m)[ut],p=m[ut])
return(mat)
}


#Function for plotting kernel density of  one  user selected feature 
#density_single_ftr_plot(path_in="<path of feature data to be analysed>",path_out="<path of folder for plot image to be exported >",feature_list("<name of feature user want to plot>"))
density_single_ftr_plot <- function(path_in,path_out,FUN=feature_list(...)){
  a <- ftr_extr(path_in,FUN)
  feature_name=colnames(a)
  a <- sapply(a, as.numeric)
  d <- density(a)# returns the density data
  png(filename=paste(path_out,feature_name,"_Kernal_Density.png",sep=""))
  plot(d, xlab=feature_name,main=paste(feature_name,"- Kernal Density plot",sep=" "))
  polygon(d, col="red", border="blue")
  dev.off()
}



#Function for Detailed Correlation analysis of selected features 
#The output gives text files containing correlation matrix and table containing pair-wise correlation and p-values along with plot showing number of analysis(histogram for each features + scatter plot with linear fitting + correlation significance)  for the selected features 
#detail_corr_analysis_plot(path_in="<path of feature data to be analysed>",path_out="<path of folder for plot image to be exported >",feature_list("<name of features user want to plot>"))
detail_corr_analysis_plot <- function(path_in,path_out,FUN=feature_list(...)){
a <- ftr_extr(path_in,FUN)
 
# correlation matrix 
result_df=cor_mat(a)
write.table(result_df, paste(path_out,"corr_mat.txt",sep=""),sep="\t",row.names = TRUE, col.names=TRUE)

#Detail dataframe object containing correlation and p-value for each pair of features
result_df2=corr_pval_square_matrix(cor_mat(a))

#Saving the  Detail table showing correlation and p-value for each pair of features
write.table(result_df2, paste(path_out,"corr_final_results.txt",sep=""),sep="\t ",row.names = FALSE, col.names=TRUE)

 
# Save the plot
png(filename=paste(path_out,"detailed_corr_plot.png",sep=""))

chart.Correlation(a)
dev.off()
}

#Function for histogram plot showing frequency of particular feature with the normal curve
#hist_single_ftr_plot(path_in="<path of feature data to be analysed>",path_out="<path of folder for plot image to be exported >",feature_list("<name of single feature user want to plot>"))
hist_single_ftr_plot <- function(path_in,path_out,FUN=feature_list(...)){
	a <- ftr_extr(path_in,FUN)
	feature_name=colnames(a)
	a <- sapply(a, as.numeric)
	png(filename=paste(path_out,feature_name,"_histogram_with_curve.png",sep=""))
	h=hist(a,breaks=10, col="red",xlab=feature_name, main=paste(feature_name,"- histogram with normal curve",sep=" "))
	xfit<-seq(min(a),max(a),length=100)
	yfit<-dnorm(xfit,mean=mean(a),sd=sd(a))
	yfit <- yfit*diff(h$mids[1:2])*length(a)
	lines(xfit, yfit, col="blue", lwd=2) 
	dev.off()
	}
	
	
#Function for simple scatter plot between two user selected features from the list of extracted features
#scatter_plot(path_in="<path of feature data to be analysed>",path_out="<path of folder for plot image to be exported >",feature_list("<name of the two features user want to plot>"))
scatter_plot <- function(path_in,path_out,FUN=feature_list(...)){
  a <- ftr_extr(path_in,FUN)
  feature_name=colnames(a)
  png(filename=paste(path_out,feature_name[1],"_",feature_name[2],"_scatterplot.png",sep=""))
  plot(a,xlab=feature_name[1],ylab=feature_name[2],main=paste(feature_name[1],feature_name[2],"- Scatterplot",sep=" "))
  dev.off()
}

#Function for 3 Dimensional scatterplot for any 3 features selected by the user from all the extracted features 
#Requires "scatterplot3d" package
#path_in="<path of feature data to be analysed>",path_out="<path of folder for plot image to be exported >",feature_list("<name of three features user want to plot>"))
scatterplot_3D<- function(path_in,path_out,FUN=feature_list(...)){
  a <- ftr_extr(path_in,FUN)
  png(filename=paste(path_out,"scatter_plot.png",sep=""))
  scatterplot3d(a,main="3 D scatter plot for selected features" )
  dev.off()
}


#Function for correlation plot with pink to yellow colour shows highest to lowest correlation for user selected features
#Requires "gclus" package
#scatterplot_corr_plot(path_in="<path of feature data to be analysed>",path_out="<path of folder for plot image to be exported >",feature_list("<name of the features user want to plot>"))
scatterplot_corr_plot <- function(path_in,path_out,FUN=feature_list(...)){
  a <- ftr_extr(path_in,FUN)
  dta.r=abs(cor(a)) # get correlations
  dta.col <- dmat.color(dta.r) # get colors
  # reorder variables so those with highest correlation
  # are closest to the diagonal
  dta.o <- order.single(dta.r)
  png(filename=paste(path_out,"scatterplot_corr.png",sep=""))
  cpairs(a, dta.o, panel.colors=dta.col, gap=.5,
         main="Variables Ordered and Colored by Correlation" )
  dev.off()
}
























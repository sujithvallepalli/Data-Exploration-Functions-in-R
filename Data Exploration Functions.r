#Creating a function to compute the missing values in the dataset/dataframe
missing_val=function(a){ 
#converting dataset to dataframe
df=data.frame(a)
#Creating dummy data frame
df1=data.frame()
for(i in 1:ncol(a)){
  df1[i,1]=colnames(df[i]) #Writing variable names to first column
  df1[i,2]=sum(is.na(df[,i])) #computing number of missing values in ith column
  df1[i,3]=round((100*df1[i,2])/(length(df[,1])),2) # Calcluating percentage of missing values
} 
colnames(df1)=c("Column Name","No. of Missing Values","Percentage of missing values")
return(df1)
}

############## END OF FUNCTION ###############

##Examples:
#Loading library of MASS to check data in that package
library(MASS)
missing_val(survey)

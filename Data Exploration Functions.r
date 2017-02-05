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

#########################################################################################

##Below are few visually explorable functions beyond simple plot() 

#Install and Load MASS package and view the data set to get details of the data
install.packages("MASS")
library("MASS")
View(survey)
#help(survey)

#Install and load packages plotly, ggplot2 and corrgram. These are the packages to plot and visualize the data
install.packages("plotly")
install.packages("ggplot2")
install.packages("corrgram")
library(corrgram)
library(plotly)
library(ggplot2)

#Attaching this data set as we use only this data set here
attach(survey)

#Plot 1
plot_ly(survey,x=W.Hnd,y=Wr.Hnd,type="box")
## This interactive box plot shows that Left handed students are having more Span on their Writing hand compared to Right handed Students.
#Average writing hand span is almost same as the median students
#Most of the Left handed students are having span of their writing hand more than average left handed students


#Plot 2
ggplot(data=survey, aes(x=Age,y=Pulse,color=Sex))+ facet_grid(. ~ Smoke)+ geom_point(aes(shape=factor(W.Hnd)))
#This graph gives the good information of the data set as it can use most of the variables
#INsight1: Majority of the students have age less than 20
#Insight2: Very few students are left handed


#Plot 3
#Removing NA from Smoke
a=!is.na(survey$Smoke)
b=survey[a,]
qplot(Pulse,Smoke,data=b,geom="jitter",facets=.~Exer)
#Heavy Smokers do exercise
#Most of the students do not smoke
#There are very few students who either smoke occasionally or Regularly AND DO NOT EXERCISE. These students must be motivated to either doing exercise or quit smoking


#Plot 4
symbols(Wr.Hnd,NW.Hnd,circles =Age ,inches = 0.1,bg="red")
#There are Two students whose age is far away from others
#There are Three students whose Writing hand span is different from their Non Writing hand (Wr.Hnd,NW.Hnd)=(15,13),(14,16),(18,13)


#Plot 5
#Let us observe  Right Handed people and Left handed people Clapping Styles
ggplot(data=survey, aes(x=Fold,y=Height,color=Clap))+ facet_grid(. ~ W.Hnd)+geom_point(aes(shape=factor(Sex)))
#Most of the Right Handed People have their RIght Hand on top when Clapping
#Same trend is not present among Left handed people(This may be a bias as number of Left handed are less compared to right)


#Plot 6
corrgram(survey,order=T,upper.panel = panel.pie)
#This Correlogram tells that there is High Correlation between Writing Hand and Non Writing Hand
#Also There is a correlation between span of hand and Height of the student


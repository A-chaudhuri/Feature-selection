setwd("D:/arpita/data analytics/my work")
rm(list=ls(all=TRUE)) # refresh session
d=read.csv("automobile.txt",header=F,sep=",")
data=d[,-1]
str(data)
library(clusterSim)
for (i in 1:ncol(data))
{
  if(as.factor(class(data[,i]))=="integer" | as.factor(class(data[,i]))=="number")
    if(as.factor(class(data[,i]))=="integer")
      data.Normalization (data[,i],type="n1",normalization="column")
}

Mode <- function (data, na.rm) {
  xtab <- table(data)
  xmode <- names(which(xtab == max(xtab)))
  if (length(xmode) > 1) xmode <- ">1 mode"
  return(xmode)
}

library(modes)
for (i in 1:ncol(data))
{
  if(as.factor(class(data[,i]))=="integer" | as.factor(class(data[,i]))=="number")
    data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
  if(as.factor(class(data[,i]))=="Factor")
    data[is.na(data[,i]), i] <- Mode(na.omit(data[,i]))
  
}

sum(is.na(data))
library(entropy)
library("clusterCrit")
library("cluster")
library(clustMixType)
e<-rep(NA)
freq<-rep(NA)
#ncol(data)
for(i in 1:ncol(data)){
  # print("hello")
  e[i]<-entropy.empirical(freqs.empirical(table(data[,i])))
  # calculate shannon entropy over observed count y
}

#SHOW ENTROPY RESULT

for(i in 1:ncol(data)){
  print(e[i])
}


# NMI CALCULATION

w<-rep(NA)
m<-rep(NA)
NMI<-NULL
AVG_NMI<-NULL
MAX = -Inf
for(i in 1:ncol(data)){
  SUM<-0
  for (j in 1:ncol(data)){
    if(i!=j){
      
      #compute emperical MI over table of counts
      m[j]<-mi.empirical(rbind(data[,i],data[,j])) 
      
      #compute shannon entopy
      e[i]<-entropy.empirical(freqs.empirical(table(data[,i])))
      e[j]<-entropy.empirical(freqs.empirical(table(data[,j])))
      
      # print(m[j])
      #max<-min(e[i],e[j])
      NMI<-(2*m[j])/sum(e[i],e[j])
      print(NMI)
      if(is.nan(NMI)==FALSE)
      {
        SUM<-SUM+NMI
      }
     
      
    }
    
  }
  #SUM<-sum(NMI)
  AVG_NMI<-SUM/(ncol(data)-1)
  w[i]<-e[i]-AVG_NMI # weight calculation by differentiating max MI from entropy
  
  print("******")
  print(SUM)
  print("AVG NMI")
  print(AVG_NMI)
  print("****")
  MAX=-Inf
}  

#installed.packages("magicfor") # for printing for loop result as a vector
#library(magicfor)               # load library
#magic_for(print, silent = TRUE) # call magic_for()

weight<-c()

for (k in 1:ncol(data)){
  
  x<-c(w[k])
  
  weight<-append(weight,x)
}
weight


#weight<-magic_result_as_vector()
#weight

attr<-colnames(data) #retrieve header of data frame
attr

#create new column name for new data frame

x_name <- "attribute_name" 
y_name <- "weight"
df<-data.frame(attr,weight)
names(df) <- c(x_name,y_name)
df<-df[df$weight>=0,]
df
backupdata<-data
for(i in 1:nrow(df))
{
  ind<- which(df[i,1]!=names(backupdata))
  backupdata[,-ind]
}
#backupdata


#CALCULATE RANK ACCORDING TO DECREASING ORDER OF WEIGHT

rank<-data.frame()
rank<-df[order(df$weight,decreasing=TRUE),]
rank #show attribute name with corresponding weight

data_order<-data.frame()[1:nrow(backupdata),]
data_sort<-NULL
for(i in 1:nrow(rank))
{
  x<- which(rank[i,1]==colnames(backupdata)) #extract column no of original data 
  a<-subset(backupdata,select=colnames(backupdata)[x]) # extract a specific column
  data_sort<-a
  data_order<-cbind(data_order,data_sort)
}
data_order
rank

# ELBOW METHOD FOR FINDING OPTIMAL NO OF CLUSTER

set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 10
#data <- na.omit(data) # to remove the rows with NA's
wss <- sapply(1:k.max, 
              function(k){kproto(data, k)$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


a<-lambdaest(data)
large<- -Inf 
for(k in 1:10)
{ 
  kpres<-kproto(data,k,lambda=a)
  daisy.mat <- as.matrix(daisy(data, metric="gower"))
  CH_S1C1<-unlist(intCriteria(daisy.mat,kpres$cluster,c("Calinski_harabasz")))
  if(CH_S1C1>large)
  {
    large<-CH_S1C1
    print(large)
    print(k)
  }
  else 
    break
}

S_final<-NULL
Subset<-data_order
#Subset
removed_data<-data.frame()[1:nrow(Subset),]
nrow(removed_data)
a<-lambdaest(Subset)
kpres<-kproto(Subset,2,lambda=a)
#kpres
daisy.mat <- as.matrix(daisy(Subset, metric="gower")) # calculate dissimilarity matrix for original data
#daisy.mat
CH_S1C1<-unlist(intCriteria(daisy.mat,kpres$cluster,c("Calinski_harabasz")), use.names=FALSE)



for(i in ncol(data_order):1) # starting from last row with low weight
{
  
  x<-ncol(Subset)
  currentset<-Subset[,-x]
  #kprototype clustering over subset data
  a<-lambdaest(currentset)
  kpres_subset<-kproto(currentset,2,lambda=a)
  # print(kpres_subset)
  
  daisy.mat_sub <- as.matrix(daisy(currentset, metric="gower"))
  # calculate dissimilarity matrix for subset data
  # print(daisy.mat_sub)
  CH_S1C2<-unlist(intCriteria(daisy.mat,kpres_subset$cluster,c("Calinski_harabasz")), use.names=FALSE)
  #CH metric for originaldata and kpress subset cluster
  CH_S2C1<-unlist(intCriteria(daisy.mat_sub,kpres$cluster,c("Calinski_harabasz")), use.names=FALSE)
  #CH metric for subset and kpress original cluster
  
  CH_S2C2<-unlist(intCriteria(daisy.mat_sub,kpres_subset$cluster,c("Calinski_harabasz")), use.names=FALSE)
  #CH metric for subset and kpress subset cluster
  
  #install.packages("DOT") # package for dot product calculation
  #library(DOT)
  
  # ELIMINATION OF CARDINALITY BIAS OF CH METRIC
  
  normalized_S1C1<-CH_S1C1*CH_S2C1
  normalized_S2C2<-CH_S2C2*CH_S1C2
  
  CH_best<- normalized_S1C1
  
  if(normalized_S2C2>=CH_best) # comparing better result
  {
    CH_best<-normalized_S2C2
    removed_data<-cbind(removed_data,subset(Subset,select=colnames(Subset)[x]))
    #removed_data<-rbind(removed_data,myrank[i,]) # store eliminating record in removed_data dataframe
    print("removed set")
    print(head(removed_data))
    S_final<-currentset
    print("final set")
    print(head(S_final))
    Subset<-currentset
  }
}
print("subset :")
print(Subset)

optimal_set<-S_final
for(j in ncol(removed_data):1) 
{
  #y<-ncol(removed_data)
  optimal_set<-S_final
  #select_data<-removed_data[max(order(removed_data$weight,decreasing=TRUE)),]#select attribute name by choosing max weight
  #print(select_data[1,1])
  #select_data<-max(select_data$weight)
  #print(select_data)
  #col_num<-match(select_data[1,1],names(data)) # extracting col no by matching selectdata cl name with original data col name
  #x<-cbind(backupdata[-ind],data[,col_num]) # add corresponding column of resultant col no to backupdata
  tempSet<-cbind(optimal_set,subset(removed_data,select=colnames(removed_data)[j]))
  
  b<-lambdaest(tempSet)
  kpres_again<-kproto(tempSet,2,lambda=b)
  daisy.mat_again <- as.matrix(daisy(tempSet, metric="gower"))
  CH_again_S3C3<-unlist(intCriteria(daisy.mat_again,kpres_again$cluster,c("Calinski_harabasz")), use.names=FALSE)
  CH_again_S2C3<-unlist(intCriteria(daisy.mat_sub,kpres_again$cluster,c("Calinski_harabasz")), use.names=FALSE)
  normalized_CH_again<-CH_again_S3C3 * CH_again_S2C3
  if(normalized_CH_again >= CH_best)
  {
    optimal_set<-tempSet
    # rownum<-which(removed_data[,1] ==select_data[1,1])
    RS<-removed_data[,-j ]
    print("optimal Set")
    print(head(optimal_set))
    print("Removed Set")
    print(head(RS))
    s_final<-optimal_set
  }
  
}
print(head(optimal_set))


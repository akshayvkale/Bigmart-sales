traindata<-read.csv("G:/Projects/Big Mart Project/train_v9rqX0R.csv",na.strings=c("","NA"))
testdata<-read.csv("G:/Projects/Big Mart Project/test_AbJTz2l.csv",na.strings=c("","NA"))

View(traindata)
View(testdata)

dim(traindata)
dim(testdata)

traindata$Source<-"train"
testdata$Source<-"test"
testdata$Item_Outlet_Sales<-NA

data<-rbind(traindata,testdata)
View(data)

dim(data)
str(data)
colnames(data)
complete.cases(data)
table(complete.cases(data))

library(VIM)

aggr(data)
matrixplot(data)

sapply(data, function(x) class(x))

summary(data)

#Check how many NA's are present in each column
sapply(data, function(x) all(is.na(x)))
sapply(data, function(x) any(is.na(x)))
sapply(data, function(x) sum(is.na(x)))

categorial_features<-c("Item_Fat_Content","Item_Type","Outlet_Size","Outlet_Location_Type","Outlet_Type")

sapply(data[,categorial_features],function(x) unique(factor(x)))

sapply(data[,categorial_features],table)

library(dplyr)

#####################################################################
#Check all levels of the factor variable
data$Item_Fat_Content %>% unique

#Replace each level with one common word
data$Item_Fat_Content[data$Item_Fat_Content=="low fat"]<-"Low Fat"
data$Item_Fat_Content[data$Item_Fat_Content=="LF"]<-"Low Fat"
data$Item_Fat_Content[data$Item_Fat_Content=="reg"]<-"Regular"

table(data$Item_Fat_Content)
####################################################################

#Replace outlet establishment year with its age
#Treat smallest year as the base year

data$Outlet_Establishment_Year %>% unique

base_year<-data$Outlet_Establishment_Year %>% min
base_year

data$Outlet_Seniority<-data$Outlet_Establishment_Year-base_year

data$Outlet_Establishment_Year<-NULL

#################################################

data$Item_Type %>% unique

########################################

data$Outlet_Location_Type %>% unique

########################################

data$Outlet_Type %>% unique

#######################################

#Add new column of no. of items sold

data$No_of_Units_Sold<-round(data$Item_Outlet_Sales/data$Item_MRP)
data$No_of_Units_Sold

###################################################################
#Check how many distinct identifiers are present in the data
data$Item_Identifier %>% unique %>% length

#Check how many distinct values are present in each column
sapply(data, function(x) unique(x))
sapply(data, function(x) length(unique(x)))

###################################################################

#To impute NA values under Item_Weight

#Get the item identifiers of rows having NA under Item_Weight
ids_NA<-data %>% filter(is.na(Item_Weight))
ids_NA

#Filter out other rows (not having NA's item weight) and having identifier in above dataframe
tempdata<-data %>% filter(!is.na(Item_Weight) & Item_Identifier %in% ids_NA$Item_Identifier)

#Find unique value of Item_weight from the remaining data for each item identifier.
result<-tapply(tempdata$Item_Weight,tempdata$Item_Identifier,unique)
result
length(result)

#Impute for each item type
for (i in 1:length(result))
{
  data[data$Item_Identifier==names(result[i]) & is.na(data$Item_Weight),]$Item_Weight<-result[i]
}

#To confirm
table(is.na(data$Item_Weight))
sapply(data, function(x) sum(is.na(x)))

##############################################################################

#Another Approach

#Split data into two dataframes

#One having NA's in Item_Weight column
#NA_Weight<-data[is.na(data$Item_Weight),c(1,2)]
#NA_Weight
#nrow(NA_Weight)

#Another not having NA's in Item_Weight column
#nonNA_Weight<-data[!is.na(data$Item_Weight),c(1,2)]
#nonNA_Weight
#nrow(nonNA_Weight)

#Check how many distinct identifiers are present in the data with NA's
#NA_Weight$Item_Identifier
#NA_Weight$Item_Identifier %>% unique
#NA_Weight$Item_Identifier %>% unique %>% length

#Check if the identifiers in NA's dataframe are present in nonNA's
#NA_Weight$Item_Identifier %in% nonNA_Weight$Item_Identifier
#table(NA_Weight$Item_Identifier %in% nonNA_Weight$Item_Identifier)


#Replace NA's under Item_Weight column
#for (i in 1:nrow(NA_Weight))
#{
  #Find Item_Identifier in each row
#  id<-NA_Weight[i,1]
  
  #Check if that identifier is present in another data frame
#  if (id %in% nonNA_Weight$Item_Identifier)
#  {
    #Get the value in another data frame under Item_Weight column
    #corresponding to that identifier
#    val<-nonNA_Weight[nonNA_Weight$Item_Identifier==id,2] %>% mean  
    
    #Replace NA under Item_Weight with the obtained value
#    data[data$Item_Identifier==id,]$Item_Weight<-val
#    }
#}

#Check how many NA's are present in each column
sapply(data, function(x) sum(is.na(x)))

############################################################################

#Modify NA_Weight again as many NA's are now imputed
NA_Weight<-data[is.na(data$Item_Weight),c(1,2)]
NA_Weight
nrow(NA_Weight)

#Find the rows those still have NA's under Item-Weight
rownames(NA_Weight)
data[rownames(NA_Weight),]
#or
data[c(928,1923,4188,5023),]

#Median of entire Item_Weight column
median(data$Item_Weight,na.rm=T)

#Separate median for each Item_Type
median(data[data$Item_Type=="Frozen Foods",]$Item_Weight,na.rm=T)
median(data[data$Item_Type=="Snack Foods",]$Item_Weight,na.rm=T)
median(data[data$Item_Type=="Dairy",]$Item_Weight,na.rm=T)
median(data[data$Item_Type=="Baking Goods",]$Item_Weight,na.rm=T)

#Replace NA under Item_Weight with respective median
data[928,"Item_Weight"]<-median(data[data$Item_Type=="Frozen Foods",]$Item_Weight,na.rm=T)
data[1923,"Item_Weight"]<-median(data[data$Item_Type=="Snack Foods",]$Item_Weight,na.rm=T)
data[4188,"Item_Weight"]<-median(data[data$Item_Type=="Dairy",]$Item_Weight,na.rm=T)
data[5023,"Item_Weight"]<-median(data[data$Item_Type=="Baking Goods",]$Item_Weight,na.rm=T)

#Confirm that all NA's under Item_Weight are now imputed
sapply(data, function(x) sum(is.na(x)))

##########################################################################################

##Imputing Outlet_Size

#Find outlet type of those rows in which outlet size is missing
data[is.na(data$Outlet_Size),c("Outlet_Size","Outlet_Type")]
data[is.na(data$Outlet_Size),c("Outlet_Size","Outlet_Type")]$Outlet_Type
data[is.na(data$Outlet_Size),c("Outlet_Size","Outlet_Type")]$Outlet_Type %>% unique

table(data$Outlet_Size,data$Outlet_Type)

#Observe that if Outlet_Type is Grocery Store then Outlet_Size is small
data[data$Outlet_Type=="Grocery Store",]$Outlet_Size<-"Small"

table(data$Outlet_Size,data$Outlet_Type)

#Find outlet location type of those rows in which outlet size is missing
data[is.na(data$Outlet_Size),c("Outlet_Size","Outlet_Location_Type")]
data[is.na(data$Outlet_Size),c("Outlet_Size","Outlet_Location_Type")]$Outlet_Location_Type
data[is.na(data$Outlet_Size),c("Outlet_Size","Outlet_Location_Type")]$Outlet_Location_Type %>% unique

table(data$Outlet_Size,data$Outlet_Location_Type)

#Observe that if Outlet_Location_Type is Tier 2 then Outlet_Size is small
data[data$Outlet_Location_Type=="Tier 2",]$Outlet_Size<-"Small"

table(data$Outlet_Size,data$Outlet_Location_Type)

#Confirm if there are any NA's
sapply(data, function(x) sum(is.na(x)))

###############################################################################
#Another Approach using kNN
#(works only if train test data are not combined initially)
#Select only those columns which are related with Outler and not with Items
datakNN<-data[,c("Outlet_Location_Type","Outlet_Type","Item_Outlet_Sales","Outlet_Seniority")]
View(datakNN)
head(datakNN,10)

sapply(datakNN,class)

#Function to convert character into numerical
numConvert<-function(x)
{
  if(is.character(x))
  {
    as.numeric(as.factor(x))
  }
  else
  {
    x
  }
}

#Convert all columns to numerical
datakNN<-as.data.frame(sapply(datakNN,numConvert))
head(datakNN,10)

#Normalization function
normalize<-function(x)
{
  (x-min(x))/(max(x)-min(x))
}

#Normalize each column
norm_datakNN<-as.data.frame(lapply(datakNN,normalize))
head(norm_datakNN)

#Split datakNN into two dataframes
#One having NA's under under Outlet_Size
#Another do not having
trainkNN<-norm_datakNN[!is.na(data$Outlet_Size),]
testkNN<-norm_datakNN[is.na(data$Outlet_Size),]

dim(trainkNN)
dim(testkNN)

head(trainkNN)
head(testkNN)

View(trainkNN)
View(testkNN)

trainkNN$Outlet_Location_Type %>% unique
trainkNN$Outlet_Type %>%unique

#Class labels are nothing but existing outlet sizes
classLabels<-data[!is.na(data$Outlet_Size),]$Outlet_Size
classLabels
table(classLabels)

#Choose k for kNN
k<-round(sqrt(nrow(trainkNN)))
k

library(class)

#Try fitting kNN to train data and test it in the train data itself
model_kNN<-knn(trainkNN,trainkNN,classLabels,k)
model_kNN
table(model_kNN)

library(gmodels)

#Check how many labels are correctly classified
CrossTable(classLabels,model_kNN,prop.chisq = F)

#Make predictions on the test data (data having NA's)
pred_kNN<-knn(trainkNN,testkNN,classLabels,k)
pred_kNN
table(pred_kNN)

#Convert into character as the original column in of class character
pred_kNN<-as.character(pred_kNN)
pred_kNN %>% unique
table(pred_kNN)

#Impute NA's with predicted values
data[is.na(data$Outlet_Size),]$Outlet_Size<-pred_kNN
#or
data$Outlet_Size<-replace(data$Outlet_Size,is.na(data$Outlet_Size),pred_kNN)

#Confirm that all NA's are now imputed
sapply(data, function(x) sum(is.na(x)))
#######################################

#Item vislibility is 0 for few rows. To impute them

#Get the item identifiers of rows having visibility 0
ids_0<-data %>% filter(Item_Visibility==0) 
ids_0
nrow(ids_0)

#Filter out other rows (having non zero item visibility) and having identifier in above dataframe
tempdata<-data %>% filter(Item_Visibility!=0 & Item_Identifier %in% ids_0$Item_Identifier)
nrow(tempdata)

#Find mean of the remaining data for each item identifier.
result<-tapply(tempdata$Item_Visibility,tempdata$Item_Identifier,mean)
result
length(result)

#Impute for each item type
for (i in 1:length(result))
{
  data[data$Item_Identifier==names(result[i]) & data$Item_Visibility==0,]$Item_Visibility<-result[i]
}

#To confirm
table(data$Item_Visibility==0)

#############################################################################

library(ggplot2)

#Another way of Imputing Item_Visibility
#qplot(data$Item_Visibility,data$Item_Type,color="blue")
#
#qplot(data$Item_Visibility,data$Outlet_Type,color="blue")
#
#qplot(data$Item_Visibility,data$Outlet_Size,color="blue")
#
#Filter out other rows (having non zero item visibility)
# tempdata<-data %>% filter(Item_Visibility!=0)
# 
# #Find 10% trimmed mean of the remaining data for each item type.
# result<-tapply(tempdata$Item_Visibility,tempdata$Item_Type,function(x) mean(x,trim=0.1))
# result
# 
# #Impute for each item type
# for (i in 1:length(result))
# {
#   data[data$Item_Type==names(result[i]) & data$Item_Visibility==0,]$Item_Visibility<-result[i]
# }
# 
# #To confirm
# table(data$Item_Visibility==0)

##########################################################################################

#To find Item visibility mean ratio
#(Visibility of a product in a particular store as compared to
#mean visibility of that product across all the stores)

#First find average visibility of each product
Avg_Visibility<-tapply(data$Item_Visibility,data$Item_Identifier,mean)
head(Avg_Visibility)

data$Item_Visibility_Mean_Ratio<-data$Item_Visibility/Avg_Visibility[data$Item_Identifier]

##########################################################################################

#To find differences in Item MRP
#(MRP of a product in a particular store as compared to
#MRP of that product across all the stores)

#First find minimum MRP of each product
Min_MRP<-tapply(data$Item_MRP,data$Item_Identifier,min)
head(Min_MRP)

data$Item_MRP_Difference<-data$Item_MRP-Min_MRP[data$Item_Identifier]

##########################################################################################

#Create broad category for Items
#Item_Identifier starts with FD or DR or NC
#Food, Drinks, Non-consumable

data$Item_Category<-substr(data$Item_Identifier,1,2)

data$Item_Category<-factor(data$Item_Category,levels=c("FD","DR","NC"),labels=c("Food","Drink","Non-Consumable"))

data$Item_Category<-as.character(data$Item_Category)


categorial_features<-c(categorial_features,"Item_Category")

sapply(data[,categorial_features],function(x) unique(factor(x)))

sapply(data[,categorial_features],table)

################################################################################################

#For non consumable items, fat content need not be specified.
data[data$Item_Category=="Non-Consumable",]$Item_Fat_Content<-"Non-Edible"

sapply(data[,categorial_features],function(x) unique(factor(x)))

sapply(data[,categorial_features],table)

#####################################################################################

#Visualization

numeric_features<-data[data$Source=="train",c("Item_Weight","Item_Visibility","Item_MRP","Outlet_Seniority","Item_Outlet_Sales","No_of_Units_Sold","Item_Visibility_Mean_Ratio","Item_MRP_Difference")]

library(corrplot)
cor_matrix<-cor(numeric_features)
cor_matrix
corrplot.mixed(cor_matrix)



plot(data$Item_Weight,data$Item_Outlet_Sales,col="red")

plot(data$Item_Visibility,data$Item_Outlet_Sales,col="blue")

plot(data$Item_MRP,data$Item_Outlet_Sales,col="green")

plot(data$Outlet_Seniority,data$Item_Outlet_Sales,col="magenta")

plot(data$No_of_Units_Sold,data$Item_Outlet_Sales,col="magenta")

plot(data$Item_Visibility_Mean_Ratio,data$Item_Outlet_Sales,col="dark green")

plot(data$Item_MRP_Difference,data$Item_Outlet_Sales,col="purple")




plot(data$Item_Weight+data$Item_Visibility_Mean_Ratio+data$Item_MRP+data$Outlet_Seniority,data$Item_Outlet_Sales)

plot(data$Item_Weight+data$Item_Visibility_Mean_Ratio+data$No_of_Units_Sold+data$Outlet_Seniority,data$Item_Outlet_Sales)







qplot(data$Item_Weight,col="blue",bins=10)

qplot(data$Item_Fat_Content,col="red")

qplot(data$Item_Visibility,col="green",bins=20)

qplot(data$Item_Type,col="yellow")

qplot(data$Item_MRP,col="pink",bins=60)

qplot(data$Outlet_Identifier,col="magenta")

qplot(data$Outlet_Size,col="cyan")

qplot(data$Outlet_Location_Type,col="dark green")

qplot(data$Outlet_Type,col="red")

qplot(data$Item_Outlet_Sales,col="blue",bins=30)

qplot(data$Outlet_Seniority,col="green")

qplot(data$No_of_Units_Sold,col="brown")

qplot(data$Item_Visibility_Mean_Ratio,col="pink",bins=40)

qplot(data$Item_Category,col="magenta")

qplot(data$Item_MRP_Difference,col="purple")




qplot(data$Item_Identifier,data$Item_Outlet_Sales,color="cyan")

qplot(data$Item_Fat_Content,data$Item_Outlet_Sales,col="blue")

qplot(data$Item_Type,data$Item_Outlet_Sales,col="magenta")

qplot(data$Outlet_Identifier,data$Item_Outlet_Sales,col="dark green")

qplot(data$Outlet_Size,data$Item_Outlet_Sales,col="green")

qplot(data$Outlet_Location_Type,data$Item_Outlet_Sales,col="yellow")

qplot(data$Outlet_Type,data$Item_Outlet_Sales,col="red")

qplot(data$Outlet_Seniority,data$Item_Outlet_Sales,col="green")

qplot(data$No_of_Units_Sold,data$Item_Outlet_Sales,col="blue")

qplot(data$Item_Visibility_Mean_Ratio,data$Item_Outlet_Sales,col="blue")

qplot(data$Item_Category,data$Item_Outlet_Sales,col="red")

qplot(data$Item_MRP_Difference,data$Item_Outlet_Sales,col="brown")




qplot(data$Item_Identifier,data$Item_Outlet_Sales,color=factor(data$Item_Fat_Content))

qplot(data$Item_Identifier,data$Item_Outlet_Sales,color=factor(data$Item_Type))

qplot(data$Item_Identifier,data$Item_Outlet_Sales,color=factor(data$Outlet_Identifier))

qplot(data$Item_Identifier,data$Item_Outlet_Sales,color=factor(data$Outlet_Size))

qplot(data$Item_Identifier,data$Item_Outlet_Sales,color=factor(data$Outlet_Location_Type))

qplot(data$Item_Identifier,data$Item_Outlet_Sales,color=factor(data$Outlet_Type))#

qplot(data$Item_Identifier,data$Item_Outlet_Sales,color=factor(data$Item_Category))





qplot(data$Item_Fat_Content,data$Item_Outlet_Sales,color=factor(data$Item_Type))

qplot(data$Item_Fat_Content,data$Item_Outlet_Sales,color=factor(data$Outlet_Identifier))

qplot(data$Item_Fat_Content,data$Item_Outlet_Sales,color=factor(data$Outlet_Size))

qplot(data$Item_Fat_Content,data$Item_Outlet_Sales,color=factor(data$Outlet_Location_Type))

qplot(data$Item_Fat_Content,data$Item_Outlet_Sales,color=factor(data$Outlet_Type))

qplot(data$Item_Fat_Content,data$Item_Outlet_Sales,color=factor(data$Item_Category))





qplot(data$Item_Type,data$Item_Outlet_Sales,color=factor(data$Item_Fat_Content))

qplot(data$Item_Type,data$Item_Outlet_Sales,color=factor(data$Outlet_Identifier))

qplot(data$Item_Type,data$Item_Outlet_Sales,color=factor(data$Outlet_Size))

qplot(data$Item_Type,data$Item_Outlet_Sales,color=factor(data$Outlet_Location_Type))

qplot(data$Item_Type,data$Item_Outlet_Sales,color=factor(data$Outlet_Type))

qplot(data$Item_Type,data$Item_Outlet_Sales,color=factor(data$Item_Category))





qplot(data$Outlet_Size,data$Item_Outlet_Sales,color=factor(data$Item_Fat_Content))

qplot(data$Outlet_Size,data$Item_Outlet_Sales,color=factor(data$Item_Type))

qplot(data$Outlet_Size,data$Item_Outlet_Sales,color=factor(data$Outlet_Identifier))

qplot(data$Outlet_Size,data$Item_Outlet_Sales,color=factor(data$Outlet_Location_Type))

qplot(data$Outlet_Size,data$Item_Outlet_Sales,color=factor(data$Outlet_Type))

qplot(data$Outlet_Size,data$Item_Outlet_Sales,color=factor(data$Item_Category))





qplot(data$Outlet_Location_Type,data$Item_Outlet_Sales,color=factor(data$Item_Fat_Content))

qplot(data$Outlet_Location_Type,data$Item_Outlet_Sales,color=factor(data$Item_Type))

qplot(data$Outlet_Location_Type,data$Item_Outlet_Sales,color=factor(data$Outlet_Identifier))

qplot(data$Outlet_Location_Type,data$Item_Outlet_Sales,color=factor(data$Outlet_Size))

qplot(data$Outlet_Location_Type,data$Item_Outlet_Sales,color=factor(data$Outlet_Type))

qplot(data$Outlet_Location_Type,data$Item_Outlet_Sales,color=factor(data$Item_Category))





qplot(data$Outlet_Type,data$Item_Outlet_Sales,color=factor(data$Item_Fat_Content))

qplot(data$Outlet_Type,data$Item_Outlet_Sales,color=factor(data$Item_Type))

qplot(data$Outlet_Type,data$Item_Outlet_Sales,color=factor(data$Outlet_Identifier))

qplot(data$Outlet_Type,data$Item_Outlet_Sales,color=factor(data$Outlet_Size))

qplot(data$Outlet_Type,data$Item_Outlet_Sales,color=factor(data$Outlet_Location_Type))

qplot(data$Outlet_Type,data$Item_Outlet_Sales,color=factor(data$Item_Category))




qplot(data$Item_Category,data$Item_Outlet_Sales,color=factor(data$Item_Fat_Content))

qplot(data$Item_Category,data$Item_Outlet_Sales,color=factor(data$Item_Type))

qplot(data$Item_Category,data$Item_Outlet_Sales,color=factor(data$Outlet_Identifier))

qplot(data$Item_Category,data$Item_Outlet_Sales,color=factor(data$Outlet_Size))

qplot(data$Item_Category,data$Item_Outlet_Sales,color=factor(data$Outlet_Location_Type))

qplot(data$Item_Category,data$Item_Outlet_Sales,color=factor(data$Outlet_Type))







qplot(data$Item_Weight,data$Item_Outlet_Sales,color=factor(data$Item_Fat_Content))

qplot(data$Item_Weight,data$Item_Outlet_Sales,color=factor(data$Item_Type))

qplot(data$Item_Weight,data$Item_Outlet_Sales,color=factor(data$Outlet_Identifier))

qplot(data$Item_Weight,data$Item_Outlet_Sales,color=factor(data$Outlet_Size))

qplot(data$Item_Weight,data$Item_Outlet_Sales,color=factor(data$Outlet_Location_Type))

qplot(data$Item_Weight,data$Item_Outlet_Sales,color=factor(data$Outlet_Type))#

qplot(data$Item_Weight,data$Item_Outlet_Sales,color=factor(data$Item_Category))






qplot(data$Item_Visibility,data$Item_Outlet_Sales,color=factor(data$Item_Fat_Content))#

qplot(data$Item_Visibility,data$Item_Outlet_Sales,color=factor(data$Item_Type))

qplot(data$Item_Visibility,data$Item_Outlet_Sales,color=factor(data$Outlet_Identifier))

qplot(data$Item_Visibility,data$Item_Outlet_Sales,color=factor(data$Outlet_Size))

qplot(data$Item_Visibility,data$Item_Outlet_Sales,color=factor(data$Outlet_Location_Type))

qplot(data$Item_Visibility,data$Item_Outlet_Sales,color=factor(data$Outlet_Type))

qplot(data$Item_Visibility,data$Item_Outlet_Sales,color=factor(data$Item_Category))






qplot(data$Item_MRP,data$Item_Outlet_Sales,color=factor(data$Item_Fat_Content))

qplot(data$Item_MRP,data$Item_Outlet_Sales,color=factor(data$Item_Type))

qplot(data$Item_MRP,data$Item_Outlet_Sales,color=factor(data$Outlet_Identifier))

qplot(data$Item_MRP,data$Item_Outlet_Sales,color=factor(data$Outlet_Size))

qplot(data$Item_MRP,data$Item_Outlet_Sales,color=factor(data$Outlet_Location_Type))

qplot(data$Item_MRP,data$Item_Outlet_Sales,color=factor(data$Outlet_Type))

qplot(data$Item_MRP,data$Item_Outlet_Sales,color=factor(data$Item_Category))






qplot(data$Outlet_Seniority,data$Item_Outlet_Sales,color=factor(data$Item_Fat_Content))

qplot(data$Outlet_Seniority,data$Item_Outlet_Sales,color=factor(data$Item_Type))

qplot(data$Outlet_Seniority,data$Item_Outlet_Sales,color=factor(data$Outlet_Identifier))

qplot(data$Outlet_Seniority,data$Item_Outlet_Sales,color=factor(data$Outlet_Size))

qplot(data$Outlet_Seniority,data$Item_Outlet_Sales,color=factor(data$Outlet_Location_Type))

qplot(data$Outlet_Seniority,data$Item_Outlet_Sales,color=factor(data$Outlet_Type))

qplot(data$Outlet_Seniority,data$Item_Outlet_Sales,color=factor(data$Item_Category))





qplot(data$Item_Visibility_Mean_Ratio,data$Item_Outlet_Sales,color=factor(data$Item_Fat_Content))

qplot(data$Item_Visibility_Mean_Ratio,data$Item_Outlet_Sales,color=factor(data$Item_Type))

qplot(data$Item_Visibility_Mean_Ratio,data$Item_Outlet_Sales,color=factor(data$Outlet_Identifier))

qplot(data$Item_Visibility_Mean_Ratio,data$Item_Outlet_Sales,color=factor(data$Outlet_Size))

qplot(data$Item_Visibility_Mean_Ratio,data$Item_Outlet_Sales,color=factor(data$Outlet_Location_Type))

qplot(data$Item_Visibility_Mean_Ratio,data$Item_Outlet_Sales,color=factor(data$Outlet_Type))

qplot(data$Item_Visibility_Mean_Ratio,data$Item_Outlet_Sales,color=factor(data$Item_Category))




qplot(data$Item_MRP_Difference,data$Item_Outlet_Sales,color=factor(data$Item_Fat_Content))

qplot(data$Item_MRP_Difference,data$Item_Outlet_Sales,color=factor(data$Item_Type))

qplot(data$Item_MRP_Difference,data$Item_Outlet_Sales,color=factor(data$Outlet_Identifier))

qplot(data$Item_MRP_Difference,data$Item_Outlet_Sales,color=factor(data$Outlet_Size))

qplot(data$Item_MRP_Difference,data$Item_Outlet_Sales,color=factor(data$Outlet_Location_Type))

qplot(data$Item_MRP_Difference,data$Item_Outlet_Sales,color=factor(data$Outlet_Type))

qplot(data$Item_MRP_Difference,data$Item_Outlet_Sales,color=factor(data$Item_Category))




boxplot(data$Item_Weight,col="yellow",border="purple",horizontal = T)

boxplot(data$Item_Visibility,col="green",border="red",horizontal = T)$out
boxplot(data$Item_Visibility,col="green",border="red",horizontal = T)$out %>% length

boxplot(data$Item_MRP,col="orange",border="blue",horizontal = T)

boxplot(data$Outlet_Seniority,col="magenta",border="dark green",horizontal = T)

boxplot(data$Item_Outlet_Sales,col="cyan",border="magenta",horizontal = T)$out
boxplot(data$Item_Outlet_Sales,col="cyan",border="magenta",horizontal = T)$out %>% length

boxplot(data$Item_Visibility_Mean_Ratio,col="green",border = "red",horizontal = T)$out
boxplot(data$Item_Visibility_Mean_Ratio,col="green",border = "red",horizontal = T)$out %>% length

boxplot(data$Item_MRP_Difference,col="purple",border="dark green",horizontal = T)






boxplot(Item_Outlet_Sales~Item_Fat_Content,
        data,
        col=c("yellow","green"),
        border=c("blue","red"),
        horizontal = T)

boxplot(Item_Outlet_Sales~Item_Type,
        data,
        col=c("yellow","green","pink"),
        border=c("blue","red","dark green"),
        horizontal = F)

boxplot(Item_Outlet_Sales~Outlet_Identifier,
        data,
        col=c("yellow","green","pink"),
        border=c("blue","red","dark green"),
        horizontal = F)

boxplot(Item_Outlet_Sales~Outlet_Size,
        data,
        col=c("orange","green","red"),
        border=c("blue","purple","dark green"),
        horizontal = T)

boxplot(Item_Outlet_Sales~Outlet_Location_Type,
        data,
        col=c("orange","green","red"),
        border=c("blue","purple","dark green"),
        horizontal = T)

boxplot(Item_Outlet_Sales~Outlet_Type,
        data,
        col=c("orange","green","red","purple"),
        border=c("blue","purple","dark green","red"),
        horizontal = F)

boxplot(Item_Outlet_Sales~Item_Category,
        data,
        col=c("orange","yellow","purple"),
        border=c("blue","green","red"),
        horizontal = T)









library(scatterplot3d)
names(numeric_features)

scatterplot3d(numeric_features[,c(1,2,5)],angle=135,color="blue")
scatterplot3d(numeric_features[,c(1,3,5)],angle=135,color="green")
scatterplot3d(numeric_features[,c(1,4,5)],angle=65,color="red")
scatterplot3d(numeric_features[,c(1,6,5)],angle=65,color="orange")
scatterplot3d(numeric_features[,c(1,7,5)],angle=65,color="cyan")
scatterplot3d(numeric_features[,c(1,8,5)],angle=65,color="purple")
scatterplot3d(numeric_features[,c(2,3,5)],angle=25,color="pink") #
scatterplot3d(numeric_features[,c(2,4,5)],angle=65,color="yellow")
scatterplot3d(numeric_features[,c(2,6,5)],angle=65,color="purple")
scatterplot3d(numeric_features[,c(2,7,5)],angle=65,color="dark green")
scatterplot3d(numeric_features[,c(2,8,5)],angle=65,color="red")
scatterplot3d(numeric_features[,c(3,4,5)],angle=65,color="brown")
scatterplot3d(numeric_features[,c(3,6,5)],angle=65,color="dark green")
scatterplot3d(numeric_features[,c(3,7,5)],angle=65,color="magenta")
scatterplot3d(numeric_features[,c(3,8,5)],angle=65,color="pink")
scatterplot3d(numeric_features[,c(4,6,5)],angle=65,color="royal blue")
scatterplot3d(numeric_features[,c(4,7,5)],angle=65,color="purple")
scatterplot3d(numeric_features[,c(4,8,5)],angle=65,color="blue")
scatterplot3d(numeric_features[,c(6,7,5)],angle=65,color="yellow")
scatterplot3d(numeric_features[,c(6,8,5)],angle=65,color="green")
scatterplot3d(numeric_features[,c(7,8,5)],angle=65,color="yellow")





marginplot(data[,c("Item_MRP","Item_Outlet_Sales")],col="red")
marginplot(data[,c("Item_Weight","Item_Outlet_Sales")],col="green")
marginplot(data[,c("Item_Visibility","Item_Outlet_Sales")],col="blue")



ggplot(data,aes(Item_Weight,Item_Outlet_Sales))+
  geom_point(aes(color=Item_Fat_Content))+
    facet_wrap(~Item_Fat_Content)

ggplot(data,aes(Item_Weight,Item_Outlet_Sales))+
  geom_point(aes(color=Item_Type))+
  facet_wrap(~Item_Type)

ggplot(data,aes(Item_Weight,Item_Outlet_Sales))+
  geom_point(aes(color=Outlet_Size))+
  facet_wrap(~Outlet_Size)

ggplot(data,aes(Item_Weight,Item_Outlet_Sales))+
  geom_point(aes(color=Outlet_Location_Type))+
  facet_wrap(~Outlet_Location_Type)

ggplot(data,aes(Item_Weight,Item_Outlet_Sales))+
  geom_point(aes(color=Outlet_Type))+
  facet_wrap(~Outlet_Type)

ggplot(data,aes(Item_Weight,Item_Outlet_Sales))+
  geom_point(aes(color=Item_Category))+
  facet_wrap(~Item_Category)




ggplot(data,aes(Item_Visibility,Item_Outlet_Sales))+
  geom_point(aes(color=Item_Fat_Content))+
  facet_wrap(~Item_Fat_Content)

ggplot(data,aes(Item_Visibility,Item_Outlet_Sales))+
  geom_point(aes(color=Item_Type))+
  facet_wrap(~Item_Type)

ggplot(data,aes(Item_Visibility,Item_Outlet_Sales))+
  geom_point(aes(color=Outlet_Size))+
  facet_wrap(~Outlet_Size)

ggplot(data,aes(Item_Visibility,Item_Outlet_Sales))+
  geom_point(aes(color=Outlet_Location_Type))+
  facet_wrap(~Outlet_Location_Type)

ggplot(data,aes(Item_Visibility,Item_Outlet_Sales))+
  geom_point(aes(color=Outlet_Type))+
  facet_wrap(~Outlet_Type)

ggplot(data,aes(Item_Visibility,Item_Outlet_Sales))+
  geom_point(aes(color=Item_Category))+
  facet_wrap(~Item_Category)





ggplot(data,aes(Item_MRP,Item_Outlet_Sales))+
  geom_point(aes(color=Item_Fat_Content))+
  facet_wrap(~Item_Fat_Content)

ggplot(data,aes(Item_MRP,Item_Outlet_Sales))+
  geom_point(aes(color=Item_Type))+
  facet_wrap(~Item_Type)

ggplot(data,aes(Item_MRP,Item_Outlet_Sales))+
  geom_point(aes(color=Outlet_Size))+
  facet_wrap(~Outlet_Size)

ggplot(data,aes(Item_MRP,Item_Outlet_Sales))+
  geom_point(aes(color=Outlet_Location_Type))+
  facet_wrap(~Outlet_Location_Type)

ggplot(data,aes(Item_MRP,Item_Outlet_Sales))+
  geom_point(aes(color=Outlet_Type))+
  facet_wrap(~Outlet_Type)

ggplot(data,aes(Item_MRP,Item_Outlet_Sales))+
  geom_point(aes(color=Item_Category))+
  facet_wrap(~Item_Category)




ggplot(data,aes(Outlet_Seniority,Item_Outlet_Sales))+
  geom_point(aes(color=Item_Fat_Content))+
  facet_wrap(~Item_Fat_Content)

ggplot(data,aes(Outlet_Seniority,Item_Outlet_Sales))+
  geom_point(aes(color=Item_Type))+
  facet_wrap(~Item_Type)

ggplot(data,aes(Outlet_Seniority,Item_Outlet_Sales))+
  geom_point(aes(color=Outlet_Size))+
  facet_wrap(~Outlet_Size)

ggplot(data,aes(Outlet_Seniority,Item_Outlet_Sales))+
  geom_point(aes(color=Outlet_Location_Type))+
  facet_wrap(~Outlet_Location_Type)

ggplot(data,aes(Outlet_Seniority,Item_Outlet_Sales))+
  geom_point(aes(color=Outlet_Type))+
  facet_wrap(~Outlet_Type)

ggplot(data,aes(Outlet_Seniority,Item_Outlet_Sales))+
  geom_point(aes(color=Item_Category))+
  facet_wrap(~Item_Category)




ggplot(data,aes(Item_Visibility_Mean_Ratio,Item_Outlet_Sales))+
  geom_point(aes(color=Item_Fat_Content))+
  facet_wrap(~Item_Fat_Content)

ggplot(data,aes(Item_Visibility_Mean_Ratio,Item_Outlet_Sales))+
  geom_point(aes(color=Item_Type))+
  facet_wrap(~Item_Type)

ggplot(data,aes(Item_Visibility_Mean_Ratio,Item_Outlet_Sales))+
  geom_point(aes(color=Outlet_Size))+
  facet_wrap(~Outlet_Size)

ggplot(data,aes(Item_Visibility_Mean_Ratio,Item_Outlet_Sales))+
  geom_point(aes(color=Outlet_Location_Type))+
  facet_wrap(~Outlet_Location_Type)

ggplot(data,aes(Item_Visibility_Mean_Ratio,Item_Outlet_Sales))+
  geom_point(aes(color=Outlet_Type))+
  facet_wrap(~Outlet_Type)

ggplot(data,aes(Item_Visibility_Mean_Ratio,Item_Outlet_Sales))+
  geom_point(aes(color=Item_Category))+
  facet_wrap(~Item_Category)




ggplot(data,aes(Item_MRP_Difference,Item_Outlet_Sales))+
  geom_point(aes(color=Item_Fat_Content))+
  facet_wrap(~Item_Fat_Content)

ggplot(data,aes(Item_MRP_Difference,Item_Outlet_Sales))+
  geom_point(aes(color=Item_Type))+
  facet_wrap(~Item_Type)

ggplot(data,aes(Item_MRP_Difference,Item_Outlet_Sales))+
  geom_point(aes(color=Outlet_Size))+
  facet_wrap(~Outlet_Size)

ggplot(data,aes(Item_MRP_Difference,Item_Outlet_Sales))+
  geom_point(aes(color=Outlet_Location_Type))+
  facet_wrap(~Outlet_Location_Type)

ggplot(data,aes(Item_MRP_Difference,Item_Outlet_Sales))+
  geom_point(aes(color=Outlet_Type))+
  facet_wrap(~Outlet_Type)

ggplot(data,aes(Item_MRP_Difference,Item_Outlet_Sales))+
  geom_point(aes(color=Item_Category))+
  facet_wrap(~Item_Category)









ggplot(data,aes(Item_Weight,Item_Outlet_Sales))+
  geom_point(aes(color=Outlet_Type))+
  facet_grid(Item_Fat_Content~Item_Type)

ggplot(data,aes(Item_Weight,Item_Outlet_Sales))+
  geom_line(aes(color=Item_Fat_Content))+
  facet_grid(Outlet_Size~Item_Type)

ggplot(data,aes(Item_Weight,Item_Outlet_Sales))+
  geom_boxplot(aes(color=Item_Fat_Content))+
  facet_grid(Outlet_Size~Item_Type)

ggplot(data,aes(Item_MRP,Item_Outlet_Sales))+
  geom_boxplot(aes(color=Item_Fat_Content))+
  facet_grid(Item_Category~Outlet_Type)

#################################################################################

#Convert data back into train-test

#Drop the columns which are converted to different types.
data$Item_Type<-NULL
data$No_of_Units_Sold<-NULL

#Divide into train and test
traindata<-data[data$Source=="train",]
testdata<-data[data$Source=="test",]

#Drop target column from test set
testdata$Item_Outlet_Sales<-NULL

#Drop Source columns
traindata$Source<-NULL
testdata$Source<-NULL

#Save modified files.
write.csv(traindata,"G:/Projects/Big Mart Project/train_processed.csv",row.names = F)
write.csv(testdata,"G:/Projects/Big Mart Project/test_processed.csv",row.names = F)

###############################################################################

#Model Building

traindata_model<-read.csv("G:/Projects/Big Mart Project/train_processed.csv")
View(traindata_model)

dim(traindata_model)
str(traindata_model)

#Train-Test Splitting

library(caret)

indicator<-createDataPartition(traindata_model$Item_Outlet_Sales,p=0.7,list=F)
#or
indicator<-sample(c(T,F),nrow(traindata_model),replace=T,prob=c(0.7,0.3))

trainData<-traindata_model[indicator,]
testData<-traindata_model[-indicator,]

View(trainData)
View(testData)

#################################################################################
#Linear Regression

model1<-lm(Item_Outlet_Sales~Outlet_Location_Type+Item_MRP+Outlet_Type+Item_Visibility_Mean_Ratio+Item_Category,trainData)
summary(model1)

df.residual(model1) #degrees of freedom

coefficients(model1) #extract model coefficients

confint(model1,level=0.95) #computes confidence intervals for model parameters

fitted(model1)
#or
fitted.values(model1) #extract model fitted values

data.frame(trainData$Item_Outlet_Sales,fitted.values(model1))

resid(model1)
#or
residuals(model1) #extract model residuals

data.frame(ActualValue=trainData$Item_Outlet_Sales,
           EstimatedValue=fitted.values(model1),
           Residual=residuals(model1))

par(mfrow=c(2,2))
plot(model1)

predictions<-predict(model1,testData)
predictions

predict(model1,testData,interval="predict")

data.frame(ActualValue=testData$Item_Outlet_Sales,
           EstimatedValue=predictions,
           Residual=testData$Item_Outlet_Sales-predictions)

plot(testData$Item_Outlet_Sales,predictions)

sqrt(mean((testData$Item_Outlet_Sales-predictions)^2)) #RMSE

##############################

testdata_model<-read.csv("G:/Projects/Big Mart Project/test_processed.csv")

model1<-lm(Item_Outlet_Sales~Outlet_Location_Type+Item_MRP+Outlet_Type+Item_Visibility_Mean_Ratio+Item_Category,traindata_model)
summary(model1)

predictions<-predict(model1,testdata_model)
table(predictions<0)


submission<-data.frame(Item_Identifier=testdata_model$Item_Identifier,
                       Outlet_Identifier=testdata_model$Outlet_Identifier,
                       Item_Outlet_Sales=predictions)

write.csv(submission,"G:/Projects/Big Mart Project/submission.csv",row.names = F)

############################################################################

#Regression Tree

library(rpart)

model2<-rpart(Item_Outlet_Sales~.,data=trainData[,-1])
model2

summary(model2)

library(rpart.plot)
rpart.plot(model2,digits=3)

predictions<-predict(model2,testData[,-1])
predictions

summary(predictions)
summary(testData$Item_Outlet_Sales)

cor(predictions,testData$Item_Outlet_Sales)

sqrt(mean((testData$Item_Outlet_Sales-predictions)^2)) #RMSE

#############################

model2<-rpart(Item_Outlet_Sales~.,data=traindata_model[,-1])
summary(model2)

testdata_model<-read.csv("G:/Projects/Big Mart Project/test_processed.csv")

predictions<-predict(model2,testdata_model)
table(predictions<0)


submission_reg_tree<-data.frame(Item_Identifier=testdata_model$Item_Identifier,
                       Outlet_Identifier=testdata_model$Outlet_Identifier,
                       Item_Outlet_Sales=predictions)

write.csv(submission_reg_tree,"G:/Projects/Big Mart Project/submission_reg_tree.csv",row.names = F)

##################################################################################

#Neural Network

#Function to convert character into numerical
numConvert<-function(x)
{
  if(is.character(x))
  {
    as.numeric(as.factor(x))
  }
  else
  {
    x
  }
}

#Convert all columns to numerical
trainData<-as.data.frame(sapply(trainData,numConvert))
testData<-as.data.frame(sapply(testData,numConvert))

#Normalization function
normalize<-function(x)
{
  (x-min(x))/(max(x)-min(x))
}

#Normalize each column
norm_trainData<-as.data.frame(lapply(trainData[,-1],normalize))
norm_testData<-as.data.frame(lapply(testData[,-1],normalize))

library(neuralnet)

model3<-neuralnet(Item_Outlet_Sales~.,data=trainData[,-1],hidden=10)
model3

summary(model3)

plot(model3)

model3_results<-compute(model3,testData[,-1])

predictions<-model3_results$net.result
predictions

###################################################################################

#Random Forests

library(randomForest)

###################################################################################

#Boosting

library(xgboost)

model5<-xgboost(data.matrix(trainData),
                trainData$Item_Outlet_Sales,
                nrounds=10)

predictions<-predict(model5,data.matrix(testData))
predictions

summary(predictions)
summary(testData$Item_Outlet_Sales)

cor(predictions,testData$Item_Outlet_Sales)

sqrt(mean((testData$Item_Outlet_Sales-predictions)^2)) #RMSE

#############################

model5<-xgboost(data.matrix(traindata_model[,-c(1,10)]),
                traindata_model$Item_Outlet_Sales,
                nrounds=10)

summary(model5)

testdata_model<-read.csv("G:/Projects/Big Mart Project/test_processed.csv")

predictions<-predict(model5,data.matrix(testdata_model[,-1]))
table(predictions<0)

submission_xgboost<-data.frame(Item_Identifier=testdata_model$Item_Identifier,
                                Outlet_Identifier=testdata_model$Outlet_Identifier,
                                Item_Outlet_Sales=predictions)

write.csv(submission_xgboost,"G:/Projects/Big Mart Project/submission_xgboost.csv",row.names = F)

##################################################################################

#Xgboost with selected features

model5<-xgboost(data.matrix(traindata_model[,-c(1,2,4,10,13)]),
                traindata_model$Item_Outlet_Sales,
                nrounds=10)

summary(model5)

testdata_model<-read.csv("G:/Projects/Big Mart Project/test_processed.csv")

predictions<-predict(model5,data.matrix(testdata_model[,-c(1,2,4,12)]))
table(predictions<0)

submission_xgboost<-data.frame(Item_Identifier=testdata_model$Item_Identifier,
                               Outlet_Identifier=testdata_model$Outlet_Identifier,
                               Item_Outlet_Sales=predictions)

write.csv(submission_xgboost,"G:/Projects/Big Mart Project/submission_xgboost.csv",row.names = F)

##################################################################################

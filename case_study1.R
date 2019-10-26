library(tm) #text mining library provides the stopwords() function
library(tidyr)
library(plyr)
library(jsonlite)
library(dplyr)
library(tidyverse)
library(RCurl)
library(e1071)
library(caret)
library(ggplot2)


brewery_data = read.csv(file.choose(), header = TRUE);

beers_data = read.csv(file.choose(), header = TRUE);


#How many breweries are present in each state?
state_count = count(brewery_data$State)


colnames(brewery_data)[1] = "Brewery_id"

#Merge beer data with the breweries data. Print the first 6 observations and the last six observations to check the merged file.  (RMD only, this does not need to be included in the presentation or the deck.)
beers_brewery_merge = merge(beers_data,brewery_data,"Brewery_id")
colnames(beers_brewery_merge)[2] = "beer_name"
colnames(beers_brewery_merge)[8] = "brewery_name"


#Address the missing values in each column
filtered_df = beers_brewery_merge %>% filter(!(is.na(beers_brewery_merge$Brewery_id) &&
                                   is.na(beers_brewery_merge$beer_name) &&
                                   is.na(beers_brewery_merge$Beer_ID) &&
                                   is.na(beers_brewery_merge$ABV) &&
                                   is.na(beers_brewery_merge$IBU) &&
                                   is.na(beers_brewery_merge$Style) &&
                                   is.na(beers_brewery_merge$Ounces) &&
                                   is.na(beers_brewery_merge$brewery_name) &&
                                   is.na(beers_brewery_merge$City) && 
                                   is.na(beers_brewery_merge$State))) 


#Compute the median alcohol content and international bitterness unit for each state. Plot a bar chart to compare.
no_na <- na.omit(beers_brewery_merge)
no_na$ABV
result_abv <- aggregate(x = no_na[c("ABV","IBU")], by = no_na[c("State")], FUN = median, na.action = na.rm)
result_ibu <- aggregate(x = no_na[c("ABV")], by = no_na[c("State")], FUN = median, na.action = na.rm)

p<-ggplot(data=result, aes(x=State, y=ABV)) +
  geom_bar(stat="identity")

p<-ggplot(data=result, aes(x=State, y=IBV)) +
  geom_bar(stat="identity")



#Which state has the maximum alcoholic (ABV) beer? Which state has the most bitter (IBU) beer?
no_na[which.max(no_na$ABV),] #KY
no_na[which.max(no_na$IBU),] #OR

#Comment on the summary statistics and distribution of the ABV variable.
summary(no_na$ABV)


#Is there an apparent relationship between the bitterness of the beer and its alcoholic content? Draw a scatter plot.  Make your best judgment of a relationship and EXPLAIN your answer.
no_na %>% ggplot(aes(x = ABV, y = IBU)) + geom_point()


trainIndices = sample(1:dim(iris)[1], round(.7*dim(iris)[1]))
train = data.frame(iris[trainIndices,])
test = data.frame(iris[-trainIndices,])

dim(train)
dim(test)
dim(train[,c("ABV")])

dim(train$brew_style)

head(iris)

classifications = knn(train[,c(1)],test[,c(1)],train$Species, prob = TRUE, k = 5)



create_model <- function(train_data,columns,classifications){
  model <- naiveBayes(train_data[,columns],train_data[,classifications])
  return(model)
}

test_model <- function(model,test_data,columns,classifications){
  CM = confusionMatrix(table(predict(model,test_data[,columns]),test_data[,classifications]))
  return(c(CM$overall[1],CM$byClass[1],CM$byClass[2]))
}

create_and_test_model <- function(dataset,columns,classifications,ratio,seed = as.numeric(Sys.time())){
  train_indeces <- generate_train_indices(dataset,ratio,seed);
  train_data <- dataset[train_indeces,]
  test_data <- dataset[-train_indeces,]
  return(test_model(create_model(train_data,columns,classifications),test_data,columns,classifications))
}

prediction <- function(dataset,columns,classifications,ratio,iterations){
  master_acc = matrix(nrow = iterations)
  master_specificity = matrix(nrow = iterations)
  master_sensitivity = matrix(nrow = iterations)
  
  for(i in 1:iterations){
    test_result = create_and_test_model(dataset,columns,classifications,ratio)
    master_acc[i] = test_result[1]
    master_sensitivity[i] = test_result[2]
    master_specificity[i] = test_result[3]
  }
  return(c(colMeans(master_acc),colMeans(master_specificity),colMeans(master_sensitivity)))
}


prediction(iris,c(1,2),c("Species"), .7, 100)


  




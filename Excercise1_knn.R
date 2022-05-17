install.packages("ggplot2")
library(glue)       # used for string formatting
library(class)      # contains KNN classifier
library(ggplot2)    # for plotting and visualization
install.packages("gmodels")
library(gmodels)

#1.4.1
# Data shuffle
set.seed(423)
ciphers_shuffle <- ciphers[sample(nrow(ciphers[6000:8000,])),]

dim(ciphers_shuffle)
# Data splittting

split_ratio = 0.5
samples <- sample.int(n = nrow(ciphers_shuffle), size = floor(split_ratio*nrow(ciphers_shuffle)))
train <- ciphers_shuffle[samples, ]
test  <- ciphers_shuffle[-samples, ]

train_labels <- train[,2]
test_labels <- test[,2]



accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
run_knn <- function(train, test, train_labels, k){
  start_time <- Sys.time()
  test_prediction <- knn(train,test,cl=train_labels,k)
  end_time <- Sys.time()
  confusion_matrix <- table(test_prediction, test_labels)
  runtime = end_time - start_time
  confusion_matrix
  acc = accuracy(confusion_matrix)
  cat("K:",k," Accuracy:",acc," Runtime:",runtime, "\n")
  df <- list(K=k,Accuracy=acc,Runtime=runtime)
  return(df)
}

run_knn(train, test, train_labels, 3)

run_knn(train, test, train_labels, 1)

# 1.4.2
accs = c()
runtimes = c()
Ks = c()
for (i in c(1:100) ) {
  if(i %% 2 == 0){
    
  } else {
    ret <- run_knn(train, test, train_labels, i)
    accs[i] = ret$Accuracy
    runtimes[i] = ret$Runtime
    Ks[i] = ret$K
  }
}
plot(accs,type = "o")
plot(runtimes,type = "o")
plot(Ks,type = "o")



# cross validation
install.packages("tidyverse")
install.packages("ggplot2", type="source",dependencies = TRUE)
install.packages("ggplot2")
install.packages("scales")
install.packages('caret')
library(tidyverse)
library(ggplot2)
library(caret)

# 1.4.3: cross validation
#10% testing rest is training => the 10% slide over the data (so 10 runs in total)
accs <- c(1:10)
runtimes <- c(1:10)
folds <- createFolds(id$X1, k=10)
for (i in 1:10) {
  train_split <- id[-folds[[i]], -1]
  test_split <- id[folds[[i]],-1]
  train_classes <- id[-folds[[i]], 1]
  test_classes <- id[folds[[i]],1]
  
  ret <- run_knn(train_split, test_split, train_classes, k=3)
  accs[i] = ret$Accuracy
  runtimes[i] = ret$Runtime
}
print(runtimes)
print(accs)
mean(accs)
var(accs)

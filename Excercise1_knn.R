install.packages("ggplot2")
library(glue)       # used for string formatting
library(class)      # contains KNN classifier
library(ggplot2)    # for plotting and visualization
install.packages("gmodels")
library(gmodels)

# Data shuffle
set.seed(423)
ciphers_shuffle <- ciphers[sample(nrow(ciphers)),]

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

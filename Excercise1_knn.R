library(glue)       # used for string formatting
library(class)      # contains KNN classifier
library(ggplot2)    # for plotting and visualization

# Data shuffle
set.seed(423)
ciphers_shuffle <- ciphers[sample(nrow(ciphers)),]

# Data splittting

split_ratio = 0.5
samples <- sample.int(n = nrow(ciphers_shuffle), size = floor(split_ratio*nrow(ciphers_shuffle)))
train <- ciphers_shuffle[samples, ]
test  <- ciphers_shuffle[-samples, ]

install.packages("ggplot2")
install.packages("glue")
library(glue)       # used for string formatting
library(class)      # contains KNN classifier
library(ggplot2)    # for plotting and visualization
install.packages("gmodels")
library(gmodels)
install.packages("spatstat")
library(spatstat)
install.packages("ggfortify")
library(ggfortify)  

#Pre-Processing

remove_constant <- function(dat, na.rm = FALSE, quiet=TRUE) {
  mask <-
    sapply(
      X=seq_len(ncol(dat)),
      FUN=function(idx) {
        column_to_test <-
          if (is.matrix(dat)) {
            dat[, idx]
          } else {
            dat[[idx]]
          }
        length(unique(
          if (na.rm) {
            stats::na.omit(column_to_test)
          } else {
            column_to_test
          }
        )) <= 1 # the < is in case all values are NA with na.rm=TRUE
      }
    )
  if (!quiet) {
    remove_message(dat=dat, mask_keep=!mask, which="columns", reason="constant")
  }
  dat[ , !mask, drop=FALSE]
}

digit_data <- remove_constant(ciphers[,3:786])
dim(digit_data)

ciphers_processed <- matrix(0, nrow = dim(digit_data)[1], ncol = dim(digit_data)[2] + 2)
ciphers_processed[,1:2] <- ciphers[,1:2]
dim(ciphers_processed)
ciphers_processed[,3:531] <- digit_data

get_img <- function(img_data) {
  x <- 1:23
  y <- 1:23
  img_data <- rev(img_data)
  
  img <- matrix(0, nrow = 23, ncol = 23)
  for(i in x) { 
    for (j in y) {                                  
      img[i,j] <- img_data[j*23 - (i-1)]
    }
  }
  img
}

# This function visualizes a given datapoint

visualize_datapoint <- function(datapoint) {
  img_data <- get_img(datapoint[3:531])
  gt <- datapoint[2]
  student_id <- datapoint[1]
  
  image(img_data, axes=FALSE, col=gray.colors(10), useRaster=TRUE)
  title_string = "Ground truth: {gt} | Student id: {student_id}"
  title(main=glue(title_string), col.main="blue", font.main=15)
}

par(mfrow=c(3,3))    # set the plotting area into a 3*3 array

random_datpoint_dxs <- sample(1:66000, 9, replace=FALSE)

for (i in random_datpoint_dxs) {
  visualize_datapoint(ciphers_processed[i,])
}

gaussian_filter <- function(img, sig){
  smoothed <- as.matrix(blur(as.im(img), sigma = sig, normalise=FALSE, bleed = TRUE, varcov=NULL))
  return(smoothed)
}

img <- get_img(ciphers_processed[500,3:531])
smt_img <- gaussian_filter(img, 1.3)

image(img, axes=FALSE, col=gray.colors(10), useRaster=TRUE)
title_string = "Original image"
title(main=glue(title_string), col.main="blue", font.main=15)

image(smt_img, axes=FALSE, col=gray.colors(10), useRaster=TRUE)
title_string = "After applying Gaussian smoothing"
title(main=glue(title_string), col.main="blue", font.main=25)

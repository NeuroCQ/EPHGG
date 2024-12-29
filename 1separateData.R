#=======================================================

#=======================================================

rm(list = ls())

library(dplyr)
library(MatchIt)
library(survival)
library(survminer)
library(officer)
library(flextable)
library(compareGroups)
library(broom)
library(cobalt)
library(randomForestSRC)
library(randomForestSRC)
library(pec)
library(risksetROC)
library(survcomp)
library(randomForestSRC)
library(survcomp)
library(caret)
library(timeROC)
library(tdROC)

jco_colors  <- c("#E07B39", "#3D3BF3", "#62825D", "#1A1A1D", "#F0C1E1")

#===============================================================

#===============================================================

data <- read.csv("DataSim.csv", header = T, row.names = 1)

str(data)

str(data)


data[] <- lapply(data, function(x) {
  if (is.character(x)) {
    return(as.factor(x))
  } else {
    return(x)
  }
})

str(data)


#===============================================================

#===============================================================


set.seed(123)

sample_size <- floor(0.7 * nrow(data))


train_indices <- sample(seq_len(nrow(data)), size = sample_size)


train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

save(train_data, file = "train_data.Rdata")
save(test_data, file = "test_data.Rdata")






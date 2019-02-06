#############################################################################
#   
#	Nigussie Abiyie - ATR/4172/05
#
############################################################################
library(foreign)
library(nnet)
library(tidyverse)
library(caret)

match_data <- read.csv("file:///C:/Users/Administrator/Music/E0.csv")
#str(match_data)
#inspect the data
##############################################
#cleaning na values
match_data <- as.data.frame(match_data, row.names = NULL,cut.names = FALSE, col.names = names(x), 
                            fix.empty.names = TRUE,stringsAsFactors = default.stringsAsFactors())
head(match_data)
tail(match_data)
any(is.na(match_data[]))
#match_data <- match_data[-381,]
#return complete values
#str(match_data2)
#head(match_data2)
#tail(match_data2)
#any(is.na(match_data2[]))
#sum(is.na(match_data2[]))
#colSums(is.na(match_data2[]))
#match_data1 <- dplyr::select(match_data,HS,AS,HC,AC,HST,AST,HR,AR)
#any(is.na(match_data1[]))
#[1] FALSE
#sum(is.na(match_data1[]))
#[1] 0
#colSums(is.na(match_data1[]))
#HS  AS  HC  AC HST AST  HR  AR 
#0   0   0   0   0   0   0   0 
##################################################

####################################################
n_matches = nrow(match_data) 
n_matches
n_features = length(match_data) - 1
n_features
homewins = filter(match_data,FTR == "H")
n_homewins = nrow(homewins)
awaywins = filter(match_data,FTR == "A")
n_awaywins = nrow(awaywins)
n_awaywins
draws = filter(match_data,FTR == "D")
n_draws = nrow(draws)
n_homewins
winrate = (n_homewins / n_matches) * 100
lossrate = (n_awaywins / n_matches) * 100
drawrate = (n_draws / n_matches) * 100
winrate
lossrate
drawrate
##############################################################
## set the seed to make partition reproducible
# Timings on relatively large data
set.seed(123)

# Split the data into trianing and test set

match_data$FTR2 <- relevel(match_data$FTR, ref = "D")
training_samples <- match_data$FTR2 %>% createDataPartition(p = 0.8, list = FALSE)
train_data <- match_data[training_samples,]
test_data <- match_data[-training_samples,]
# Fit the model
#model <- multinom(FTR2 ~ HS + AS + HST + AST,data = train_data)
model <- multinom(FTR2 ~ HS + AS + HST + AST + B365H + B365A + B365D + BWH + BWD + BWA,data = train_data)
summary(model)
head(match_data)
z <- summary(model)$coefficients/summary(model)$standard.errors
z
prob_distribition <- (1 - pnorm(abs(z), 0, 1)) * 2
prob_distribition

exp(coef(model))
head(pp <- fitted(model))


# Make prediction 
predicted_classes <- model %>% predict(test_data)
head(predicted_classes)
predicted_classes
# Model accuracy
# 64.6 %
# melt method for reshaping data in to the same data type(object)
melt(predicted_classes) == melt(test_data$FTR)
check <- select(match_data,FTR,FTR2)
check
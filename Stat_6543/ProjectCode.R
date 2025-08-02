##POSTER SUBMISSION PROJECT##
##Housing Data##

library(caret)
library(corrplot)
library(ldsr)
library(mlbench)
library(ggplot2)
library(earth)
library(dplyr)
library(tidyr)
library(tibble)
library(e1071)
library(AppliedPredictiveModeling)
library(patchwork)
library(fastDummies)
library(pls)

#Read in the data
Test_data <- read.csv("C:/Users/Kolby/OneDrive/Documents/School Stuff/Stat 6543/Poster/test.csv", header = TRUE, sep = ",")
Train_data <- read.csv("C:/Users/Kolby/OneDrive/Documents/School Stuff/Stat 6543/Poster/train.csv", header = TRUE, sep = ",")

####Preprocessing####

head(Train_data)
str(Train_data)

#Impute 0 for NAs

missing_values <- sapply(Train_data, function(x) sum(is.na(x)))
print(missing_values)
Train_data<- lapply(Train_data, function(x) {
  x[is.na(x)] <- 0
  return(x)
})

missing_values2 <- sapply(Test_data, function(x) sum(is.na(x)))
print(missing_values2)
Test_data<- lapply(Test_data, function(x) {
  x[is.na(x)] <- 0
  return(x)
})

Test_data <- as.data.frame(Test_data)
Test_data
Train_data <- as.data.frame(Train_data)

#Store our response variable

Train_dataY <- Train_data$SalePrice

#One-hot code our categorical variables so I can use them in our analysis

Train_data <- Train_data %>%
  mutate(across(where(is.character), as.factor))
Train_data <- dummy_cols(Train_data, remove_first_dummy = TRUE, remove_selected_columns = TRUE)
TrainNZV <- nearZeroVar(Train_data) #Remove categorical outcomes and other variables that have near zero variance
Train_data <- Train_data[,-TrainNZV]

Test_data1 <- Test_data %>%
  mutate(across(where(is.character), as.factor))
Test_data2 <- dummy_cols(Test_data1, remove_first_dummy = TRUE, remove_selected_columns = TRUE)
TestNZV <- nearZeroVar(Test_data2) #Remove categorical outcomes and other variables that have near zero variance
Test_data3 <- Test_data2[,-TestNZV]

#The variables that showed to have near zero variance varied to some degree between the test and training set.
#So I choose to get rid of any variable that showed near zero variance across both sets. 

ZVCtrain <- names(Train_data)[TrainNZV]
ZVCtest <- names(Test_data3)[TestNZV]

all_zero_var_cols <- union(ZVCtrain, ZVCtest)
Train_data <- Train_data[, !(names(Train_data) %in% all_zero_var_cols)]
Test_data3 <- Test_data3[, !(names(Test_data3) %in% all_zero_var_cols)]

extra_cols_in_test <- setdiff(colnames(Test_data3), colnames(Train_data))
extra_cols_in_train <- setdiff(colnames(Train_data), colnames(Test_data3))
Test_data3 <- Test_data3[, !(colnames(Test_data3) %in% extra_cols_in_test)]
Train_data <- Train_data[, !(colnames(Train_data) %in% extra_cols_in_train)]

#Makes sure the remaining variables are the same

print(colnames(Train_data))
print(colnames(Test_data3))

#Add our response variable back to the training set. 

Train_data$SalePrice <- Train_dataY

#Split to maintain data structure
Splits <- createDataPartition(Train_data$SalePrice, p = 0.8, list = FALSE)

Trained_data <- Train_data[Splits,]
Testing_data <- Train_data[-Splits,] #Split the training data into Trained/Testing - this is different from Test_data

#Check for skewness

SkewCheck <- function(df) {
  df %>%
    summarise(across(where(is.numeric), ~ skewness(.x, na.rm = TRUE)))
}

SkewCheck(Trained_data) #The data is relatively normal already

z<- ggplot(data = Trained_data, aes(x = SalePrice))+
  geom_histogram(fill = "pink", binwidth = 5000)+
  labs(y = "Non-Transformed")

#lightly preprocess the data by scaling and centering

Preprocess <- preProcess(Trained_data, method = c("center", "scale"))
Preprocess2 <- preProcess(Testing_data, method = c("center", "scale"))

Trained_data_trans <- predict(Preprocess, Trained_data)
Testing_data_trans <- predict(Preprocess2, Testing_data)

a <- ggplot(data = Trained_data_trans, aes(x = SalePrice))+
  geom_histogram(fill = "skyblue", binwidth = 0.1)+
  labs(y = "Transformed")

b <- ggplot(data = Testing_data_trans, aes(x = SalePrice))+
  geom_histogram()

(a | b)

(z | a)

####Model Building####

cntrl <- trainControl(method = "cv", number = 10) #set a universal trainControl

set.seed(1)

tuneGrid <- expand.grid(C = 2^(-2:4), sigma = seq(0, 0.1, 0.005))
tuneGrid

svmTune <- train(SalePrice~.,
                 data = Trained_data_trans,
                 method = "svmRadial",
                 tuneLength = 15,
                 tuneGrid = tuneGrid,
                 trControl = cntrl)
svmTune
#The final values used for the model were sigma = 0.005 and C = 8
plot(svmTune)
svmImp <- varImp(svmTune, scale = FALSE)
plot(svmImp)
# Num.1 is "OverallQual"

Results <- data.frame(Observed = Testing_data_trans$SalePrice)

Results$SVMr <- predict(svmTune, Testing_data_trans)

set.seed(1) 

plsTune <- train(SalePrice~.,
                 data = Trained_data_trans,
                 method = "pls",
                 tuneGrid = expand.grid(ncomp = 1:40),
                 trControl = trainControl(method = "cv", number = 10))
plsTune
#The final value used for the model was ncomp = 21
plot(plsTune)
plsImp <- varImp(plsTune, scale = FALSE)
plot(plsImp)
# Num.1 is "OverallQual"

Results$PLS <- predict(plsTune, Testing_data_trans)

nnetGrid <- expand.grid(decay = c(0, .1, .2, .3),
                        size = c(0, 1, 3, 5),
                        bag = FALSE)

ptm <- proc.time()

NetTune <- train(SalePrice~.,
                 data = Trained_data_trans,
                 method = "avNNet",
                 tuneGrid = nnetGrid,
                 trControl = cntrl,
                 linout = TRUE,
                 trace = FALSE,
                 MaxNWts = 2000,
                 maxit = 500,
                 allowParallel = FALSE,
                 learningrate = c(0.001, 0.01, 0.1))
NetTune
#The final values used for the model were size = 3, decay = 0.2 and bag = FALSE.
plot(NetTune)
proc.time() - ptm

netImp <- varImp(NetTune, scale = FALSE)
plot(netImp)
# Num.1 is "OverallQual"

Results$NNet <- predict(NetTune, Testing_data_trans)

marsTune <- train(SalePrice~.,
                  data = Trained_data_trans,
                  method = "earth",
                  tuneGrid = expand.grid(degree = c(2, 3, 4), nprune = 2:30),
                  trControl = cntrl)
marsTune
#The final values used for the model were nprune = 9 and degree = 2
plot(marsTune)
marsImp <- varImp(marsTune, scale = FALSE)
plot(marsImp)
# Num.1 is "OverallQual"

Results$MARS <- predict(marsTune, Testing_data_trans)

TestResults <- data.frame(rbind(SVM = postResample(pred = Results$SVMr ,obs = Testing_data_trans$SalePrice),
                                PLS = postResample(pred = Results$PLS ,obs = Testing_data_trans$SalePrice),
                                NNet = postResample(pred = Results$NNet ,obs = Testing_data_trans$SalePrice),
                                MARS = postResample(pred = Results$MARS, obs = Testing_data_trans$SalePrice)))

TestResults
#Neural network does much better in all three categoies, RSME, MAE and R squared

####Prediction####
set.seed(1)
str(Test_data3) #Reamining variables from the Test_data provided

Preprocess3 <- preProcess(Test_data3, method = c("center", "scale"))
Test_data_trans <- predict(Preprocess3, Test_data3)

#Using the neural netwrok model to predict the SalePrice for the Test_data after transformation
predicted_SalePrice <- predict(NetTune, Test_data_trans)
Test_data_trans$SalePrice <- predicted_SalePrice
summary(Test_data_trans$SalePrice)

summary(Train_data$SalePrice)

####REVERSE THE TRANSFORMATIONS####
mean_price <- mean(Train_data$SalePrice) #Reverse the centering
mean_price
sd_price <- sd(Train_data$SalePrice) #Reverse the scaling
sd_price
summary(Test_data_trans$SalePrice)

#The training data set was used to build the NNet model.
#I used the mean and standard deviation from that dataset's SalePrice to un-transform the sale price from this data. 

mean_price_train <- mean(Train_data$SalePrice)
sd_price_train <- sd(Train_data$SalePrice)

Test_data_trans$SalePrice <- Test_data_trans$SalePrice * sd_price_train + mean_price_train
summary(Test_data_trans$SalePrice)
summary(Train_data$SalePrice)

#performing the same transformation for OverallQual for further analysis. 

moq <- mean(Train_data$OverallQual, na.rm = TRUE)
sdoq <- sd(Train_data$OverallQual, na.rm = TRUE)

str(Test_data_trans)
summary(Test_data_trans$OverallQual)
Test_data_trans$OverallQual <- Test_data_trans$OverallQual * sdoq + moq
summary(Test_data_trans$OverallQual)



####Plots for Poster####

#General plots to be used for the poster#
#Checking correlation the SalePrice from the original dataset
cor_matrix <- cor(Train_data, use = "complete.obs")
cor_with_saleprice <- cor_matrix["SalePrice", ]
Cor_df <- data.frame(variable = names(cor_with_saleprice), correlation = cor_with_saleprice)
print(Cor_df)

svmImp <- varImp(svmTune, scale = FALSE) 
plot(svmImp)


#Capturing the most important variable, threashold set to 0.1
imp_df <- as.data.frame(netImp$importance)
imp_df$Feature <- rownames(imp_df)
imp_df <- imp_df %>%
  filter(Overall > 0.1)
#Converting importance to a percentage
imp_df$Overall <- (imp_df$Overall / sum(imp_df$Overall)) * 100

rownames(imp_df)

ImpNames <- c("Lot Area", "Overall Quality", "Year Built", "Year Remodeled", "Masonry Veneer Area", "Basement Finish", "Total Basement Area", "First Floor Area",
              "Second Floor Area", "Number of Full Baths", "Number of Rooms Above Ground", "Number of Cars - Garage", "Garage Area", "Wood Deck Area",
              "Porch Space", "Northridge Heights","Stone Veneer", "Good Exterior Quality", "Cinderblock Foundation")

imp_df$Feature <- rownames(imp_df)

#Horizontal bar graph of the variables model importance
FIG<- ggplot(imp_df, aes(x = reorder(Feature, Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "palegreen") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Importance of Features",
       x = "Features",
       y = "Importance (%)") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12)
  )
FIG



#Histogram of the sale price to show overal distribution
HomePriceTrunc <- ggplot(Test_data_trans, aes(x = SalePrice)) + 
  geom_histogram(binwidth = 5000, fill ="lightgoldenrod") + 
  theme_minimal() +
  labs(title = "Distribution of Predicted Sale Price",
       x = "Sale Price ($)",
       y = "Number of Homes") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12)
  ) +
  scale_x_continuous(labels = scales::comma)
HomePriceTrunc

#Distribution of the sale price from the given data set
Given <- ggplot(Train_data, aes(x = SalePrice)) + 
  geom_histogram(binwidth = 5000, fill = "lavender") + 
  theme_minimal() +
  labs(title = "Distribution of Observed Sale Prices",
       x = "Sale Price",
       y = "Number of Homes") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12)
  ) +
  scale_x_continuous(labels = scales::comma)
Given

#Tracking to affect of overall quality on sale price
ggplot(Test_data_trans, aes(x = OverallQual, y = SalePrice))+
  geom_jitter(color = "black", size = 1.5, alpha = 1) + 
  geom_smooth(color = "lightcoral", linetype = "solid", size = 1.2, se = FALSE) + 
  theme_minimal() + 
  labs(
    title = "Relationship Between Quality and Sale Price",
    x = "Rating of Overall Qualtity",
    y = "Sale Price"
  ) + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12)
  ) +
  scale_y_continuous(labels = scales::comma)

#More exploration of the top variables and some summary statistics for the poster
TopVar <- varImp(NetTune, scale = FALSE)
TopVar <- as.data.frame(TopVar$importance)
TopVar$Variable <- rownames(TopVar)

Top10Vars <- TopVar[order(-TopVar$Overall), ][1:10, ]

ImpVec <- as.vector(Top10Vars$Variable)

ImpVarTesting <- Test_data3[, ImpVec]
ImpVarTraining <- Train_data[, ImpVec]

summary(ImpVarTesting)
summary(ImpVarTraining)

HighQual <- Test_data_trans %>%
  filter(OverallQual >= 9)
NotHighQual <- Test_data_trans %>%
  filter(OverallQual < 9)
summary(HighQual$SalePrice)
min(HighQual$SalePrice)/mean(NotHighQual$SalePrice)


BigBSMT <- Test_data_trans %>%
  filter(TotalBsmtSF >= quantile(TotalBsmtSF, 0.75))

NotBigBSMT <- Test_data_trans %>%
  filter(TotalBsmtSF < quantile(TotalBsmtSF, 0.75))

(min(BigBSMT$SalePrice)-mean(NotBigBSMT$SalePrice)) /mean(NotBigBSMT$SalePrice)

#Citation of data used#
#misc{house-prices-advanced-regression-techniques,
#  author = {Anna Montoya, DataCanary},
#  title = {House Prices - Advanced Regression Techniques},
#  publisher = {Kaggle},
#  year = {2016},
#  url = {https://kaggle.com/competitions/house-prices-advanced-regression-techniques}
#}

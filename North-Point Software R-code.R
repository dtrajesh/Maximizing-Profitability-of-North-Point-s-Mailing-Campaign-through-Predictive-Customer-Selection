#---------------------------------Importing Libraries------------------------------------------
library(tidyverse)
library(dplyr)
library(caret)
library(fastDummies)
library(ggplot2)
library(gridExtra)
library(GGally)
library(MASS)
library(C50)
require(randomForest)
library(e1071)
library(rpart.plot)
library(psych)
library(plotly)
library(randomForest)
library(gains)
library(RWeka)
#--------------------------------Step 1 Collecting data-----------------------------------------------
#loading the data
northpointlist.df <- read.csv("North-Point List.csv")

#--------step 2 exploring and preparing data----------------------------------------------
#viewing first 6 rows of the data
head(northpointlist.df)

#Getting dimensions of the data
dim(northpointlist.df) #2000 rows and 25 columns

#Getting the structure of the data
str(northpointlist.df)

#Checking for null values
sum(is.na(northpointlist.df))

#summary for the numerical columns
summary(northpointlist.df[,c(18:20,25)])

#Correlation for all numerical columns
cor(northpointlist.df[,c(18:20,25)])

#VIF for columns
car::vif(lm(Purchase ~ ., data=northpointlist.df[,-1]))

# US Column  vs Purchase
barplot(table(northpointlist.df$US,northpointlist.df$Purchase),
        legend.text = TRUE,
        beside = TRUE,
        main = "US vs Purchase",
        xlab = "US",
        ylab = "Purchase")

#boxplot plot for US and Spending
boxplot(northpointlist.df$Spending ~ northpointlist.df$US, xlab="US", ylab="Spending")

#Source_a Column
#Source_a vs Purchase
barplot(table(northpointlist.df$source_a,northpointlist.df$Purchase),
        legend.text = TRUE,
        beside = TRUE,
        main = "source_a vs Purchase",
        xlab = "source_a",
        ylab = "Purchase")

#boxplot plot for Source_a and Spending
boxplot(northpointlist.df$Spending ~ northpointlist.df$source_a, xlab="source_a", ylab="Spending")

#Source_c Column vs purchase
barplot(table(northpointlist.df$source_c,northpointlist.df$Purchase),
        legend.text = TRUE,
        beside = TRUE,
        main = "source_c vs Purchase",
        xlab = "source_c",
        ylab = "Purchase")


#boxplot plot for Source_c and Spending
boxplot(northpointlist.df$Spending ~ northpointlist.df$source_c, xlab="source_c", ylab="Spending")


#Source_b Column
#source_b vs Purchase
barplot(table(northpointlist.df$source_b,northpointlist.df$Purchase),
        legend.text = TRUE,
        beside = TRUE,
        main = "source_b vs Purchase",
        xlab = "source_b",
        ylab = "Purchase")

#boxplot plot for Source_b and Spending
boxplot(northpointlist.df$Spending ~ northpointlist.df$source_b, xlab="source_b", ylab="Spending")

#Source_d Column

#Source_d vs Purchase
barplot(table(northpointlist.df$source_d,northpointlist.df$Purchase),
        legend.text = TRUE,
        beside = TRUE,
        main = "source_d vs Purchase",
        xlab = "source_d",
        ylab = "Purchase")

#boxplot plot for Source_d and Spending
boxplot(northpointlist.df$Spending ~ northpointlist.df$source_d, xlab="source_d", ylab="Spending")


#source_e Column
#source_e vs Purchase
barplot(table(northpointlist.df$source_e,northpointlist.df$Purchase),
        legend.text = TRUE,
        beside = TRUE,
        main = "source_e vs Purchase",
        xlab = "source_e",
        ylab = "Purchase")

#boxplot plot for Source_e and Spending
boxplot(northpointlist.df$Spending ~ northpointlist.df$source_e, xlab="source_e", ylab="Spending")


#Source_m Column

#Source_m vs Purchase
barplot(table(northpointlist.df$source_m,northpointlist.df$Purchase),
        legend.text = TRUE,
        beside = TRUE,
        main = "source_m vs Purchase",
        xlab = "source_m",
        ylab = "Purchase")

#boxplot plot for Source_m and Spending
boxplot(northpointlist.df$Spending ~ northpointlist.df$source_m, xlab="source_m", ylab="Spending")

#Source_o Column

#Source_o vs Purchase
barplot(table(northpointlist.df$source_o,northpointlist.df$Purchase),
        legend.text = TRUE,
        beside = TRUE,
        main = "source_o vs Purchase",
        xlab = "source_o",
        ylab = "Purchase")

#boxplot plot for Source_m and Spending
boxplot(northpointlist.df$Spending ~ northpointlist.df$source_o, xlab="source_o", ylab="Spending")

#Source_h Column
#Source_h vs Purchase
barplot(table(northpointlist.df$source_h,northpointlist.df$Purchase),
        legend.text = TRUE,
        beside = TRUE,
        main = "source_h vs Purchase",
        xlab = "source_h",
        ylab = "Purchase")


#boxplot plot for Source_h and Spending
boxplot(northpointlist.df$Spending ~ northpointlist.df$source_h, xlab="source_h", ylab="Spending")

#Source_r Column
#Source_r vs Purchase
barplot(table(northpointlist.df$source_r,northpointlist.df$Purchase),
        legend.text = TRUE,
        beside = TRUE,
        main = "source_r vs Purchase",
        xlab = "source_r",
        ylab = "Purchase")

#boxplot plot for Source_r and Spending
boxplot(northpointlist.df$Spending ~ northpointlist.df$source_r, xlab="source_r", ylab="Spending")


#Source_s Column
#Source_s vs Purchase
barplot(table(northpointlist.df$source_s,northpointlist.df$Purchase),
        legend.text = TRUE,
        beside = TRUE,
        main = "source_s vs Purchase",
        xlab = "source_s",
        ylab = "Purchase")

#boxplot plot for Source_s and Spending
boxplot(northpointlist.df$Spending ~ northpointlist.df$source_s, xlab="source_s", ylab="Spending")


#Source_t Column

#Source_t vs Purchase
barplot(table(northpointlist.df$source_t,northpointlist.df$Purchase),
        legend.text = TRUE,
        beside = TRUE,
        main = "source_t vs Purchase",
        xlab = "source_t",
        ylab = "Purchase")

#boxplot plot for Source_t and Spending
boxplot(northpointlist.df$Spending ~ northpointlist.df$source_t, xlab="source_t", ylab="Spending")

#Source_u Column
#Source_u vs Purchase
barplot(table(northpointlist.df$source_u,northpointlist.df$Purchase),
        legend.text = TRUE,
        beside = TRUE,
        main = "source_u vs Purchase",
        xlab = "source_u",
        ylab = "Purchase")

#boxplot plot for Source_u and Spending
boxplot(northpointlist.df$Spending ~ northpointlist.df$source_u, xlab="source_u", ylab="Spending")

#Source_p Column
#Source_p vs Purchase
barplot(table(northpointlist.df$source_p,northpointlist.df$Purchase),
        legend.text = TRUE,
        beside = TRUE,
        main = "source_p vs Purchase",
        xlab = "source_p",
        ylab = "Purchase")

#boxplot plot for Source_p and Spending
boxplot(northpointlist.df$Spending ~ northpointlist.df$source_p, xlab="source_p", ylab="Spending")

#Source_x Column
#Source_x vs Purchase
barplot(table(northpointlist.df$source_x,northpointlist.df$Purchase),
        legend.text = TRUE,
        beside = TRUE,
        main = "source_x vs Purchase",
        xlab = "source_x",
        ylab = "Purchase")

#boxplot plot for Source_x and Spending
boxplot(northpointlist.df$Spending ~ northpointlist.df$source_x, xlab="source_x", ylab="Spending")

#Source_w Column

#Source_w vs Purchase
barplot(table(northpointlist.df$source_w,northpointlist.df$Purchase),
        legend.text = TRUE,
        beside = TRUE,
        main = "source_w vs Purchase",
        xlab = "source_w",
        ylab = "Purchase")

#boxplot plot for Source_w and Spending
boxplot(northpointlist.df$Spending ~ northpointlist.df$source_w, xlab="source_w", ylab="Spending")


#Web.order Column

#Web.order vs Purchase
barplot(table(northpointlist.df$Web.order,northpointlist.df$Purchase),
        legend.text = TRUE,
        beside = TRUE,
        main = "Web.order vs Purchase",
        xlab = "Web.order",
        ylab = "Purchase")

#boxplot plot for Web.order and Spending
boxplot(northpointlist.df$Spending ~ northpointlist.df$Web.order, xlab="Web.order", ylab="Spending")


#Gender.male Column

#Gender.male vs Purchase
barplot(table(northpointlist.df$Gender.male,northpointlist.df$Purchase),
        legend.text = TRUE,
        beside = TRUE,
        main = "Gender.male vs Purchase",
        xlab = "Gender.male",
        ylab = "Purchase")

#boxplot plot for Gender.male and Spending
boxplot(northpointlist.df$Spending ~ northpointlist.df$Gender.male, xlab="Gender.male", ylab="Spending")


#Address_is_res Column

#Address_is_res vs Purchase
barplot(table(northpointlist.df$Address_is_res,northpointlist.df$Purchase),
        legend.text = TRUE,
        beside = TRUE,
        main = "Address_is_res vs Purchase",
        xlab = "Address_is_res",
        ylab = "Purchase")

#boxplot plot for Address_is_res and Spending
boxplot(northpointlist.df$Spending ~ northpointlist.df$Address_is_res, xlab="Address_is_res", ylab="Spending")

#freq column
plot_ly(northpointlist.df, x = ~Freq, y = ~Spending, type = 'bar')
#freq column vs Purchase
boxplot(northpointlist.df$Freq ~ northpointlist.df$Purchase, xlab="Purchase", ylab="Freq")

#last_update_days_ago column
plot_ly(northpointlist.df, x = ~last_update_days_ago, y = ~Spending, type = 'bar')
#last_update_days_ago column vs Purchase
boxplot(northpointlist.df$last_update_days_ago ~ northpointlist.df$Purchase, xlab="Purchase", ylab="last_update_days_ago")


#X1st_update_days_ago column
plot_ly(northpointlist.df, x = ~X1st_update_days_ago, y = ~Spending, type = 'bar')
#X1st_update_days_ago column vs Purchase
boxplot(northpointlist.df$X1st_update_days_ago ~ northpointlist.df$Purchase, xlab="Purchase", ylab="X1st_update_days_ago")

#histogram of Spending column
hist(northpointlist.df$Spending, xlab="Spending")

#Purchase vs Spending
boxplot(northpointlist.df$Spending ~ northpointlist.df$Purchase, xlab="Purchase", ylab="Spending")

# removing sequence_number column and 
northpointlist.df <- northpointlist.df[,2:25] %>%
  mutate(Spending = ifelse(Purchase == 0, 0, Spending))

#Getting the structure of the data after Preprocessing
str(northpointlist.df)

#setting seed value for repetition
set.seed(1)

# partitioning into training (70%), validation (20%) and test (10%)

train.rows <- sample(rownames(northpointlist.df),size = nrow(northpointlist.df)*0.4)
valid.rows <- sample(setdiff(rownames(northpointlist.df), train.rows),
                     size =nrow(northpointlist.df)*0.35)
test.rows <- setdiff(rownames(northpointlist.df), union(train.rows, valid.rows))

# create the 3 data frames by collecting all columns from the appropriate rows
train.df <- northpointlist.df[train.rows, ]
valid.df <- northpointlist.df[valid.rows, ]
test.df <- northpointlist.df[test.rows, ]

#Getting the dimensions of the partition data

dim(train.df)
dim(valid.df)
dim(test.df)



#Modelling for classification Task
###########################################################################################################
#Logistic Regression
model.logistic <- glm(Purchase ~ . ,data = train.df[,1:23], family = "binomial" )
#Summary of the model
summary(model.logistic)


#Predicting the validation data
model.logistic.validation.pred <- predict(model.logistic, valid.df[,1:22], type = "response")

confusionMatrix(factor(ifelse(model.logistic.validation.pred>0.5, 1, 0),levels=c(1,0)),
                factor(valid.df$Purchase,levels = c(1,0)),positive = "1")

#Setting full and null model
fit.null <- glm(Purchase ~ 1, data = train.df[,1:23],family = "binomial")
fit.full <- glm(Purchase ~ . ,data = train.df[,1:23], family = "binomial")

#Step wise logistic regression backward
model.logistic.backward <- step(fit.full, scope=list(lower=fit.null, upper=fit.full), direction = "backward")

#Summary of the backward model
summary(model.logistic.backward)

#Predicting the validation data
model.logistic.validation.pred <- predict(model.logistic.backward, valid.df[,1:22], type = "response")

confusionMatrix(factor(ifelse(model.logistic.validation.pred>0.5, 1, 0),levels=c(1,0)),
                factor(valid.df$Purchase,levels = c(1,0)),positive = "1")

#Step wise logistic regression forward
model.logistic.forward <- step(fit.null, scope=list(lower=fit.null, upper=fit.full), direction = "forward")
summary(model.logistic.forward)

#Predicting the validation data
model.logistic.validation.pred <- predict(model.logistic.forward, valid.df[,1:22], type = "response")

confusionMatrix(factor(ifelse(model.logistic.validation.pred>0.5, 1, 0),levels=c(1,0)),
                factor(valid.df$Purchase,levels = c(1,0)),positive = "1")

#Predicting the train  data to check if the model is over fitting or not
model.logistic.train.pred <- predict(model.logistic.forward, train.df[,1:22], type = "response")

confusionMatrix(factor(ifelse(model.logistic.train.pred>0.5, 1, 0),levels=c(1,0)),
                factor(train.df$Purchase,levels = c(1,0)),positive = "1")

#Confusion matrix for best classifier model
confusionMatrix(factor(ifelse(model.logistic.validation.pred>0.5, 1, 0),levels=c(1,0)),
                factor(valid.df$Purchase,levels = c(1,0)),positive = "1")

#Modelling for Regression Task
###############################################################################################################
#Filtering the data with only purchased customers

train.purchased.df <- train.df[train.df$Purchase == 1, ]
valid.purchased.df <- valid.df[valid.df$Purchase == 1, ]

dim(train.purchased.df)
dim(valid.purchased.df)


#Fitting linear model
model.linear <- lm(Spending ~ ., data = train.purchased.df[,c(1:22,24)])

#Summary of the model
summary(model.linear)
#Predicting 
linear.pred.valid <- predict(model.linear, valid.purchased.df[,c(1:22)])
#Rmse
caret::RMSE(linear.pred.valid, valid.purchased.df$Spending)


#Improving the model performance 
#Setting full and null model
linear.fit.null <- lm(Spending ~ 1, data = train.purchased.df[,c(1:22,24)])
linear.fit.full <- lm(Spending ~ . ,data = train.purchased.df[,c(1:22,24)])

#Step wise logistic regression backward
model.linear.forward <- step(linear.fit.null, scope=list(lower=linear.fit.null, upper=linear.fit.full), direction = "forward")

#Summary of the backward model
summary(model.linear.forward)

#Predicting the validation data 
linear.forward.pred.valid <- predict(model.linear.forward, valid.purchased.df[,c(1:22)])
#Rmse
caret::RMSE(linear.forward.pred.valid, valid.purchased.df$Spending)

#Decision Tree for regression 
model.regression.rpart <-rpart(Spending~ ., data=train.purchased.df[,c(1:22,24)])
#summary of the tree
summary(model.regression.rpart)

#plotting the tree
rpart.plot(model.regression.rpart, digits=3)
#Rules
rpart.rules(model.regression.rpart)
#Predicting the validation data
model.rpart.valid.pred <- predict(model.regression.rpart, valid.purchased.df[,1:22])

#Rmse
caret::RMSE(model.rpart.valid.pred, valid.purchased.df$Spending)

#Improving Performance 
Rmse_list <- list()
cp_values_list <- list()
cp_values <- seq(0.02, 0.1, by = 0.01)
for (cp in cp_values) {
model.regression.rpart.cp <-rpart(Spending~ ., data=train.purchased.df[,c(1:22,24)],cp = cp)
#Predicting the validation data
model.rpart.valid.pred <- predict(model.regression.rpart.cp, valid.purchased.df[,1:22])
#Rmse
rmse <-caret::RMSE(model.rpart.valid.pred, valid.purchased.df$Spending)
Rmse_list <- c(Rmse_list, rmse)}

#Getting the minimum Rmse value
Rmse_list

#Training the model with best cp 

model.regression.rpart.cp <-rpart(Spending~ ., data=train.purchased.df[,c(1:22,24)],cp =0.02 )

#plotting the tree
rpart.plot(model.regression.rpart.cp, digits=3)
#Rules
rpart.rules(model.regression.rpart.cp)

#Predicting the validation data
model.rpart.valid.pred <- predict(model.regression.rpart.cp, valid.purchased.df[,1:22])
#Rmse
caret::RMSE(model.rpart.valid.pred, valid.purchased.df$Spending)

#Predicting the train  data with best regression model
linear.backward.pred.train <- predict(model.linear.forward, train.purchased.df[,c(1:22)])
#Rmse
caret::RMSE(linear.backward.pred.train, train.purchased.df$Spending)



#Classifying the customers on the test data
test.df$pred_purchase_probability <- predict(model.logistic.forward, test.df[,1:22], type="response")

#Predicting the spending amount of the test data
test.df$predicted_spending <- predict(model.linear.forward, test.df[,1:22])

#Adjusting the predicted probability 
test.df$adj_pred_purchase_probability <- test.df$pred_purchase_probability*0.1065

#Adjusting the spending
test.df$excepted_spending <- (test.df$predicted_spending)*(test.df$adj_pred_purchase_probability)

head(test.df)
Spending <- test.df$Spending
gain <- gains(Spending, test.df$excepted_spending)

# cumulative lift chart
# we will compute the gain relative to Spending
df <- data.frame(
  ncases=c(0, gain$cume.obs),
  cumSpending=c(0, gain$cume.pct.of.total * sum(Spending))
)
g1 <- ggplot(df, aes(x=ncases, y=cumSpending)) +
  geom_line() +
  geom_line(data=data.frame(ncases=c(0, nrow(test.df)), cumSpending=c(0, sum(Spending))),
            color="gray", linetype=2) + # adds baseline
  labs(x="# Cases", y="Cumulative Spending", title="Cumulative gains chart") +
  scale_y_continuous(labels = scales::comma) 

# Decile-wise lift chart
df <- data.frame(
  percentile=gain$depth,
  meanResponse=gain$mean.resp / mean(Spending)
)
g2 <- ggplot(df, aes(x=percentile, y=meanResponse)) +
  geom_bar(stat="identity") +
  labs(x="Percentile", y="Decile mean / global mean", title="Decile-wise lift chart")

grid.arrange(g1, g2, ncol=2)




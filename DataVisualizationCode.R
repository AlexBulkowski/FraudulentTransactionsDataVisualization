# Data source https://www.kaggle.com/datasets/chitwanmanchanda/fraudulent-transactions-data?resource=download

# Used libraries
library(ggplot2)
library(magrittr)
library(dplyr)
library(reshape2)
library(tidyr)
library(pROC)
library(rpart)
library(rpart.plot)

d <- read.csv('https://www.kaggle.com/datasets/chitwanmanchanda/fraudulent-transactions-data?resource=download', sep=',')
df <- d %>% filter(amount > 0) # Discarding empty transactions

# Chart 1 - Histogram
# Total number of transactions per type of operation
options(scipen=1)
type <- df %>% count(type)
ggplot(data=type, aes(x=reorder(type, -n), y=n, fill=type))+
  ggtitle("Total number of transactions made")+
  geom_histogram(stat='identity')+
  labs(x="Operation type", y="Total number of operations")+
  scale_fill_discrete(name ="Operation type")

# Chart 2 - Freqpoly
# Number of transactions per type at a given step(hour)
df2_1 <- data.frame()
x <- 1
while(x < max(df$step)){
  df_now <- filter(df, step==x)
  df_now <- df_now %>% dplyr::count(type)
  df_now <- mutate(df_now, step = x)
  df2_1 <- rbind(df2_1,df_now)
  x <- x + 1
}

ggplot(data=df2_1, aes(x=step, y=n, color=type))+
  geom_freqpoly(stat='identity')+
  ggtitle("Number of operations during 30 days (744 steps)")+
  labs(x="Hours block(step)", y="Total number of operations")+
  scale_fill_discrete(name ="Operation type")

# Chart 3 - Histogram
# Total value of all transactions during 30 days (744 steps)
type_list <-list("CASH_IN", "CASH_OUT", "DEBIT", "PAYMENT", "TRANSFER")
df3 <- data.frame(matrix(ncol=2, nrow=0))

for (type in type_list) {
  df_now <- df[df$type ==type,]
  sum_amount <- sum(df_now$amount)
  object_now <- c(type, sum_amount)
  df3 <- rbind(df3, object_now)
}
col_names <- c("type", "sum_amount")
colnames(df3) <- col_names

ggplot(data=df3, aes(x=reorder(type, -as.numeric(sum_amount)), y=as.numeric(sum_amount)/1000000, fill=type))+
  geom_histogram(stat='identity')+
  ggtitle("Total value of all transactions")+
  labs(x="Operation type", y="Total value of all transactions in millions")+
  scale_fill_discrete(name ="Operation type")

# Chart 4 - Histogram
# Comparison of the number of transactions, to their total value. 
type <- df %>% count(type)

ggplot()+
  geom_histogram(data=df3, aes(x=reorder(type, -as.numeric(sum_amount)), y=as.numeric(sum_amount)/1000000, fill=type),stat='identity')+
  geom_col(data=type, aes(x=reorder(type, -n), y=n, fill=type), stat='identity',alpha=0.3)+
  ggtitle("Number and value of transactions")+
  labs(x="Operation type", y="Value", subtitle = "Transparent bar = number of transactions | Visible bar = total value of transactions in millions")+
  scale_fill_discrete(name ="Operation type")

# Chart 5 - Pie chart
# Comparison of the number of fraudulent transactions to all.
d4_1 <- df %>% count(isFraud) 

ggplot(d4_1, aes(x="", y=n, fill=factor(isFraud)))+
  geom_bar(width=1, stat='identity')+
  coord_polar("y", start=0)+
  theme_void()+
  ggtitle("Fraud and non-Fraud transations pie chart ")+
  scale_fill_discrete(name = "Type", labels = c("non-Fraud(0)", "Fraud(1)"))+
  ylab("") +
  xlab("") 

# Chart 6 - Boxplot
# Distribution of fraud and non-fraud transaction values
d5_fraud <- filter(d, isFraud==1)
d5_fraudType <- d5_fraud %>% count(type)

ggplot(data=d5_fraudType, aes(x=type, y=n, fill=type))+
  geom_bar(stat="identity")+
  ggtitle("Number and type of fraud transactions")+
  labs(x="Operation type", y="Number of transactions")+
  scale_fill_discrete(name ="Operation type")
#Chart shows that frauds only appear at CASH_OUT and TRANSFER operations
d5 <- filter(d, type == "CASH_OUT"|type=="TRANSFER")

ggplot(d5, aes(x=type, y=amount))+
  geom_boxplot(aes(fill=factor(isFraud)))+
  ggtitle("Distribution of CASH_OUT and TRANSFER transaction values")+
  labs(x="Operation type", y="Value of transactions", subtitle = "They are the only ones where fraud is perpetrated")+
  scale_fill_discrete(name = "Type", labels = c("non-Fraud(0)", "Fraud(1)"))

# Trying to build a models to predict fraud transations

# Logistic regression model
set.seed(2137)
n <- nrow(d)
i <- floor(0.5*n)
s <- sample.int(n, i, replace=FALSE)
d.train <- d[s,]
d.test <- d[-s,]

m_test <- glm(
  data=d.train,
  isFraud ~ amount + type + step  + oldbalanceOrg + oldbalanceDest,
  family="binomial")
summary(m_test)
# Ejection of variable with high p-value

m1 <- glm(
  data=d.train,
  isFraud ~ amount  + step  + oldbalanceDest,
  family="binomial")

predict.m1 <-predict(m1, d.test, type = "response")
#Chart 7
#Histogram of the model
hist(predict.m1)
#ROC of the model -> Sensitivity to Specificity
roc.m1 <-roc(d.test$isFraud, predict.m1)
plot(roc.m1)

#Decision tree model
m2 <- rpart(
  data = d.train,
  isFraud ~ amount + step + oldbalanceOrg
)
# Chart 8
# Plot of the model
rpart.plot(m2, box.palette="RdBu", shadow.col="gray", nn=TRUE)

# Model too complicated, must be turned into only the amount of transation decision tree

# Chart 9
# Right decision tree chart
m3 <- rpart(
  data = d.train,
  isFraud ~ amount 
)
rpart.plot(m3, box.palette="RdBu", shadow.col="gray", nn=TRUE)
plotcp(m2)


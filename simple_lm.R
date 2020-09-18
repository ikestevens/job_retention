#Script to read in the training data
setwd('C:/Users/isstevens/Documents/job_retention')
print(getwd())
options(scipen=20)

data <- read.csv("PP Train ALL.csv", header = TRUE)
full_test <- read.csv("PPP Test ALL.csv", header = TRUE)

head(data)

attach(data)

ind <- sample(2, nrow(data), replace = T, prob = c(0.8,0.2))
train <- data[ind==1, ]
test <- data[ind==2, ]

print(typeof(data$JobsRetained))

simpleModel1 <- lm(data$JobsRetained~data$LoanLower)

summary(simpleModel1)

plot(data$LoanLower, data$JobsRetained)
abline(simpleModel1, col='red')

str(data)
data$Zip = as.factor(data$Zip)
memory.limit(size= NA)
multiple_lm <- lm(JobsRetained~LoanLower + BusinessType + Gender + RaceEthnicity)

summary(multiple_lm)

p <- predict(multiple_lm, test, type="response") 
full_test_predict <- predict(multiple_lm, full_test, type="response") 
print(length(p))

plot(p,test$JobsRetained, xlab="predicted",ylab="actual")
abline(a=0,b=1)

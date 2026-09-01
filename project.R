## Tsithembile Tembo 
## Partner Name 
## CSP 571 

#loading data and looking at dataset 
diabetes <- read.csv("diabetes.csv")
str(diabetes)
dim(diabetes)

#clean data 
sum(is.na(diabetes))
sum(duplicated(diabetes)) #it is possible for people ot have similar cases, there is no patient id 

#which features are the most significant -- inference 
all.fit <- glm(Diabetes_binary ~ ., data = diabetes, family = binomial)
summary(all.fit) #smoker and NoDocbcCost have no significance 
diabetes$Smoker <- NULL
diabetes$NoDocbcCost <- NULL

# levels instead of begin significant of one another 
factor_columns <- c("Diabetes_binary", "HighBP", "HighChol", "CholCheck", "Stroke", "HeartDiseaseorAttack",
                    "PhysActivity", "Fruits", "Veggies", "HvyAlcoholConsump", "AnyHealthcare", "DiffWalk",
                    "Sex")
diabetes[factor_columns] <- lapply(diabetes[factor_columns], as.factor)
str(diabetes)


## logistic regression -- predictive 
# split training and testing data, 50 percent
set.seed(13)
train <- sample(1:nrow(diabetes), nrow(diabetes) * 0.5)
train.data <- diabetes[train, ]
test.data <- diabetes[-train, ]

glm.fit <- glm(Diabetes_binary ~ ., data = train.data, family = binomial)
summary(glm.fit)

# predict on testing data 
glm.probs <- predict(glm.fit, test.data, type = "response")
glm.pred <- rep(0, nrow(test.data))
glm.pred[glm.probs > .5] <- 1 

#evaluate model
mean(glm.pred != test.data$Diabetes_binary)
table(Predicted = glm.pred, Actual = test.data$Diabetes_binary)





















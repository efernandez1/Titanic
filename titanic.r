setwd("~/Desktop/Titanic Analysis")
.libPaths("~/Desktop/R_Packages/")

library("caret")

train <- read.csv("train.csv", header = TRUE) # Read in Titanic train set from Kaggle
test <- read.csv("test.csv", header = TRUE) # Read in Titanic test set from Kaggle

## Combine datasets -----------------------------
test$Survived <- "None"
combined <- rbind(train, test)
str(combined)
summary(combined)
## Refactor
combined$Survived <- as.factor(combined$Survived)
combined$Pclass <- as.factor(combined$Pclass)

## Create new variable "Title" ------------------
extractTitle <- function(name) {
    name <- as.character(name)
    
    if (length(grep("Miss.", name)) > 0) {
        return ("Miss.")
    } else if (length(grep("Master.", name)) > 0) {
        return ("Master.")
    } else if (length(grep("Mrs.", name)) > 0) {
        return ("Mrs.")
    } else if (length(grep("Mr.", name)) > 0) {
        return ("Mr.")
    } else {
        return ("Other")
    }
}

titles <- NULL
for (i in 1:nrow(combined)) {
    titles <- c(titles, extractTitle(combined[i,"Name"]))
}
combined$title <- as.factor(titles)

combined <- combined[c(2,3,5,13)]

train <- combined[1:891,]
test <- combined[892:1309,]

## Train Model ------------------------------
train$Survived <- droplevels(train$Survived)
model <- train(Survived~., data = train, method = "gbm")

## Make prediction---------------------------
Survived <- predict(model, test)

submission <- cbind(combined[892:1309,], Survived)
submission <- submission[,c(1,14)]

write.table(submission, file = "submission.csv", col.names = TRUE, row.names = FALSE, sep = ",")
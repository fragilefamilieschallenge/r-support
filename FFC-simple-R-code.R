# Code for the Fragile Families Challenge
# Stephen McKay AKA the_Brit

rm(list=ls())   # clean-up past R sessions
setwd("C:\\Users\\steve mckay\\DropBox\\Data\\FFChallenge")  # Use own directory here

# Read in training and background data, the latter may take a few minutes to run
train <- read.csv(file="train.csv",head=TRUE,sep=",")
background <- read.csv(file="background.csv",head=TRUE,sep=",")

# Create a dummy or T/F variable for gender of child
background$BOY=(background$cm1bsex==1)

# merge together a working file of training data with background data
m1 <- merge(background, train, by = "challengeID", all = TRUE)

# Run OLS models for the continuous outcomes
# gpa
model.gpa <-  lm(formula = gpa ~ BOY, data = m1)
summary(model.gpa)

# grit
model.grit <-   lm(formula = grit ~ BOY, data = m1)
summary(model.grit)

# materialHardship
model.h <- lm(formula = materialHardship ~ BOY, data = m1)
summary(model.h)

# Run logistic regression models for the binary outcomes
# eviction
model.e <- glm(formula = eviction ~ BOY, family = binomial(link = "logit"), 
      data = m1)
summary(model.e)

# layoff
model.l <-  glm(formula = layoff ~ BOY, family = binomial(link = "logit"), 
      data = m1)
summary(model.l)

# job training
model.j <- glm(formula = jobTraining ~ BOY, family = binomial(link = "logit"), 
      data = m1)
summary(model.j)

# create predictions on the background dataset
background$gpa <- predict(model.gpa, newdata = background)
background$grit <- predict(model.grit, newdata = background)
background$materialHardship <- predict(model.h, newdata = background)
# clarify want probabilites for the logits, not classification prediction
background$eviction <- predict(model.e, newdata = background, type = "response")
background$layoff <- predict(model.l, newdata = background, type = "response")
background$jobTraining <- predict(model.j, newdata = background, type = "response")

# Extract just those variables needed for the submission
MyData<-subset(background,select=c(challengeID,gpa,grit,materialHardship,eviction,layoff,jobTraining))

# patch any missing values left, with the mean, to avoid a submission failure
MyData$gpa[is.na(MyData$gpa)] <- 2.866738197
MyData$grit[is.na(MyData$grit)] <- 3.427538787
MyData$materialHardship[is.na(MyData$materialHardship)] <- 0.103744782
MyData$eviction[is.na(MyData$eviction)] <- 0.059629883
MyData$layoff[is.na(MyData$layoff)] <- 0.20908379
MyData$jobTraining[is.na(MyData$jobTraining)] <- 0.234770705

# Ensure no remaining NAs, and sensible-looking results
summary(MyData)

# Output to prediction file
write.csv(MyData, file = "prediction.csv",row.names=FALSE)

# Write out the narrative file too
fileConn<-file("narrative.txt")
writeLines(c("This file uses predictions based on gender of the child.",
             "Based on OLS for the continuous outcomes, and logits for the binary outcomes.",
             "Version 1, 23 April 2017."), fileConn)
close(fileConn) 
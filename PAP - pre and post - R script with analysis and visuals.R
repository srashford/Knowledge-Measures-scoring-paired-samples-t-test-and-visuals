-----------------------------------------------------------------------------
  # This script achieves the following:
  ## Scores pre- and post-test answers
  ## Calculates number correct and percent correct for each participant
  ## Joins and exports pre- and post-test data (inner join and full outer join)
  ## Compares pre- and post-test data using a paired samples t-test
  ## Creates visuals depicting pre- and post-test knowledge measures
  ----------------------------------------------------------------------------
  # Import pre and post files and rename -------------------------------------
library(readxl)
KG.pre <- read_excel("TheProgressiveASPractitioner_05_02_19 JJ Pre.xls")
KG.post <- read_excel("TheProgressiveASPractitioner_05_02_19 JJ Post.xls")

View(KG.pre)
View(KG.post)

# Score and prepare Pre-test data before joining --------------------------------------

KG.pre <-as.data.frame(KG.pre)

### Change column names by column number
names(KG.pre)[10]<-paste("QuestionA")
names(KG.pre)[11]<-paste("QuestionB")
names(KG.pre)[12]<-paste("QuestionC")
names(KG.pre)[13]<-paste("QuestionD")
names(KG.pre)[14]<-paste("QuestionE")
names(KG.pre)[15]<-paste("QuestionF")
names(KG.pre)[16]<-paste("Q1")
names(KG.pre)[17]<-paste("Q2")
names(KG.pre)[18]<-paste("Q3")

# Count number of rows to determine # of practitioners
number.participants <- nrow(KG.pre)
number.participants

# Using if statement - create a new column for each question that indicates correct or not correct (this will need to be done for each question)

#Q1
extractCorrectQ1 <- function(Q1) {
  Q1 <- as.factor(Q1)
  
  if (length(grep("Mastery", Q1)) > 0) {
    return ("1")
  } else {
    return ("0")
  }
}
correctQ1 <- NULL
for (i in 1:nrow(KG.pre)) {
  correctQ1 <- c(correctQ1, extractCorrectQ1(KG.pre[i,"Q1"]))
}

KG.pre$correctQ1 <- as.numeric(correctQ1)

#Q2

extractCorrectQ2 <- function(Q2) {
  Q2 <- as.factor(Q2)
  
  if (length(grep("A set of guidelines for the knowledge, skills and abilities necessary for individuals who work with youth?", Q2)) > 0) {
    return ("1")
  } else {
    return ("0")
  }
}
correctQ2 <- NULL
for (i in 1:nrow(KG.pre)) {
  correctQ2 <- c(correctQ2, extractCorrectQ2(KG.pre[i,"Q2"]))
}

KG.pre$correctQ2 <- as.numeric(correctQ2)

#Q3

extractCorrectQ3 <- function(Q3) {
  Q3 <- as.factor(Q3)
  
  if (length(grep("Interaction with Children/Youth", Q3)) > 0) {
    return ("1")
  } else {
    return ("0")
  }
}
correctQ3 <- NULL
for (i in 1:nrow(KG.pre)) {
  correctQ3 <- c(correctQ3, extractCorrectQ3(KG.pre[i,"Q3"]))
}

KG.pre$correctQ3 <- as.numeric(correctQ3)

head(KG.pre)

### Create a new column with total correct (number correct, percent correct in decimal, percent correct whole number)
KG.pre$pre.ncorrect<-KG.pre[,24]+KG.pre[,25]+KG.pre[,26]
KG.pre$pre.percorrect<-KG.pre[,27]/3
KG.pre$pre.percorrect.formated<-KG.pre[,27]/3*100

View(KG.pre)
# Score and prepare post test data before joining -----------------------------------

#
KG.post <-as.data.frame(KG.post)

View(KG.post)

# Change column names by column number
names(KG.post)[10]<-paste("QuestionA")
names(KG.post)[11]<-paste("QuestionB")
names(KG.post)[12]<-paste("QuestionC")
names(KG.post)[13]<-paste("QuestionD")
names(KG.post)[14]<-paste("QuestionE")
names(KG.post)[15]<-paste("QuestionF")
names(KG.post)[16]<-paste("Q1")
names(KG.post)[17]<-paste("Q2")
names(KG.post)[18]<-paste("Q3")



# Count number of rows to determine # of practitioners
number.participants <- nrow(KG.post)
number.participants

# Using if statement - create a new column for each question that indicates correct or not correct (this will need to be done for each question)

#Q1

extractCorrectQ1 <- function(Q1) {
  Q1 <- as.factor(Q1)
  
  if (length(grep("Mastery", Q1)) > 0) {
    return ("1")
  } else {
    return ("0")
  }
}
correctQ1 <- NULL
for (i in 1:nrow(KG.post)) {
  correctQ1 <- c(correctQ1, extractCorrectQ1(KG.post[i,"Q1"]))
}

KG.post$correctQ1 <- as.numeric(correctQ1)


#Q2

extractCorrectQ2 <- function(Q2) {
  Q2 <- as.factor(Q2)
  
  if (length(grep("A set of guidelines for the knowledge, skills and abilities necessary for individuals who work with youth?", Q2)) > 0) {
    return ("1")
  } else {
    return ("0")
  }
}
correctQ2 <- NULL
for (i in 1:nrow(KG.post)) {
  correctQ2 <- c(correctQ2, extractCorrectQ2(KG.post[i,"Q2"]))
}

KG.post$correctQ2 <- as.numeric(correctQ2)

#Q3

extractCorrectQ3 <- function(Q3) {
  Q3 <- as.factor(Q3)
  
  if (length(grep("Interaction with Children/Youth", Q3)) > 0) {
    return ("1")
  } else {
    return ("0")
  }
}
correctQ3 <- NULL
for (i in 1:nrow(KG.post)) {
  correctQ3 <- c(correctQ3, extractCorrectQ3(KG.post[i,"Q3"]))
}

KG.post$correctQ3 <- as.numeric(correctQ3)

head(KG.post)

### Create a new column with total correct (number correct, percent correct in decimal, percent correct whole number)
KG.post$post.ncorrect<-KG.post[,24]+KG.post[,25]+KG.post[,26]
KG.post$post.percorrect<-KG.post[,27]/3
KG.post$post.percorrect.formated<-KG.post[,27]/3*100


View(KG.post)


# Count the number correct for each question
correctQ1sum <- sum(KG.post$correctQ1)
correctQ1sum

correctQ2sum <- sum(KG.post$correctQ2)
correctQ2sum

correctQ3sum <- sum(KG.post$correctQ3)
correctQ3sum


# calculate percentile score

Q1percentile <- correctQ1sum / number.participants
Q1percentile

Q2percentile <- correctQ2sum / number.participants
Q2percentile

Q3percentile <- correctQ3sum / number.participants
Q3percentile


# Inner and full outer join -----------------------------------------------

KG.innerjoin.prepost<-merge(x=KG.pre,y=KG.post,by="Please enter your Last Name")
View(KG.innerjoin.prepost)

KG.fulljoin.prepost<-merge(x=KG.pre,y=KG.post,by="Please enter your Last Name", all=TRUE)
View(KG.fulljoin.prepost)

# Export tables-------------------------------------------------------------

write.table(KG.innerjoin.prepost, file="PAP_5.02.19_JJ_PrePost_InnerJoin.csv", sep=",", row.names = FALSE)

write.table(KG.fulljoin.prepost, file="PAP_5.02.19_JJ_PrePost_FullJoin.csv", sep=",", row.names = FALSE)

# Determine if the differences are significant INNER JOIN DATA-----

## Inner join boxplot

attach(KG.innerjoin.prepost)
boxplot(post.percorrect, pre.percorrect)

plot(post.percorrect, pre.percorrect)
abline(a=0, b=1)

## Inner join data - paired samples t-test

t.test(post.percorrect, pre.percorrect, mu=0, paired=T)

## Graph results

attach(KG.innerjoin.prepost)
m=c(mean(pre.percorrect.formated), mean(post.percorrect.formated))
names(m)=c("Pre-Knowledge Measure", "Post-Knowledge Measure")
se=c(sd(pre.percorrect.formated)/sqrt(length(pre.percorrect.formated)),
     sd(post.percorrect.formated)/sqrt(length(post.percorrect.formated)))

library(ggplot2)
library(scales)
windows()
bp=barplot(m, ylim=c(0,100), 
           xpd=F, 
           main="Progressive Afterschool Practitioner",
           ylab="Percent Correct",
           col="light blue")
arrows(x0=bp, y0=m-se, y1=m+se, code=3, angle=90)


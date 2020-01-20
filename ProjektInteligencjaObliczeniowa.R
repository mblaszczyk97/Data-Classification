library(readr)
file <- "C:/ProgramyZainstalowane/R_Workspace/drug_consumption.data"
drugs <- read_csv(file, col_names = FALSE)

names <- c("id", "age", "gender", "education", "country", "ethnicity",
           "nscore", "escore", "oscore", "ascore", "cscore", "impulsive",
           "ss", "alcohol", "amphet", "amyl", "benzos", "caff", "cannabis",
           "choc", "coke", "crack", "ecstasy", "heroin", "ketamine", "legalh",
           "lsd", "meth", "mushrooms", "nicotine", "semer", "vsa")
drugs <- setNames(drugs, names)
drugs[30]
drugs2 <- drugs

#Data normalization
for (i in 1:nrow(drugs)) {
  if (drugs["age"][i,] == -0.95197) drugs["age"][i,] <- "18-24"
  else if (drugs["age"][i,] == -0.07854) drugs["age"][i,] <- "25-34"
  else if (drugs["age"][i,] == 0.49788) drugs["age"][i,] <- "35-44"
  else if (drugs["age"][i,] == 1.09449) drugs["age"][i,] <- "45-54"
  else if (drugs["age"][i,] == 1.82213) drugs["age"][i,] <- "55-64"
  else if (drugs["age"][i,] == 2.59171) drugs["age"][i,] <- "65+"
  
  if (drugs["gender"][i,] == 0.48246) drugs["gender"][i,] <- "Female"
  else if (drugs["gender"][i,] == -0.48246) drugs["gender"][i,] <- "Male"
  
  if (drugs["education"][i,] == -2.43591) drugs["education"][i,] <- "Left school before 16 years"
  else if (drugs["education"][i,] == -1.73790) drugs["education"][i,] <- "Left school at 16 years"
  else if (drugs["education"][i,] == -1.43719) drugs["education"][i,] <- "Left school at 17 years"
  else if (drugs["education"][i,] == -1.22751) drugs["education"][i,] <- "Left school at 18 years"
  else if (drugs["education"][i,] == -0.61113) drugs["education"][i,] <- "Some college or university, no certificate or degree"
  else if (drugs["education"][i,] == -0.05921) drugs["education"][i,] <- "Professional certificate/ diploma"
  else if (drugs["education"][i,] == 0.45468) drugs["education"][i,] <- "University degree"
  else if (drugs["education"][i,] == 1.16365) drugs["education"][i,] <- "Masters degree"
  else if (drugs["education"][i,] == 1.98437) drugs["education"][i,] <- "Doctorate degree"
  
  if (drugs["country"][i,] == -0.09765) drugs["country"][i,] <- "Australia"
  else if (drugs["country"][i,] == 0.24923) drugs["country"][i,] <- "Canada"
  else if (drugs["country"][i,] == -0.46841) drugs["country"][i,] <- "New Zealand"
  else if (drugs["country"][i,] == -0.28519) drugs["country"][i,] <- "Other"
  else if (drugs["country"][i,] == 0.21128) drugs["country"][i,] <- "Republic of Ireland"
  else if (drugs["country"][i,] == 0.96082) drugs["country"][i,] <- "UK"
  else if (drugs["country"][i,] == -0.57009) drugs["country"][i,] <- "USA"
  
  if (drugs["ethnicity"][i,] == -0.50212) drugs["ethnicity"][i,] <- "Asian"
  else if (drugs["ethnicity"][i,] == -1.10702) drugs["ethnicity"][i,] <- "Black"
  else if (drugs["ethnicity"][i,] == 1.90725) drugs["ethnicity"][i,] <- "Mixed-Black/Asian"
  else if (drugs["ethnicity"][i,] == 0.12600) drugs["ethnicity"][i,] <- "Mixed-White/Asian"
  else if (drugs["ethnicity"][i,] == -0.22166) drugs["ethnicity"][i,] <- "Mixed-White/Black"
  else if (drugs["ethnicity"][i,] == 0.11440) drugs["ethnicity"][i,] <- "Other"
  else if (drugs["ethnicity"][i,] == -0.31685) drugs["ethnicity"][i,] <- "White"

  if (drugs["nicotine"][i,] == "CL0") drugs["nicotine"][i,] <- "Never Used"
  else if (drugs["nicotine"][i,] == "CL1") drugs["nicotine"][i,] <- "Used over a Decade Ago"
  else if (drugs["nicotine"][i,] == "CL2") drugs["nicotine"][i,] <- "Used in Last Decade"
  else if (drugs["nicotine"][i,] == "CL3") drugs["nicotine"][i,] <- "Used in Last Year"
  else if (drugs["nicotine"][i,] == "CL4") drugs["nicotine"][i,] <- "Used in Last Month"
  else if (drugs["nicotine"][i,] == "CL5") drugs["nicotine"][i,] <- "Used in Last Week"
  else if (drugs["nicotine"][i,] == "CL6") drugs["nicotine"][i,] <- "Used in Last Day"
}

statistics <- list("ages" = table(drugs["age"]),
                   "genders" = table(drugs["gender"]),
                   "education" = table(drugs["education"]),
                   "country" = table(drugs["country"]),
                   "ethnicity" = table(drugs["ethnicity"]),
                   "alcohol" = table(drugs.alcohol["alcohol"]))

statistics$ages
statistics$genders
statistics$education
statistics$country
statistics$ethnicity
statistics$alcohol

# -----Classification data in alcohol -------------------------------

drugs.alcohol <- drugs2[c(2:13,14,30)]
drugs.alcohol2 <- drugs.alcohol
drugs.alcohol2$alcoholNum <- NA

for (i in 1:nrow(drugs.alcohol)) {
  nonuserSet <- c("CL0", "CL1","CL2", "CL3")
  userSet <- c("CL4", "CL5", "CL6")
  if (drugs.alcohol["alcohol"][i,] %in% nonuserSet) {
    drugs.alcohol["alcohol"][i,] <- "Did not drunk for at least 1 year"
  } else if (drugs.alcohol["alcohol"][i,] %in% userSet) {
    drugs.alcohol["alcohol"][i,] <- "Drunk alcohol for at least 1 year"
  }
}


# ----- WYKRESY ----------------------------------------------------------

install.packages("plotrix")
library(plotrix)

#---Alcohol drunk by gender---
drugsGender <- data.frame("Gender" <- drugs["gender"],
                          "Class" <- drugs.alcohol["alcohol"])

tabDG <- table(drugsGender)
round(tabDG/sum(tabDG)*100, 2)

drugsGenderVal <- c(tabDG[1], tabDG[2], tabDG[3], tabDG[4])
drugsGenderNames <- c("Female not drinker",
                      "Male not drinker",
                      "Female drinker",
                      "Male drinker")

pie(drugsGenderVal, labels = drugsGenderNames,
    col = c("pink", "blue", "red", "darkblue"),
    main="Alcohol drunk by gender")

#---Alcohol drunk by age---
drugsAge <- data.frame("Age" <- drugs["age"],
                       "Class" <- drugs.alcohol["alcohol"])

tabDA <- table(drugsAge)
round(tabDA/sum(tabDA)*100, 2)

barplot(t(table(drugsAge)),
        main="Alcohol drunk by age",
        xlab="Age",
        ylab="Number of people")

#---Alcohol drunk by education---
drugsEducation <- data.frame("Education" <- drugs["education"],
                             "Class" <- drugs.alcohol["alcohol"])

tabDEdu <- table(drugsEducation)
round(tabDEdu/sum(tabDEdu)*100, 2)

par(mar=c(5, 20, 5, 1))
barplot(t(table(drugsEducation)),
        las=1,
        main="Alcohol drunk by education",
        xlab="Number of people",
        horiz = TRUE)

# ---Alcohol Drunk by country---
drugsCountry <- data.frame("Country" <- drugs["country"],
                           "Class" <- drugs.alcohol["alcohol"])

tabDC <- table(drugsCountry)
round(tabDC/sum(tabDC)*100, 2)

barplot(t(table(drugsCountry)),
        main="Alcohol drunk by country",
        xlab="Country",
        ylab="Number of people")


# ----- Normalization----------------------------------------------

norm <- function(x) {
  (x-min(x))/(max(x)-min(x))
}

drugs.norm <- data.frame(norm(drugs.alcohol[1]), norm(drugs.alcohol[2]),
                         norm(drugs.alcohol[3]), norm(drugs.alcohol[4]),
                         norm(drugs.alcohol[5]), norm(drugs.alcohol[6]),
                         norm(drugs.alcohol[7]), norm(drugs.alcohol[8]),
                         norm(drugs.alcohol[9]), norm(drugs.alcohol[10]),
                         norm(drugs.alcohol[11]), norm(drugs.alcohol[12]),
                         drugs.alcohol[13])

# -----Test and Training Sets----------------------------

set.seed(1234)
ind <- sample(2, nrow(drugs.norm), replace=TRUE, prob=c(0.67, 0.33))
drugs.train <- drugs.norm[ind==1,1:13]
drugs.test <- drugs.norm[ind==2,1:13]

drugsOld.train <- drugs.norm[ind==1,1:13]
drugsOld.test <- drugs.norm[ind==2,1:13]

set.seed(1234)
ind <- sample(2, nrow(drugs.norm2), replace=TRUE, prob=c(0.67, 0.33))
drugs.train2 <- drugs.norm2[ind==1,1:13]
drugs.test2 <- drugs.norm2[ind==2,1:13]
#------Linear Classification - Quadratic Discriminant Analysis and Linear Discriminant Analysis-----------------------------
#install.packages("MASS")
library(MASS)

#Linear Discriminant Analysis
drugs.lda<-MASS::lda(factor(alcohol)~age+gender+education+country+
              ethnicity+nscore+escore+oscore+ascore+cscore+impulsive+ss,drugsOld.train)
summary(drugs.lda)
drugs.lda.predicted<-predict(drugs.lda,drugsOld.test[,1:12])
drugs.lda.predicted[1]
drugs.lda.tab1<-table(as.matrix(as.data.frame(drugs.lda.predicted[1])),drugs.real)
drugs.lda.conf<-confusionMatrix(drugs.lda.tab1)
drugs.lda.accuracy=drugs.lda.conf$overall[1][1]*100
drugs.lda.accuracy

#Quadratic Discriminant Analysis
drugs.mass<-MASS::qda(factor(alcohol)~age+gender+education+country+
                  ethnicity+nscore+escore+oscore+ascore+cscore+impulsive+ss,drugsOld.train)

summary(drugs.mass)
drugs.mass.predicted<-predict(drugs.mass,drugsOld.test[,1:12])
drugs.mass.predicted[1]
drugs.mass.tab1<-table(as.matrix(as.data.frame(drugs.mass.predicted[1])),drugs.real)
drugs.mass.conf<-confusionMatrix(drugs.mass.tab1)
drugs.mass.accuracy=drugs.mass.conf$overall[1][1]*100


#------Linear Classification - Logistic regression--------------------------------------------------
#install.packages("VGAM")
library(VGAM)

drugs.vgam<-vglm(factor(alcohol)~age+gender+education+country+
                   ethnicity+nscore+escore+oscore+ascore+cscore+impulsive+ss,
                 family = "multinomial",drugsOld.train)

summary(drugs.vgam)
drugs.vgam.predicted<-predict(drugs.vgam,drugsOld.test[,1:12],type="response")
drugs.vgam.tab1<-table(apply(drugs.vgam.predicted,1,which.max),drugs.real)
drugs.vgam.tab<-drugs.mda.tab
drugs.vgam.tab[1]=drugs.vgam.tab1[1]
drugs.vgam.tab[2]=drugs.vgam.tab1[2]
drugs.vgam.tab[2][1]=drugs.vgam.tab1[2][1]
drugs.vgam.conf<-confusionMatrix(drugs.vgam.tab)
cf$overall[1][1]*100
drugs.vgam.accuracy=drugs.vgam.conf$overall[1][1]*100

#------Flexible Discriminant Analysis--------------------------------------------------
#install.packages("mda")
#install.packages("caret")
library(mda)
library(caret)

drugs.mda<-fda(factor(alcohol)~age+gender+education+country+
                 ethnicity+nscore+escore+oscore+ascore+cscore+impulsive+ss,data=drugsOld.train)
summary(drugs.mda)
drugs.mda.predicted<-predict(drugs.mda,drugsOld.test[,1:12],type="class")
drugs.mda.tab<-table(drugs.mda.predicted,drugs.real)
cf<-confusionMatrix(drugs.mda.tab)
cf$overall[1][1]*100
drugs.mda.accuracy=cf$overall[1][1]*100



#------GBM--------------------------------------------------
library(gbm)
drugs.gbm<-gbm(factor(alcohol)~age+gender+education+country+
             ethnicity+nscore+escore+oscore+ascore+cscore+impulsive+ss,data=drugsOld.train,
             distribution="multinomial")
summary(drugs.gbm)
drugs.gbm.predicted<-predict(drugs.gbm,drugsOld.test[,1:12],n.trees=1)
drugs.gbm.eval<-colnames(drugs.gbm.predicted)[apply(drugs.gbm.predicted,1,which.max)]
drugs.real <- drugsOld.test[,13]
drugs.gbm.tab<-table(drugs.gbm.eval,drugs.real)
drugs.gbm.tab
confusionMatrix(drugs.gbm.tab)
drugs.gbm.eval.acc=491/590 *100
# -----C4.5-----------------------------------
#install.packages("RWeka")
library(RWeka)

drugs.c45<-J48(factor(alcohol) ~ age + gender + education + country + ethnicity
               + nscore + escore + oscore + ascore + cscore + impulsive + ss,
               data=drugsOld.train)
summary(drugs.c45)
drugs.c45.predicted <- predict(drugs.c45, drugsOld.test[,1:12])
drugs.c45.real <- drugsOld.test[,13]
drugs.c45.conf.matrix <- table(drugs.c45.predicted, drugs.c45.real)
drugs.c45.accuracy <- sum(diag(drugs.c45.conf.matrix))/sum(drugs.c45.conf.matrix) *100
drugs.c45.accuracy
# -----Bagging CART-----------------------------------
#install.packages("ipred")
library(ipred)

drugs.bagg<-bagging(factor(alcohol) ~ age + gender + education + country + ethnicity
                    + nscore + escore + oscore + ascore + cscore + impulsive + ss,
                    data=drugsOld.train)
summary(drugs.bagg)
drugs.bagg.predicted <- predict(drugs.bagg, drugsOld.test[,1:12])
drugs.bagg.real <- drugsOld.test[,13]
drugs.bagg.conf.matrix <- table(drugs.bagg.predicted, drugs.bagg.real)
drugs.bagg.accuracy <- sum(diag(drugs.bagg.conf.matrix))/sum(drugs.bagg.conf.matrix) *100
drugs.bagg.accuracy

# -----Decision Tree-----------------------------------
#install.packages("partykit")
#install.packages("party")
library(party)
library(partykit)

#New algorythm 18 July 2019
drugsNew.ctree <- partykit::ctree(factor(alcohol) ~ age + gender + education + country + ethnicity
                            + nscore + escore + oscore + ascore + cscore + impulsive + ss,
                            data=drugsOld.train)
print(drugs.ctree)
plot(drugs.ctree)
plot(drugs.ctree, type="simple")

treeNew.predicted <- predict(drugsNew.ctree, drugsOld.test[,1:12])
treeNew.real <- drugsOld.test[,13]
treeNew.conf.matrix <- table(treeNew.predicted, treeNew.real)
treeNew.accuracy <- sum(diag(treeNew.conf.matrix))/sum(treeNew.conf.matrix)
treeNew.accuracy

#Old algorythm
drugs.ctree <- party::ctree(factor(alcohol) ~ age + gender + education + country + ethnicity
                     + nscore + escore + oscore + ascore + cscore + impulsive + ss,
                     data=drugsOld.train)
print(drugs.ctree)
plot(drugs.ctree)
plot(drugs.ctree, type="simple")

tree.predicted <- predict(drugs.ctree, drugsOld.test[,1:12])
tree.real <- drugsOld.test[,13]
tree.conf.matrix <- table(tree.predicted, tree.real)
tree.accuracy <- sum(diag(tree.conf.matrix))/sum(tree.conf.matrix)


# -----KNN-------------------------------------------------

#install.packages("class")
library(class)

knn.3 <- knn(drugs.train[,1:12], drugs.test[,1:12], cl=drugs.train[,13], k=3, prob=FALSE)

knn.predicted <- knn.3
knn.real <- drugs.test[,13]
knn.conf.matrix <- table(knn.predicted, knn.real)
knn.accuracy <- sum(diag(knn.conf.matrix))/sum(knn.conf.matrix)


# -----NaiveBayes Porbability------------------------------------------
#install.packages("naivebayes")
#install.packages("e1071")
library(e1071)
library(naivebayes)

#Method from June 3/2019 Year 
nbayesNew <- bernoulli_naive_bayes(as.matrix(drugs.train[,1:12]), drugs.train[,13], laplace = 0.5)
nbayesNew.predicted <- predict(nbayesNew, as.matrix(drugs.test[,1:12]))
nbayesNew.real <- drugs.test[,13]
nbayesNew.conf.matrix <- table(nbayesNew.predicted, nbayesNew.real)
nbayesNew.accuracy <- sum(diag(nbayesNew.conf.matrix))/sum(nbayesNew.conf.matrix)


#Older method
nbayes <- naiveBayes(drugs.train[,1:12], drugs.train[,13])
nbayes$levels <- c("Did not drunk for at least 1 year", "Drunk alcohol for at least 1 year")
nbayes.predicted <- predict(nbayes, drugs.test[,1:12])
nbayes.real <- drugs.test[,13]
nbayes.conf.matrix <- table(nbayes.predicted, nbayes.real)
nbayes.accuracy <- sum(diag(nbayes.conf.matrix))/sum(nbayes.conf.matrix)

#-----Random Forest-----------------------------------------------
#install.packages("randomForest")
library(randomForest)
head(drugsOld.train)
data <- drugsOld.train
dataValid <- drugsOld.test
summary(data)
sapply(data, class)
data <- transform(
  data,
  alcohol=as.factor(alcohol)
)
dataValid <- transform(
  dataValid,
  alcohol=as.factor(alcohol)
)
model1 <- randomForest(alcohol ~ ., data = data, importance = TRUE)
model1
predTrain <- predict(model1, dataValid, type = "class")
# Checking classification accuracy
table(predTrain, data$alcohol)  

accuracyRandomForest = mean(predTrain == dataValid$alcohol) 

# -----SVM-----------------------------------------------
library("e1071")
data <- drugs.train
model_svm <- svm(alcohol ~ ., data=data)
summary(model_svm)
pred <- predict(model_svm,dataValid)
table(pred, data$alcohol)
accuracySVM = mean(pred == dataValid$alcohol) 

# -----Neural Net-----------------------------------------------
#install.packages("gbm")
library(neuralnet)
library("class")
library("e1071")
library(tidyverse)
drugs.train[,1:12]
drugs.train[,13]
drugs.test[,1:12]
drugs.norm
drugs.train$tested_positive <- 0
drugs.train$tested_negative <- 0
for (row in 1:nrow(drugs.train)) {
  if (drugs.train[row,]["alcohol"] == "Drunk alcohol for at least 1 year") drugs.train[row,]["tested_positive"] = 1
  if (drugs.train[row,]["alcohol"] == "Did not drunk for at least 1 year") drugs.train[row,]["tested_negative"] = 1
}

drugs.train <- subset(drugs.train, select = -c(alcohol))
drugs.neuralnet <- neuralnet(tested_positive + tested_negative ~ age + gender + education + country + ethnicity + nscore + escore + oscore + ascore + cscore + impulsive + ss,
                             drugs.train, hidden=4)
drugs.pred <- neuralnet::compute(drugs.neuralnet, drugs.test[,1:12])
plot(drugs.neuralnet)
drugs.pred_species <- c()
for (row in 1:nrow(drugs.pred$net.result)) {
  column <- match(max(drugs.pred$net.result[row,]), drugs.pred$net.result[row,])
  if (column == 1) drugs.pred_species <- c(drugs.pred_species, "tested_positive")
  if (column == 2) drugs.pred_species <- c(drugs.pred_species, "tested_negative")
}

drugs.comparison <- cbind("real" = as.character(drugs.test["alcohol"][,1]), "predicted" = drugs.pred_species)
drugs.result <- c()
for (row in 1:nrow(drugs.comparison)) {
  if (drugs.comparison[,1][row] == "Drunk alcohol for at least 1 year" && drugs.comparison[,2][row] == "tested_positive" 
   || drugs.comparison[,1][row] == "Did not drunk for at least 1 year" && drugs.comparison[,2][row] == "tested_negative" ) drugs.result <- c(drugs.result, TRUE)
  else drugs.result <- c(drugs.result, FALSE)
}
sum(drugs.result, na.rm = TRUE)
NeuronNetAccuracy <- as.numeric(table(drugs.result)["TRUE"])/as.numeric(table(drugs.result)["TRUE"]+table(drugs.result)["FALSE"])*100
NeuronNetAccuracy

# -----Accuracy Charts-----------------------------------------------

accuracies <- c("Decision Tree" = tree.accuracy*100,
                "SVM"=accuracySVM*100,
                "NaiveBayes" = nbayes.accuracy*100,
                "Random Forest" = accuracyRandomForest*100,
                "Neural Net" = NeuronNetAccuracy,
                "kNN" = knn.accuracy*100)
accuracyPlot <- barplot(accuracies,
                        col = c("pink", "lightblue", "lightgreen", "lightyellow","black","grey"),
                        main="Accuracy of every method",
                        xlab="Method", ylab="% Percent of accuracy")
text(x = accuracyPlot, y = accuracies, label = round(accuracies,5),
     pos = 1, cex = 0.8, col = "red")


accuracies <- c(
  "Naive Bayes Bernoulli" = nbayesNew.accuracy*100,
  "CIT New" = treeNew.accuracy*100,
  "Logistic Regression" = drugs.lda.accuracy,
  "Neuron Net" = NeuronNetAccuracy)
accuracyPlot <- barplot(accuracies,
                        col = c("pink", "lightblue", "lightgreen", "lightyellow","black","grey"),
                        main="Accuracy of every method",
                        xlab="Method", ylab="% Percent of accuracy")
text(x = accuracyPlot, y = accuracies, label = round(accuracies,5),
     pos = 1, cex = 0.8, col = "red")

accuracies <- c("QDA" = drugs.mass.accuracy,
                "Naive Bayes Bernoulli" = nbayesNew.accuracy*100,
                "SVM" = accuracySVM*100,
                "FDA" = drugs.mda.accuracy,
                "Naive Bayes" = nbayes.accuracy*100,
                "Knn" = knn.accuracy*100)
accuracyPlot <- barplot(accuracies,
                        col = c("pink", "lightblue", "lightgreen", "lightyellow","black","grey"),
                        main="Accuracy of every method",
                        xlab="Method", ylab="% Percent of accuracy")
text(x = accuracyPlot, y = accuracies, label = round(accuracies,5),
     pos = 1, cex = 0.8, col = "red")

accuracies <- c("Ctree New" = treeNew.accuracy*100,
                "Ctree" = tree.accuracy*100)
accuracyPlot <- barplot(accuracies,
                        col = c("pink", "lightblue"),
                        main="Accuracy of Old and New Ctree",
                        xlab="Method", ylab="% Accuracy percent")

text(x = accuracyPlot, y = accuracies, label = round(accuracies,5),
     pos = 1, cex = 0.8, col = "red")

accuracies <- c(
  "CIT" = tree.accuracy*100,
  "CIT New" = treeNew.accuracy*100,
  "GBM"=drugs.gbm.eval.acc,
  "Random Forest" = accuracyRandomForest*100,
  "Bagging Tree" = drugs.bagg.accuracy,
  "4.5 Decision Tree" = drugs.c45.accuracy)
accuracyPlot <- barplot(accuracies,
                        col = c("pink", "lightblue", "lightgreen", "lightyellow","black","grey"),
                        main="Accuracy of every method",
                        xlab="Method", ylab="% Percent of accuracy")
text(x = accuracyPlot, y = accuracies, label = round(accuracies,5),
     pos = 1, cex = 0.8, col = "red")

accuracies <- c("Logistic Regression" = drugs.vgam.accuracy,
                "Linear Discriminant Analysis" = drugs.lda.accuracy)
accuracyPlot <- barplot(accuracies,
                        col = c("pink", "lightblue", "lightgreen", "lightyellow","black","grey"),
                        main="Accuracy of every method",
                        xlab="Method", ylab="% Percent of accuracy")
text(x = accuracyPlot, y = accuracies, label = round(accuracies,5),
     pos = 1, cex = 0.8, col = "red")

# ----- K-means -------------------------------------
#install.packages("fpc")
#install.packages("dbscan")
#install.packages("editrules")
library(editrules)

drugs.log <- log(drugs.norm[,1:12])
drugs.log <- drugs.log[is.finite(rowSums(drugs.log)),]
drugs.log$gender <- NULL

drugs.scale <- scale(drugs.log, center=TRUE)
drugs.pca <- prcomp(drugs.scale)
drugs.final <- predict(drugs.pca)
drugs.final <- drugs.final[,1:11]

drugs.kmeans <- kmeans(drugs.final, 2)
table(drugs.kmeans[["cluster"]])

dev.off() # par(...) reset
plot(drugs.final, col = drugs.kmeans[["cluster"]],
     main="K-mean")
points(drugs.kmeans[["centers"]], col = 1:2, pch = 16, cex=1.5)
#DBSCAN
library("factoextra")
set.seed(123)
# fpc package
dbscan::kNNdistplot(drugs.final, k =  2)
res.fpc <- fpc::dbscan(drugs.final, eps = 3.3, MinPts = 2)
plot(res.fpc, drugs.final, main = "DBSCAN", frame = FALSE)
fviz_cluster(drugs.kmeans, drugs.final, stand = FALSE, frame = FALSE, geom = "point")

#K-medoids
library(cluster)
library(factoextra)
pam.res <- pam(drugs.final, 2)
print(pam.res)
fviz_cluster(pam.res, drugs.final, stand = FALSE, frame = FALSE, geom = "point")

#-----Associastion rulsets-----------------------------------------------

library(arules)
library(arulesViz)
raDrugs <- drugs[,2:6]
raDrugs$alcohol <- drugs.alcohol$alcohol

raDrugs$age <- factor(raDrugs$age)
raDrugs$gender <- factor(raDrugs$gender)
raDrugs$education <- factor(raDrugs$education)
raDrugs$country <- factor(raDrugs$country)
raDrugs$ethnicity <- factor(raDrugs$ethnicity)
raDrugs$alcohol <- factor(raDrugs$alcohol)

rules <- apriori(raDrugs)
inspect(rules)

# -----interesting rulesets------------------------------------

rules2 <- apriori(raDrugs,
                  appearance = list(rhs=c("alcohol=Drunk alcohol for at least 1 year","alcohol=Did not drunk for at least 1 year"),
                                    default="lhs"),
                  control = list(verbose=F))
rules2 <- sort(rules2, by="lift")
inspect(rules2)
inspect(rules2[2])
plot(rules2)
plot(rules2, method="graph", control=list(type="items"))
plot(rules2, method="paracoord", control=list(reorder=TRUE))

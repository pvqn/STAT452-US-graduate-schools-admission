data<-read.csv('smoke.csv', header = FALSE)
header_names <- c("educ", "cigpric", "white", "age", "income", "cigs", "restaurn", "lincome", "agesq", "lcigpric")
colnames(data) <- header_names
attach(data)
str(data)
dim(data)
sum(is.na(data))
summary(data)
mode <- function( x, na.rm = FALSE) {
  if(na.rm){ x = x[!is.na(x)]  }
  val <- unique(x)
  return(val[which.max(tabulate(match(x, val)))])
}

outliers <- function(x) {   
  # 1st and 3rd quantiles   
  q75 = quantile(x, 0.75)   
  q25 = quantile(x, 0.25)   
  IQR = q75-q25   
  # lower bound   
  lower_bound = q25 - 1.5 * IQR   
  # upper bound   
  upper_bound = q75 + 1.5 * IQR   
  # outliers   
  outlier_ind <- which(x < lower_bound | x > upper_bound)   
  if (length(outlier_ind) == 0) return (0)   
  return(outlier_ind) 
}
par(mfrow=c(1,2))
whiteTab <- table(white)
whiteTab
barplot(whiteTab)
title(main = "Barplot")
pie(whiteTab, labels = c("0", "1"), col = c("white", "lightblue"))
title(main = "Pie Chart")
par(mfrow=c(1,2))
restaurnTab <- table(restaurn)
restaurnTab
barplot(restaurnTab)
title(main = "Barplot")
pie(whiteTab, labels = c("0", "1"), col = c("white", "lightblue"))
title(main = "Pie Chart")
educTab<-table(educ)
educTab
barplot(educTab)
cat("Mode of years of education: ", mode(educ) ,"\n")
cat("Summary of years of education: \n")
summary(educ)
cigTab<-table(cigs)
cigTab
par(mfrow=c(1,2))
hist(cigs)
boxplot(cigs)
title(main="Boxplot of cigs")
summary(cigs)
par(mfrow=c(1,2))
hist(cigpric)
boxplot(cigpric)
title(main="Boxplot of cigpric")

hist(lcigpric)
boxplot(lcigpric)
title(main="Boxplot of lcigpric")

cat("Total outlier of cigpric: ", length( boxplot.stats(cigpric)$out ), " ", length( boxplot.stats(cigpric)$out) / length(cigpric), "\n")
cat("Summary of cigpric: \n")
summary(cigpric)
par(mfrow=c(1,2))
hist(age)
hist(agesq)

boxplot(age)
title(main="Boxplot of age")
boxplot(agesq)
title(main="Boxplot of agesq")
cat("Mode of age: ", mode(age), "\n")
cat("Summary of age \n")
summary(age)
par(mfrow=c(1,2))
barplot(table(income))
boxplot(income)
title(main="Boxplot of income")

hist(lincome)
boxplot(lincome)
title(main="Boxplot of lincome")
cat("Mode of age: ", mode(income), "\n")
cat("Summary of income: \n")
summary(income)
t.test(x=cigpric, alternative = "two.sided", mu=60)
boxplot(cigpric~restaurn, ylab="cigpric(cents)")
t.test(cigpric[restaurn==1], cigpric[restaurn==0], alternative = "less")
t.test(x=educ, alternative = "two.sided", mu=13)

length(white[white==0])/length(white)
prop.test(table(white),p=0.10, alternative = "greater")
length(restaurn[restaurn==0])/length(restaurn)
prop.test(table(restaurn),p=0.75, alternative = "less")
#smoker and non-smoker
smoker=1:length(cigs)
for (i in 1:length(cigs))
{if (cigs[i]<1)
{smoker[i]=0}
  else 
  {smoker[i]=1}
}

table(restaurn,smoker)
cat("Smoking restrictions and non-smoker: ", nrow(data[cigs == 0 & restaurn == 1,])/nrow(data), "\n")
cat("Smoking restrictions and smoker: ", nrow(data[cigs > 0 & restaurn == 1,])/nrow(data), "\n")


prop.test( x = c( nrow(data[cigs == 0 & restaurn == 1,]), 
                  nrow(data[cigs > 0 & restaurn == 1,])), n = c(length(cigs), length((restaurn))), alternative = "greater" )

result_vector <- unlist(apply(data[,c(1,2,3,4,6,7,8,9,10)],2,outliers))
length(unique(result_vector[result_vector != 0]))

length(unique(result_vector[result_vector != 0])) / nrow(data)
#smoker =1 if cigs>0
smoker=1:length(cigs)
for (i in 1:length(cigs))
{if (cigs[i]<1)
{smoker[i]=0}
  else 
  {smoker[i]=1}
}

#ageGroup=1 if age>30
ageGroup=1:length(age)
for (i in 1:length(age))
{if (age[i]<=30)
{ageGroup[i]=0}
  else 
  {ageGroup[i]=1}
}

data$young<-ageGroup
data$smoker<-smoker
pairs(cigs ~ ., data = data )
r <- cor(dplyr::select_if(data, is.numeric))
ggcorrplot::ggcorrplot(r,
                       hc.order = TRUE,
                       lab = TRUE)
car::vif(lm(cigs~., data = data))
model<-lm(data$cigs ~ (.-age-cigpric-income), data=data)
summary(model)
car::vif(model)
model_exceptCigpricAndWhite<-lm(data$cigs ~(.-age-lcigpric-income-white-cigpric), data=data)
summary(model_exceptCigpricAndWhite)
anova(model, model_exceptCigpricAndWhite)
model_exceptCigpricAndWhiteAndResAndAge<-lm(data$cigs ~(.-age-agesq-cigpric-income-white-lcigpric-restaurn), data=data)
summary(model_exceptCigpricAndWhiteAndResAndAge)
anova(model, model_exceptCigpricAndWhiteAndResAndAge)
model_best<-step(lm(data$cigs ~(.-age-cigpric-income), data=data),direction = "both",k = 2)
summary(model_best)
lag.plot(model_best$residuals)
car::durbinWatsonTest(model_best)
lmtest::bptest(model_best)
shapiro.test(model_best$residuals)
summary(model_best)
car::vif(model_best)
anova(model_best)
par(mfrow=c(1,2))
plot(model_best)
plot(predict(model_best, newdata = data), cigs, xlab = "Predicted Values", ylab = "Observed Values")
abline(a = 0, b = 1, col = "blue", lwd = 2)
dataGA<-read.csv('US_graduate_schools_admission_parameters_dataset.csv', header = TRUE)
str(dataGA)
dim(dataGA)
sum(is.na(dataGA))
summary(dataGA)
par(mfrow=c(1,2))
REtable<-table(dataGA$Research)
REtable
barplot(REtable)
title(main = "Barplot")

pie(REtable,  labels = c("0", "1"), col = c("white", "lightblue"))
title(main = "Pie Chart")
par(mfrow=c(1,2))
summary(dataGA$GRE.Score)
hist(dataGA$GRE.Score,freq = FALSE, main="Historgram of GRE.score", xlab="GRE.score")
lines(density(dataGA$GRE.Score), col = "red")
boxplot(dataGA$GRE.Score)
title(main="Boxplot of GRE.score")

par(mfrow=c(1,2))
summary(dataGA$TOEFL.Score)
hist(dataGA$TOEFL.Score,freq = FALSE, main="Historgram of TOEFL.Score", xlab="TOEFL.Score")
lines(density(dataGA$TOEFL.Score), col = "red")
boxplot(dataGA$TOEFL.Score)
title(main="Boxplot of TOEFL.Score")
par(mfrow=c(1,2))
summary(dataGA$GPA)
hist(dataGA$CGPA,freq = FALSE, main="Historgram of CGPA", xlab="CGPA")
lines(density(dataGA$CGPA), col = "red")
boxplot(dataGA$CGPA)
title(main="Boxplot of CGPA")

par(mfrow=c(2,2))
barplot(table(dataGA$Research), xlab="Research")
boxplot(dataGA$GRE.Score~dataGA$Research, xlab="Research", ylab="GRE.Score")
boxplot(dataGA$TOEFL.Score~dataGA$Research, xlab="Research", ylab="TOEFL.Score")
boxplot(dataGA$CGPA~dataGA$Research, xlab="Research", ylab="CGPA")

par(mfrow=c(1,2))
summary(dataGA$University.Rating)
rank<-table(dataGA$University.Rating)
barplot(rank, main="Bar chart of University Ranking")
pie(rank,main="Pie chart of University Ranking")
par(mfrow=c(1,1))
table(dataGA$Research,dataGA$University.Rating)

barplot(table(dataGA$Research,dataGA$University.Rating), beside=TRUE,legend = c("Non-Researcher", "Researcher"),main="Relationship between University Ranking and Reseacher")
par(mfrow=c(1,2))
summary(dataGA$SOP)
hist(dataGA$SOP, freq=FALSE, main="Histogram of SOP score", xlab="SOP")
lines(density(dataGA$SOP), col = "red")
boxplot(dataGA$SOP, main="Boxplot of SOP score")
par(mfrow=c(1,2))
summary(dataGA$LOR)
hist(dataGA$LOR, freq=FALSE, main="Histogram of LOR score", xlab="LOR")
lines(density(dataGA$LOR), col = "red")
boxplot(dataGA$LOR, main="Boxplot of LOR score")
par(mfrow=c(1,2))
summary(dataGA$Chance.of.Admit)
hist(dataGA$Chance.of.Admit, freq=FALSE, main="Histogram of Chance.of.Admit", xlab="Chance.of.Admit")
lines(density(dataGA$Chance.of.Admit), col = "red")
boxplot(dataGA$Chance.of.Admit~dataGA$Research, xlab="Research", ylab="Chance.of.Admit")

result_vector <- unlist(apply(dataGA[,c(1,2,3,4,5,6,7,8,9)],2,outliers))
length(unique(result_vector[result_vector != 0]))
length(unique(result_vector[result_vector != 0])) / nrow(dataGA)
dataGA_filtered <- dataGA[-unique(result_vector[result_vector != 0]),]
dim(dataGA_filtered)

pairs(dataGA_filtered$Chance.of.Admit ~ ., data = dataGA_filtered )
r <- cor(dplyr::select_if(dataGA_filtered, is.numeric))
ggcorrplot::ggcorrplot(r, hc.order = TRUE, lab = TRUE)
car::vif( lm(dataGA_filtered$Chance.of.Admit ~ ., data = dataGA_filtered ) )

set.seed(190)  # Set a random seed for reproducibility

# Calculate the number of observations for training and validation
num_train <- round(0.8 * nrow(dataGA_filtered))  # Use 80% for training
num_val <- nrow(dataGA_filtered) - num_train

# Generate random indices for training and validation
train_indices <- sample(nrow(dataGA_filtered), num_train, replace = FALSE)
val_indices <- setdiff(1:nrow(dataGA_filtered), train_indices)

# Create training and validation datasets
training_data <- dataGA_filtered[train_indices, ]
validation_data <- dataGA_filtered[val_indices, ]

# Print the dimensions of training_data
cat("Dimensions of training_data:", dim(training_data), "\n")

# Print the dimensions of validation_data
cat("Dimensions of validation_data:", dim(validation_data), "\n")

car::vif( lm( Chance.of.Admit ~ (.-Serial.No.) , data = training_data ) )
car::vif( lm( Chance.of.Admit ~ (.-Serial.No.) , data = validation_data ) )

model<-lm( Chance.of.Admit ~ (.-Serial.No.) , data = training_data )
summary(model)
model_best<-step(model, direction='both', k=2)
summary(model_best)
lag.plot(model_best$residuals)
car::durbinWatsonTest(model_best)
lmtest::bptest(model_best)
shapiro.test(model_best$residuals)
par(mfrow=c(1,2))
MASS::boxcox(model_best, plotit = TRUE)
MASS::boxcox(model_best, plotit = TRUE, lambda = seq( 1.5,2.5, by = 0.01 ) )

model_final<-lm((Chance.of.Admit^2.2-1)/2.2 ~ GRE.Score + TOEFL.Score + LOR + 
                  CGPA + Research, data = training_data)
summary(model_final)
lag.plot(model_final$residuals) 
car::durbinWatsonTest(model_final)
lmtest::bgtest(model_final)
shapiro.test(model_final$residuals)
predict_Chance.of.Admit<-log(((predict(model_final, newdata = validation_data))*2.2+1),base = 2.2)
head(predict_Chance.of.Admit)
plot(predict_Chance.of.Admit, validation_data$Chance.of.Admit,
     xlab = "Predicted Values",
     ylab = "Observed Values") 

#RMSE 
print("Root mean square error:")
sqrt(mean((predict_Chance.of.Admit - validation_data$Chance.of.Admit)^2))

predict_Chance.of.Admit <- predict(model_best, newdata = validation_data)
head(predict_Chance.of.Admit)
#RMSE 
print("Root mean square error:")
sqrt(mean((predict_Chance.of.Admit - validation_data$Chance.of.Admit)^2))

par(mfrow=c(1,2))
plot(model_best)
par(mfrow=c(1,1))

plot(predict(model_best, newdata = validation_data),                              
     validation_data$Chance.of.Admit,
     xlab = "Predicted Values",
     ylab = "Observed Values")
abline(a = 0,                                       
       b = 1,
       col = "red",
       lwd = 2)



summary(model_best)
car::vif(model_best)

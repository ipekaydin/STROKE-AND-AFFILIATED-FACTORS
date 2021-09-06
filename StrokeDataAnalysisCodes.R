#STAT364 FINAL REPORT CODES
#Deniz Bezer 2290559
#Ecenaz Tunç 2290971
#İpek Aydın 2290526
#Levent Sarı 2290930
#Miray Çınar 2290609


library(readr)
library(epitools)

healthcare_dataset_stroke_data = read_csv("healthcare-dataset-stroke-data.csv")
stroked = data.frame(healthcare_dataset_stroke_data)
str(stroked)

stroked$id = NULL
stroked = stroked[-which(stroked$gender=='Other'),]
stroked$gender = as.factor(stroked$gender)
stroked = stroked[-which(stroked$bmi=='N/A'),]
stroked$bmi = as.numeric(stroked$bmi)
stroked$hypertension = as.factor(stroked$hypertension)
stroked$heart_disease = as.factor(stroked$heart_disease)
stroked$ever_married = as.factor(stroked$ever_married)
stroked$work_type = as.factor(stroked$work_type)
stroked$Residence_type = as.factor(stroked$Residence_type)

stroked$smoking_status = as.factor(stroked$smoking_status)
stroked$stroke = as.factor(stroked$stroke)

str(stroked)

summary(stroked)

strokedpositive = stroked[which(stroked$stroke==1),]
strokedpositive
strokednegative = stroked[which(stroked$stroke==0),]
strokednegative

# Research Question 1: Do people who have had strokes have 
# worse physical conditions (such as suboptimal BMI or older age)

#### RQ1 - Physicals

## BMI

library(MASS) 

shapiro.test(stroked$bmi)

boxcox(bmi~stroke, data = stroked , lambda = seq(-0.5,0.2,0.1))

shapiro.test(log(stroked$bmi)) # Log transformation doesnt make normal

wilcox.test(strokedpositive$bmi,strokednegative$bmi,paired = F) # The medians differ, relation btw bmi and stroke

library(tidyverse)

stroked %>% 
  ggplot(.,aes(x=stroked$stroke,y=stroked$bmi,color=stroked$stroke)) + 
  geom_boxplot()+scale_color_manual(values=c("#E69F00", "#56B4E9")) + theme_minimal()+
  stat_summary(fun.y=mean, geom="point", shape=23, size=4) +
  theme(plot.title = element_text(size=14,face='bold'),
        axis.title.x = element_text(size = 12,face='bold'),
        axis.title.y = element_text(size = 12,face='bold'),
        axis.text.x = element_text(face='bold'),
        axis.text.y = element_text(face='bold'),
        legend.position = 'none') + 
  labs(x = 'Past Stroke', y = 'BMI', title = 'Boxplot of BMI versus Past Stroke Experience') + 
  scale_x_discrete(labels = c('No','Yes'))

# stroke seems to have higher bmi

# Computes a heterogenous correlation matrix, consisting of Pearson product-moment correlations
# between numeric variables, polyserial correlations between numeric and ordinal variables,
# and polychoric correlations between ordinal variables.

library(polycor)
hetcor(stroked$stroke,stroked$bmi) # Very Weak positive correlation

## Age

shapiro.test(stroked$age) 
boxcox(age~stroke, data=stroked,lambda=seq(0.6,0.9,0.1))
shapiro.test(stroked$age^0.85) # Still not normal

wilcox.test(strokedpositive$age,strokednegative$age) # The medians differ, relation btw age and stroke

stroked %>% 
  ggplot(.,aes(x=stroked$stroke,y=stroked$age,color=stroked$stroke)) + 
  geom_boxplot()+scale_color_manual(values=c("#E69F00", "#56B4E9")) + theme_minimal() +
  theme(plot.title = element_text(size=14,face='bold'),
        axis.title.x = element_text(size = 12,face='bold'),
        axis.title.y = element_text(size = 12,face='bold'),
        axis.text.x = element_text(face='bold'),
        axis.text.y = element_text(face='bold'),
        legend.position = 'none') + 
  stat_summary(fun.y=mean, geom="point", shape=23, size=4) + 
  labs(x = 'Past Stroke', y = 'Age', title = 'Boxplot of Age versus Past Stroke Experience') + 
  scale_x_discrete(labels = c('No','Yes'))

# stroke seems to have higher age

hetcor(stroked$stroke,stroked$age) # average positive correlation

# By looking at the data we have, we can conclude that the risk of stroke increases with age,
# but that BMI does not have much of an effect. 

# Research Question 2: Do external conditions (Such as work type, residence type, smoking and marriage status) of people change their likelihood to have a stroke? 
# Proposed method: Measures of association, Contingency table with Cramer’s V score, odds ratio
# Response: Likelihood of having a stroke

library(DescTools)
str(stroked)
# work type, residence type, smoking and marriage status are categoric variables. Response variable is nominal.

# What is passed as the parameter is a contingency table created with the table() function that 
# cross-classifies the number of rows that are in the categories specified by the two categorical variables.

###########################

##
reorderFactors <- function(df, column = "my_column_name", 
                           desired_level_order = c("fac1", "fac2", "fac3")) {
  
  x = df[[column]]
  lvls_src = levels(x) 
  
  idxs_target <- vector(mode="numeric", length=0)
  for (target in desired_level_order) {
    idxs_target <- c(idxs_target, which(lvls_src == target))
  }
  
  x_new <- factor(x,levels(x)[idxs_target])
  
  df[[column]] <- x_new
  
  return (df)
}
##
strokeda = reorderFactors(stroked,'stroke',c(1,0))
table1 = table(stroked$stroke,stroked$work_type)
table1

strokeda %>% 
  ggplot(.,aes(x=work_type,fill=stroke)) + 
  geom_bar(stat='count',position='stack') + 
  scale_fill_manual(values=c("#E69F00","darkgrey"),labels = c('Yes','No')) + 
  theme_minimal() +
  theme(plot.title = element_text(size=14,face='bold'),
        axis.title.x = element_text(size = 12,face='bold'),
        axis.title.y = element_text(size = 12,face='bold'),
        axis.text.x = element_text(face='bold'),
        axis.text.y = element_text(face='bold'),
        legend.position = 'right') + 
  labs(x = 'Work Type', y = 'Count', title = 'Bar Graph of Past Stroke Experience Versus Work Type', fill = 'Past Stroke\nExperience')
  

# H0: The variables are independent, there is no relationship between the two categorical variables.
# Knowing the value of one variable does not help to predict the value of the other variable
# H1: The variables are dependent, there is a relationship between the two categorical variables.
# Knowing the value of one variable helps to predict the value of the other variable

chisq1 = chisq.test(table(stroked$stroke,stroked$work_type)) 
chisq1$expected # Since never worked - stroke1 is <5, we can't use X^2. Use fisher.
fisher.test(stroked$stroke,stroked$work_type,simulate.p.value = TRUE)

# p-value is less than the significance level of 5%. We reject the NULL hypothesis.
# This means that there is a significant relationship between the two categorical variables.

hetcor(stroked$stroke,stroked$work_type) 

# The output of table() shows weak positive correlation between stroke and work type.
# We can deduce that people work in private jobs and self-employed have more strokes. 

###########################

table2 = table(stroked$stroke,stroked$Residence_type)
table2

chisq2 = chisq.test(table2)
chisq2$expected # No <5 so we can use chisq.
chisq2 # No relation between stroke and residence type.
(109/2381)/(100/2318) # Odds ratio almost 1, meaning there is no significant relation.
OR <- oddsratio(table2, method = "wald")
OR$measure # 0 var significant de?il
strokeda %>% 
  ggplot(.,aes(x=Residence_type,fill=stroke)) + 
  geom_bar(stat='count',position='stack',width = 0.3) + 
  scale_fill_manual(values=c("#E69F00","darkgrey"),labels = c('Yes','No')) + 
  theme_minimal() +
  theme(plot.title = element_text(size=14,face='bold'),
        axis.title.x = element_text(size = 12,face='bold'),
        axis.title.y = element_text(size = 12,face='bold'),
        axis.text.x = element_text(face='bold'),
        axis.text.y = element_text(face='bold'),
        legend.position = 'right') + 
  labs(x = 'Residence Type', y = 'Count', title = 'Bar Graph of Past Stroke Experience Versus Residence Type', fill = 'Past Stroke\nExperience')


# Graph also confirms.

###########################
## Smoking status - stroke
library(sqldf)
summary(stroked$smoking_status)
stroked2 = sqldf("SELECT * from stroked where smoking_status != 'Unknown'")
stroked2$smoking_status = droplevels(stroked2$smoking_status)
levels(stroked2$smoking_status)
table3 = table(stroked2$stroke,stroked2$smoking_status)
table3
strokedb = reorderFactors(stroked2,'stroke',c(1,0))
strokedb %>% 
  ggplot(.,aes(x=smoking_status,fill=stroke)) + 
  geom_bar(stat='count',position='stack',width = 0.3) + 
  scale_fill_manual(values=c("#E69F00","darkgrey"),labels = c('Yes','No')) + 
  theme_minimal() +
  theme(plot.title = element_text(size=14,face='bold'),
        axis.title.x = element_text(size = 12,face='bold'),
        axis.title.y = element_text(size = 12,face='bold'),
        axis.text.x = element_text(face='bold'),
        axis.text.y = element_text(face='bold'),
        legend.position = 'right') + 
  labs(x = 'Smoking Status', y = 'Count', title = 'Bar Graph of Past Stroke Experience Versus Smoking Status', fill = 'Past Stroke\nExperience')


# Looking at the graph, it can be said that there is no relationship between smoking status and stroke,
# but a Chi-square test should be done to examine it in more detail. 

chisq3 = chisq.test(table3)
chisq3$expected # none <5, can proceed with X2.
chisq3 # Almost limitte
hetcor(stroked2$smoking_status,stroked2$stroke)  # Yoksayilabilir cok kucuk korelasyon


###########################

table4 = table(stroked$stroke,stroked$ever_married)
table4

# Ho: The odds ratio is equal to 1
# Ha: The odds ratio is not equal to 1

# We can find odds ratio (OR) by simple calculation:
(186/3018)/(23/1681) # OR around 4.5, means married people are 4.5 times more likely.
OR <- oddsratio(table4, method = "wald")
OR$measure # 1 yok, anlaml? ili?ki.

strokeda %>% 
  ggplot(.,aes(x=ever_married,fill=stroke)) + 
  geom_bar(stat='count',position='stack',width = 0.3) + 
  scale_fill_manual(values=c("#E69F00","darkgrey"),labels = c('Yes','No')) + 
  theme_minimal() +
  theme(plot.title = element_text(size=14,face='bold'),
        axis.title.x = element_text(size = 12,face='bold'),
        axis.title.y = element_text(size = 12,face='bold'),
        axis.text.x = element_text(face='bold'),
        axis.text.y = element_text(face='bold'),
        legend.position = 'right') + 
  scale_x_discrete(labels = c('Never Married','Have/Had a Marriage')) +
  labs(x = 'Marriage Status', y = 'Count', title = 'Bar Graph of Past Stroke Experience\nVersus Marriage Experience', fill = 'Past Stroke\nExperience')

# Plot can also confirm this conclusion.


# Overall, We found that married people and people that work in private jobs and
# self-employed are more likely to have a stroke, while smoking status and type of
# residence are not related to the probability of stroke. 

# Research Question 3: Do present or past diseases / disorders (such as heart disease, hypertension, suboptimal glucose level) increase the likelihood of having a stroke?
# Proposed method: Contingency table on two categorical variables against the response and two sample t-test on the continuous variable (of samples grouped by binary stroke status)
# Response: Likelihood of having a stroke

## Heart disease

table5 = table(stroked$stroke,stroked$heart_disease)
table5

strokeda %>% 
  ggplot(.,aes(x=heart_disease,fill=stroke)) + 
  geom_bar(stat='count',position='stack',width = 0.3) + 
  scale_fill_manual(values=c("#E69F00","darkgrey"),labels = c('Yes','No')) + 
  theme_minimal() +
  theme(plot.title = element_text(size=14,face='bold'),
        axis.title.x = element_text(size = 12,face='bold'),
        axis.title.y = element_text(size = 12,face='bold'),
        axis.text.x = element_text(face='bold'),
        axis.text.y = element_text(face='bold'),
        legend.position = 'right') + 
  scale_x_discrete(labels = c('No','Yes')) + 
  labs(x = 'Heart Disease', y = 'Count', title = 'Bar Graph of Past Stroke Experience versus Heart Diseases', fill = 'Past Stroke\nExperience')


# According to the graph, people with heart disease are more likely to have a stroke.
# To measure this, let's look at the odds ratio. 

(40/203)/(169/4496) # People with heart diseases are 5.24 times more likely to get strokes.
OR <- oddsratio(table5, method = "wald")
OR$measure # 0 yok significant

## Hypertension

table6 = table(stroked$stroke,stroked$hypertension)
table6

(60/391)/(149/4308) # OR 4.43 means 4.43 times more likely if hypertension.
OR <- oddsratio(table6, method = "wald")
OR$measure # 0 yok significant

strokeda %>% 
  ggplot(.,aes(x=hypertension,fill=stroke)) + 
  geom_bar(stat='count',position='stack',width = 0.3) + 
  scale_fill_manual(values=c("#E69F00","darkgrey"),labels = c('Yes','No')) + 
  theme_minimal() +
  theme(plot.title = element_text(size=14,face='bold'),
        axis.title.x = element_text(size = 12,face='bold'),
        axis.title.y = element_text(size = 12,face='bold'),
        axis.text.x = element_text(face='bold'),
        axis.text.y = element_text(face='bold'),
        legend.position = 'right') +   scale_x_discrete(labels = c('No','Yes')) + 
  labs(x = 'Hypertension', y = 'Count', title = 'Bar Graph of Past Stroke Experience\nVersus Hypertension Presence', fill = 'Past Stroke\nExperience')

# Plot can also confirm this conclusion.

## Suboptimal glucose level

stroked %>% 
  ggplot(.,aes(x=stroked$stroke,y=stroked$avg_glucose_level,color=stroked$stroke)) + 
  geom_boxplot()+scale_color_manual(values=c("#E69F00", "#56B4E9")) + theme_minimal()+
  stat_summary(fun.y=mean, geom="point", shape=23, size=4) +
  theme(plot.title = element_text(size=14,face='bold'),
        axis.title.x = element_text(size = 12,face='bold'),
        axis.title.y = element_text(size = 12,face='bold'),
        axis.text.x = element_text(face='bold'),
        axis.text.y = element_text(face='bold'),
        legend.position = 'none') + 
  labs(x = 'Past Stroke', y = 'Avg. Glucose Level', title = 'Boxplot of Average Glucose Level\nversus Past Stroke Experience') + 
  scale_x_discrete(labels = c('No','Yes'))

shapiro.test(stroked$avg_glucose_level)#not normal
boxcox(stroked$avg_glucose_level~stroked$stroke)#lambda = -1
shapiro.test(stroked$avg_glucose_level^-1)#still not normal
#do wilcox
wilcox.test(strokedpositive$avg_glucose_level,strokednegative$avg_glucose_level)
# The medians differ, relation btw suboptimal glucose level and stroke.
hetcor(stroked$avg_glucose_level,stroked$stroke) 
# There is a weak positive correlation(0.2274) between glucose level and stroke. 

# To sum up, it can be conclude that present or past diseases / disorders (such as heart disease,
# hypertension, suboptimal glucose level) increase the likelihood of having a stroke.


# Research question 4: Can an accurate prediction be made for the possibility of 
# having a stroke by using any or all of the independent variables?
# Proposed method: Logistic Regression with stroke status (binary) as response and 
# the remaining variables as regressors.

library(pROC)
library(caret)

### ?nce train test yapt?k.

# Data Seperation

comp.stroked = stroked %>% drop_na() 

set.seed(1)

train.size = 0.9
train.stroked = comp.stroked %>% 
  sample_frac(train.size)
dim(train.stroked)

test.stroked = anti_join(comp.stroked, train.stroked)
dim(test.stroked)

# Model building
glm.train = glm(stroke~.,data=train.stroked,family='binomial')
summary(glm.train)
# StepAIC for variable selection
stepAIC(glm.train, direction = "backward", trace = T)
stepAIC(glm.train, direction = "forward", trace = T) 
stepAIC(glm.train, direction = "both", trace = T)
glm.train2 = glm(stroke ~ age + hypertension + avg_glucose_level,family = 'binomial',data=train.stroked)
summary(glm.train2)

# Model checking

mydata <- train.stroked %>%
  dplyr::select_if(is.numeric) 
predictors = c('age','logglucose')
probabilities <- predict(glm.train2, type = "response")
mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)
summary(mydata)
ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

# Avg-glucose-level not ok. try log transformation?

stroked$logglucose = log(stroked$avg_glucose_level)

comp.stroked = stroked %>% drop_na() 

set.seed(1)

train.size = 0.9
train.stroked = comp.stroked %>% 
  sample_frac(train.size)
dim(train.stroked)

test.stroked = anti_join(comp.stroked, train.stroked)
dim(test.stroked)

glm.train3 = glm(stroke ~ age + hypertension + log(avg_glucose_level),family = 'binomial',data=train.stroked)
summary(glm.train3)

mydata <- train.stroked[,-which(colnames(train.stroked)=='avg_glucose_level')] %>%
  dplyr::select_if(is.numeric) 
predictors = c('age','logglucose')
probabilities <- predict(glm.train3, type = "response")
mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)
summary(mydata)
ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

# not much better, check performance of first?

roc_curve = roc(train.stroked$stroke ~ fitted(glm.train2))
best_threshold = coords(roc_curve, "best", ret = "threshold")
best_threshold

invisible(plot(roc(factor(ifelse(train.stroked$stroke=='1',1,0)),fitted(glm.train2)),print.thres=0.03641819,print.auc=TRUE,col='blue'))
prediction = ifelse(glm.train2$fit > 0.03641819,1,0) 
confusionMatrix(as.factor(prediction),as.factor(train.stroked$stroke),positive='1')

# good accuracy, bad NIR, good sens-specivity. Good negative pred value, bad postivie pred value.
# use on test data

glm.test = glm(stroke ~ age + hypertension + avg_glucose_level,family = 'binomial',data=test.stroked)
glm.test
invisible(plot(roc(factor(ifelse(test.stroked$stroke=='1',1,0)),fitted(glm.test)),print.thres=0.03641819,print.auc=TRUE,col='blue'))
prediction = ifelse(glm.test$fit > 0.03641819,1,0) 
confusionMatrix(as.factor(prediction),as.factor(test.stroked$stroke),positive='1')

# worse accuracy, still bad nir, good sensitivity, bad specificity, very bad pos pred, good neg pred.
# train test method is not very useful

### Fit a model on the whole data.

glm.model1 <- glm(stroke ~ gender + age + hypertension + heart_disease + ever_married + work_type + 
                    Residence_type + avg_glucose_level + bmi + smoking_status, data=stroked, 
                  family="binomial")

stepAIC(glm.model1, direction = "backward", trace = T)
stepAIC(glm.model1, direction = "forward", trace = T) 
stepAIC(glm.model1, direction = "both", trace = T)

glm.model2 = glm(stroke ~ age + hypertension + heart_disease + avg_glucose_level,family = 'binomial',data=stroked)
summary(glm.model2)
#
stroked$logglucose = NULL
mydata <- stroked %>%
  dplyr::select_if(is.numeric) 
predictors = c('age','avg_glucose_level')
probabilities <- predict(glm.model2, type = "response")
mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)
summary(mydata)
ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")
# Glucose seems exponential. Try log transformation?

glm.model3 = glm(stroke ~ age + hypertension + heart_disease + log(avg_glucose_level),family = 'binomial',data=stroked)
summary(glm.model3)
#
stroked$logglucose = log(stroked$avg_glucose_level)
mydata <- stroked %>%
  dplyr::select_if(is.numeric) 
predictors = c('age','logglucose')
probabilities <- predict(glm.model3, type = "response")
mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)
summary(mydata)
ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")
# Seems better.

roc_curve = roc(stroked$stroke ~ fitted(glm.model2))
best_threshold = coords(roc_curve, "best", ret = "threshold")
best_threshold

invisible(plot(roc(factor(ifelse(stroked$stroke=='1',1,0)),fitted(glm.model2)),print.thres=0.03626418,print.auc=TRUE,col='blue'))
prediction = ifelse(glm.model2$fit > 0.03626418,'1','0') 
confusionMatrix(as.factor(prediction),as.factor(stroked$stroke),positive='1')

# Good accuracy, good sensitivity and specificity. Bad positive predictive value however good negative predictive value. 

exp(coef(glm.model2)[2]-1)*100 # The odds of having a stroke increases by 39.4 % with each additional age of the patients.
exp(coef(glm.model2)[3]-1)*100 # The odds of having a stroke increases by 63.1 % with if the patient has hypertension  of the patients.
exp(coef(glm.model2)[4]-1)*100 # The odds of having a stroke increases by 55.1 % with if the patient has heart disease of the patients.
exp(coef(glm.model2)[5]-1)*100 # The odds of having a stroke increases by 37 % with each additional average level of glucose of the patients.

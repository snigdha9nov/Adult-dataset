
data<-read.csv('adult.csv')
suppressWarnings(suppressMessages(library(tidyverse)))

str(data)
summary(data)


colSums(is.na(data))
## No missing values are showing but summary is showing "?" in many columns and it can be missing values.Therefore repalce ? with NA
data[ data == "?" ] <- NA


#Missing values Replacement By KNN 
suppressWarnings(suppressMessages(library(VIM)))
data1<-kNN(data,variable = colnames(data))
data <- subset(data1, select = -c(16:30))

#mark <=50K as low and >50 as high
data$income<-ifelse(data$income=='<=50K',0,1)

# correaltion of target variable with numeric independent variables
res <- cor(data%>% select_if(is.numeric))
round(res, 2)


#change income from numeric to factor
data$income<-as.factor(as.character(data$income))



# check the percentage of each levels of incpme with pie chart
table(data$income)
df1 <- data %>% 
  group_by(income) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(per=`n`/sum(`n`)) %>% 
  arrange(desc(income))
df1$label <- scales::percent(df1$per)
ggplot(data=df1)+
  geom_bar(aes(x="", y=per, fill=income), stat="identity", width = 1)+
  coord_polar("y", start=0)+
  theme_void()+
  geom_text(aes(x=1, y = cumsum(per) - per/2, label=label))

## The target varaible seems unbalaced as percenatge of 1(>50k) is only 24% while 0 is approx 76%



# workclass vs income visualisation
df2 <- data %>% 
  group_by(income, workclass) %>% 
  tally() %>% 
  complete(workclass, fill = list(n = 0)) %>% 
  mutate(percentage = n / sum(n) * 100)
ggplot(df2, aes(workclass, percentage, fill = income)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_bw()
## Most of the people are in Private and only in private percenatge of low income is more than high .In all other groups percenatge of high income is more then low.



# Education vs income
table(data$education)
df2 <- data %>% 
  group_by(income, education) %>% 
  tally() %>% 
  complete(education, fill = list(n = 0)) %>% 
  mutate(percentage = n / sum(n) * 100)
ggplot(df2, aes(education, percentage, fill = income)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_bw()
## very obvious thet the less education category has more percenatge of low income then high.



table(data$marital.status)
df2 <- data %>% 
  group_by(income, marital.status) %>% 
  tally() %>% 
  complete(marital.status, fill = list(n = 0)) %>% 
  mutate(percentage = n / sum(n) * 100)
ggplot(df2, aes(marital.status, percentage, fill = income)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_bw()

# income vs occupation
table(data$occupation)
df2 <- data %>% 
  group_by(income, occupation) %>% 
  tally() %>% 
  complete(occupation, fill = list(n = 0)) %>% 
  mutate(percentage = n / sum(n) * 100)
ggplot(df2, aes(occupation, percentage, fill = income)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_bw()


# Race vs income
table(data$race)
df2 <- data %>% 
  group_by(income, race) %>% 
  tally() %>% 
  complete(race, fill = list(n = 0)) %>% 
  mutate(percentage = n / sum(n) * 100)
ggplot(df2, aes(race, percentage, fill = income)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_bw()

## barplot clearly showing race biasing . White people mostly are in high income category and black has high low percenatge of income.


# sex vs income
table(data$sex)
df2 <- data %>% 
  group_by(income, sex) %>% 
  tally() %>% 
  complete(sex, fill = list(n = 0)) %>% 
  mutate(percentage = n / sum(n) * 100)
ggplot(df2, aes(sex, percentage, fill = income)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_bw()
## here also gender biasing males have high percenatge of high income means most of the men has high income has compared to women.



# native.country vs income
table(data$native.country)
## almost all people belongs to Unite state and all othercategory are negligible..so we can make only two category either belongs to United state or other


data$native.country<-ifelse(data$native.country=='United-States',1,0)

suppressWarnings(suppressMessages(library(ggpubr)))
df2 <- data %>% 
  group_by(income,race,native.country) %>% 
  tally() %>% 
  complete(race, fill = list(n = 0)) %>% 
  mutate(percentage = n / sum(n) * 100)
ggplot(df2, aes(race, percentage, fill = income)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  facet_wrap(~native.country)+
  fill_palette("jco")

## Its very obvious people having US as native-country most have high income irrespevtive to their race but only one race that is 'Asian-Pac-Islander' who are non-aerican(US) earning more than american of same race.

# numeric variables vs income (Target-variable)
# education.num vs income
df2 <- data %>% 
  group_by(income, education.num) %>% 
  tally() %>% 
  complete(education.num, fill = list(n = 0)) %>% 
  mutate(percentage = n / sum(n) * 100)
ggplot(df2, aes(education.num, percentage, fill = income)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_bw()
## Clearly showing highly educated(high education num)has high income

#hours..per.week vs income
df2 <- data %>% 
  group_by(income, hours.per.week) %>% 
  tally() %>% 
  complete(hours.per.week, fill = list(n = 0)) %>% 
  mutate(percentage = n / sum(n) * 100)
ggplot(df2, aes(hours.per.week, percentage, fill = income)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_bw()
## People working more hours had high income

# age vs income
df2 <- data %>% 
  group_by(income, age) %>% 
  tally() %>% 
  complete(age, fill = list(n = 0)) %>% 
  mutate(percentage = n / sum(n) * 100)
ggplot(df2, aes(age, percentage, fill = income)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_bw()

## low income age is postively skewed but most of the high income people are in their middle age





p <- ggplot(data = data, aes(x = age, y = education.num, color = income)) + geom_point()
p + facet_wrap(~sex)

## highly educated middle aged males are earning good.


library(ggridges)
p<-ggplot(data, aes(x = age, y = income)) +
  geom_density_ridges(aes(fill = income)) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))
p + facet_wrap(~marital.status)



p<-ggdotchart(data, x = "education", y = "education.num",palette = c("#00AFBB", "#E7B800", "#FC4E07"), 
 sorting = "asc", sort.by.groups = TRUE,                      
add = "segments",                            
add.params = list(color = "lightgray", size = 2), 
group = "income",                                
 dot.size = 4,                                 
  ggtheme = theme_pubclean()
)+
font("x.text", size = 8, vjust = 0.5)
p+facet_wrap(~income)



#fnlwgt vs income
# as fnlwgt is not much corrrelated with income..it can be dropped

data=data[,-3]


# Model Building

# Train test split
set.seed(200)
index<-sample(nrow(data),0.70*nrow(data),replace = F)
train<-data[index,]
test<-data[-index,]


suppressWarnings(suppressMessages(library(ROSE)))
table(train$income)
data_balanced_over <- ovun.sample(income~.,data=train,method = "both")$data
table(data_balanced_over$income)

train<-data_balanced_over







suppressWarnings(suppressMessages(library(randomForest)))
rf<-randomForest(income~.,data = train)
rf


suppressWarnings(suppressMessages(library(irr)))
#install.packages("caret")
suppressWarnings(suppressMessages(library(caret)))
#install.packages("e1071")
suppressWarnings(suppressMessages(library(e1071)))


prediction_rf<-predict(rf,test)

#confusion matrix to validate it
confusionMatrix(prediction_rf,test$income,positive = "1")

x<-varImp(rf)
x<-add_rownames(x, var = "rowname")
x<-x%>%rename(variable=rowname,meanDecreaseGini=Overall)


p<-ggplot(data=x,aes(x=variable,y=order(meanDecreaseGini),fill=variable))+geom_bar(stat="identity")
p + coord_flip()


#Area under curve
library(ROCR)
library(pROC)
auc(as.numeric(test$income),as.numeric(prediction_rf ))



svm_class = svm(formula = income ~ ., 
                data = train, 
                type = 'C-classification', 
                kernel = 'linear')

prediction = predict(svm_class, newdata =  test) 
conf_mat<-confusionMatrix(prediction,test$income,positive = "high")
conf_mat



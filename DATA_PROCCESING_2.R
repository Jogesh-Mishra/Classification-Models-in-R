df<- read.csv("C:/Program Files/R/2. Classification/House-Price.csv",header= TRUE)
View(df)
str(df)

summary(df) #EDD ANALYSIS

boxplot(df$n_hot_rooms)
plot(df$rainfall,df$Sold)
barplot(table(df$bus_ter))
which(is.na(df$n_hos_beds))

# n_hot_rooms and rainfall have outliers
#n_hos_beds has missing values
# bus_term has only one type of value

##outlier treatment

uv <- 3* quantile(df$n_hot_rooms,0.99)
df$n_hot_rooms[df$n_hot_rooms>uv] <-uv
summary(df$n_hot_rooms)

lv <- 0.3*quantile(df$rainfall,0.01)
df$rainfall[df$rainfall<lv]<-lv
summary(df$rainfall)

##missing value imputation
which((is.na(df$n_hos_beds)))
df$n_hos_beds[is.na(df$n_hos_beds)]<-mean(df$n_hos_beds,na.rm = TRUE)
summary(df$n_hos_beds)


#variable transformation
df$avg_dist = (df$dist1+df$dist2+df$dist3+df$dist4)/4
str(df)
df<- df[,-6:-9]
str(df)

df<-df[,-13]
str(df)

##dummy variable creation
install.packages("dummies")
df <- dummy.data.frame(df)
str(df)
df<-df[,-8]
str(df)
df<-df[,-13]

## DATA PREPROCESSING DONE ##

##TEST-TRAIN SPLIT

install.packages("caTools")
set.seed(0)
split <- sample.split(df,SplitRatio = 0.8)
train_a = subset(df, split==TRUE )
test_a= subset(df, split==FALSE)

##LOGISTIC REGRESSION

train.fit= glm(Sold~.,data= train_a,family = binomial)
train.fit
test.probs = predict(train.fit, test_a,type="response")
test.pred = rep("NO",120)
test.pred[test.probs>0.5] = "YES"
table(test.pred,test_a$Sold)


##LINEAR DISCRIMINANT ANALYSIS 

install.packages("MASS")
lda.train.fit = lda(Sold~.,data=train_a)
lda.test.pred= predict(lda.train.fit,test_a)
lda.test.pred$posterior
lda.test.class= lda.test.pred$class
table(lda.test.class,test_a$Sold)



##KNN ANALYSIS

install.packages("class")

train_x = train_a[,-16]
test_x = test_a[,-16]

train_y = train_a$Sold
test_y= test_a$Sold

train_x_s = scale(train_x)
test_x_s = scale(test_x)

knn.pred = knn(train_x_s,test_x_s,train_y, k=3)
table(knn.pred,test_y)

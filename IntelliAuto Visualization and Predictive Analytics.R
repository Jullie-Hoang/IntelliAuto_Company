df <- IntelliAuto_company_raw_data

#TASK 1: 5 figures that have the highest association with the years of employment
library(ggplot2)
colnames(df)
str(df)

install.packages("dplyr")
library(dplyr)

#Figure 001 - Relation of Employee year among Sex in different Occupation
p1 <- ggplot(df, aes (x = Sex , y = EmpYears , fill = Sex)) + facet_wrap(~Occup_n)
p2 <- p1 + geom_boxplot()
p3 <- p2 + labs(x = "Sex" , y = "Employment year" , 
                title = "The different of Employment years in different Sex among Occupation in IntelliAuto organization") 
p3

df1 <- filter(df , Sex == "Male")
summary(df1$EmpYears)

df2 <- filter(df , Sex == "Female")
summary(df2$EmpYears)

p1 <- ggplot(df, aes (x = Occup_n , y = EmpYears , fill = Occup_n))
p2 <- p1 + geom_boxplot()
p3 <- p2 + labs(x = "Occupation" , y = "Employment year" , 
                title = "The difference of Employment year in different Occupations in IntelliAuto organization") 
p3

#Figure 010 - Relation of Employee year and Number of Promotion among Sex in different Occupation
cor(df1$EmpYears , df1$NumPromo)#Male
cor(df2$EmpYears , df2$NumPromo)#Female

p1 <- ggplot(df, aes (x = NumPromo , y = EmpYears , fill = Sex , color = Sex)) + facet_wrap(~Occup_n)
p2 <- p1 + geom_point()
p3 <- p2 + labs(x = "Number of Promotion" , y = "Employment year" , 
                title = "Association between Employment years and Number of Promotion among Sex in different Occupation in IntelliAuto organization") 
#Find way top relocate the title position 
p4 <- p3 + geom_smooth(method = "lm")
p4

#Figure 011 - Relation of Employee year and Age among Sex
cor(df1$EmpYears , df1$Age) #Male
cor(df2$EmpYears , df2$Age) #Female

p1 <- ggplot(df, aes (x = Age , y = EmpYears , fill = Sex , color = Sex)) 
p2 <- p1 + geom_point()
p3 <- p2 + labs(x = "Age" , y = "Employment year" , 
                title = "Association between Employment years and Age among Sex in IntelliAuto organization") 
p4 <- p3 + geom_smooth(method = "lm")
p4

#Figure 100 - Relation of Employee year and Education year among Sex
cor(df1$EmpYears , df1$Educ_Yrs) #Male
cor(df2$EmpYears , df2$Educ_Yrs) #Female

p1 <- ggplot(df, aes (x = Educ_Yrs , y = EmpYears , fill = Sex , color = Sex)) + facet_wrap(~Occup_n)
p2 <- p1 + geom_point()
p3 <- p2 + labs(x = "Education year" , y = "Employment year" , 
                title = "Association between Employment years and Education years among Sex in different Occupations in IntelliAuto organization") 
p4 <- p3 + geom_smooth(method = "lm")
p4

#Figure 101 - Relation of Employee year and Working year among Sex
cor(df1$EmpYears , df1$WrkYears) #Male
cor(df2$EmpYears , df2$WrkYears)#Female

p1 <- ggplot(df, aes (x = WrkYears , y = EmpYears , fill = Sex , color = Sex))
p2 <- p1 + geom_point()
p3 <- p2 + labs(x = "Working year" , y = "Employment year" , 
                title = "Association between Employment year and Working year among Sex in IntelliAuto organization") 
p4 <- p3 + geom_smooth(method = "lm")
p4

#TASK 2: Predict the years of employment among the staff
#Manage the dummy variables (6 dummies)
str(df)

as.factor(df$Occup_n) 
df$Occup_n <- as.factor(df$Occup_n)

as.factor(df$Sex) 
df$Sex <- as.factor(df$Sex)

as.factor(df$FutPromo) 
df$FutPromo <- as.factor(df$FutPromo)

as.factor(df$MemUnion) 
df$MemUnion <- as.factor(df$MemUnion)

as.factor(df$SexPromo) 
df$SexPromo <- as.factor(df$SexPromo)

as.factor(df$AwareI4) 
df$AwareI4 <- as.factor(df$AwareI4)

str(df)
colnames(df)

#Linear regression model
reg <- lm(EmpYears ~ WorkHrs + Occup_n + Age + Educ_Yrs + Sex + MemUnion + 
            WrkYears + NumPromo + FutPromo + SexPromo + AwareI4, df)
reg

#Regression table
summary(reg)

#TASK 3: Predict the employees with years of employment more than 10
#Create new variable and deal with it (dummy)
df$EmpYears
df$EmpYear10 <- ifelse(df$EmpYears > 10 , 1 , 0)
df$EmpYear10
str(df)

as.factor(df$EmpYear10) 
df$EmpYear10 <- as.factor(df$EmpYear10)
str(df)
colnames(df)

#Logistic regression
logreg <- glm(EmpYear10 ~ WorkHrs + Occup_n + Age + Educ_Yrs + Sex + MemUnion +
                WrkYears + NumPromo + FutPromo + SexPromo + AwareI4, 
              df ,
              family = binomial)
logreg

#Regression table
summary(logreg)

#Confusion matrix
actual_value <- df$EmpYear10

predict(logreg , df) 
predicted_value <- ifelse(predict(logreg , df , type = "response") > 0.5 ,1, 0)

table(actual_value , predicted_value)

#Overall accuracy
(633 + 219) / (633 + 56 + 92 + 219)
#Error
1 - 0.852
#Sensitivity
219 / (92 + 219)
#Specificity
633 / (633 + 56)

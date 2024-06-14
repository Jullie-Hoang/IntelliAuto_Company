#TASK 1: Identify the probability of employee leaving the company in less than 10 years
df <- IntelliAuto_company_raw_data
colnames(df) 

df$lessThan10 <- ifelse(df$EmpYears < 10 , 1 , 0) 
colnames(df) 

df$lessThan10 <- as.factor(df$lessThan10) 

model <- glm(lessThan10 ~ WorkHrs + Age + Educ_Yrs, df, family = binomial ) 
df$probability <- predict(model, df, type = "response") 
head(df) 

#TASK 2: Identify the 5 employees with the highest probability of leaving the company in less than 10 years
df[ order(df$probability, decreasing = TRUE) , ] 

library(dplyr) 

arrange(df, desc(probability))    
df1 <- arrange(df, desc(probability)) 
df1


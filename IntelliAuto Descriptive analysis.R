df <- IntelliAuto_company_raw_data
is.na(df)
sum(is.na(df))

#TASK 1: Overall summary of the number of working hours at the IntelliAuto
summary(df$WorkHrs)
boxplot(df$WorkHrs ,
        ylab = "Working hours" ,
        main = "IntelliAuto employees's working hours box and whisker plot")
table (df$WorkHrs)
t <- table (df$WorkHrs) 
barplot(t ,
        xlab = "Working hours" , 
        ylab = "Frequency" ,
        main = "Frequency of IntelliAuto employees's Working hours")
prop.table(t)

#TASK 2:Different in hours worked based on different categories
#Gender
boxplot(df$WorkHrs ~ df$Sex,
        xlab = "Gender" ,
        ylab = "Working hours" ,
        main = "Difference of IntelliAuto employees's working hours in terms of Gender" ,
        col = c ("lightpink1" , "lightskyblue"))

#Occupation
boxplot(df$WorkHrs ~ df$Occup_n,
        xlab = "Occupation" ,
        ylab = "Working hours" ,
        names = c("Manag" , "Prof'na" , "Tech/Sales" , "Admin" , "Service" , "Prod'n" , "Laborer") ,
        main = "Difference of IntelliAuto employees's working hours in terms of Occupation" ,
        col = c("orange" , "steelblue1" , "tomato" , "seagreen" , "palevioletred1" , "burlywood4" , "mediumpurple3"))

#Industry 4.0 awareness
boxplot(df$WorkHrs ~ df$AwareI4,
        xlab = "Awareness of industry 4.0" ,
        ylab = "Working hours" ,
        main = "Difference of IntelliAuto employees's working hours in terms of their Awareness of Industry 4.0" ,
        col = c ("lightcoral" , "darkolivegreen3"))

#TASK 3:Relationship of working hours with different categories
#Age
cor(df$WorkHrs , df$Age)
plot(df$WorkHrs , df$Age ,
     xlab = "Working hours" ,
     ylab = "Age" ,
     main = "Relationship between IntelliAuto employees's working hours and their Age" ,
     las = 1 ,
     cex = 0.5 ,
     mtext ("cor = -0.093" , side = 3))
abline(lm(df$WorkHrs~df$Age) , 
       col = "blue")
lines(smooth.spline(df$WorkHrs , df$Age) ,
      col = "gold")

#Education years
cor(df$WorkHrs , df$Educ_Yrs)
plot(df$WorkHrs , df$Educ_Yrs ,
     xlab = "Working hours" ,
     ylab = "Education years" ,
     main = "Realtionship between IntelliAuto employees's working hours and their education years" ,
     las = 1 ,
     cex = 0.5 ,
     mtext ("cor = 0.05" , side = 3))
abline(lm(df$WorkHrs~df$Educ_Yrs) , 
       col = "blue")
lines(smooth.spline(df$WorkHrs , df$Educ_Yrs) ,
      col = "gold")

#TASK 4:Distribution of Male and Female employees across Occupations
t1 <- table(df$Sex , df$Occup_n)
barplot(t3 ,
        xlab = "Occupation" ,
        ylab = "Frequency" ,
        names = c("Manag" , "Prof'na" , "Tech/Sales" , "Admin" , "Service" , "Prod'n" , "Laborer") ,
        main = "Distributiuon of Gender pertaining to Occupation of employees in IntelliAuto" ,
        col = c("lightpink1" , "lightskyblue") ,
        legend = TRUE ,
        args.legend = list(x = "topright" , inset = c(0.25 , 0) , bty = "n") ,
        beside = TRUE)






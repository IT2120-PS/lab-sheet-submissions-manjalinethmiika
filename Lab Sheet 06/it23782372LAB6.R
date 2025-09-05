getwd()
setwd("C:\\Users\\Asus\\OneDrive - Sri Lanka Institute of Information Technology\\Desktop\\it23782372")
getwd()

#Import the dataset (’Exercise.txt’) into R and store it in a data frame called ”branch data”
branch_data <-read.table("Exercise.txt" , header = TRUE, sep=",")
#view first few rows
head(branch_data)

#2. Identify the variable type and scale of measurement for each variable.
#Branch-categorical(Nominal scale)
#Sales_X1-continuous(ratio scale)
#Advertising_X2-continuous(Ratio scale)
#Years_X3-Discrete(ratio scale)

#3.Obtain boxplot for sales and interpret the shape of the sales distribution.
boxplot(branch_data$Sales_X1,main= "Boxplot of sales", horizontal = TRUE)


#4.Calculate the five number summary and IQR for advertising variable.
summary(branch_data$Advertising_X2)

#IQR
IQR(branch_data$Advertising_X2)


#5.Write an R function to find the outliers in a numeric vector and check for outliers in years variables.
get_outliers <- function(x){
   q1 <- quantile(x, 0.25)
   q3 <- quantile(x,0.75)
   iqr <- q3-q1
   lb <- q1-1.5*iqr
   ub <- q3+1.5*iqr
   outliers <- x[x < lb | x > ub]
   return(outliers)
  
}

get_outliers(branch_data$Years_X3)

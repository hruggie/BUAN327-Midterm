library(ggplot2)
library(dplyr)

hospitals = read.csv('Hospitals.csv')


#NUMBER ONE:

#a How big is the dataset?
  
dim(hospitals)
  
#b What are the names of the columns

colnames(hospitals)
  
#c What data types are each column?
  
colDataTypes = sapply(hospitals, class)
cat(colDataTypes)
  
  
#d Are there missing values?
missingvalues = any(is.na(hospitals))
print(missingvalues)
#Value returned is False therefore no missing values.

#E Hospital numbers 1064 and 1751 have the lowest number of beds (3)
LowNumBed=hospitals %>% filter(Beds==min(Beds))
print(LowNumBed)

#F Hospital number 826 has the lowest expense of 1,487
LowExpense = hospitals %>% filter(Total.Expense==min(Total.Expense))
print(LowExpense)

#G 1,112 hospitals deliver babies
Babies = hospitals %>% filter(Births.or.Not==1)
count(Babies)

#H scatter plot beds vs total expense
ggplot(hospitals, aes(x= Beds, y=Total.Expense)) + geom_point()

#I admissions vs total expense
ggplot(hospitals, aes(x= Admissions, y=Total.Expense)) + geom_point()

#J beds vs total expense for hospitals that deliver babies
ggplot(Babies, aes(x= Beds, y=Total.Expense)) + geom_point()


#Correlation between the number of beds and total expense
#K one other question
ggplot(hospitals, aes(x=Beds, y=Payroll.Expense)) + geom_point() #NOTE: make a line here to show correlation


#NUMBER TWO: Descriptive Analysis
#Pie Chart
df = data.frame(
  AdTotExp = c("Admissions", "Total Expense"),
  Values = c(sum(hospitals$Admissions), sum(hospitals$`Total.Expense`)) 
)

df %>% ggplot(aes(x="", y=AdTotExp, fill=Values)) +
  geom_bar(stat='identity', width=1) +
  coord_polar('y', start=0) +
  ggtitle('Pie Chart Admissions vs Total Expense')

#Bar Chart
AdCensus = sum(hospitals$Personnel)
print(AdCensus)
df2 =data.frame(
  category = c('Admissions', 'Personnel'),
  chart = c(sum(hospitals$Admissions), AdCensus)
  
)
ggplot(df2, aes(x=category, y=chart, fill=category)) + geom_bar(stat='identity')


#Line Chart
hospital.line = rank(hospitals$Total.Expense)
ggplot(hospitals, aes(x=hospital.line)) + geom_line(aes(y=Total.Expense))


#QUESTION 3 Simple Regression
SR = lm(Total.Expense ~ Beds, data=hospitals)
summary (SR)

#iii. What is the value of the R^2
  #0.6043

#iv. What does the R^2 measure in this case? ( Hint: Percentage of
                                               #variation in … explained by …)
  #percentage of variation in Total Expense that is explained by Beds

#v. What are the pvalues ? How many pvalues are reported, why ?
  #What does each pvalue mean? (Hint: pvalues have are related to
                               #generalizing from the sample to the population: what is the sample
                               #here, what is the population?) (1-2 sentences)
  #The intercept P-value is <2e-16 (reject h0) 
  #This tells us there is correlation between beds and total expense
  #the sample is the hospitals within this dataset and the population is all hospitals including data not in this dataset

#vi. Explain R square, pvalues.
  # R^2 tells us the correlation and fit of our data and what we are trying to predict
  # on a scale 0-1
  #pvalues test the hypothesis (<0.05) rejects the H0

#vii. What would be the right attribute size (independent variable) that
#seems most appropriate to lead you in the expense range of $55–$75 million?

#55,000,000=1084.56(x)-16060.93
#50,726.62

#75,000,000=1084.56(x)-16060.93
#69,167.27

#QUESTION 4 Perform one multivariate regression to provide recommendations
multi = lm(hospitals$Total.Expense ~ hospitals$Beds + hospitals$Personnel, hospitals)
summary(multi)

# ii. What is the value of the R^2
  #0.9121
#   iii. What does the R^2 measure in this case? ( Hint: Percentage of
                                                 #variation in … explained by …)
  #R^2 measures the percentage of variation in total expense that is explained by beds AND Personnel

# iv. What are the pvalues ? How many pvalues are reported, why ?
  #The Pvalue for the intercept is 0.000355, beds is 1.05e-11, and personnel
  #is <2.2e-16 which means they are <0.05 and we reject the H0

#   What does each pvalue mean? (Hint: pvalues have are related to
#                                generalizing from the sample to the population: what is the sample
#                                here, what is the population?) (1-2 sentences)
  #each pvalue tests the hypothesis indicating weak or strong evidence against H0
  #the sample is the hospitals within this dataset and the population is all hospitals 

# v. Explain R square, pvalues.
  #R^2 measures the goodness of fit in regression model 
  #total expense can be explained by personnel and beds with a pvalue of <2.2e-16
  #pvalues are used in hypothesis testing to test the significance 
  #if <0.05 we reject H0 and if >0.05 we fail to reject H0

#Based on my data and graphs above I believe option A would be the sufficient answer
#my multi regression with personnel and beds suggests it would fit option
#A's criteria of involving fewer beds and personnel while also maintaining expenses
#This also suggests room for expansion

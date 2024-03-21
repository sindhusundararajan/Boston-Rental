# Clear the variable
rm(list=ls())
# Clears the plot
dev.off()

# Import the libraries
library(ISLR)
library(caret)
library(knitr)
library(kableExtra)
library(plotrix)
library(lessR)
library(magrittr)
library(grDevices)
library(see)
library(ggplot2)
library(gridExtra)
library(caTools)
library(psych)
library(pROC)
library(readxl)
library(readr)
library(tidyverse)
library(dplyr)
library(corrplot) #for visualisation of correlation
library(lattice) #for visualisation
library(plotly) #converting ggplot to plotly
library(Hmisc)
library(leaps)
library(car)
library(MASS)

# Set working directory and Reading the Dataset~~
setwd("C:/Users/neeha/OneDrive/Documents/Northeastern University Assignmnets/Intermediete Analytics/FinalProject")
df <- read.csv(file = 'Boston Rentals.csv', stringsAsFactors = FALSE, na.strings = c(NA,"NA"," NA",""))

# Summary and Structure of the dataset
summary(df)
str(df)
psych::describe(df)

# Data Cleaning
# Keeping the important attributes
df <- df[c('Q1', 'Q2', 'Q3','Q4','Q5','Q6','Q7','Q8','Q9','Q10','Q11','Q12','Q13','Q14')]
df


colnames(df) <- c('NUID','House_Location','Gender','Age','Marital_Status','Apartment_Type','No_Of_Bedrooms','No_Of_Bathrooms','Rent','Zip_Code','Distance_From_University','House_Sqft','Furnished_Type','No_Of_Tenants')
df <- df[-c (1, 2), ]
df <- df[complete.cases(df),]
df %>% drop_na()
str(df)

# Removing the character from the column
df$No_Of_Bedrooms = gsub('[BHK]','',df$No_Of_Bedrooms)
df$No_Of_Bedrooms <- as.numeric(df$No_Of_Bedrooms)
df$Rent <- as.numeric(df$Rent)
df$No_Of_Bathrooms <- as.numeric(df$No_Of_Bathrooms)
df$Distance_From_University <- as.numeric(df$Distance_From_University)
df$No_Of_Tenants <- as.numeric(df$No_Of_Tenants)
str(df)

#### EDA
summary(df)
# Display rows and columns
dim(df)
attach(df)

# Visualization of the Dataset
# Scatter-Plot
ggplot(df) +
  aes(
    x = Distance_From_University,
    y = Rent,
    colour = House_Location
  ) +
  geom_point(shape = "circle", size = 3L) +
  scale_color_hue(direction = 1) +
  labs(
    x = "Distance from the University to the House",
    y = "House Rent",
    title = "Scatter Plot for the Distance from University vs. Amount of Rent",
    color = "Locality"
  ) +
  theme_gray()
### From the chart, it can be observed that the majority of the population stays in distance radius of 0-2.5 miles from the University. 
### Most of the population pays around $1500-$4000 and stays within the radius of 2.5 miles. We can conclude that, they are paying more than the people who are staying far away.

# Stacked Histogram of Number of Bedrooms and Apartment Types
ggplot(df) +
  aes(x = No_Of_Bedrooms, fill = Apartment_Type) +
  geom_histogram(bins = 15L) +
  scale_fill_hue(direction = -1) +
  labs(
    x = "Number of Bedrooms",
    y = "Frequency",
    title = "Stacked Histogram of Number of Bedrooms and Apartment Types",
    fill = "Apartment Type"
  ) +
  theme_gray()

# Tile Plot illustrating furnishment level in each locality
ggplot(df) +
  aes(x = House_Location, y = Furnished_Type, fill = Furnished_Type) +
  geom_tile() +
  scale_fill_manual(values = c(Furnished = "#F8766D",
                               Semifurnished = "#00C19F", Unfurnished = "#FF61C3")) +
  labs(x = "House_Location", y = "Furnished_Type",
       title = "Furnishment level in locality") +
  theme_gray()

# Age vs Distance from the university
ggplot(df) +
  aes(x = Age, y = Distance_From_University) +
  geom_boxplot(fill = "#FF8C00") +
  labs(x = "Age",
       y = "Distance from the University", title = "Age vs. Distance from the University") +
  theme_gray()

# Finding mean of Rent with respect to Apartment Type and assigning it to new data frame
RentVsApartmentType <- aggregate(as.numeric(Rent) ~ Apartment_Type, data = df, mean)
RentVsApartmentType # adding all the mean of Rent with respect to Apartment Type for an new data frame
attach(RentVsApartmentType) #attach data frame to workspace
data_bar <- RentVsApartmentType$`as.numeric(Rent)` # Extract values
names(data_bar) <- RentVsApartmentType$Apartment_Type # Assign names to values
library(RColorBrewer) #Adding RcolorBrewer package
coul <- brewer.pal(8, "Set2") 

# Barplot for average Rent vs Apartment Type
options(scipen=999)
barplot(data_bar, col = coul,las = 1, cex.names = 1, main="Bar plot for Average Rent Vs Apartment Type",xlab="Apartment Type",ylab="Rents in Boston") 

# Correlation Plot
df1 <- data.frame(as.numeric(df$No_Of_Bathrooms),as.numeric(df$Rent),as.numeric(df$Distance_From_University),as.numeric(df$No_Of_Tenants),as.numeric(df$No_Of_Bedrooms))
colnames(df1) <- c('No_Of_Bathrooms','Rent','Distance_From_University','No_Of_Tenants','No_Of_Bedrooms')
cors <- cor(df1,use = "pairwise")
cors
corrplot(cors, type="upper")
rcorr(cbind(df1$No_Of_Bathrooms,df1$Rent,df1$Distance_From_University,df1$No_Of_Tenants,df1$No_Of_Bedrooms))

# Strong positive correlation
cor.test(df1$Rent,df1$No_Of_Tenants)

# Strong negative correlation
cor.test(df1$Rent,df1$Distance_From_University)

# Chi square test
# Hypotheses
# H0: The rent of all areas in boston is the same.
# Ha: The rent of all areas in boston is not the same
alpha <- 0.05

# Compute test value
observed <- c(df$Rent)
observed
test <- chisq.test(observed,p=rep(1/ length(df$Rent),length(df$Rent)))
test

# Decision
if (test$p.value < alpha) {
  print("Reject the null hypothesis. The rent of all areas in boston is not the same")
} else {
  print("Fail to reject the null hypothesis. The rent of all areas in boston is the same.")
}


#### Question-1: Determine whether those who live close to a college pay higher or lower rent than those who live farther away by comparing the two groups of people.

# Separate the dataset into two groups based on distance from college
close_to_college <- as.numeric(df$Rent[as.numeric(df$Distance_From_University) <= 4])
far_from_college <- as.numeric(df$Rent[as.numeric(df$Distance_From_University) > 4])

# Calculate the mean rent for each group
mean_rent_close_to_college <- mean(close_to_college)
mean_rent_far_from_college <- mean(far_from_college)

# Two Sample T-Test
# Hypotheses
# H0: The mean differences of rent for houses closer to college is more than, that of the houses far from the college
# Ha: The mean differences of rent for houses closer to college is not same as the houses far from the college

# Print the Results
cat("Mean rent for those close to college: $", round(mean_rent_close_to_college, 2), "\n")
cat("Mean rent for those far from college: $", round(mean_rent_far_from_college, 2), "\n")

# Perform a Two-Sample T-Test
t_test_result <- t.test(close_to_college, far_from_college, alternative = "greater")
t_test_result

# Print the results
cat("P-value: ", t_test_result$p.value, "\n")
cat("95% confidence interval: [", t_test_result$conf.int[1], ", ", t_test_result$conf.int[2], "]\n")

# Decision
if (t_test_result$p.value < alpha) {
  print("Reject the null hypothesis. The mean differences of rent for houses closer to college is more than, that of the houses far from the college")
} else {
  print("Fail to reject the null hypothesis. The mean differences of rent for houses closer to college is same as the houses far from the college")
}
# Conclusion: We conclude that The mean differences of rent for houses closer to college is more than, that of the houses far from the college


#### Question-2 : Is the apartment or individual housing expensive to rent when there are the same number of bedrooms? 

# Two-Sample T-Test
# H0: The rent for the apartment is same as that of individual houses for 2BHKs
# Ha: The rent for the apartment is different as that of individual houses for 2BHKs

apt <- subset(df, Apartment_Type == "Apartment" & No_Of_Bedrooms==2)$Rent
indiv <- subset(df, Apartment_Type == "Individual Houses" & No_Of_Bedrooms==2)$Rent
t_test_result <- t.test(apt, indiv, var.equal = TRUE)
t_test_result

# Decision
if (t_test_result$p.value < alpha) {
  print("Reject the null hypothesis. The rent for the apartment is different as that of individual houses for 2BHKs")
} else {
  print("Fail to reject the null hypothesis. The rent for the apartment is same as that of individual houses for 2BHKs")
}

# Conclusion: The rent for the apartment is same as that of individual houses for 2BHKs


#### Question 3: Is the number of tenants directly proportional to the average rent?

# Perform a simple linear regression analysis
model <- lm(Rent ~ No_Of_Tenants, data = df)

# View the summary of the model
summary(model)

# Plot the data and the fitted regression line
plot(df$No_Of_Tenants, df$Rent, xlab = "Number of Tenants", ylab = "Rent")
abline(model, col = "red")

# Conclusion: This regression analysis would involve fitting a line to the data points, where the dependent variable is the rent
#             and the independent variable is the number of tenants. The slope of the line would indicate the change in rent for 
#             each additional tenant. If the slope is positive and statistically significant, then we can say that there is a relationship
#             between the number of tenants and rent, and that having a higher number of tenants is associated with higher rent.


#### Question-4: Does the rent change with respect to the furnishing type?

# H0: The variances of rents for furnished, semi furnished and fully furnished houses are same 
# Ha: The variances of rents for furnished, semi furnished and fully furnished houses are different 

# Create dummy variables for the "furnished" column
furnished_dummies <- model.matrix(~ Furnished_Type + 0, data = df)
furnished_dummies

# Bind the dummy variables to the original data frame
df <- cbind(df, furnished_dummies)

# Rename the dummy variable columns
colnames(df)[colnames(df) %in% c("furnishedYes", "furnishedNo", "furnishedSemi")] <- c("fully_furnished", "unfurnished", "semi_furnished")

# View the updated data frame with dummy variables
head(df)

# Create three groups based on the level of furnishing
furnished <- df$Furnished_TypeFurnished
semi_furnished <- df$Furnished_TypeSemifurnished
unfurnished <- df$Furnished_TypeUnfurnished

# Convert "Furnished_TypeFurnished" to a factor variable
df$Furnished_TypeFurnished <- as.factor(df$Furnished_TypeFurnished)

# Perform an ANOVA test to compare the means of the three groups
model <- aov(Rent ~ Furnished_TypeFurnished, data = df)
model

# View the summary of the ANOVA model
sum <- summary(model)
sum

# Perform a post-hoc test using Tukey's HSD method
TukeyHSD(model)

# Assigning the p-value to the variable
p_value <- sum[[1]][1,"Pr(>F)"]

# Decision
if (p_value < alpha) {
  print("Reject the null hypothesis. The variances of rents for furnished, semi furnished and fully furnished houses are different")
} else {
  print("Fail to reject the null hypothesis. The variances of rents for furnished, semi furnished and fully furnished houses are same")
}

# Conclusion: From this, we can conclude that rent are varying when compared to furnished, semi furnished and fully furnished houses

# Model Training and Testing
# Split the data into training and testing sets (70/30 split)
set.seed(123)
trainIndex <- createDataPartition(y = df$Rent, p = 0.7, list = FALSE)
train <- df[trainIndex, ]
test <- df[-trainIndex, ]

# Fit a linear regression model on the training set
model <- lm(Rent ~ No_Of_Bedrooms + No_Of_Bathrooms + Distance_From_University + No_Of_Tenants , data = train)
model

# Make predictions on the testing set
predictions <- predict(model, newdata = test)
predictions

# Rent = 1058.01 + (108.04 * No_Of_Bedrooms) + (15.10 * No_Of_Bathrooms) - (77.47 * Distance_From_University) + (632.18 * No_Of_Tenants)

# AIC and BIC 
summary(model)
AIC(model)
BIC(model)

# Evaluate the model using mean squared error (MSE)
mse <- mean((predictions - test$Rent)^2)
print(paste0("Mean squared error: ", mse))
var(test$Rent)

# Conclusion: As MSE is much smaller than variance, we can conclude that, this model is a good fit for predicting rent.

# Review diagnostic plans
plot(model)
dev.off()
par(mfrow = c(2,2))
par(mar = c(1, 1, 1, 1))
crPlots(model)
spreadLevelPlot(model)
vif(model)

# Unusual Observations
outlierTest(model = model) #outlier exists as p is less than 0.05

# High leverage observation
hat.plot <- function(model){
  p <- length(coefficients(model))
  n <- length(fitted(model))
  plot(hatvalues(model), main="Index plot of hat values")
  abline(h = c(2,3)*p/n, col="red",lty=2)
  identify(1:n,hatvalues(model), names(hatvalues(model)))
}
hat.plot(model)

# Influential observations
cutoff <- 4/(nrow(df) - length(model$coefficients)-2)
plot(model, which =4, cook.levels = cutoff)
abline(h=cutoff,lty=2,col="red")

# Feature selection
# Backward step wise selection
stepAIC(model,direction="backward")

# Forward step wise selection
stepAIC(model,direction="forward")

# Stepwise step wise selection
stepAIC(model,direction="both")

# Bestsubset
leaps <- regsubsets(Rent ~ Distance_From_University + No_Of_Tenants, data = df, nbest = 2)
plot(leaps,scale="adjr2")
summary(leaps)

#Clear the variable
rm(list=ls())
#Clears the plot
dev.off()


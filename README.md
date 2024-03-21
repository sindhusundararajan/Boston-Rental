# Boston-Rental
This code performs exploratory data analysis, data cleaning, and regression modeling on a dataset containing information about rental properties in Boston. The main objectives of the analysis are:

1. Determine whether proximity to a college influences rent prices.
2. Compare rental prices for apartments and individual houses with the same number of bedrooms.
3. Investigate the relationship between the number of tenants and average rent.
4. Assess whether rent varies based on the level of furnishing (furnished, semi-furnished, or unfurnished).

## Data Preparation

The code begins by importing the necessary libraries and setting the working directory. It then reads the "Boston Rentals.csv" file into a data frame called `df`. Data cleaning steps are performed, including removing unnecessary columns, handling missing values, and converting data types.

## Exploratory Data Analysis (EDA)

The EDA section includes:

- Summary statistics and data structure exploration
- Visualization of the dataset using scatter plots, histograms, tile plots, and box plots
- Calculation of average rent based on apartment type
- Correlation analysis between variables such as the number of bathrooms, rent, distance from the university, number of tenants, and number of bedrooms

## Hypothesis Testing

The code performs hypothesis testing to address the following questions:

1. **Proximity to a college and rent prices**: A two-sample t-test is conducted to compare the mean rent for properties close to a college (within 4 miles) and those farther away.

2. **Rental prices for apartments vs. individual houses**: Another two-sample t-test is performed to determine if there is a difference in rent between apartments and individual houses with the same number of bedrooms (2 BHKs).

3. **Relationship between the number of tenants and rent**: Simple linear regression is used to analyze the relationship between the number of tenants and rent.

4. **Rent variation based on furnishing level**: An ANOVA test and Tukey's HSD post-hoc test are employed to compare the means of rent for furnished, semi-furnished, and unfurnished properties.

## Model Training and Evaluation

The dataset is split into training and testing sets (70/30 split). A linear regression model is trained on the training set using the following features: number of bedrooms, number of bathrooms, distance from the university, and number of tenants. The model's performance is evaluated using mean squared error (MSE) on the testing set.

Additional diagnostic plots and feature selection techniques (backward, forward, and stepwise selection, as well as best subset selection) are also included.

## Conclusion

The analysis provides insights into the rental market in Boston, including the impact of proximity to colleges, property types, number of tenants, and furnishing levels on rent prices. The linear regression model can be used to predict rent based on the selected features.

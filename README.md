# <div align = "center"> Predicting Residential Electricity Consumption Variability Using Temperature & Total Customers </div>
This project aims to predict residential electricity consumption variability based on temperature changes and the total number of customers. Using data analysis and machine learning techniques, the model identifies patterns and key factors driving fluctuations in electricity usage, helping to optimize energy distribution and forecast future demands. The repository includes data preprocessing, exploratory analysis, model development, and performance evaluation.

## Introduction
The goal is to develop a machine learning model that accurately predicts the variability in residential electricity consumption based on temperature and total customers in the state of Tennessee. I'll utilize R's powerful statistical and data manipulation capabilities for this analysis.

## Research Questions
#### Primary Question: 

  - To what extent do temperature and customers predict variability 
  in electricity consumption?

#### Secondary Questions:
  - Is there a linear relationship between temperature and electricity consumption?

  - Are there specific temperature ranges where energy usage changes significantly?

## Approach
Multiple Linear Regression (lm function): This will quantify the impact of both temperature and total customers on energy consumption, providing coefficients and statistical significance tests.Using summary to view the statistics.

I Will apply advanced techniques to achieve an understanding on the data if the is a non-linear relationship.

## Datasets
  - **Electricity Consumption Data (Sales):** Monthly electricity consumption data (kWh). "Sales" in electricity consumption data refers to the total electricity (in kilowatt-hours) sold to end-users over a period.

  - **Temperature Data:** Monthly temperature data °F.

#### Links
  - https://www.eia.gov/electricity/data/state/
  
  - https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/national/time-series/110/tavg/1/0/2018-2024
  
## Required Packages (R)

  - readxl: For reading Excel files.
  
  - dplyr: For data manipulation and transformation.
  
  - tidyr: For tidying data (reshaping, cleaning, etc.).
  
  - ggplot2: For creating data visualizations.
  
  - caret: For machine learning tasks (classification and regression).
  
  - forecast: For time series forecasting.
  
  - lubridate: For working with dates and times.
  
  - mgcv: for non-linear models.

## Plots and Table Needs
  - Scatter plots (ggplot2): To visualize relationships between variables.

  - Line plots (ggplot2): To show trends in electricity consumption over time.

  - Residual plots (plot function): To assess regression assumptions.

  - Summary table (summary function): To present regression results.
  
  - Pair plots: To visualize relationships between multiple pairs of variables.
  

## Questions for Future Steps
  - How to account for other factors?

  - Can we develop a predictive model for future consumption?

## What do you not know how to do right now that you need to learn to answer your research questions?
  - To more fully answer my research questions, I need to expand my knowledge of advanced regression techniques beyond linear regression and basic generalized linear models (GLMs) and GAMs. Exploring a wider range of advanced regression models will allow for a more comprehensive analysis and potentially uncover deeper insights into the relationships within my data.
  
## How to import and clean my data
  1. Read data using read_csv
  2. Standardize column names.
  3. fix column displaying date and convert it to date format
  4. check for null data, and remove them.

## How do you plan to slice and dice the data?

  - By Time Periods: Analyze by months, seasons, and years to observe trends and seasonal effects.
  - By Temperature Ranges: Group data into different temperature ranges to see how electricity consumption varies.
  
## What does the final data set look like?

  - will use several methods in R to merge and bind columns from all data sets into one that will be used to for analysis, it will include columns such as electricity consumption, price per kWh and temperature.

## Do you plan on incorporating any machine learning techniques to answer your research questions? Explain.

  - I will perform regression analysis on data set and provide answers to my initial question, I will also sample some years and predict later years to check accuracy of model.
  
## Dataset preview

The dataset comprises columns merged from multiple datasets, with additional variables retained specifically for usage determination during regression analysis.

  - Year: Represents the year in which the data was recorded.
  
  - Month: Represents the month in which the data was recorded.
  
  - Temperature: Represents the average temperature during the recorded period.
  
  - Revenue: Total revenue generated from electricity consumption during the recorded period.
  
  - Sales: Total amount of electricity sold during the recorded period, typically measured in kilowatt-hours (kWh) or similar units.
  
  - Customers: Number of residential customers who purchased electricity during the recorded period.
  
  - Price/kWh: Average price charged for electricity during the recorded period, typically measured in cents per kilowatt-hour (¢/kWh) or a similar unit. It was calculated by dividing the revenue by sales.
  
```{r}
# Load packages
library(readxl,warn.conflicts = FALSE)
library(dplyr,warn.conflicts = FALSE)
library(ggplot2,warn.conflicts = FALSE)
library(caret,warn.conflicts = FALSE)
library(forecast,warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(mgcv, warn.conflicts = FALSE)

# File path for the CSV file containing temperature data
url1 <- "C:\\Users\\Hamza\\Desktop\\Final\\monthly\\avg_temp_tn.csv"

# File path for the Excel file containing energy data

url2 <- "C:\\Users\\Hamza\\Desktop\\Final\\monthly\\Energy2010-2024.xlsx"  

# Read temperature data from the CSV file
data1 <- read.csv(url1)  

# Read energy data from the Excel file
data2 <- read_excel(url2, col_names = TRUE)  

# Extract year and month from the 'Date' column in data1
data1$Year <- substr(as.character(data1$Date), 1, 4)
data1$Month <- substr(as.character(data1$Date), 5, 6)

# Convert 'Year' and 'Month' to numeric format
# Treated those variables as numeric to facilitate their utilization in analysis

data1$Year <- as.numeric(data1$Year)
data1$Month <- as.numeric(data1$Month)

# Remove the 'Date' column and rename 'Value' column to 'Temperature' in data1
data1 <- subset(data1, select = -Date)
names(data1)[1] <- "Temperature"
names(data2)[6] <- "Price"


# Display the first few rows of data1 and data2
head(data1, 2)
head(data2, 2)

# Merge data1 and data2 based on 'Year' and 'Month'
df_combined <- merge(data1, data2, by = c("Year", "Month"))


## combined date
# Create date column using the `make_date()` function
df_combined$Date <- make_date(year = df_combined$Year, month = df_combined$Month)

# Remove the 'Price' column because the Price column was derived by dividing 
# Revenue by Sales. Including the Price column in the analysis would 
# introduce multicollinearity.

df_combined <- subset(df_combined, select = -c(Revenue, Month, Year , Price))

# rename sales column to consumption to avoid commercial connotation of 
# the word "sales".
names(df_combined)[2] <- "Consumption"


# Display the first 10 rows of the merged data frame sorted by year and month
df_combined <- df_combined[order(df_combined$Date, 
                                 decreasing = FALSE), ]

head(df_combined, 10)
```
![image](https://github.com/user-attachments/assets/fdb3d02b-510e-42b1-9f35-68dd8cd301f2)

## Summary
```{r}
summary(df_combined)
```
![image](https://github.com/user-attachments/assets/d7869c0c-9ab1-4428-b9a9-3ad19ea1d3db)

## Checking for missing data
```{r}
# Count NA values in the entire data frame
sum(is.na(df_combined))
```
![image](https://github.com/user-attachments/assets/0d5b2a43-77d0-4cd4-807c-532c357007bb)

```{r}
library(ggplot2)
# Scatter plot of electricity consumption vs temperature
ggplot(df_combined, aes(x=Temperature, y=Consumption)) +
  geom_point() +
  geom_smooth(method='lm', col='red') +
  labs(title="Electricity Consumption vs Temperature")
![image](https://github.com/user-attachments/assets/b2a301b6-4da6-4f75-b4a5-a5cfbd0499c3)

# Scatter plot of electricity consumption vs customer count
ggplot(df_combined, aes(x=Customers, y=Consumption)) +
  geom_point() +
  geom_smooth(method='lm', col='blue') +
  labs(title="Electricity Consumption vs Customer Count")
![image](https://github.com/user-attachments/assets/695c9098-d7d7-4457-a308-e41a8251f0a5)

# Pair plot to see relationships among all variables
pairs(df_combined)
```
![image](https://github.com/user-attachments/assets/9f501262-d114-4452-98ff-83b2944b40bf)
  
  - It is apparent that there is lower electricity consumption when the temperature is between 50 and 70 degrees Fahrenheit.
  
  - The customer and electricity consumption seem scattered evenly across the chart.

## Correlation Analysis
```{r}
df_cor <- cor(df_combined[,c("Temperature", "Consumption", "Customers")] )
df_cor

df_cor_test <- cor.test(df_combined$Temperature, df_combined$Consumption)
df_cor_test
```
![image](https://github.com/user-attachments/assets/3ea699c9-10b0-441b-ae63-e7ffc1c49d6e)
![image](https://github.com/user-attachments/assets/9f302f6b-1055-4af4-b4fe-76259a01d372)
![image](https://github.com/user-attachments/assets/c6ff8919-db29-4bda-9f36-d9c0f96dda0b)

 - The correlations between the variables are weak, indicating that changes in one variable are not strongly associated with changes in the other variables.

## Electricity Consumption my months
```{r}
# converting date column to Date format
# data$Date <- as.Date(data$Date, format="%Y-%m-%d")

# Extract month from the date
df_combined$Month <- format(df_combined$Date, "%m")

# Summarize data by month
monthly_summary <- df_combined %>%
  group_by(Month) %>%
  summarise(AvgConsumption = mean(Consumption),
            AvgTemperature = mean(Temperature),
            AvgCustomerCount = mean(Customers))

# Plot monthly electricity consumption
ggplot(monthly_summary, aes(x=Month, y=AvgConsumption)) +
  geom_line(group=1, color="blue") +
  geom_point(color="red") +
  labs(title="Average Monthly Electricity Consumption",
       x="Month", y="Average Consumption")
```
![image](https://github.com/user-attachments/assets/2fd5e763-3a7b-4a96-b5d8-3e98d26e48b6)
  
  - There is an apparent decline in electric consumption starting January and ending in june suggesting seasonality.

## Seasonality in data
```{r}
# Create a season column
df_combined$Season <- case_when(
  df_combined$Month %in% c("12", "01", "02") ~ "Winter",
  df_combined$Month %in% c("03", "04", "05") ~ "Spring",
  df_combined$Month %in% c("06", "07", "08") ~ "Summer",
  df_combined$Month %in% c("09", "10", "11") ~ "Fall"
)

# Summarize data by season
seasonal_summary <- df_combined %>%
  group_by(Season) %>%
  summarise(AvgConsumption = mean(Consumption),
            AvgTemperature = mean(Temperature),
            AvgCustomerCount = mean(Customers))

# Plot seasonal electricity consumption
ggplot(seasonal_summary, aes(x=Season, y=AvgConsumption, fill = Season)) +
  geom_bar(stat="identity") +
  labs(title="Average Seasonal Electricity Consumption",
       x="Season", y="Average Consumption")

```
![image](https://github.com/user-attachments/assets/96a713cc-df01-4b6e-8b9b-1f07ef4c7909)
  
  - It is clear that electric consumption increases in Summer and Winter.
  
# Electricity Consumption by year
```{r}
# Extract year from the date
df_combined$Year <- format(df_combined$Date, "%Y")

# Summarize data by year
yearly_summary <- df_combined %>%
  group_by(Year) %>%
  summarise(AvgConsumption = mean(Consumption),
            AvgTemperature = mean(Temperature),
            AvgCustomerCount = mean(Customers))

# Plot yearly electricity consumption
ggplot(yearly_summary, aes(x=Year, y=AvgConsumption)) +
  geom_line(group=1, color="blue") +
  geom_point(color="red") +
  labs(title="Average Yearly Electricity Consumption",
       x="Year", y="Average Consumption")
```
 ![image](https://github.com/user-attachments/assets/60f7e747-3b7b-40ad-9e3f-87ffc93d1dc5)
 
  - Disregarding the partial 2024 figures, the electricity consumption data reveals a relatively stable pattern across the remaining years, marked by two periods of decreased consumption (2010-2012, 2016-2017) and a subsequent surge in 2017-2018.

  
## Temperature Ranges
```{r}
# Define temperature ranges
df_combined$TempRange <- cut(df_combined$Temperature, 
                      breaks=c(-Inf, 0, 20, 40, 60, 80, Inf), 
                      labels=c("Below 0", "0-20", "20-40", "40-60", "60-80", "Above 80"))

# Summarize data by temperature range
temp_range_summary <- df_combined %>%
  group_by(TempRange) %>%
  summarise(AvgConsumption = mean(Consumption),
            AvgCustomerCount = mean(Customers))

# Plot electricity consumption by temperature range
ggplot(temp_range_summary, aes(x=TempRange, y=AvgConsumption)) +
  geom_bar(stat="identity", fill="lightblue") +
  labs(title="Average Electricity Consumption by Temperature Range",
       x="Temperature Range", y="Average Consumption")

```
![image](https://github.com/user-attachments/assets/9c18ae6b-0710-468f-8a7f-fd2e5e5858d1)
 
  - Summarizing electricity consumption by temperature range we can see a clear increase when the temperature is really cold and really hot.
  
## Create training and testing sets
```{r}
set.seed(123)
trainIndex <- createDataPartition(df_combined$Consumption, p=0.8, list=FALSE)
trainData <- df_combined[trainIndex, ]
testData <- df_combined[-trainIndex, ]

```

## Linear Regression GLM
  - The goal is to predict electricity usage (represented by the Consumption variable).
  
  - Even though i suspect the relationship to be non-linear, i will perfrom linear regression and the numbers would provide a clear answer.

```{r}
model1 <- lm(Consumption ~ Customers + Temperature, data = trainData)
summary(model1)
```
![image](https://github.com/user-attachments/assets/71515b4a-f6cf-4e42-b537-010e51dd104f)

  - The R-squared values for both models are very close to zero. This indicates that the linear regression models do not explain much of the variation in consumption based on Temperature and total customers. In other words, there is not a strong linear relationship between average temperature and the variables in the dataset.
  
## Evaluate the model
### Checking the model's performance using the testing dataset.
```{r}
# Make predictions
predictions <- predict(model1, newdata=testData)

# Compare predictions with actual values
results <- data.frame(Actual = testData$Consumption, Predicted = predictions)

# Calculate performance metrics
RMSE <- sqrt(mean((results$Actual - results$Predicted)^2))
MAE <- mean(abs(results$Actual - results$Predicted))
R2 <- cor(results$Actual, results$Predicted)^2

# Print performance metrics
cat("RMSE:", RMSE, "\n")
cat("MAE:", MAE, "\n")
cat("R-squared:", R2, "\n")


```
![image](https://github.com/user-attachments/assets/5aed9bb3-010a-43e8-bad5-b67ee0055c27)

![image](https://github.com/user-attachments/assets/348fe72c-5be2-4fd0-b881-c128d0ab70ed)

![image](https://github.com/user-attachments/assets/8a95e57b-71be-4ff3-8bce-17595e36c310)

  - The low R-squared value suggests that the linear model is not capturing much of the variability in electricity consumption.
   
### Since using linear models did not yeild a good result i will be using non-linear techniques.

   
## Generalized Additive Model (GAM)
### Using GAM that captures non-linear relationships.
```{r}
gam_model <- gam(Consumption ~ s(Customers) + s(Temperature), data = trainData)
summary(gam_model)  # Check model summary

library(ggplot2)
ggplot(trainData, aes(x = Customers, y = Consumption)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x)) +  # Add GAM smooth to the plot
  labs(title = "Consumption vs. Customers (GAM)")

ggplot(trainData, aes(x = Temperature, y = Consumption)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x)) +  # Add GAM smooth to the plot
  labs(title = "Consumption vs. Temperature (GAM)",
       x = "Temperature",
       y = "Consumption")

```
![image](https://github.com/user-attachments/assets/d27dbdc4-751d-4fb2-aaa9-2d1d7b87d893)

![image](https://github.com/user-attachments/assets/49e758a6-a31c-4ed4-8fee-255b901a97ae)

![image](https://github.com/user-attachments/assets/0fbf086d-df87-44f3-ae06-33399288e80c)

 - The adjusted R-squared value is 0.675, indicating that approximately 67.5% of the variability in electricity Consumption is explained by the model.
 
 - The model explains 68.6% of the deviance in the data, which aligns with the adjusted R-squared value, indicating a significant portion of variability in the consumption data is accounted for by the model.
 
 - The smooth function plot for temperature shows electricity consumption changes with temperature, highlighting increases at both low and high temperatures.
 
# PREDICTIONS
```{r}
# Predicting using the GAM model
predictions <- predict(gam_model, newdata = testData)

# Adding predictions to the test data for comparison
testData$Predicted_Consumption <- predictions

# Plot observed vs predicted consumption
ggplot(testData, aes(x = Consumption, y = Predicted_Consumption)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Observed vs. Predicted Consumption",
       x = "Observed Consumption",
       y = "Predicted Consumption")
```

![image](https://github.com/user-attachments/assets/95b7f48f-0081-4095-8a37-437700bf9686)

 - The scatter plot shows a positive correlation between observed and predicted consumption, indicating the model is generally accurate.
 
 
 
**Implications:** The model effectively predicts electricity consumption, particularly its dependence on temperature and total customers. This can aid in energy demand forecasting and resource allocation.

**Limitations:** The model doesn't account for all factors influencing consumption and may not generalize well to different contexts. Its accuracy can be improved by incorporating additional variables.

**Concluding Remarks:** The model offers valuable insights into electricity consumption patterns. Further refinement and expansion can enhance its predictive power and applicability in real-world scenarios.

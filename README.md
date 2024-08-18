# <div align = "center"> Predicting Residential Electricity Consumption Variability Using Temperature & Total Customers </div>
This project aims to predict residential electricity consumption variability based on temperature changes and the total number of customers. Using data analysis and machine learning techniques, the model identifies patterns and key factors driving fluctuations in electricity usage, helping to optimize energy distribution and forecast future demands. The repository includes data preprocessing, exploratory analysis, model development, and performance evaluation.

```{r}
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
library(lattice)
library(caret)

url <- "C:\\Users\\Hamza\\Desktop\\Final Assignment\\binary-classifier-data.csv"
url2 <- "C:\\Users\\Hamza\\Desktop\\Final Assignment\\trinary-classifier-data.csv"

## read data frame. 
df <- read.csv(url)
df2 <- read.csv(url2)

## convert label to type factor.
df$label <- as.factor(df$label)
df2$label <- as.factor(df2$label)

## view data frame.
head(df, 2)
```
  - In this dataframe the variables x and y are already standard so there is no need to standardize them.

## check balance of dependent variable.
```{r}
prop.table(table(df$label))
prop.table(table(df2$label))
```
   - for the first dataset we can see that it is balanced, 51% of the data points are labeled 0 and 49% are labeled 1.
   
   - for the second dataset we see 25%, 46%, and 29% respectively for 0, 1, and 2.
  
## Create a scatter plot for both datasets
```{r}

scatter_1 <- ggplot(data = df, aes(x = x, y = y)) +
  geom_point(color = 'lightblue') + 
  labs(title = "Scatter Plot #1 of X vs Y",
       x = "X-Axis",
       y = "Y-Axis") + 
  theme_minimal()

scatter_2 <- ggplot(data = df2, aes(x = x, y = y)) +
  geom_point(color = 'grey') + 
  labs(title = "Scatter Plot #2 of X vs Y",
       x = "X-Axis",
       y = "Y-Axis") + 
  theme_minimal()

scatter_1
scatter_2
```

# Training KNN
## Creating Training and Testing Data
```{r}
# Create a test/train split so that we can train a KNN model. 
set.seed(123)

sample_size <- floor(0.8 * nrow(df))
train_indices <- sample(seq_len(nrow(df)), size = sample_size)

train_df <- df[train_indices, ]
test_df <- df[-train_indices, ]

sample_size2 <- floor(0.8 * nrow(df2))
train_indices2 <- sample(seq_len(nrow(df2)), size = sample_size2)

train_df2 <- df2[train_indices2, ]
test_df2 <- df2[-train_indices2, ]

# Define k values
k_values <- c(3, 5, 10, 15, 20, 25)

train_control_df <- trainControl(method = 'cv', number = 10)

# The tuneGrid parameter indicates which values the main parameter will take, 
# in this case we used the values 3 , 5, 10, 15, 20, and 25.

tune_grid <- expand.grid(k = k_values)

model_knn_df <- train(label ~. , data = train_df, method = "knn", trControl 
                      = train_control_df, tuneGrid = tune_grid)

model_knn_df2 <- train(label ~. , data = train_df2, method = "knn", trControl 
                       = train_control_df, tuneGrid = tune_grid)
```


#Models

## model 1 for df 1

```{r}
model_knn_df
```

## model 2 for df 2
```{r}
model_knn_df2
```
 - The first model's optimal performance was achieved with k = 10, resulting in an accuracy of approximately 96.91% and a kappa coefficient of around 0.938.
 
 -  The 2nd model's optimal performance was achieved with k = 10, resulting in an accuracy of approximately 88.83% and a kappa coefficient of around 83%

# Predictions

## prediction for model 1
```{r}
predict_knn <- predict(model_knn_df, test_df)
confusionMatrix(as.factor(predict_knn), as.factor(test_df$label), 
                positive = "1")
```
## prediction for model 2
```{r}
predict_knn2 <- predict(model_knn_df, test_df2)
# Ensuring that both predict_knn2 and test_df2$label are factors with the same 
# levels to avoid errors
predict_knn2 <- factor(predict_knn2, levels = levels(test_df2$label))

confusionMatrix(predict_knn2, test_df2$label, positive = "1")
```

  - The models shows almost perfect accuracy (99%) and strong performance, with high sensitivity, specificity, and a substantial Kappa coefficient of 0.9799.
  
# Accuracy

## Accuracy for model 1
```{r}

accuracy_values <- numeric(length(k_values))

# Loop over each value of k
for (i in seq_along(k_values)) {
    # Fit the KNN model
    model <- train(label ~ ., data = train_df, method = "knn", trControl 
                   = train_control_df, tuneGrid = data.frame(k = k_values[i]))
    
    # Predict on the test set
    predictions <- predict(model, newdata = test_df)
    
    # Compute accuracy
    accuracy_values[i] <- mean(predictions == test_df$label)
}

# Plotting the results
plot(k_values, accuracy_values, type = "o", xlab = "k", ylab 
     = "Accuracy", main = "Accuracy vs. k for KNN Model")
```

## Accuracy for model 2
```{r}
accuracy_values <- numeric(length(k_values))

# Loop over each value of k
for (i in seq_along(k_values)) {
    # Fit the KNN model
    model <- train(label ~ ., data = train_df2, method = "knn", 
                   trControl = train_control_df, tuneGrid = data.frame(k = k_values[i]))
    
    # Predict on the test set
    predictions <- predict(model, newdata = test_df2)
    
    # Compute accuracy
    accuracy_values[i] <- mean(predictions == test_df2$label)
}

# Plot the results
plot(k_values, accuracy_values, type = "o", xlab = "k", ylab 
     = "Accuracy", main = "Accuracy vs. k for KNN Model")
```
  
  - More neighbors (higher k) generally lead to better accuracy, but only up to a point (around 15 or 25 in this case). Using too many neighbors can slightly decrease accuracy.
  

  - The difference in accuracy between logistic regression (58%) and KNN of (98%) indicates that the data likely nonlinear relationships. KNN's flexibility makes it better at handling such data compared to the linear assumptions of logistic regression.
  
# Excersie 2: Clustering

```{r}
url3 <- "C:\\Users\\Hamza\\Desktop\\Final Assignment\\clustering-data.csv"
df3 <- read.csv(url3)
head(df3)
```

## set seed

```{r}
# Create a sample dataset
set.seed(123)  # For reproducibility

# 1. Scatter Plot of Original Data
ggplot(df3, aes(x, y)) +
  geom_point() +
  labs(title = "Original Dataset")

# 2. K-Means Clustering and Scatter Plots for k = 2 to 12
avg_distances <- numeric(11)  # To store average distances for each k

for (k in 2:12) {
  kmeans_result <- kmeans(df3, centers = k)
  
  # Calculate average distance to cluster centers
  avg_distances[k - 1] <- mean(kmeans_result$withinss / kmeans_result$size)
  
  # Scatter plot for this k value
  data_with_clusters <- cbind(df3, cluster = kmeans_result$cluster)
  print(  # Print each plot separately
    ggplot(data_with_clusters, aes(x, y, color = factor(cluster))) +
      geom_point() +
      labs(title = paste("k =", k))
  )
}

# 3. Plot of Average Distance vs. Number of Clusters (k)
elbow_data <- data.frame(k = 2:12, avg_distance = avg_distances)
ggplot(elbow_data, aes(x = k, y = avg_distance)) +
  geom_line() +
  geom_point(alpha = 0.25) +
  labs(title = "Elbow Method for Optimal k",
       x = "Number of Clusters (k)",
       y = "Average Distance to Centroid")
```

  - The elbow point for this dataset seems to be 5. So the the “right” number of clusters is 5.

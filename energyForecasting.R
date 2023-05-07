library("readxl")
library("dplyr")
library("neuralnet")
library("Metrics")

data <- read_excel("/home/elpatatone/Documents/rcw/data/uow_consumption.xlsx")

#renaming the columns as they have weird names by default
names(data)[2] <- '18:00'
names(data)[3] <- '19:00'
names(data)[4] <- '20:00'
head(data)

#getting the 20:00 column only as it is our focus
input_data <- subset(data, select = c('20:00'))
input_data

#normalising the data
normalize <- function(x) {
return((x - min(x)) / (max(x) - min(x)))
}

#normalizing data before creating the delayed input I/O matrices
normalized_input_data <- as.data.frame(lapply(input_data, normalize))
normalized_input_data

# Create a data frame with the time delayed input vectors
time_lagged_data <- bind_cols(
    lag(normalized_input_data, 3),  # Two periods ago
    lag(normalized_input_data, 2),   # One period ago
    lag(normalized_input_data, 1),    # Current period
    normalized_input_data            # Predicted value
)
#renaming dataframe with names() as it does not work with bind_cols()
names(time_lagged_data) <- c("previous2", "previous", "current", "predicted")
# Print the data frame, which includes NA values due to the time lagging
time_lagged_data
# Remove rows with missing values
time_lagged_data <- time_lagged_data[complete.cases(time_lagged_data),]
# Print the first few rows of the resulting time-lagged data
head(time_lagged_data)

#I/O matrix with previous7, previous1, predicted
time_lagged_data2 <- bind_cols(
    lag(normalized_input_data, 8),
    lag(normalized_input_data, 2),
    normalized_input_data
)
names(time_lagged_data2) <- c("previous7", "previous1", "predicted")
time_lagged_data2 <- time_lagged_data2[complete.cases(time_lagged_data2),]

#I/O matrix with previous7, previous4, previous3, previous2, previous1, current, predicted
time_lagged_data3 <- bind_cols(
    lag(normalized_input_data, 8),
    lag(normalized_input_data, 5),
    lag(normalized_input_data, 4),
    lag(normalized_input_data, 3),
    lag(normalized_input_data, 2),
    lag(normalized_input_data, 1),
    normalized_input_data
)
names(time_lagged_data3) <- c("previous7", "previous4", "previous3", "previous2", "previous1", "current", "predicted")
time_lagged_data3 <- time_lagged_data2[complete.cases(time_lagged_data3),]

#Beginning MLP-NN training using the I/O as the dataset.

#NN-1 with the first I/O matrix
train_data1 <- time_lagged_data[1:380,]
test_data1 <- time_lagged_data[381:467,]
train_data1
test_data1
# previous2, previous1, t, predicted
nn1 <- neuralnet(predicted ~ previous2 + previous + current, data=train_data1, hidden=5, linear.output=TRUE)
plot(nn1)
predicted <- predict(nn1, newdata = test_data1)
actual <- test_data1$predicted
rmse <- rmse(predicted, actual)

#NN-2 with the first I/O matrix
train_data2 <- time_lagged_data2[1:380,]
test_data2 <- time_lagged_data2[381:467,]
# previous2, previous1, t, predicted
nn1 <- neuralnet(predicted ~ previous2 + previous + current, data=train_data1, hidden=5, linear.output=TRUE)
plot(nn1)

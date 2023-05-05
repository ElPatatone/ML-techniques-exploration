library("readxl")
library("dplyr")
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
names(time_lagged_data) <- c("t-2", "t-1", "t", "t+1")
# Print the data frame, which includes NA values due to the time lagging
time_lagged_data

# Remove rows with missing values
time_lagged_data <- time_lagged_data[complete.cases(time_lagged_data),]

# Print the first few rows of the resulting time-lagged data
head(time_lagged_data)
dim(time_lagged_data)

# Create a data frame with the time delayed input vectors
time_lagged_data2 <- bind_cols(
    lag(normalized_input_data, 8),  # Two periods ago
    lag(normalized_input_data, 2),   # One period ago
    normalized_input_data            # Predicted value
)

#renaming dataframe with names() as it does not work with bind_cols()
names(time_lagged_data2) <- c("t-7", "t-1", "t+1")
# Print the data frame, which includes NA values due to the time lagging
time_lagged_data2 <- time_lagged_data2[complete.cases(time_lagged_data2),]
head(time_lagged_data2)

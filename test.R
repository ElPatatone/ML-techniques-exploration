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
time_lagged_data_2 <- bind_cols(
    lag(normalized_input_data, 8),
    lag(normalized_input_data, 2),
    normalized_input_data
)
names(time_lagged_data_2) <- c("previous7", "previous1", "predicted")
time_lagged_data_2 <- time_lagged_data_2[complete.cases(time_lagged_data_2),]

#I/O matrix with previous7, previous4, previous3, previous2, previous1, current, predicted
time_lagged_data_3 <- bind_cols(
    lag(normalized_input_data, 7),
    lag(normalized_input_data, 4),
    lag(normalized_input_data, 3),
    lag(normalized_input_data, 2),
    lag(normalized_input_data, 1),
    normalized_input_data
)
time_lagged_data_3
names(time_lagged_data_3) <- c("previous7", "previous4", "previous3", "previous2", "previous1", "current", "predicted")
time_lagged_data_3 <- time_lagged_data_3[complete.cases(time_lagged_data_3),]

#I/O matrix with previous7, previous3, current, predicted
time_lagged_data_4 <- bind_cols(
    lag(normalized_input_data, 8),
    lag(normalized_input_data, 4),
    lag(normalized_input_data, 1),
    normalized_input_data
)
names(time_lagged_data_4) <- c("previous7", "previous3", "current", "predicted")
time_lagged_data_4 <- time_lagged_data_4[complete.cases(time_lagged_data_4),]
time_lagged_data_4

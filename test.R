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

#NN-1 with the first I/O matrix
#creating the training and testing batches
train_data_1 <- time_lagged_data[1:380,]
test_data_1 <- time_lagged_data[381:467,]

nn1 <- neuralnet(predicted ~ previous2 + previous + current,
                data=train_data_1,
                hidden=5,
                linear.output=TRUE)
plot(nn1)
predicted <- predict(nn1, newdata = test_data_1)
actual <- test_data_1$predicted

#using the RMSE, MAE, MAPE and SMAPE to evaluate the models
rmse(actual, predicted)
mae(actual, predicted)
mape(actual, predicted)
smape(actual, predicted)

#NN-2 with the first I/O matrix
train_data_2 <- time_lagged_data_2[1:380,]
test_data_2 <- time_lagged_data_2[381:462,]

nn2 <- neuralnet(predicted ~ previous7 + previous1,
                data=train_data_2,
                hidden=10,
                linear.output=TRUE)
# plot(nn3)
predicted <- predict(nn2, newdata = test_data_2)
actual <- test_data_2$predicted
#using the RMSE, MAE, MAPE and SMAPE to evaluate the models
rmse(actual, predicted)
mae(actual, predicted)
mape(actual, predicted)
smape(actual, predicted)

#NN-3 with the first I/O matrix
train_data_3 <- time_lagged_data_3[1:380,]
test_data_3 <- time_lagged_data_3[381:462,]

nn3 <- neuralnet(predicted ~ previous7 + previous4 + previous3 + previous2 + previous1 + current, 
                data=train_data_3,
                hidden=10,
                act.fct = "tanh",
                linear.output=FALSE)
# plot(nn3)
predicted <- predict(nn3, newdata = test_data_3)
actual <- test_data_3$predicted
#using the RMSE, MAE, MAPE and SMAPE to evaluate the models
rmse(actual, predicted)
mae(actual, predicted)
mape(actual, predicted)
smape(actual, predicted)

#NN-4 with the first I/O matrix
train_data_4 <- time_lagged_data_3[1:380,]
test_data_4 <- time_lagged_data_3[381:462,]

nn4 <- neuralnet(predicted ~ previous7 + previous4 + previous3 + previous2 + previous1 + current, 
                data=train_data_4,
                hidden=c(8,5),
                linear.output=TRUE)
plot(nn4)
predicted <- predict(nn4, newdata = test_data_4)
actual <- test_data_4$predicted
#using the RMSE, MAE, MAPE and SMAPE to evaluate the models
rmse(actual, predicted)
mae(actual, predicted)
mape(actual, predicted)
smape(actual, predicted)

#NN-5 with the first I/O matrix
train_data_5 <- time_lagged_data_3[1:380,]
test_data_5 <- time_lagged_data_3[381:462,]

nn5 <- neuralnet(predicted ~ previous7 + previous1,
                data=train_data_5,
                hidden=c(8,5),
                linear.output=FALSE)
plot(nn5)
predicted <- predict(nn5, newdata = test_data_5)
actual <- test_data_5$predicted
#using the RMSE, MAE, MAPE and SMAPE to evaluate the models
rmse(actual, predicted)
mae(actual, predicted)
mape(actual, predicted)
smape(actual, predicted)

#NN-6 with the first I/O matrix
train_data_6 <- time_lagged_data_4[1:380,]
test_data_6 <- time_lagged_data_4[381:462,]

nn6 <- neuralnet(predicted ~ previous7 + previous3 + current,
                data=train_data_6,
                hidden=c(8,5),
                linear.output=FALSE)
plot(nn6)
predicted <- predict(nn6, newdata = test_data_6)
actual <- test_data_6$predicted
#using the RMSE, MAE, MAPE and SMAPE to evaluate the models
rmse(actual, predicted)
mae(actual, predicted)
mape(actual, predicted)
smape(actual, predicted)

#creating the NN with the different I/O matrix as input and different model parameters
# nn_1 <- train_neuralnet(time_lagged_data_1, 8)
# nn_2 <- train_neuralnet(time_lagged_data_2, c(6,4))
# nn_3 <- train_neuralnet(time_lagged_data_4, c(8,4))
# nn_1["RMSE"]

#creating a new dataframe that will hold the output values for each of the metrics
# nn_output <- data.frame(
#     NN_Model = c("nn_1", "nn_2", "nn_3"),
#     RMSE = c(nn_1["RMSE"], nn_2["RMSE"], nn_3["RMSE"])
# )
# names(nn_output)

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

#function to create the time delayed input variables I/O matrices
create_time_lagged_data <- function(data, lag_values) {

    #this lets me specify the time lag and it will bind the time delayed column to the orginal dataset which acts as the column for the predicted column, as it has no lag
    time_lagged_data <- cbind(lapply(lag_values, function(x) lag(data, x)), data)
    names(time_lagged_data) <- paste(c("previous2", "previous", "current", "predicted"))
    # Remove rows with missing values
    time_lagged_data <- time_lagged_data[complete.cases(time_lagged_data),]
    return(time_lagged_data)
}

# Create different versions of the time-lagged data
time_lagged_data_1 <- create_time_lagged_data(normalized_input_data, c(3,2,1))
#renaming the columns to make it the data easier to work with
# names(time_lagged_data_1) <- c("previous2", "previous", "current", "predicted")
names(time_lagged_data_1)
time_lagged_data_2 <- create_time_lagged_data(normalized_input_data, c(7, 2))
time_lagged_data_3 <- create_time_lagged_data(normalized_input_data, c(7, 4:1))
time_lagged_data_4 <- create_time_lagged_data(normalized_input_data, c(1, 3, 7))

# Define a function to make training the nn models easier
train_neuralnet <- function(data, layers) {

    #creating the training and testing batches for the nn, keeping the training sample to 380.
    train_data <- data[1:380,]
    test_data <- data[381:nrow(data),]

    nn <- neuralnet(predicted ~ ., data = train_data, hidden = layers)
    predicted <- predict(nn, newdata = test_data)
    actual <- test_data$predicted
    #using the RMSE, MAE, MAPE and SMAPE to evaluate the models, will be outputted nicely for easy comparisons
    metrics <- c("RMSE" = rmse(actual, predicted),
               "MAE" = mae(actual, predicted),
               "MAPE" = mape(actual, predicted),
               "SMAPE" = smape(actual, predicted))
    #plotting for visual representation
    plot(nn)
    return(metrics)
}

nn_1 <- train_neuralnet(time_lagged_data_1, 8)
nn_1




#Beginning MLP-NN training using the I/O as the dataset.

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


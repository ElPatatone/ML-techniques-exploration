library("readxl")
library("dplyr")
library("neuralnet")
library("Metrics")

data <- read_excel("~/Documents/rcw/data/uow_consumption.xlsx")

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
create_time_lagged_data <- function(data, lag_values, name) {

    #this lets me specify the time lag and it will bind the time delayed column to the orginal dataset which acts as the column for the predicted column, as it has no lag
    time_lagged_data <- cbind(lapply(lag_values, function(x) lag(data, x)), data)
    # Remove rows with missing values
    time_lagged_data <- time_lagged_data[complete.cases(time_lagged_data),]
    #Raname the data with the relevant names
    names(time_lagged_data) <- name
    return(time_lagged_data)
}

# Create different versions of the time-lagged data
time_lagged_data_1 <- create_time_lagged_data(normalized_input_data, c(3,2,1), c("previous2", "previous", "current", "predicted"))
time_lagged_data_2 <- create_time_lagged_data(normalized_input_data, c(8, 2), c("previous7", "previous1", "predicted"))
time_lagged_data_3 <- create_time_lagged_data(normalized_input_data, c(8, 5:1), c("previous7", "previous4", "previous3", "previous2", "previous1", "current", "predicted"))
time_lagged_data_4 <- create_time_lagged_data(normalized_input_data, c(8, 3, 1), c("previous7", "previous2", "current", "predicted"))

# Define a function to make training the nn models easier
train_neuralnet <- function(data, layers, linear_output) {

    #creating the training and testing batches for the nn, keeping the training sample to 380.
    train_data <- data[1:380,]
    test_data <- data[381:nrow(data),]

    #making the nn using the neuralnet function.
    nn <- neuralnet(predicted ~ ., data = train_data, hidden = layers, linear.output = linear_output)
    predicted <- predict(nn, newdata = test_data)
    actual <- test_data$predicted
    #using the RMSE, MAE, MAPE and SMAPE to evaluate the models, will be nn_outputted nicely for easy comparisons
    metrics <- c("RMSE" = rmse(actual, predicted),
               "MAE" = mae(actual, predicted),
               "MAPE" = mape(actual, predicted),
               "SMAPE" = smape(actual, predicted))
    #plotting for visual representation
    # plot(nn)
    return(metrics)
}

#Here I am making a list that holds all the parameters I will be using to train each of the 15 models
#So for example any values at index 1 of the different keys of the nn_parameters list will be used to train nn_1, index 2 for nn_2 and so on.
nn_parameters <- list()
#specifying the data to be used to train the model
nn_parameters$data <- list(time_lagged_data_1,
                       time_lagged_data_2,
                       time_lagged_data_3)
#specifying the number of layers and nodes per layer
nn_parameters$layers <- list(c(8), c(6,4), c(8,4))
#specifying if the output should be linear or not
nn_parameters$linear_output <- list(TRUE, FALSE, TRUE)
nn_parameters

#this is the training loop, it makes use of the nn_parameters list made above, makes it easy to train a variety of models and experimenting with parameters
#As each loop trains a new model, I made an empty list which I use to store the model outputs after each loop.
#It also makes it easier to find the best model as I can run max and min on the actual list for each of the metrics used to evaluate the models.
nn_output <- list()
for (i in 1:length(nn_parameters$data)) {
    nn_output[[i]] <- train_neuralnet(nn_parameters$data[[i]], nn_parameters$layers[[i]], nn_parameters$linear_output[[i]])
    #renaming the keys in the list to make it easier to compare results
    names(nn_output)[i] <- paste0("nn_", i)
}

nn_output

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
head(normalized_input_data)
#function to create the time delayed input variables I/O matrices
create_time_lagged_data <- function(data, lag_values, name) {
    #this lets me specify the time lag and it will bind the time delayed column 
    #to the orginal dataset orginal 
    #which acts as the column for the predicted column, as it has no lag
    time_lagged_data <- cbind(lapply(lag_values, function(x) lag(data, x)), data)
    # Remove rows with missing values
    time_lagged_data <- time_lagged_data[complete.cases(time_lagged_data),]
    #Raname the data with the relevant names
    names(time_lagged_data) <- name
    return(time_lagged_data)
}

# Create different versions of the time-lagged data
time_lagged_data_1 <- create_time_lagged_data(normalized_input_data, 
                                            c(3,2,1), 
                                            c("previous2", "previous1", "current", "predicted"))
time_lagged_data_2 <- create_time_lagged_data(normalized_input_data, 
                                            c(8, 2), 
                                            c("previous7", "previous1", "predicted"))
time_lagged_data_3 <- create_time_lagged_data(normalized_input_data, 
                                            c(8, 5:1), 
                                            c("previous7", "previous4", "previous3", "previous2", 
                                                "previous1", "current", "predicted"))
time_lagged_data_4 <- create_time_lagged_data(normalized_input_data, 
                                            c(8, 3, 1), 
                                            c("previous7", "previous2", "current", "predicted"))
time_lagged_data_5 <- create_time_lagged_data(normalized_input_data, 
                                            c(2, 1), 
                                            c("previous1", "current", "predicted"))

# Define a function to make training the nn models easier
train_neuralnet <- function(data, layers, linear_output) {

    #creating the training and testing batches for the nn, keeping the training sample to 380.
    train_data <- data[1:380,]
    test_data <- data[381:nrow(data),]

    #making the nn using the neuralnet function.
    nn <- neuralnet(predicted ~ ., 
                    data = train_data, 
                    hidden = layers, 
                    linear.output = linear_output)

    # calculating the total number of weights in the neural network
    #I was not able to get the weights to show properly as it was giving an 
    #output of 2 and 3 for all models 
    total_weights <- sum(sapply(nn$weights, length))

    predicted <- predict(nn, newdata = test_data)
    actual <- test_data$predicted
    #using the RMSE, MAE, MAPE and SMAPE to evaluate the models, 
    #It is also saved into a list which makes it easy to view all results 
    metrics <- c("RMSE" = rmse(actual, predicted),
               "MAE" = mae(actual, predicted),
               "MAPE" = mape(actual, predicted),
               "SMAPE" = smape(actual, predicted),
                "Total Weights" = total_weights)
    #plotting for visual representation
    # plot(nn)
    return(metrics)
}

#I am making a list that holds all the parameters I will be using to train each of the 15 models
#So for example any values at index 1 of the different keys of the nn_parameters list 
#will be used to train nn_1, index 2 for nn_2 and so on.
nn_parameters <- list()
#specifying the data to be used to train the model
nn_parameters$data <- list(time_lagged_data_1,#nn1
                       time_lagged_data_2,    #nn2
                       time_lagged_data_4,    #nn3
                       time_lagged_data_3,    #nn4
                       time_lagged_data_5,    #nn5
                       time_lagged_data_4,    #nn6
                       time_lagged_data_2,    #nn7
                       time_lagged_data_1,    #nn8
                       time_lagged_data_3,    #nn9
                       time_lagged_data_5,    #nn10
                       time_lagged_data_4,    #nn11
                       time_lagged_data_3)    #nn12

#specifying the number of layers and nodes per layer
nn_parameters$layers <- list(c(8),    #nn1
                            c(6,4),   #nn2
                            c(5),     #nn3
                            c(12,6),  #nn4
                            c(4),     #nn5
                            c(6),     #nn6
                            c(4),     #nn7
                            c(7,3),   #nn8
                            c(10),    #nn9
                            c(4,2),   #nn10
                            c(5,3),   #nn11
                            c(6,2))   #nn12

#specifying if the output should be linear or not
nn_parameters$linear_output <- list(TRUE,  #nn1
                                    FALSE, #nn2
                                    TRUE,  #nn3
                                    FALSE, #nn4
                                    TRUE,  #nn5
                                    TRUE,  #nn6
                                    FALSE, #nn7
                                    FALSE, #nn8
                                    TRUE,  #nn9
                                    FALSE, #nn10
                                    FALSE, #nn11
                                    TRUE)  #nn12


nn_parameters

#this is the training loop, it makes use of the nn_parameters list made above,
#it makes it easy to train a variety of models and experimenting with parameters
#I made an empty list which I use to store the model outputs after each loop.
nn_output <- list()
for (i in 1:length(nn_parameters$data)) {
    #using my custom function to make the models and save the results for the evaluation metrics
    nn_output[[i]] <- train_neuralnet(nn_parameters$data[[i]], 
                                      nn_parameters$layers[[i]], 
                                      nn_parameters$linear_output[[i]])
    #renaming the keys in the list to make it easier to extract results
    names(nn_output)[i] <- paste0("nn_", i)
}

nn_output

#using the NARX approach to include the 18th and 19th hour attributes
narx_input_data <- subset(data, select = c("18:00", "19:00", "20:00"))
head(narx_input_data)

# narx_create_time_lagged_data <- function(target_data, data, lag_values) {
#     time_lagged_data <- cbind(lapply(lag_values, function(x) lag(data, x)), target_data)
#     # Remove rows with missing values
#     # time_lagged_data <- time_lagged_data[complete.cases(time_lagged_data),]
#     #Raname the data with the relevant names
#     # names(time_lagged_data) <- name
#     return(time_lagged_data)
# }

#Above I tried to make a similar function to make the time lagged data, 
#as 18th and 19th hour need to be accounted for I struggled to find a solution.
#So instead I just manually made the time lagged data for the narx approach as the actual result 
#I was at least able to use the nn training function and loop with some minor tweaking.

# normalizing data before creating the delayed input I/O matrices
narx_normalized_input_data <- as.data.frame(lapply(narx_input_data, normalize))
head(narx_normalized_input_data)
#I had to rename the columns as after normalizing the data they changed for some reason
names(narx_normalized_input_data) <- c("18:00", "19:00", "20:00")

#begging to manually make the I/O matrices
narx_time_lagged_data_1 <- cbind(previous2_18= lag(narx_normalized_input_data$"18:00", 3),
                          previous1_19 = lag(narx_normalized_input_data$"19:00", 2),
                          current_19 = lag(narx_normalized_input_data$"19:00", 1),
                          predicted = narx_normalized_input_data$"20:00")

narx_time_lagged_data_1 <- narx_time_lagged_data_1[complete.cases(narx_time_lagged_data_1),]

narx_time_lagged_data_2 <- cbind(previous7_20= lag(narx_normalized_input_data$"20:00", 8),
                          previous7_19 = lag(narx_normalized_input_data$"19:00", 8),
                          previous4_18 = lag(narx_normalized_input_data$"18:00", 5),
                          previous2_20 = lag(narx_normalized_input_data$"20:00", 3),
                          current_19 = lag(narx_normalized_input_data$"19:00", 1),
                          predicted = narx_normalized_input_data$"20:00")

narx_time_lagged_data_2 <- narx_time_lagged_data_2[complete.cases(narx_time_lagged_data_2),]

narx_time_lagged_data_3 <- cbind(previous4_18= lag(narx_normalized_input_data$"18:00", 5),
                          current_19 = lag(narx_normalized_input_data$"19:00", 1),
                          predicted = narx_normalized_input_data$"20:00")

narx_time_lagged_data_3 <- narx_time_lagged_data_3[complete.cases(narx_time_lagged_data_3),]

narx_time_lagged_data_4 <- cbind(previous7_18 = lag(narx_normalized_input_data$"18:00", 8),
                          previous4_20 = lag(narx_normalized_input_data$"20:00", 5),
                          current_19 = lag(narx_normalized_input_data$"19:00", 1),
                          current_20 = lag(narx_normalized_input_data$"20:00", 1),
                          predicted = narx_normalized_input_data$"20:00")

narx_time_lagged_data_4 <- narx_time_lagged_data_4[complete.cases(narx_time_lagged_data_4),]


#for the NARX approach I am only making 5 models to see if there is any difference in outputs
#So for example any values at index 1 of the different keys of the nn_parameters list will b
#e used to train nn_1, index 2 for nn_2 and so on.
narx_nn_parameters <- list()
#specifying the data to be used to train the model
narx_nn_parameters$data <- list(narx_time_lagged_data_1,#nn1
                       narx_time_lagged_data_2,    #nn2
                       narx_time_lagged_data_4,    #nn3
                       narx_time_lagged_data_3,    #nn4
                       narx_time_lagged_data_2)    #nn5

#specifying the number of layers and nodes per layer
narx_nn_parameters$layers <- list(c(8), #nn1
                            c(10,4),    #nn2
                            c(8,3),     #nn3
                            c(6),       #nn4
                            c(12))      #nn5

#specifying if the output should be linear or not
narx_nn_parameters$linear_output <- list(TRUE,  #nn1
                                    FALSE, #nn2
                                    TRUE,  #nn3
                                    FALSE, #nn4
                                    TRUE)  #nn5

#I was getting an error saying "$ is invalid for atmoic vectors" when running this function.
#After quite a while I found out that the test data was being turned into an atomic vector 
#and needed to be accessed using the [] instead of $. 
#R error messages aren't the greatest which made finding the issue even more challening.
train_neuralnet <- function(data, layers, linear_output) {

    #creating the training and testing batches for the nn, keeping the training sample to 380.
    train_data <- data[1:380,]
    test_data <- data[381:nrow(data),]

    #making the nn using the neuralnet function.
    nn <- neuralnet(predicted ~ ., 
                    data = train_data, 
                    hidden = layers, 
                    linear.output = linear_output)

    #code for the total weights does not seem to be working
    total_weights <- sum(sapply(nn$weights, length))
    predicted <- predict(nn, newdata = test_data)
    #this is where the error was happening.
    actual <- test_data[1]
    #using the RMSE, MAE, MAPE and SMAPE to evaluate the models, 
    #It is also saved into a list which makes it easy to view all results 
    metrics <- c("RMSE" = rmse(actual, predicted),
               "MAE" = mae(actual, predicted),
               "MAPE" = mape(actual, predicted),
               "SMAPE" = smape(actual, predicted),
                "Total Weights" = total_weights)
    #plotting for visual representation
    # plot(nn)
    return(metrics)
}

narx_nn_output <- list()
for (i in 1:length(narx_nn_parameters$data)) {
    narx_nn_output[[i]] <- train_neuralnet(narx_nn_parameters$data[[i]], 
                                           narx_nn_parameters$layers[[i]], 
                                           narx_nn_parameters$linear_output[[i]])
    #renaming the keys in the list to make it easier to compare results
    names(narx_nn_output)[i] <- paste0("nn_", i)
}

narx_nn_output


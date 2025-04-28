#' @title Standardise features
#' 
#' @description Function standardising features based on a set of reference measurements (i.e. the training data). 
#' 
#' @param x A numeric vector.
#' @param ref A logical vector indicating which values should be used as a reference.
#' 
#' @return The standardised feature.
standardise_features_reference <- function(x, ref) {
    x_ref <- x[ref]
    
    x_ref_mean <- mean(x_ref, na.rm = TRUE)
    x_ref_sd <- sd(x_ref, na.rm = TRUE)
    
    if (x_ref_sd == 0) {
        x_ref_sd <- 1
    }
    
    return((x - x_ref_mean) / x_ref_sd)
}

#' @title Train deep feed forward neural network
#' 
#' @description Function creating and training a deep feed forward neural network. 
#' 
#' @param X_train A matrix containing the training features.
#' @param y_train A vector containing the training target.
#' @param X_val A matrix containing the validation features.
#' @param y_val A vector containing the validation target.
#' @param units A vector defining the number of neurons in each hidden layer. 
#' @param activations A vector defining the activation functions applied in each hidden layer.
#' @param loss A vector of loss functions for each round of optimisation.
#' @param optimisers A list of optimisers for each round of optimisation.
#' @param epochs A vector of epochs for each round of optimisation.
#' @param batch_size The batch-size used when training the neural network (identical for each optimiser).
#' @param metrics A vector of additional metrics passed to each of the optimisers.
#' @param verbose A number indicating the verbosity exhibited during training.
#' 
#' @return A trained neural network.
train_nn_model <- function(
        X_train, y_train, X_val = NULL, y_val = NULL,
        units = c(60, 30, 15),
        activations = c("relu", "relu", "relu"),
        loss = c("mse", "mse"), 
        optimisers = list(optimizer_sgd(learning_rate = 0.001, weight_decay = 1e-6, momentum = 0.95, clipnorm = 1.0, clipvalue = 1.0), optimizer_adam()),
        epochs = c(500, 6000), 
        batch_size = 10000,
        metrics = c("mean_absolute_error", "mean_absolute_error"),
        verbose = 0
) {
    batch_size <- min(batch_size, dim(X_train)[1])
    X_dim <- dim(X_train)[-1]
    
    ##
    model <- keras_model_sequential() |> 
        layer_dense(input_shape = X_dim, units = units[1], activation = activations[1])
    
    for (i in seq_along(units)[-1]) {
        model <- model |> 
            layer_dense(units = units[i], activation = activations[i])
    }  
    
    model <- model |> 
        layer_dense(units = 1, activation = "linear")
    
    #
    for (i in seq_along(loss)) {
        compile(
            model,
            loss = loss[i],
            optimizer = optimisers[[i]],
            metrics = list(metrics[i])
        )
        
        if (is.null(X_val) || is.null(y_val)) {
            fit(
                model,
                x = X_train, 
                y = y_train, 
                batch_size = batch_size, 
                epochs = epochs[i],
                verbose = verbose
            )
        }
        else {
            fit(
                model, 
                x = X_train, y = y_train, 
                validation_data = list(X_val, y_val), 
                batch_size = batch_size, 
                epochs = epochs[i],
                verbose = verbose
            )
        }
    }
    
    return(model)
}

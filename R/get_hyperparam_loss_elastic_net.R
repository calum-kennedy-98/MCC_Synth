# Name of script: get_hyperparam_loss_elastic_net
# Description: Function which iterates over control units as 'pseudo-treated' units
# to determine the (optimal) loss associated with given hyperparameter settings
# for the elastic net loss
# and an intercept term 
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 13-03-2025
# Latest update by: Calum Kennedy
# Latest update on: 13-03-2025

# Comments ---------------------------------------------------------------------

# Function to find optimal synthetic control params iterating over all control units (used to 
# determine optimal elastic net parameters in Doudchenko-Imbens estimator)
get_hyperparam_loss_elastic_net <- function(par,
                                           Y_controls_pre,
                                           Y_controls_post){
  
  # Extract values of hyperparameters from par vector
  lambda <- par["lambda"]
  alpha <- par["alpha"]
  
  # Generate list of integers to iteratively define treated unit
  n_controls <- seq(1:ncol(Y_controls_pre))
  
  # Find optimal synthetic control weights for each pseudo-treated unit in pre-treatment
  # period, generate predictions in post-treatment period, and return average squared
  # prediction error across all pseudo-treated and post-treatment periods
  loss_pseudo_controls <- map_dbl(n_controls, function(x){
    
    # Generate pseudo treatment group for pre/post treatment
    pseudo_Y_treated_pre <- Y_controls_pre[,x]
    pseudo_Y_treated_post <- Y_controls_post[,x]
    
    # Generate pseudo-control group for pre/post treatment
    pseudo_Y_controls_pre <- Y_controls_pre[,-x]
    pseudo_Y_controls_post <- Y_controls_post[,-x]
    
    # Get predictions on pseudo-post-treatment outcomes
    pseudo_Y_treated_post_hat <- predict_Y_post_elastic_net(Y_treated_pre = pseudo_Y_treated_pre,
                                                   Y_treated_post = pseudo_Y_treated_post,
                                                   Y_controls_pre = pseudo_Y_controls_pre,
                                                   Y_controls_post = pseudo_Y_controls_post,
                                                   alpha = alpha,
                                                   lambda = lambda)
    
    # Generate mean squared difference from true pseudo post-treatment outcomes
    mean_squared_pred_error <- mean((pseudo_Y_treated_post - pseudo_Y_treated_post_hat)^2)
    
    # Return mean squared prediction error for each pseudo-treated unit in list
    return(mean_squared_pred_error)
    
  })
  
  # Take mean of mean_squared_error over all pseudo-treated units to get overall error estimate
  loss_hyperparams <- mean(loss_pseudo_controls)
  
  return(loss_hyperparams)
  
}
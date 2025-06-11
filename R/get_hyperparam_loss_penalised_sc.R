# Name of script: get_hyperparam_loss_penalised_sc
# Description: Function to return leave-one-out CV loss for a given value
# of the L1 penalty parameter lambda. Computes optimal penalised SC for 
# each control unit (pseudo-treated unit), and returns the overall MSPE across all the
# conrol units
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 02-06-2025
# Latest update by: Calum Kennedy
# Latest update on: 02-06-2025

# Comments ---------------------------------------------------------------------

# At present, not using weight vector V - in future could look to incorporate

# @ param ...

# Function ---------------------------------------------------------------------

get_hyperparam_loss_penalised_sc <- function(par,
                                             Y_controls_pre,
                                             Y_controls_post){
  
  # Extract starting value of hyperparameter
  lambda <- par
  
  # Generate list of integers to iteratively define treated unit
  n_controls <- seq(1:ncol(Y_controls_pre))
  
  loss_pseudo_controls <- lapply(n_controls, function(x){
    
    # Generate pseudo treatment group for pre/post treatment
    pseudo_Y_treated_pre <- Y_controls_pre[,x]
    pseudo_Y_treated_post <- Y_controls_post[,x]
    
    # Generate pseudo-control group for pre/post treatment
    pseudo_Y_controls_pre <- Y_controls_pre[,-x]
    pseudo_Y_controls_post <- Y_controls_post[,-x]
    
    # Find optimal penalised SC weights
    W_opt <- get_penalised_sc_weights(pseudo_Y_treated_pre,
                                      pseudo_Y_controls_pre,
                                      lambda)
    
    # Get post-treatment predictions
    pseudo_Y_treated_post_hat <- pseudo_Y_controls_post %*% W_opt
    
    # Generate mean squared difference from true pseudo post-treatment outcomes
    mean_squared_pred_error <- mean((pseudo_Y_treated_post - pseudo_Y_treated_post_hat)^2)
    
    # Return mean squared prediction error for each pseudo-treated unit in list
    return(mean_squared_pred_error)
    
  })
  
  # Take mean of mean_squared_error over all pseudo-treated units to get overall error estimate
  loss_lambda <- mean(unlist(loss_pseudo_controls))
  
  return(loss_lambda)
  
}
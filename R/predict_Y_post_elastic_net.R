# Name of script: predict_Y_post_elastic_net
# Description: Function to generate optimal synthetic control parameters (in style
# of Doudchenko and Imbens, given an arbitrary loss function)
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 13-03-2025
# Latest update by: Calum Kennedy
# Latest update on: 13-03-2025

# Comments ---------------------------------------------------------------------

# Function to generate optimal synthetic control parameters using Doudchenko/Imbens
# method and then get predicted Y1_post_hat values
predict_Y_post_elastic_net <- function(Y1_pre,
                                     Y1_post,
                                     Y0_pre,
                                     Y0_post,
                                     alpha,
                                     lambda){
  
  # Run optimisation using glm net with lambda fixed (i.e. no gradient descent
  # and no iterations over lambda. The reason for this is that in our application
  # we want to choose a lambda which performs best across all pseudo-controls, 
  # so is a higher-level parameter)
  fit <- glmnet(Y0_pre, 
                Y1_pre, 
                alpha = alpha, 
                lambda = lambda, 
                nlambda = 1)
  
  # Get predictions on pseudo-post-treatment outcomes
  Y1_post_hat <- predict(fit, Y0_post, type = "response")
  
  return(Y1_post_hat)
  
}
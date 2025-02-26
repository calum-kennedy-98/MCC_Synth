### Factor model simulation
### CK
### 14-02-2025

# Load necessary library
library(data.table)
library(dplyr)
library(ggplot2)
library(Synth)

# Set seed for reproducibility
set.seed(123)

# Load Env
source("R/LoadEnv.R") # Remove this when make pipeline

# Gaussian function generator
generate_fat_tail <- function(start_index, end_index, t_var, amplitude = 10, gamma = 5) {
  indices <- seq(start_index, end_index, length.out = end_index - start_index + 1)
  midpoint <- mean(indices)
  cauchy_like <- amplitude / (1 + (({{t_var}} - midpoint)^2) / gamma^2)
  return(cauchy_like)
}

# Define parameters
N <- 30    # Number of units
T <- 100    # Number of time periods
K <- 3 # Number of factors
rho <- 0.8 # AR(1) correlation coefficient for lambda_t

# Set parameters for the Gaussian function
start_index <- 60
end_index <- 100
amplitude <- 30
gamma <- 10

# Generate AR(1) process for lambda_t
lambda_t <- matrix(0, nrow = T, ncol = K)  # Initialize matrix
lambda_t[1, ] <- rnorm(K, mean = 0, sd = 0.1) # Initial values

for (t in 2:T) {
  lambda_t[t, ] <- rho * lambda_t[t - 1, ] + rnorm(K, mean = 0, sd = 0.1)
}

# Generate time-varying components (K-dimensional)
theta_t <- matrix(rnorm(T * K, mean = 5, sd = 0.25), nrow = T, ncol = K)  # Factor loadings for Z_j

# Define correlation matrix for unit-specific characteristics (Z_j and mu_j)
corr_matrix <- matrix(c(1,  0.5, 0.9, 
                        0.5, 1,  0.4, 
                        0.9, 0.4, 1), nrow = K, ncol = K)  # Example correlation structure

# Ensure positive semi-definiteness
chol_decomp <- chol(corr_matrix)

# Generate correlated unit-specific characteristics
uncorrelated_Z <- matrix(rnorm(N * K, mean = 5, sd = 1), nrow = N, ncol = K)
uncorrelated_mu <- matrix(rnorm(N * K, mean = 5, sd = 1), nrow = N, ncol = K)

Z_j <- uncorrelated_Z %*% chol_decomp # Apply Cholesky decomposition for correlation
Z_j <- data.table(cbind(exp(0.5*Z_j), c(1:N))) # Add constant term
mu_j <- uncorrelated_mu %*% chol_decomp  # Ensure correlation in mu_j too

# Generate id and time sequences
id <- rep(1:N, each = T)
time <- rep(1:T, times = N)

# Generate error terms
epsilon_jt <- rnorm(N * T, mean = 0, sd = 1)

# Compute factor contributions
factor_contributions <- rowSums(theta_t[time, ] * Z_j[id, ] + lambda_t[time, ] * mu_j[id, ])

# Construct response variable
Y_jt <- factor_contributions + epsilon_jt

# Create a data frame
df <- data.table(
    id = id,
    time = time,
    Y = Y_jt
) %>%
    left_join(Z_j, by = c("id" = "V4")) %>%
    mutate(Y_event = ifelse(id == 1 & between(time, start_index, end_index),
                      Y + generate_fat_tail(start_index, end_index, time, amplitude, gamma),
                      Y))

df %>% ggplot() +
    geom_line(aes(x = time, y = Y_event, group = id), colour = "grey", alpha = 0.5) +
    geom_line(data = df %>% filter(id == 1), aes(x = time, y = Y_event)) +
    scatter_plot_opts

ggsave("Output/Figures/plot_outcome_all.png", width = 8, height = 5)

df %>%
  filter(id == 1) %>%
  ggplot() +
  geom_line(aes(x = time, y = Y, linetype = "Y(0)")) +
  geom_line(aes(x = time, y = Y_event, linetype = "Y(1)")) +
  geom_vline(xintercept = 60, linetype = "longdash") +
  ylim(0, 150) +
  theme_minimal() +
  scale_colour_manual(values = cbbPalette) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  ) +
  labs(x = "Time", y = "Outcome") +
  scale_linetype_manual(
    breaks = c("Y(1)", "Y(0)"),
    values = c("solid", "dotted")
    )

ggsave("Output/Figures/plot_outcome_unit1.png", width = 8, height = 5)

# Fit baseline synthetic control model to simulated data --------------------------------

# Generate data to pass to synth
data_prepared <- dataprep(
  foo = df,
  predictors = c("V1", "V2", "V3"),
  predictors.op = "mean",
  special.predictors = list(
    list("Y_event", 1:59, "mean")
  ),
  dependent = "Y_event",
  unit.variable = "id",
  time.variable = "time",
  treatment.identifier = 1,
  controls.identifier = 2:30,
  time.optimize.ssr = 1:59,
  time.predictors.prior = 1:59,
  time.plot = 1:100
)

# Run synth and store results
synth_out <- synth(data_prepared, method = "BFGS")

# Plot outcome path of synthetic vs. exposed unit
png("Output/Figures/path_plot_synth.png",
  width = 800,
  height = 500
)

path_plot <- path.plot(
  synth.res = synth_out,
  dataprep.res = data_prepared,
  Xlab = "Time",
  Ylab = "Outcome"
)

dev.off()
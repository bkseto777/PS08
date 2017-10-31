library(tidyverse)
library(caret)

# Package for easy timing in R
library(tictoc)



# Demo of timer function --------------------------------------------------
# Run the next 5 lines at once
tic()
Sys.sleep(3)
timer_info <- toc()
runtime <- timer_info$toc - timer_info$tic
runtime



# Get data ----------------------------------------------------------------
# Accelerometer Biometric Competition Kaggle competition data
# https://www.kaggle.com/c/accelerometer-biometric-competition/data
train <- read_csv("~/Downloads/train.csv")

# YOOGE!
dim(train)



# knn modeling ------------------------------------------------------------
model_formula <- as.formula(Device ~ X + Y + Z)

# Values to use:
n_values <- c(10, 20, 30)
k_values <- c(2, 3, 4)

runtime_dataframe <- expand.grid(n_values, k_values) %>%
  as_tibble() %>%
  rename(n=Var1, k=Var2) %>%
  mutate(runtime = n*k)
runtime_dataframe




# Time knn here -----------------------------------------------------------
# We want to see how time varies with k,n,p (p=number of predictor variables)
# But going to hold p constant at 3

knnTimer <- function(k,n){
  data <- sample_n(train, n)
  tic()
  model_knn <- caret::knn3(model_formula, data=data, k = k)
  timer_info <- toc()
  runtime <- timer_info$toc - timer_info$tic
  return(runtime)
}

# Create a data frame of k and ns
kns <- data_frame()
ks <- c(2,3,4, 10, 20,30,40,50,60,100)
ns <- c(10, 100,1000,10000, 100000, 1000000,2000000,3000000,4000000,5000000,6000000, 10000000)
for(i in ks){
  n <- data_frame(n=ns) %>% mutate(k = i)
  kns <- rbind(kns,n)
}

##### Now I want to find the fastest method to actually find this
system.time(runtimes_mapply <- mapply(knnTimer, k = kns$k, n = kns$n))
### This took 72.751 s
m_m_90 <- mean(runtimes_mapply)

runtimes_for <- c()
system.time(for(i in ks){for(j in ns){runtimes_for <- c(runtimes_for, knnTimer(i,j))}})
### This took 73.442 s
m_f_90 <- mean(runtimes_for)

##### Guess they about the same

runtimes_mapply <- mapply(knnTimer, k = kns$k, n = kns$n)
runtimes_dataframe <- cbind(kns, runtimes_mapply)

# Plot your results ---------------------------------------------------------
# Think of creative ways to improve this barebones plot. Note: you don't have to
# necessarily use geom_point
library(plotly)
runtime_plot <- plot_ly(runtimes_dataframe, x = ~n, y = ~k, z = ~runtimes_mapply,
             marker = list(color = ~runtimes_mapply, colorscale = c('#FFE1A1', '#683531'))) %>%
  add_markers() %>%
  layout(title = 'K-Nearest Neighbor Runtime Based on Sample Size and K',
         scene = list(xaxis = list(title = 'Sample Size'),
                      yaxis = list(title = 'K'),
                      zaxis = list(title = 'runtime'),
                      title = list(title = "K-Nearest Neighbor Runtime")))
runtime_plot



ggsave(filename="firstname_lastname.png", width=16, height = 9)




# Runtime complexity ------------------------------------------------------
# Can you write out the rough Big-O runtime algorithmic complexity as a function
# of:
# -n: number of points in training set
# -k: number of neighbors to consider
# -d: number of predictors used? In this case d is fixed at 3



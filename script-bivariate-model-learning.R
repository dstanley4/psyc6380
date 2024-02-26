library(tidyverse)
library(broom)

# Small N: Theoretical Relation - NO Error ------------------------------------------------------------------
n = 10
set.seed(1)
x = rnorm(n = n,
          mean = 150,
          sd = 10)

y_theory = 1.5*x +10

my_model_df <- data.frame(x, y_theory)

ggplot() +
  geom_point(data = my_model_df,
             mapping = aes(x = x, y = y_theory),
             color = "red") +
  geom_line(data = my_model_df,
             mapping = aes(x = x, y = y_theory),
             color = "red")


# Small N: Theoretical Relation - WITH Error ------------------------------------------------------------------

errors = rnorm(n = n,
               mean = 0,
               sd = 5)

y = 1.5*x + 10 + errors

my_model_df <- data.frame(x, y_theory, y)

ggplot() +
  geom_point(data = my_model_df,
             mapping = aes(x = x, y = y),
             color = "blue") +
  geom_point(data = my_model_df,
             mapping = aes(x = x, y = y_theory),
             color = "red") +
  geom_line(data = my_model_df,
            mapping = aes(x = x, y = y_theory),
            color = "red")
       
# Large N: Theoretical Relation - NO Error ------------------------------------------------------------------
n = 100000

set.seed(1)
x = rnorm(n = n,
          mean = 150,
          sd = 10)

y_theory = 1.5*x +10

my_model_df <- data.frame(x, y_theory)

ggplot() +
  geom_point(data = my_model_df,
             mapping = aes(x = x, y = y_theory),
             color = "red") +
  geom_line(data = my_model_df,
            mapping = aes(x = x, y = y_theory),
            color = "red")

# Large N: Theoretical Relation - WITH Error ------------------------------------------------------------------

errors = rnorm(n = n,
               mean = 0,
               sd = 5)

y = 1.5*x + 10 + errors

my_model_df <- data.frame(x, y_theory, y)

ggplot() +
  geom_point(data = my_model_df,
             mapping = aes(x = x, y = y),
             color = "blue") +
  geom_point(data = my_model_df,
             mapping = aes(x = x, y = y_theory),
             color = "red") +
  geom_line(data = my_model_df,
            mapping = aes(x = x, y = y_theory),
            color = "red")



# Large N: Recover Relation From Data  ------------------------------------------------------------------

linear_model <- lm(y~x)

# Two approaches to reviewing R output
# 1) Base R
# Examine: slope, intercept, residual standard error. 
summary_linear_model <- summary(linear_model)
print(summary_linear_model)

#2) Broom (same information two steps)
#2a) See slope and intercept
tidy(linear_model)
#2b) See Sigma (residual standard error by a different name)
glance(linear_model)

# What is the calculation for recovering Sigma?
# Let's see the predicted and residual values

lm_data_augmented <- augment(linear_model)
head(lm_data_augmented)

# Direct calculation from residuals: Residual standard error
sd(lm_data_augmented$.resid)

# Classical formula for residual standard error
lm_data_augmented %>%
  summarise( sqrt(sum((y-.fitted)^2)/(n-2)) )

# Less common formula for residual standard error
sd(y)*sqrt(1- cor(x,y)^2)

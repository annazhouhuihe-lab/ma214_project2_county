# ============================================================
# Project 2, Part 1: Real Data and Diagnostics
# MA 214 Applied Statistics
# ============================================================
# Working code for lab. This file is not submitted.
# Do not use setwd().
#
# Three steps, matching Parts 1-3 of the worksheet:
#   1. load your data   2. fit the line   3. diagnose

library(dplyr)
library(ggplot2)

# ------------------------------------------------------------
# Part 1: Load your data
# ------------------------------------------------------------
# Pick the ONE block for your dataset (numbers match the handout list),
# uncomment it, and delete the others. Every block makes a data frame
# my_data with two columns: y (the response) and x (the predictor).

# 1. bdims (openintro): wgt ~ hgt
# library(openintro)
# my_data <- bdims %>% transmute(y = wgt, x = hgt)

# 2. evals (openintro): score ~ bty_avg
# library(openintro)
# my_data <- evals %>% transmute(y = score, x = bty_avg)

# 3. Salaries and Batting (Lahman): salary ~ HR, 2016, at least 300 at-bats
# library(Lahman)
# # one row per player and team: the salary that team paid, the at-bats for it
# my_data <- inner_join(filter(Batting, yearID == 2016),
#                       filter(Salaries, yearID == 2016),
#                       by = c("playerID", "teamID")) %>%
#   filter(AB >= 300) %>% transmute(y = salary, x = HR)

# 4. cats (MASS): Hwt ~ Bwt
# library(MASS)
# my_data <- cats %>% transmute(y = Hwt, x = Bwt)

# 5. county (openintro): median_hh_income ~ per_capita_income
# library(openintro)
# my_data <- county %>% transmute(y = median_hh_income, x = per_capita_income)

# 6. SaratogaHouses (mosaicData): price ~ livingArea
# library(mosaicData)
# my_data <- SaratogaHouses %>% transmute(y = price, x = livingArea)

# 7. hills (MASS): time ~ dist
# library(MASS)
# my_data <- hills %>% transmute(y = time, x = dist)

# 8. gapminder (gapminder): lifeExp ~ log(gdpPercap), year 2007
# library(gapminder)
# my_data <- gapminder %>% filter(year == 2007) %>%
#   transmute(y = lifeExp, x = log(gdpPercap))

# 9. Boston (MASS): medv ~ rm
# library(MASS)
# my_data <- Boston %>% transmute(y = medv, x = rm)

# 10. mtcars (datasets): mpg ~ hp
# my_data <- mtcars %>% transmute(y = mpg, x = hp)

# 11. gapminder (gapminder): lifeExp ~ gdpPercap, year 2007
# library(gapminder)
# my_data <- gapminder %>% filter(year == 2007) %>%
#   transmute(y = lifeExp, x = gdpPercap)

# 12. ncbirths (openintro): weight ~ weeks
# library(openintro)
# my_data <- ncbirths %>% transmute(y = weight, x = weeks)

# 13. Utilities (mosaicData): gasbill ~ temp
# library(mosaicData)
# my_data <- Utilities %>% transmute(y = gasbill, x = temp)

# PLACEHOLDER so the commands below run before you pick a dataset.
# TODO: delete this line once you have uncommented your block above.
my_data <- mtcars %>% transmute(y = mpg, x = hp)

# Read the documentation for your dataset, e.g. ?bdims or ?cats.
# TODO: one row of this data is one ______________________

# Remove rows with a missing y or x, and record how many you removed.
n_before <- nrow(my_data)
my_data  <- my_data %>% filter(!is.na(y), !is.na(x))
n_before - nrow(my_data)   # rows removed
nrow(my_data)              # n used


# ------------------------------------------------------------
# Part 2: Fit the line
# ------------------------------------------------------------
# Label the axes with your variables and their units.
ggplot(my_data, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "x (units)", y = "y (units)", title = "Scatterplot of y against x")

fit <- lm(y ~ x, data = my_data)
summary(fit)

# Classical 95% CI for the slope: b1 +/- t(0.975, n - 2) * SE(b1)
b1    <- coef(summary(fit))["x", "Estimate"]
se_b1 <- coef(summary(fit))["x", "Std. Error"]
t_crit <- qt(0.975, df = nrow(my_data) - 2)
c(lower = b1 - t_crit * se_b1, upper = b1 + t_crit * se_b1)

# TODO: interpret the slope in context, with units.


# ------------------------------------------------------------
# Part 3: Diagnose the failed assumption
# ------------------------------------------------------------
res <- resid(fit)
diag_data <- my_data %>% mutate(fitted = fitted(fit), resid = res)

ggplot(diag_data, aes(x = fitted, y = resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fitted values", y = "Residuals", title = "Residuals vs. fitted")

ggplot(diag_data, aes(x = x, y = resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "x", y = "Residuals", title = "Residuals vs. x")

ggplot(diag_data, aes(sample = resid)) +
  geom_qq() +
  geom_qq_line() +
  labs(x = "Theoretical quantiles", y = "Sample quantiles",
       title = "Normal Q-Q plot of residuals")

# Save a plot to include in the Google Doc (optional).
# ggsave("qq_plot.png", width = 6, height = 4)

# Optional numbers to back up the plots (the plots carry the argument).
# Skewness of the residuals: near 0 if symmetric; large positive = right skew.
mean((res - mean(res))^3) / sd(res)^3

# Curvature check: a small p-value on I(x^2) points to a curved mean.
summary(lm(y ~ x + I(x^2), data = my_data))$coefficients

# TODO: which assumption fails most clearly, and which plot shows it?
# Match it to Study A (skew), B (fan), C (heavy tails), or D (curve).

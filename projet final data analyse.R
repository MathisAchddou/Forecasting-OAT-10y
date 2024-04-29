## Project DATA ANALYSIS R PROGRAMMING

# Import necessary librairies
library(readxl) # to read Excel file
library(ggplot2) # to create plots
library(tidyverse) # to manipulate, analyse and visualize data
library(moments) #to calculate basics statistics
library(corrplot) #to calculate and plot correlation
library(lmtest) #for testing linear regression assumptions
library ("car") #for regression diagnostics and advanced statistical method

# Import data
path<-'/Users/mathi2s/Documents/Documents/école/M2/S2/R/projet final/projet final excel.xlsm'
df <- read_xlsx(path)

# Rename variables
names <- c("date", "yield", "Euribor_1m","Corpo_Borrowings", "IPCH", "Unemployment", "House_Borrowings", "key_interest_rate", "Business_confidence")
colnames(df) <- names

# Convert all variables to numeric except the date column
df <- mutate_at(df, vars(-matches('Date')), as.numeric)


# Basic Statistics = Mean, Median, Mode, Standard Deviation, Skewness, Kurtosis

mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

statistics <- function(df) {
  resultats <- list()
  for (variable in names(df)) {
    if (is.numeric(df[[variable]])) {
      mean <- mean(df[[variable]], na.rm = TRUE)
      median <- median(df[[variable]], na.rm = TRUE)
      std <- sd(df[[variable]], na.rm = TRUE)
      skewness <- skewness(df[[variable]], na.rm = TRUE)
      kurtosis <- kurtosis(df[[variable]], na.rm = TRUE)
      mode_val <- mode(df[[variable]])
      resultats[[variable]] <- c(Mean = mean,
                                 Median = median,
                                 Mode = mode_val,
                                 Standard_Deviation = std,
                                 Skewness = skewness,
                                 Kurtosis = kurtosis)
    }
  }
  return(as.data.frame(resultats))
}

stats_df <- statistics(df)
print(stats_df)

# Representation of density
density_plot <- function(data, variable) {
  mean_value <- mean(data[[variable]], na.rm = TRUE)
  median_value <- median(data[[variable]], na.rm = TRUE)
  mode_value <- mode(data[[variable]])
  plot(density(data[[variable]]), main = paste("Shape of", variable))
  
  abline(v = mean_value, col = "red", lty = 2)
  abline(v = mode_value, col = "green", lty = 2)
  abline(v = median_value, col = "blue", lty = 2)
  legend("topright", legend = c("Mean", "Mode", "Median"),
         col = c("red", "green", "blue"), lty = 2)
}

density_plot(df, "yield") #positive skewness
density_plot(df, "Euribor_1m") # positive skewness
density_plot(df, "Unemployment") #zero skewness
density_plot(df, "Business_confidence") #negative skewness


##### GRAPHICAL REPRESENTATION of TIME SERIES ####
df_long <- df %>%
  pivot_longer(cols = -date, names_to = "variable", values_to = "value")

p <- ggplot(df_long, aes(x = date, y = value)) +
  geom_line() +
  facet_wrap(~ variable, scales = "free_y") +  #facet_wrap=graphique par variable
  labs(title = "Time Series Evolution by Variable", x = "Date", y = "Valeur") +
  theme_minimal() +
  theme(legend.position = "none")
print(p)



### OLS ASSUMPTIONS

# 1. MLTICOLINEARITY with CORRELATION
df_cor <- df[, -which(names(df) == "date")]
matrice_correlation <- cor(df_cor, use = "complete.obs")

corrplot(matrice_correlation, method = "color", type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45,  
         addCoef.col = "black")

# corpo and house -> 0.79 : supp Corpo
# Euribor and corpo/house and IPCH : supp Euribor
# Unemployment and corpo/house and IPCH and unemploynement : supp Unemployment

# LINEAR REGRESSION

reg1<-lm(yield~IPCH+Unemployment+House_Borrowings+Business_confidence,data=df)

# 1. MULTICOLINEARITY with VIF
vif(reg1)

# 2. LINEARITY
plot(reg1, which=1)

# 3. HOMOSCEDASTICITY
plot(reg1, which=3)

# 4. NORMALITY OF RESIUDAlS
plot (reg1, which = 2)


# 5. INDEPENDENCE or AUTOCORRELATION of RESIDUALS with plot
residual_model<-resid(reg1)
plot(residual_model)

# 5. INDEPENDENCE or AUTOCORRELATION of RESIDUALS with Durbin-Watson test
dwtest(reg1)

# RESULT of LINEAR REGRESSION
summary(reg1)


### PROBIT REGRESSION

#Standardisation of data
df_stand <- df

df_stand$Business_confidence <- scale(df_stand$Business_confidence)
df_stand$Euribor_1m <- scale(df_stand$Euribor_1m)
df_stand$House_Borrowings <- scale(df_stand$House_Borrowings)
df_stand$Corpo_Borrowings <- scale(df_stand$Corpo_Borrowings)
df_stand$Unemployment <- scale(df_stand$Unemployment)
df_stand$IPCH <- scale(df_stand$IPCH)
df_stand$key_interest_rate <- scale(df_stand$key_interest_rate)


# 1. Test with variations
df_var <- df_stand

df_var$var<-c(NA, diff(df_var$yield))
df_var$var <- as.factor(ifelse(abs(df_var$var) > 0.05, 1, 0))
df_var$var

# 2. Test with dépassement d'un seuil
df_dep <- df_stand

seuil_taux<-1.5
#definition of the new binaire variable
df_dep$high_yield <- as.factor(ifelse(df_dep$yield > seuil_taux, 1, 0))
df_dep$high_yield


# 1. Modèle Probit pour var
modele_logit <- glm(var ~ Unemployment+Business_confidence, data = df_var, family = binomial())
summary(modele_logit)


# Compute the VIF
vif_result <- vif(modele_logit)
print(vif_result)

# Prediction of probabilities
df_varb<-df_var[-1,]
df_varb$predicted_prob <- predict(modele_logit, type = "response")
df_varb$predicted_prob
plot(df_varb$predicted_prob)


# 2. Modèle Probit pour dépassement
modele_logit2 <- glm(high_yield ~ Unemployment+Business_confidence, data = df_dep, family = binomial())
summary(modele_logit2)

# Compute the VIF
vif_result2 <- vif(modele_logit2)
print(vif_result2)

# Prediction of probabilities
df_dep$predicted_prob_2 <- predict(modele_logit2, type = "response")
df_dep$predicted_prob_2
plot(df_dep$predicted_prob_2)

df_dep$predicted_prob_2

#### Regression LASSO

library(glmnet)

var_exp<- df_stand[, -which(names(df_stand) %in% c("date", "yield"))]
var_exp

y <- df_stand$yield
X <- as.matrix(var_exp)


lasso.model <- glmnet(X, y, alpha = 1)
cv.lasso <- cv.glmnet(X, y, alpha = 1)
plot(cv.lasso)

# looking for better lambda
best.lambda <- cv.lasso$lambda.min
lasso.best <- glmnet(X, y, alpha = 1, lambda = best.lambda)

best.lambda

# Predictions
new_df<- df_stand[, -which(names(df_stand) %in% c("date"))]
new_df
predictions <- predict(lasso.best, s = best.lambda, newx = as.matrix(var_exp))
coef(lasso.best, s = best.lambda)
r_squared<-1-sum((predictions-y)^2)/sum((y-mean(y))^2)
r_squared
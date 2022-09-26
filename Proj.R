# Read in data
data <- read.table("data/Ghosal2020.txt", sep = '\t', header=TRUE, fill = TRUE)
data
data_clean <- data[-16,] # Removes India
attach(data)
# Part 1
# Fit linear model, no transformation
fit <- lm(formula = Week5deaths~Totalcases + Activecases + Recoverycases + Week4deaths + CFR, data = data_clean)
summary(fit)
#cor(data[1:nrow(data)-1,2:ncol(data_clean)])
# Predict Week5deaths for India
X <- data[16, 2:(ncol(data)-1)]
predict.lm(fit, X) # Point estimate
predict.lm(fit, X, interval = "confidence") # Prediction interval
# Pair plots
par(mfrow=c(3,2))
for (i in 2:(ncol(data)-1)) {
  plot(formula = data$Week5deaths~data[,i], xlab=sprintf("%s", colnames(data)[i]), ylab="Week 5 Deaths")
}
pairs(data[, 2:ncol(data)])
Week4deaths/Totalcases*100
CFR
par(mfrow=c(1,1))
plot(fit$residuals)
hist(fit$residuals)
#library(olsrr)
ols_test_breusch_pagan(fit)
# Part 2
# Log transformation
log_data <- cbind(Countries, log(data[,2:ncol(data)]))
log_clean <- log_data[-c(13,16),] # Removes Brazil and India
# Fit linear model, log tansformation
log_fit <- lm(formula = Week5deaths~Totalcases + Activecases + Recoverycases + Week4deaths + CFR, data = log_clean)
summary(log_fit)
# Predict Week5deaths for India
log_X <- log_data[16, 2:(ncol(log_data)-1)]
exp(predict.lm(fit, log_X)) # Point estimate
exp(predict.lm(fit, log_X, interval = "confidence")) # Prediction interval
# Pair plots
par(mfrow=c(3,2))
for (i in 2:(ncol(data)-1)) {
  plot(formula = log_clean$Week5deaths~log_clean[,i], xlab=sprintf("%s", colnames(data)[i]), ylab="Week 5 Deaths")
}
pairs(log_clean[, 2:ncol(log_clean)])
log_clean$Week4deaths/log_clean$Totalcases
log_clean$CFR
par(mfrow=c(1,1))
plot(log_fit$residuals)
hist(log_fit$residuals)
ols_test_breusch_pagan(log_fit)
# Part 4
data = read.table("data/Ghosal2020.txt", header = TRUE)
dataU = data[1:15, 1:7]
dataU
df = data.frame(X1 = character(0), X2 = character(0), RSquared = numeric(0), Tolerance = numeric(0), VIF =
                  numeric(0))
for (i in 2:5){
  for (j in (i+1):6){
    r2 = summary(lm(dataU[,i] ~ dataU[,j]))$r.squared
    tol = 1 - r2
    VIF = 1 / tol
    df = rbind(df, data.frame(colnames(dataU)[i], colnames(dataU)[j], r2, tol, VIF))
  }
}
colnames(df) = c('X1', 'X2', "R-Squared", "Tolerance", "VIF")
df
newmodel1 = lm(dataU$Week5deaths ~ dataU$Totalcases + dataU$Week4deaths + dataU$CFR)
summary(newmodel1)
newmodel2 = lm(dataU$Week5deaths ~ dataU$Activecases + dataU$Week4deaths + dataU$CFR)
summary(newmodel2)
newmodel3 = lm(dataU$Week5deaths ~ dataU$Recoverycases + dataU$Week4deaths + dataU$CFR)
summary(newmodel3)
summary(lm(data = dataU, formula = Week5deaths ~ Totalcases + Activecases + Recoverycases + Week4deaths +
             CFR))$adj.r.squared # 0.9700
# remove first
r2_1 = summary(lm(data = dataU, formula = Week5deaths ~ Activecases + Recoverycases + Week4deaths +
                    CFR))$adj.r.squared # 0.9727
r2_2 = summary(lm(data = dataU, formula = Week5deaths ~ Totalcases + Recoverycases + Week4deaths +
                    CFR))$adj.r.squared # 0.9712
r2_3 = summary(lm(data = dataU, formula = Week5deaths ~ Totalcases + Activecases + Week4deaths + CFR))$adj.r.squared
# 0.9708
r2_4 = summary(lm(data = dataU, formula = Week5deaths ~ Totalcases + Activecases + Recoverycases +
                    CFR))$adj.r.squared # 0.8991
r2_5 = summary(lm(data = dataU, formula = Week5deaths ~ Totalcases + Activecases + Recoverycases +
                    Week4deaths))$adj.r.squared # 0.9715
r2_c = c(r2_1, r2_2, r2_3, r2_4, r2_5)
for (i in 1:5){
  print(paste("Linear model without ", colnames(dataU)[i+1], ":", sep = ""))
  print(r2_c[i])
}
# eliminate two
r2_1 = summary(lm(data = dataU, formula = Week5deaths ~ Recoverycases + Week4deaths + CFR))$adj.r.squared # 0.9701
r2_2 = summary(lm(data = dataU, formula = Week5deaths ~ Activecases + Week4deaths + CFR))$adj.r.squared # 0.9450
r2_3 = summary(lm(data = dataU, formula = Week5deaths ~ Activecases + Recoverycases + CFR))$adj.r.squared # 0.8389
r2_4 = summary(lm(data = dataU, formula = Week5deaths ~ Activecases + Recoverycases + Week4deaths))$adj.r.squared #
0.9740
r2_c = c(r2_1, r2_2, r2_3, r2_4)
for (i in 1:4){
  print(paste("Linear model without Totalcases and ", colnames(dataU)[i + 2], ":", sep = ""))
  print(r2_c[i])
}
# eliminate three
r2_1 = summary(lm(data = dataU, formula = Week5deaths ~ Recoverycases + Week4deaths))$adj.r.squared # 0.9723
r2_2 = summary(lm(data = dataU, formula = Week5deaths ~ Activecases + Week4deaths))$adj.r.squared # 0.9491
r2_3 = summary(lm(data = dataU, formula = Week5deaths ~ Activecases + Recoverycases))$adj.r.squared # 0.7353
r2_c = c(r2_1, r2_2, r2_3)
for (i in 1:3){
  print(paste("Linear model without Totalcases, CFR, and ", colnames(dataU)[i + 2], ":", sep = ""))
  print(r2_c[i])
}
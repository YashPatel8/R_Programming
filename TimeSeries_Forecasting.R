
install.packages("readxl")
library(readxl)
install.packages("forecast")
library(forecast)
excel_sheets(file.choose())
hon1 <- read_excel("Honeywell.xlsx",sheet = 1, col_names = TRUE)
print(hon1)
hon2 <- read_excel("Honeywell.xlsx",sheet = 2, col_names = TRUE)
print(hon2)
hon3 <- read_excel("Honeywell.xlsx", sheet = 3, col_names = TRUE)
print(hon3)
hon1$Close
plot.ts(hon1$Close)


hon1exp1 <- HoltWinters(hon1$Close, alpha = 0.15, beta = FALSE, gamma = FALSE)
print(hon1exp1)
hon1exp1.pred <- predict(hon1exp1,n.ahead = 1,prediction.interval = TRUE)
print(hon1exp1.pred)
plot.ts(hon1$Close, xlim=c(1,125), main = "alpha 0.15 & beta 0.00")
lines(hon1exp1$fitted[,1],col = "red")
acc1 <- accuracy(hon1exp1$fitted,hon1$Close)
print(acc1)
mse1 <- acc1[2]^2
print(mse1)

hon1exp2 <- HoltWinters(hon1$Close,alpha = 0.35, beta = FALSE, gamma = FALSE)
print(hon1exp2)
hon1exp2.pred <- predict(hon1exp2, n.ahead = 1, prediction.interval = TRUE)
print(hon1exp2.pred)
plot.ts(hon1$Close,xlim = c(1,125), main = "alpha 0.35 & beta 0.00")
lines(hon1exp2$fitted[,1],col = "red")
acc2 <- accuracy(hon1exp2$fitted, hon1$Close)
print(acc2)
mse2 <- acc2[2]^2
print(mse2)

hon1exp3 <- HoltWinters(hon1$Close,alpha = 0.55, beta = FALSE, gamma = FALSE)
print(hon1exp3)
hon1exp3.pred <- predict(hon1exp3, n.ahead = 1, prediction.interval = TRUE)
print(hon1exp3.pred)
plot.ts(hon1$Close, xlim = c(1,125), main = "alpha 0.55 & beta 0.00")
lines(hon1exp3$fitted[,1], col = "red")
acc3 <- accuracy(hon1exp3$fitted, hon1$Close)
print(acc3)
mse3 <- acc3[2]^2
print(mse3)

hon1exp4 <- HoltWinters(hon1$Close,alpha = 0.75, beta = FALSE, gamma = FALSE)
print(hon1exp4)
hon1exp4.pred <- predict(hon1exp3, n.ahead = 1, prediction.interval = TRUE)
print(hon1exp4.pred)
plot.ts(hon1$Close, xlim = c(1,125), main = "alpha 0.75 & beta 0.00")
lines(hon1exp4$fitted[,1], col = "red")
acc4 <- accuracy(hon1exp4$fitted, hon1$Close)
print(acc4)
mse4 <- acc4[2]^2
print(mse4)


hon1exp5 <- HoltWinters(hon1$Close, alpha = 0.75, beta = 0.15, gamma = FALSE)
print(hon1exp5)
hon1exp5.pred <- predict(hon1exp5,n.ahead = 1,prediction.interval = TRUE)
print(hon1exp5.pred)
plot.ts(hon1$Close, xlim=c(1,125), main = "alpha 0.75 & beta 0.15")
lines(hon1exp5$fitted[,1],col = "blue")
acc5 <- accuracy(hon1exp5$fitted,hon1$Close)
print(acc5)
mse5 <- acc5[2]^2
print(mse5)

hon1exp6 <- HoltWinters(hon1$Close,alpha = 0.75, beta = 0.25, gamma = FALSE)
print(hon1exp6)
hon1exp6.pred <- predict(hon1exp6, n.ahead = 1, prediction.interval = TRUE)
print(hon1exp6.pred)
plot.ts(hon1$Close,xlim = c(1,125), main = "alpha 0.75 & beta 0.25")
lines(hon1exp6$fitted[,1],col = "blue")
acc6 <- accuracy(hon1exp6$fitted, hon1$Close)
print(acc6)
mse6 <- acc6[2]^2
print(mse6)

hon1exp7 <- HoltWinters(hon1$Close,alpha = 0.75, beta = 0.45, gamma = FALSE)
print(hon1exp7)
hon1exp7.pred <- predict(hon1exp7, n.ahead = 1, prediction.interval = TRUE)
print(hon1exp7.pred)
plot.ts(hon1$Close, xlim = c(1,125), main = "alpha 0.75 & beta 0.45")
lines(hon1exp7$fitted[,1], col = "blue")
acc7 <- accuracy(hon1exp7$fitted, hon1$Close)
print(acc7)
mse7 <- acc7[2]^2
print(mse7)

hon1exp8 <- HoltWinters(hon1$Close,alpha = 0.75, beta = 0.85, gamma = FALSE)
print(hon1exp8)
hon1exp8.pred <- predict(hon1exp8, n.ahead = 1, prediction.interval = TRUE)
print(hon1exp8.pred)
plot.ts(hon1$Close, xlim = c(1,125), main = "alpha 0.75 & beta 0.85")
lines(hon1exp8$fitted[,1], col = "blue")
acc8 <- accuracy(hon1exp8$fitted, hon1$Close)
print(acc8)
mse8 <- acc8[2]^2
print(mse8)


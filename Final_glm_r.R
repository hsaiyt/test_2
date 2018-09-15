

library(dummies)

## データの読み込み
d <- read.csv(file='data/insurance.csv')

## ダミー変数
dummy_sex <- dummy(d$sex)
dummy_smoker <- dummy(d$smoker)
dummy_region <- dummy(d$region)
d_dum <- cbind(d, dummy_sex, dummy_smoker, dummy_region)

str(d_dum)    # Factor型がInt型となっていることを確認
summary(d_dum)

d_dum_glm <- d_dum[,c(1,3,4,8,9,10,11,12,13,14,15,7)]

fit_gauss <- glm(charges~.,family=gaussian,data=d_dum_glm)
summary(fit_gauss)

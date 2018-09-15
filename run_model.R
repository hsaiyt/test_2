
library(rstan)

d1 <- read.csv('data/corporate_user.csv')
d2 <- read.csv('data/corporate_session.csv')
N <- nrow(d1)
I <- nrow(d2)
data <- list(N=N, I=I, device=d1$device, revisit=d2$revisit,
             holiday=d2$holiday, daytime=d2$daytime, 
             top=d2$top, UID=d2$audience_id, Y=d2$bounce)
fit <- stan(file='model/model8-8.stan', data=data,
            pars=c('b', 's_u', 's_s'), seed=1234)



d3 <- read.csv('data/corporate.csv')
glm_fit <- glm(bounce~device+revisit+holiday+daytime+top, family=binomial, data=d3)

summary(glm_fit)







library(pROC)

ms <- rstan::extract(fit)
N_mcmc <- length(ms$lp__)
spec <- seq(from=0, to=1, len=201)
probs <- c(0.1, 0.5, 0.9)

auces <- numeric(N_mcmc)
m_roc <- matrix(nrow=length(spec), ncol=N_mcmc)
for (i in 1:N_mcmc) {
  roc_res <- roc(d2$Y, ms$q[i,])
  auces[i] <- as.numeric(roc_res$auc)
  m_roc[,i] <- coords(roc_res, x=spec, input='specificity', ret='sensitivity')
}
quantile(auces, prob=probs)

for (i in 1:1) {
  roc_res <- roc(d2$Y, ms$q[i,])
  auces[i] <- as.numeric(roc_res$auc)
  m_roc[,i] <- coords(roc_res, x=spec, input='specificity', ret='sensitivity')
}


N_mcmc
ms$q[4000,]
roc(d2$Y, ms$q[1,])

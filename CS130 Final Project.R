install.packages("rgenoud")
library(rgenoud)
install.packages("Matching")
install.packages("MatchIt")
library(Matching)
library(MatchIt)


data_new <- datamatch[, c(2, 3, 4, 5, 6, 7, 8, 9, 17)]
datamatch <- na.omit(data_new)
mb <- MatchBalance(EV ~ age.group + educ + white.collar + not.full.time + male + tech + pol.info, data = data_omit)


set.seed(36466)
genout <- GenMatch(Tr = datamatch$EV, X = cbind(datamatch$age.group, datamatch$educ, datamatch$white.collar, datamatch$not.full.time, datamatch$male, datamatch$tech, datamatch$pol.info), pop.size = 20, nboots = 250)
mout <- Match(Y = datamatch$agree.evoting, Tr = datamatch$EV, X = cbind(datamatch$age.group, datamatch$educ, datamatch$white.collar, datamatch$not.full.time, datamatch$male, datamatch$tech, datamatch$pol.info), Weight.matrix = genout)
mb_after <- MatchBalance(EV ~ age.group + educ + white.collar + not.full.time + male + tech + pol.info, data = datamatch, match.out = mout)
summary(mout)
mout$est - 1.96 * mout$se
mout$est + 1.96 * mout$se
nrow(data_omit[data_omit$EV == 1, ])
nrow(data_omit[data_omit$EV == 0, ])

mb_after$BMsmallest.p.value
mb <- MatchBalance(EV ~ age.group + educ + white.collar + not.full.time + male + tech + pol.info, data = datamatched)


nn.match <- matchit(EV ~ age.group + educ + white.collar + not.full.time + male + tech + pol.info, data = datamatch, caliper = 0.05, method = "nearest")
summary(nn.match)
nrow(datamatch[datamatch$EV == 1, ])
nrow(datamatch[datamatch$EV == 0, ])
nn.gen <- GenMatch(Tr = data_omit$EV, X = cbind(data_omit$age.group, data_omit$educ, data_omit$white.collar, data_omit$not.full.time, data_omit$male, data_omit$tech, data_omit$pol.info), BalanceMatrix = cbind(data_omit$age.group, I(data_omit$age.group^2), I(data_omit$age.group^3), data_omit$age.group:data_omit$educ, data_omit$age.group:data_omit$tech, data_omit$educ, I(data_omit$educ^2), data_omit$tech, I(data_omit$tech^2), data_omit$pol.info, data_omit$educ:data_omit$pol.info, data_omit$age.group:data_omit$pol.info, data_omit$tech:data_omit$pol.info, data_omit$white.collar, data_omit$not.full.time, data_omit$male), pop.size = 20, nboots = 250)
m.out <- Match(Y = data_omit$agree.evoting, Tr = data_omit$EV, X = cbind(data_omit$age.group, data_omit$educ, data_omit$white.collar, data_omit$not.full.time, data_omit$male, data_omit$tech, data_omit$pol.info), Weight.matrix = nn.gen)
mb.out <- MatchBalance(EV ~ age.group + educ + white.collar + not.full.time + male + tech + pol.info, data = data_omit, match.out = nn.match)

summary(m.out)
#### MATCHING PSM
model <- glm(EV ~ age.group + I(age.group^2) + I(age.group^3) + age.group:educ + age.group:tech + educ + I(educ^2) + tech + I(tech^2) + pol.info + educ:pol.info + age.group:pol.info + tech:pol.info + white.collar + not.full.time + male, data = data_omit, family = "binomial")
model <- glm(EV ~ age.group + I(age.group^2) + I(age.group^3) + age.group:educ + age.group:tech + educ + I(educ^2) + tech + I(tech^2) + pol.info + educ:pol.info + age.group:pol.info + tech:pol.info + white.collar + not.full.time + male, data = dataomit, family = "binomial")
hist(model$fitted.values)
model_reg <- lm(agree.evoting ~ EV + age.group + I(age.group^2) + I(age.group^3) + age.group:educ + age.group:tech + educ + I(educ^2) + tech + I(tech^2) + pol.info + educ:pol.info + age.group:pol.info + tech:pol.info + white.collar + not.full.time + male, data = data_omit)
summary(model_reg)

X <- model$fitted
sd(X)
Y <- datamatch$agree.evoting
Tr <- datamatch$EV

rr <- Match(Y=Y, Tr=Tr, X=X, caliper = 0.05)

mb_psm <- MatchBalance(EV ~ age.group + educ + white.collar + not.full.time + male + tech + pol.info, data = datamatch, match.out = rr, nboots = 2000)
summary(rr)

datamatched <- match.data(rr)
max(X[rr$index.treated] - X[rr$index.control])

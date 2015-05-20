#---- Getting started ----
require(copula)
require(scatterplot3d)

set.seed(1)
myCop.norm <- ellipCopula(family = "normal", dim = 3, dispstr = "ex", param = 0.4)
v <- rCopula(n = 1000, copula = myCop.norm)
scatterplot3d::scatterplot3d(v, highlight.3d = T)


myCop.t <- ellipCopula(family = "t", dim = 3, dispstr = "toep", param = c(0.8, 0.5), df = 8)
u <- rCopula(n = 4, copula = myCop.t)
cbind(dCopula(u = u, copula = myCop.t), pCopula(u = u, copula = myCop.t))
scatterplot3d::scatterplot3d(u)


myCop.clayton <- archmCopula(family = "clayton", param = 2, dim = 2)
myMvd <- mvdc(copula = myCop.clayton, margins = c("norm", "norm"), paramMargins = list(list(mean = 1, sd = 2), list(mean = 2, sd = 1)))
x <- rMvdc(mvdc = myMvd, n = 4)
par(mfrow = c(1, 1), mar = c(2, 2, 1, 1), oma = c(1, 1, 0, 0), mgp = c(2, 1, 0))
contour(myMvd, dMvdc, xlim = c(-5, 6), ylim = c(-5, 5), col = c('purple', 'green'))


#---- Fit a copula model ----

myMvd <- mvdc(copula = ellipCopula(family = "normal", param = 0.5), margins = c("gamma", "gamma"), paramMargins = list(list(shape = 2, scale = 1), list(shape = 3, scale = 2)))
n <- 200
dat <- rMvdc(mvdc = myMvd, n = n)
loglikMvdc(c(2, 1, 3, 2, 0.5), dat, myMvd1)


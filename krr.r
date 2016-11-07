# KRR: makes some data fit a kernel Ridge regression model
# how can I vary the number of basis elements used in this package?

# resources 
# (DRR)Fast: https://cran.r-project.org/web/packages/DRR/DRR.pdf
# (CVST) reg: https://cran.r-project.org/web/packages/CVST/CVST.pdf



library(CVST)
library(DRR)

# Data

# Fast Learner
ns <- noisySinc(1000)
nsTest <- noisySinc(1000)
fast.krr <- constructFastKRRLearner()
fast.p <- list(kernel="rbfdot", sigma=1, lambda=.1/getN(ns), nblocks = 4)
system.time(fast.m <- fast.krr$learn(ns, fast.p))
fast.pred <- fast.krr$predict(fast.m, nsTest)
sum((fast.pred - nsTest$y)^2) / getN(nsTest)


# normal KRRLearner
krr <- CVST::constructKRRLearner()
p <- list(kernel="rbfdot", sigma=1, lambda=.1/getN(ns))
system.time(m <- krr$learn(ns, p))
pred <- krr$predict(m, nsTest)
sum((pred - nsTest$y)^2) / getN(nsTest)
plot(ns, col = '#00000030', pch = 19)
lines(sort(nsTest$x), fast.pred[order(nsTest$x)], col = '#00C000', lty = 2)
lines(sort(nsTest$x), pred[order(nsTest$x)], col = '#0000C0', lty = 2)
legend('topleft', legend = c('fast KRR', 'KRR'),
col = c('#00C000', '#0000C0'), lty = 2)
## End(Not run)


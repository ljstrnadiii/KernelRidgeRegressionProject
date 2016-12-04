# KRR: makes some data fit a kernel Ridge regression model

# resources 
# (DRR)Fast: https://cran.r-project.org/web/packages/DRR/DRR.pdf
# (CVST) reg: https://cran.r-project.org/web/packages/CVST/CVST.pdf


# TO DO: 
# 1. consider two data sets with a few levels of variance in the noise. 
# 2. Then choose the proper sigma by both fast CV and K-fold CV
library(CVST)
library(DRR)

# Data


# Fast Learner with noisySinc data with minimal variance:
plotSigmaFastsinc <- function(sigvals){
    #performance
    mse = c()
    time = c()
    #data
    ns = noisySinc(1000)
    nsTest = noisySinc(1000) #No true function
    #plot
    png("sigmakernsfast.png")
    plot(ns)

    for(sig in sigvals){ 
        # fit KRR with RBF kernel using noisySinc toy data for each sigma in kernel
                fast.krr = constructFastKRRLearner()
        fast.p = list(kernel="rbfdot", sigma = sig, 
                      lambda = .1/getN(ns), nblocks = 4)
        time <- c(time, as.numeric(system.time(fast.m 
                <- fast.krr$learn(ns, fast.p))[3]))
        fast.pred <- fast.krr$predict(fast.m,nsTest)
        mse = c(mse, sum((fast.pred-nsTest$y)^2) / getN(nsTest))
        # add lines an different colors for each sigma
        lines(sort(nsTest$x), fast.pred[order(nsTest$x)] , lty = 1) 
    }
    dev.off()
    return(list("mse" = mse, "time"= time))
}

# Regular Learner with noisySinc data with minimal variance:
plotSigmaRegsinc <- function(sigvals){
    #performance
    mse = c()
    time = c()
    #data
    ns = noisySinc(1000)
    nsTest = noisySinc(1000) #No true function
    #plot
    png("sigmakernsreg.png")
    plot(ns)

    for( sig in sigvals){ 
        # fit KRR with RBF kernel using noisySinc toy data for each sigma in kernel
        krr = constructKRRLearner()
        p = list(kernel="rbfdot", sigma = sig, 
                      lambda = .1/getN(ns))
        time <- c(time, as.numeric(system.time(fast.m 
                <- fast.krr$learn(ns, fast.p))[3]))
        pred <- krr$predict(m,nsTest)
        mse = c(mse, sum((pred-nsTest$y)^2) / getN(nsTest))
        # add lines an different colors for each sigma
        lines(sort(nsTest$x), pred[order(nsTest$x)] , lty = 1) 
    }
    dev.off()
    return(list("mse" = mse, "time"= time))
}

# Fast learner with Donoho data
plotSigmaFastDon <- function(sigvals){
    #performance
    mse = c()
    time = c()
    #data
    ns <- noisyDonoho(1000,fun=doppler,sigma=1)
    nsTest <- noisyDonoho(1000,fun=doppler,sigma=1)
    #plot
    png("sigmakernsfastdon.png")
    plot(ns)

    for( sig in sigvals){ 
        # fit KRR with RBF kernel using noisySinc toy data for each sigma in kernel
        krr = constructKRRLearner()
        p = list(kernel="rbfdot", sigma = sig, 
                      lambda = .1/getN(ns), nblocks = 4)
        time <- c(time, as.numeric(system.time(fast.m 
                <- fast.krr$learn(ns, fast.p))[3]))
        fast.pred <- fast.krr$predict(fast.m,nsTest)
        mse = c(mse, sum((fast.pred-nsTest$y)^2) / getN(nsTest))
        # add lines an different colors for each sigma
        lines(sort(nsTest$x),fast.pred[order(nsTest$x)] , lty = 1) 
    }
    dev.off()
    return(list("mse" = mse, "time"= time))
}

# Regular learner with Donoho
plotSigmaRegDon <- function(sigvals){
    #performance
    mse = c()
    time = c()
    #data
    ns <- noisyDonoho(1000,fun=doppler,sigma=1)
    nsTest <- noisyDonoho(1000,fun=doppler,sigma=1)
    #plot
    png("sigmakernsregdon.png")
    plot(ns)

    for( sig in sigvals){ 
        # fit KRR with RBF kernel using noisySinc toy data for each sigma in kernel
        krr = constructKRRLearner()
        p = list(kernel="rbfdot", sigma = sig, 
                      lambda = .1/getN(ns))
        time <- c(time, as.numeric(system.time(m 
                <- krr$learn(ns, p))[3]))
        pred <- krr$predict(m,nsTest)
        mse = c(mse, sum((pred-nsTest$y)^2) / getN(nsTest))
        # add lines an different colors for each sigma
        lines(sort(nsTest$x), pred[order(nsTest$x)] , lty = 1) 
    }
    dev.off()
    return(list("mse" = mse, "time"= time))
}

# K-folds CV with regular Donoho
#CVRegDon <- function(sigvals, lambdavals){

    #ns <- noisyDonoho(1000,fun=doppler,sigma=1)
    




# perform fast CV of each data set

# plot final models on top of each other




if(FALSE){



# Fast Learner
ns <- noisyDonoho(1000,fun=doppler,sigma=1)#noisySinc(1000)
nsTest <- noisyDonoho(1000,fun=doppler,sigma=1)#noisySinc(1000)
fast.krr <- constructFastKRRLearner()
fast.p <- list(kernel="rbfdot", sigma=200, lambda=.1/getN(ns), nblocks = 4)
print(system.time(fast.m <- fast.krr$learn(ns, fast.p)))
fast.pred <- fast.krr$predict(fast.m, nsTest)
print(sum((fast.pred - nsTest$y)^2) / getN(nsTest))


# normal KRRLearner
krr <- CVST::constructKRRLearner()
p <- list(kernel="rbfdot", sigma=200, lambda=.1/getN(ns))
print(system.time(m <- krr$learn(ns, p)))
pred <- krr$predict(m, nsTest)
print(sum((pred - nsTest$y)^2) / getN(nsTest))
plot(ns, col = '#00000030', pch = 19)
lines(sort(nsTest$x), fast.pred[order(nsTest$x)], col = '#00C000', lty = 2)
lines(sort(nsTest$x), pred[order(nsTest$x)], col = '#0000C0', lty = 2)
legend('topleft', legend = c('fast KRR', 'KRR'),
col = c('#00C000', '#0000C0'), lty = 2)
## End(Not run)
}

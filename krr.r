# this R script needs to be imported by calling source("krr.r") in the interpretter.


#################
# The script provides functions for plotting KRR fitted values for vaious leves
# of sigma values in the gaussian kernel for two different data sets shown below,
# plotting functions for plotting the estimated eigen functions of the underlying
# kernel operator, and executed simple CV k-folds model section methods. 

# resources 
# (DRR)Fast: https://cran.r-project.org/web/packages/DRR/DRR.pdf
# (CVST) reg: https://cran.r-project.org/web/packages/CVST/CVST.pdf


library(CVST)
library(DRR)



# Regular Learner with noisySinc data with minimal variance:
plotSigmaRegsinc <- function(sigvals){
    #performance
    mse = c()
    time = c()
    #data
    ns = noisySinc(1000, sigma=.1, d=2)
    nsTest = noisySinc(1000) #No true function
    #plot
    png("1-2.png")
    plot(ns, xlab="x",ylab="y")

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


# Regular learner with Donoho
plotSigmaRegDon <- function(sigvals){
    #performance
    mse = c()
    time = c()
    #data
    ns <- noisyDonoho(1000,fun=doppler,sigma=3)
    nsTest <- noisyDonoho(1000,fun=doppler)
    #plot
    png("3d.png")
    plot(ns, xlab="x",ylab="y")

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

# plot the approximations for the eigenfunctions of kernel operator
plotEigFun <- function(x,ksigma){
    library(kernlab)
    rbf = rbfdot(sigma = ksigma)
    x = x[order(x)]
    k = kernelMatrix(rbf,x)
    pc = princomp(k)
    eig = eigen(k)

    
    png("evals.png")
    plot(eig$values[1:50], xlab="eigenvector",ylab="eigenvalue")
    dev.off()

    #plot top nine e.functions
    png("efunctions.png")
    par(mfrow=c(3,3))
    #plot(x, k %*% pc$loadings[,1], type="l", xlab = "x", ylab=expression(phi))
    plot(x, sqrt(eig$values[1])/eig$values[1] * k %*% eig$vectors[,1], type="l",
         xlab = "x", ylab = expression(phi))
    for( i in 2:9){
      #plot(x,k %*% pc$loadings[,i], lty=1, xlab="x", ylab=expression(phi))
      plot(x, sqrt(eig$values[i])/eig$values[i] * k %*% eig$vectors[,i], type="l",
           xlab="x", ylab = expression(phi))
    dev.off()}
}

# K-folds CV with regular Donoho
cv <- function(data,sigvals, lambdavals){
    krr=constructKRRLearner()
    params = constructParams(kernel="rbfdot", sigma = sigvals, lambda = lambdavals)
    opt = CV(data, krr, params)

    png("nsfit.png")
    p = list(kernel="rbfdot", sigma=opt$kernel$sigma, lambda=opt$kernel$lambda)
    m = krr$learn(data, p)
    plot(data, xlab="x", ylab="y", xlim = c(-3,3), ylim=c(-1.5,1.5))
    pred = krr$predict(m, data)
    lines(sort(data$x), pred[order(data$x)], lty=1, lwd = 5)

    dev.off()


    return(list("opt"=opt))
}


library(ZINB.GP)

setwd("simulations")

coverage <- function(x, draws)
{
    out <- quantile(draws, probs = c(0.025, 0.975))
    lower <- out[1]
    upper <- out[2]

    # use mean to coerce to number
    return(mean((x > lower) & (x < upper)))
}

coverageM <- function(x, draws)
{
    intervals <- apply(draws, 2, \(x) quantile(x, probs=c(0.025, 0.975)))
    lower <- intervals[1,]
    upper <- intervals[2,]
    return(mean((x > lower) & (x < upper)))
}

obs_nums <- c(2, 4, 8, 16, 24, 32, 48, 64)

# Binomial Part
alpha <- c(-0.25, 0.25)

# Count Part
beta <- c(.5, -.25)

l1t <- 2
l2t <- 3
sigma1t <- 0.5
sigma2t <- 0.5

l2s <- 1
l1s <- 2
sigma1s <- 1
sigma2s <- 1

spatial_noise <- 0.5
temporal_noise <- 0.2


l1tcover <- matrix(0, nrow=100, ncol=length(obs_nums))
l2tcover <- matrix(0, nrow=100, ncol=length(obs_nums))

sigma1tcover <- matrix(0, nrow=100, ncol=length(obs_nums))
sigma2tcover <- matrix(0, nrow=100, ncol=length(obs_nums))

sigma1scover <- matrix(0, nrow=100, ncol=length(obs_nums))
sigma2scover <- matrix(0, nrow=100, ncol=length(obs_nums))

l1scover <- matrix(0, nrow=100, ncol=length(obs_nums))
l2scover <- matrix(0, nrow=100, ncol=length(obs_nums))

snoisecover <- matrix(0, nrow=100, ncol=length(obs_nums))
tnoisecover <- matrix(0, nrow=100, ncol=length(obs_nums))

alphacover <- matrix(0, nrow=100, ncol=length(obs_nums))
betacover <- matrix(0, nrow=100, ncol=length(obs_nums))

acover <- matrix(0, nrow=100, ncol=length(obs_nums))
bcover <- matrix(0, nrow=100, ncol=length(obs_nums))
ccover <- matrix(0, nrow=100, ncol=length(obs_nums))
dcover <- matrix(0, nrow=100, ncol=length(obs_nums))

j <- 1
for (obs_num in obs_nums)
{
    print(obs_num)
    for (i in 1:100)
    {
        cat(paste(" ", i, "\n"))
        filename1 <- paste("out_", obs_num, "_rep_", i, ".rda", sep = "")
        filename2 <- paste("ob_", obs_num, "_rep_", i, ".rda", sep = "")
        
        # Load output from simulation run
        load(filename1)
        load(filename2)

        # Get coverage for this file
        l1tcover[i,j] <- coverage(l1t, output$L1t)
        l2tcover[i,j] <- coverage(l2t, output$L2t)

        sigma1tcover[i,j] <- coverage(sigma1t, output$Sigma1t)
        sigma2tcover[i,j] <- coverage(sigma2t, output$Sigma2t)

        l1scover[i,j] <- coverage(l1s, output$L1s)
        l2scover[i,j] <- coverage(l2s, output$L2s)

        sigma1scover[i,j] <- coverage(sigma1s, output$Sigma1s)
        sigma2scover[i,j] <- coverage(sigma2s, output$Sigma2s)
        
        alphacover[i,j] <- coverageM(alpha, output$Alpha)
        betacover[i,j] <- coverageM(beta, output$Beta)

        snoisecover[i,j] <- coverage(spatial_noise, rbind(output$Noise1s, output$Noise2s))
        tnoisecover[i,j] <- coverage(temporal_noise, rbind(output$Noise1t, output$Noise2t))

        acover[i,j] <- coverageM(a, output$A)
        bcover[i,j] <- coverageM(b, output$B)
        ccover[i,j] <- coverageM(c, output$C)
        dcover[i,j] <- coverageM(d, output$D)

        # Delete output
        rm(list=c("X", "y", "coords", "Vs", "Vt", "Ds", "Dt", "u", "a", "b", "c", "d", "output"))
    }
    j <- j + 1
}

save(list=ls(all = TRUE), file="fin.rda")
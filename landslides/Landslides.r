library(ZINB.GP)

observations <- read.table("landslides_county_year.dat")
out <- make_y_Vs_Vt(observations)

Vs <- out$Vs
Vt <- out$Vt
y <- out$y
Ds <- as.matrix(unname(read.table("county_dist_scale.dat")))
Dt <- as.matrix(unname(read.table("time_dist.dat")))
scale_t <- 0.0001 * max(Dt)
Dt <- Dt / scale_t
Ds <- Ds * 100

countyData <- read.csv("counties.csv")
countyCoords <- as.matrix(unname(read.table("counties_coords.dat")))
X <- Vs %*% countyData$county_elev_std[2:length(countyData$county_elev_std)] # Note this does ignore the covariate at the intercept, does not matter here as intercept county is not of much relevance
X <- cbind(1, X)
ltprior <- list(max = 16, mh_sd = 0.5, a = 1, b = 0.001)

# Example Matern kernel, v=3/2, squared exponential is used in the paper.
kernel <- function(dist.sq, ls) {
    return((1 + (sqrt(3 * dist.sq) / ls)) * exp(-sqrt(3 * dist.sq) / ls))
}

output <- ZINB_GP(X, y, countyCoords, Vs, Vt, Ds, Dt, nsim = 303000, burn = 3000, thin = 300, ltPrior = ltprior, print_progress = TRUE, print_iter = 1000, save_ypred = TRUE, kern = kernel)
save(list=c("output"), file="outMat.rda")
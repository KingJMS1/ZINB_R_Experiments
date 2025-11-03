library(ZINB.GP)
library(Matrix)
library(mvtnorm)

obs_matrix <- read.table("landslides_county_year.dat")
out <- make_y_Vs_Vt(obs_matrix)

Vs <- out$Vs
Vt <- out$Vt
y <- out$y
Ds <- as.matrix(unname(read.table("county_dist_scale.dat")))
Dt <- as.matrix(unname(read.table("time_dist.dat")))
scale_t <- 0.001 * max(Dt)
Dt <- Dt / scale_t
Ds <- Ds * 100

countyData <- read.csv("counties.csv")
countyCoords <- as.matrix(unname(read.table("counties_coords.dat")))
X <- Vs %*% countyData$county_elev_std[2:length(countyData$county_elev_std)] # Note this does ignore the covariate at the intercept, does not matter here as intercept county is not of much relevance
X <- cbind(1, X)
ltprior <- list(max = 16, mh_sd = 0.5, a = 1, b = 0.001)
output <- ZINB_GP(X, y, countyCoords, Vs, Vt, Ds, Dt, M = 10, nsim = 420000, burn = 20000, thin = 400, ltPrior = ltprior, print_progress = TRUE, print_iter = 1000, save_ypred = TRUE)
save(list=c("output"), file="out.rda")
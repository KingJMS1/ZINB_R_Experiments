library(mvtnorm)
library(Matrix)
library(ZINB.GP)
library(sp)

# This file can be run via `Rscript example_usage.R`

obs_y <- read.csv("ydata_daily.csv")
Xdata <- read.csv("Xdata_daily.csv")
obs_y$day <- NULL
Xdata$day <- NULL

N <- nrow(obs_y) * ncol(obs_y)
Vs <- as.matrix(sparseMatrix(i=1:N, j=rep(1:ncol(obs_y), nrow(obs_y)), x=1))
Vt <- as.matrix(sparseMatrix(i=1:N, j=rep(as.integer(Xdata$Month) + 1, each=ncol(obs_y)), x=1))
y <- as.vector(t(as.matrix(unname(obs_y))))

# Sacrifice to the intercept gods
Vs <- Vs[,2:ncol(Vs)]
Vt <- Vt[,2:ncol(Vt)]

print(dim(Vt))
print(dim(Xdata))

xcols <- c("windu", "windv", "temp", "gaugeOn")
X <- Matrix(0, nrow=nrow(Vs), ncol=length(xcols) + 1, sparse = FALSE)
j <- 2
X[, 1] <- 1
for (xcol in xcols)
{
    print(xcol)
    z <- 1
    stuff <- rep("", 44)

    for (i in 0:43)
    {
        stuff[i + 1] <- paste(xcol, "_", i, sep="")
    }
    X[,j] <- as.vector(t(as.matrix(unname(Xdata[, stuff]))))

    j <- j + 1
}

months <- as.matrix(unname(read.csv("monthEmbedding.csv")))
lonlat <- read.csv("tidelocs.csv")
coordinates(lonlat) <- c("lon", "lat")
proj4string(lonlat) <- CRS("+proj=longlat +datum=WGS84")
coords <- as(spTransform(lonlat, CRS("+proj=utm +zone=17 ellps=WGS84")), "SpatialPoints")
coords <- as.matrix(unname(attributes(coords)$coords)) / 1000
Ds <- as.matrix(dist(coords))
Dt <- as.matrix(dist(months) * 20)
X <- as.matrix(X)

print(head(X))

#################
# Run the Model #
#################
# Run for a short time for demo purposes
cat("Running Model\n")
output <- ZINB_GP(X, y, coords, Vs, Vt, Ds, Dt, M = 10, 6000, 1000, 4, TRUE, print_progress = TRUE)
save(list=c("output"), file="outMat.rda")
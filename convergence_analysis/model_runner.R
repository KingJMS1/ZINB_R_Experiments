library(ZINB.GP)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1)
{
    stop("Must provide rep number I am responsible for.", call. = FALSE)
}
repNum <- as.integer(args[1])

# Load: X, y, coords, Vs, Vt, Ds, Dt, u, a, b, c, d
obs_nums <- c(2, 4, 8, 16, 24, 32, 48, 64)
for (obs_num in obs_nums)
{
    cat(paste("\nRunning", obs_num, "on rep", repNum, "\n"))
    load(paste("simulations/", "ob_", obs_num, "_rep_", repNum, ".rda", sep=""))
    output <- ZINB_GP(X, y, coords, Vs, Vt, Ds, Dt, M = 10, 2200, 200, 2, TRUE, print_progress = TRUE)
    save(list=c("output"), file=paste("simulations/out_", obs_num, "_rep_", repNum, ".rda", sep=""))
    rm(list=c("X", "y", "coords", "Vs", "Vt", "Ds", "Dt", "u", "a", "b", "c", "d", "output"))
}

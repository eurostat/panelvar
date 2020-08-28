setwd(wd3)

fwhich <- which(KQ$target==itarg)
ftrue <- as.matrix(KQQ[,fwhich])
ftrue <- transform.vars(ftrue, KQ$transf[fwhich])

e <- matrix(ftrue, NROW(ftrue), NCOL(Kfest))-Kfest
rownames(e) <- rownames(fest)
colnames(e) <- colnames(fest)
e <- na.omit(e)

ftrue.diff <- transform.vars(as.matrix(ftrue), 3)
fest.diff <- transform.vars(Kfest, rep(3, NCOL(Kfest)))
ss.diff <- sign(matrix(ftrue.diff, NROW(ftrue.diff), NCOL(fest.diff)))-sign(fest.diff)
ss.diff <- na.omit(ss.diff)

# ibn <- which(colnames(e)==ibench)

MAE <- colMeans(abs(e))
RMSFE <- sqrt(colMeans(e^2))
SSR <- colMeans((ss.diff==0))

stats1 <- cbind(MAE, RMSFE, SSR)

rownames(stats1) <- mnams
colnames(stats1) <- c("MAE", "RMSFE", "SSR")

write.csv(e, paste(give.name, knams, "-error.csv", sep=""))
write.csv(SSR, paste(give.name, knams, "-SSR.csv", sep=""))

# Print the density plots as well as export the graph data
for(jj in 1:NCOL(Kfest))
{
  knam <- paste(give.name, knams)
  print.backplots(knam, ftrue, Kfest[,jj],
                  Kquant[rownames(ftrue), ,mnams[jj]], mnams[jj])
}

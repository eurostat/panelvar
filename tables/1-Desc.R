# Clear Memory
rm(list=ls(all=TRUE))

# Load functions
library("zoo"); library("lubridate"); library("forecast")
library("imputeTS"); library("panelvar"); library("moments")

get.stats <- function(Z, Znam, ZTransf, Zfreq, Ze)
{
  ZZ <- matrix(NA, NCOL(Z), 9)
  colnames(ZZ) <- c("Freq","Transf", "From", "To", "N", "Mean", "SD", "Skew", "Kurt")
  rownames(ZZ) <- Znam
  
  for(j in 1:NCOL(Z))
  {
    ZZ[j,1] <- Zfreq
    ZZ[j,2] <- ZTransf[j]
    ZZ[j,3] <- rownames(Z)[1]
    ZZ[j,4] <- rownames(Z)[NROW(Z)]
    ZZ[j,5] <- NROW(Z)
    ZZ[j,6] <- mean(Z[,j], na.rm=TRUE)
    ZZ[j,7] <- sd(Z[,j], na.rm=TRUE)
    ZZ[j,8] <- skewness(Z[,j], na.rm=TRUE)
    ZZ[j,9] <- kurtosis(Z[,j], na.rm=TRUE)
  }
  return(ZZ)
}

# Define working directories
rrrnams <- c("DE-Q-itarg11-h1.Rdata", "FR-Q-itarg11-h1.Rdata",
           "IT-Q-itarg11-h1.Rdata", "UK-Q-itarg11-h1.Rdata")
fffnams <- c("Desc-DE.csv", "Desc-FR.csv", "Desc-IT.csv", "Desc-UK.csv")

setwd("../data")

for(iu in 1:NROW(rrrnams))
{
  setwd("./output/")
  load(rrrnams[iu])
  
  xq <- apply(XQ, 2, na.interpolation, option="linear")
  xm <- apply(XM, 2, na.interpolation, option="linear")
  xd <- apply(XD, 2, na.interpolation, option="linear")
  xg <- apply(XG, 2, na.interpolation, option="linear")

  xqs <- na.omit(transform.vars(xq, ZQ$transf))
  xms <- na.omit(transform.vars(xm, ZM$transf))
  xds <- na.omit(transform.vars(xd, ZD$transf))
  xgs <- na.omit(transform.vars(xg, ZG$transf))

  s1 <- get.stats(xqs, ZQ$labels, ZQ$transf, "Q")
  s2 <- get.stats(xms, ZM$labels, ZM$transf, "M")
  s3 <- get.stats(xds, ZD$labels, ZD$transf, "D")
  s4 <- get.stats(xgs, ZG$labels, ZG$transf, "W")

  S <- rbind(s1, s2, s3, s4)
  
  setwd("../tables/")
  write.csv(S, fffnams[iu])
}

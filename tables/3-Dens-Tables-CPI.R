# Clear Memory
rm(list=ls(all=TRUE))

fload.csv <- function(Z)
{
  xe <- as.matrix(read.csv(Z, header=TRUE))
  xed <- as.character(xe[,1])
  xe <- apply(xe[,2:NCOL(xe)], 2, as.numeric)
  rownames(xe) <- xed
  return(xe)
}

print.stats <- function(h1n, h1s, h1nsav, h, ibench)
{
  e1 <- fload.csv(h1n[1])
  e2 <- fload.csv(h1n[2])
  e <- cbind(e1, e2)
  
  s1 <- colMeans(abs(e), na.rm=TRUE)
  s2 <- sqrt(colMeans(e^2, na.rm=TRUE))
  
  ibn <- which(colnames(e)==ibench)
  
  DM <- matrix(NA, NROW(s1), 2)
  for(j in 1:NCOL(e))
  {
    if(j!=ibn){
      DM[j,1] <- dm.test(e[,j], e[,ibn], alternative = c("two.sided"), h, power=1)$p.value
      DM[j,2] <- dm.test(e[,j], e[,ibn], alternative = c("two.sided"), h, power=2)$p.value
    }
  }
  
  ssr1 <- fload.csv(h1s[1]); ssr1 <- ssr1[,NCOL(ssr1)]
  ssr1 <- as.matrix(ssr1); ssr1n <- rownames(ssr1)
  ssr2 <- as.matrix(read.csv(h1s[2]))
  ssr2n <- ssr2[,1]
  ssr2 <- as.numeric(ssr2[,2])
  
  ssr <- c(ssr1, ssr2)
  
  mnamsall <- c(ssr1n, ssr2n)
  stt <- cbind(s1/s1[ibn], s2/s2[ibn], ssr, DM)
  rownames(stt) <- mnamsall
  colnames(stt) <- c("MAE", "RMSFE", "SSR", "DM1", "DM2")
  
  # setwd("../data/tables/")  
  # write.csv(stt[1:37,], h1nsav)
  return(stt)
}

get.cq <- function(sers, num1, num2)
{
  xbt.l <- which(as.numeric(sers)==num1)
  xbt.h <- which(as.numeric(sers)==num2)
  xbt <- c(xbt.l, xbt.h)
  return(xbt)
}

get.CR <- function(z, zt)
{
  crt <- matrix(NA, NROW(z), 1)
  for(j in 1:NROW(z))
  {
    if(is.na(z[j])){next}
    if( (zt[j]<=z[j,2]) & (zt[j]>=z[j,1])){crt[j] <- 1}else{crt[j] <- 0}
  }
  return(mean(crt, na.rm=TRUE)) 
}

get.allCR <- function(hh1n, hh1s, hc)
{
  q1 <- loadToEnv(hh1n)[["fquant"]]
  if(hc=="DE"){ q2 <- loadToEnv(hh1s)[["fquant1"]] }
  if(hc=="FR"){ q2 <- loadToEnv(hh1s)[["fquant2"]] }
  if(hc=="IT"){ q2 <- loadToEnv(hh1s)[["fquant3"]] }
  if(hc=="UK"){ q2 <- loadToEnv(hh1s)[["fquant4"]] }
  qtr <- loadToEnv(hh1n)[["ftrue"]]
  
  nams1 <- dimnames(q1)[3][[1]]
  nams2 <- dimnames(q2)[3][[1]]
  allqn <- c(nams1, nams2[1:10])
  
  CR <- matrix(NA, NROW(allqn), 2)
  rownames(CR) <- allqn
  colnames(CR) <- crn
  
  for(i in 1:NROW(allqn))
  {
    im <- allqn[i]
    
    w1 <- which(nams1==im)
    w2 <- which(nams2==im)
    
    if(NROW(w1)>0){ xq <- q1[,,w1]}
    if(NROW(w2)>0){ xq <- q2[,,w2]}
    
    xq1 <- xq[, get.cq(colnames(xq), cr1[1], cr2[1])]
    xq2 <- xq[, get.cq(colnames(xq), cr1[2], cr2[2])]
    
    # CR[i,] <- c(get.CR(xq1, qtr), get.CR(xq2, qtr), get.CR(xq3, qtr),
    #             get.CR(xq4, qtr), get.CR(xq5, qtr))
    
    CR[i,] <- c(get.CR(xq1, qtr), get.CR(xq2, qtr))
  }
  return(CR)
}

library("zoo"); library("lubridate"); library("forecast")
library("imputeTS"); library("panelvar"); library("moments")
library("R.utils")

iibench <- "AR.1."

setwd("../data/output")

wdd <- "../output/" # relative path
wds <- "../tables/"

# General settings
#cr1 <- c(0.05,   0.1,  0.15,  0.20,  0.25)
#cr2 <- c(0.95,   0.9,  0.85,  0.80,  0.75)
#crn <- c("90%", "80%", "70%", "60%", "50%")
cr1 <- c(0.16, 0.05)
cr2 <- c(0.84, 0.95)
crn <- c("68%", "90%")

hcc <- "DE"
c1 <- get.allCR(hh1n="DE-M-itarg4254-h1.Rdata",
                hh1s="PVAR-M-itarg4254-h1.Rdata",
                hc=hcc)

c2 <- get.allCR(hh1n="DE-M-itarg4254-h3.Rdata",
                hh1s="PVAR-M-itarg4254-h3.Rdata",
                hc=hcc)

c3 <- get.allCR(hh1n="DE-M-itarg4254-h6.Rdata",
                hh1s="PVAR-M-itarg4254-h6.Rdata",
                hc=hcc)

c4 <- get.allCR(hh1n="DE-M-itarg4254-h12.Rdata",
                hh1s="PVAR-M-itarg4254-h12.Rdata",
                hc=hcc)
cc <- cbind(c1, c2, c3, c4)

setwd(wds); write.csv(cc, "DE-Dens-CR.csv")



setwd(wdd)
hcc <- "FR"
c1 <- get.allCR(hh1n="FR-M-itarg4254-h1.Rdata",
                hh1s="PVAR-M-itarg4254-h1.Rdata",
                hc=hcc)

c2 <- get.allCR(hh1n="FR-M-itarg4254-h3.Rdata",
                hh1s="PVAR-M-itarg4254-h3.Rdata",
                hc=hcc)

c3 <- get.allCR(hh1n="FR-M-itarg4254-h6.Rdata",
                hh1s="PVAR-M-itarg4254-h6.Rdata",
                hc=hcc)

c4 <- get.allCR(hh1n="FR-M-itarg4254-h12.Rdata",
                hh1s="PVAR-M-itarg4254-h12.Rdata",
                hc=hcc)
cc <- cbind(c1, c2, c3, c4)

setwd(wds); write.csv(cc, "FR-Dens-CR.csv")


setwd(wdd)
hcc <- "IT"
c1 <- get.allCR(hh1n="IT-M-itarg4254-h1.Rdata",
                hh1s="PVAR-M-itarg4254-h1.Rdata",
                hc=hcc)

c2 <- get.allCR(hh1n="IT-M-itarg4254-h3.Rdata",
                hh1s="PVAR-M-itarg4254-h3.Rdata",
                hc=hcc)

c3 <- get.allCR(hh1n="IT-M-itarg4254-h6.Rdata",
                hh1s="PVAR-M-itarg4254-h6.Rdata",
                hc=hcc)

c4 <- get.allCR(hh1n="IT-M-itarg4254-h12.Rdata",
                hh1s="PVAR-M-itarg4254-h12.Rdata",
                hc=hcc)
cc <- cbind(c1, c2, c3, c4)

setwd(wds); write.csv(cc, "IT-Dens-CR.csv")


setwd(wdd)
hcc <- "UK"
c1 <- get.allCR(hh1n="UK-M-itarg4254-h1.Rdata",
                hh1s="PVAR-M-itarg4254-h1.Rdata",
                hc=hcc)

c2 <- get.allCR(hh1n="UK-M-itarg4254-h3.Rdata",
                hh1s="PVAR-M-itarg4254-h3.Rdata",
                hc=hcc)

c3 <- get.allCR(hh1n="UK-M-itarg4254-h6.Rdata",
                hh1s="PVAR-M-itarg4254-h6.Rdata",
                hc=hcc)

c4 <- get.allCR(hh1n="UK-M-itarg4254-h12.Rdata",
                hh1s="PVAR-M-itarg4254-h12.Rdata",
                hc=hcc)
cc <- cbind(c1, c2, c3, c4)

setwd(wds); write.csv(cc, "UK-Dens-CR.csv")

zf <- z[NROW(z)]
# b <- round(NROW(z)^(1/3))
# boots <- matrix(NA, NROW(z), B)
# for(j in 1:B){
#   boots[,j] <- MBB(z, b)
# }
# zboot <- boots[NROW(boots),]
sigmah <- sd(z, na.rm=TRUE)
if(hbounds==TRUE){
  sigmah <- sigmah*sqrt(h)
}
spseq <- qnorm(seq(0.51, 0.99, 0.01))*sigmah
outf <- zf
zout <- c(outf, outf-rev(spseq)[1], outf-rev(spseq), outf, outf+spseq, outf+spseq[NROW(spseq)])

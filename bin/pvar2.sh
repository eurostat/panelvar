#!/bin/bash
nohup R CMD BATCH ../model/PVAR-m1.R ../model/PVAR-m1.Rout &
nohup R CMD BATCH ../model/PVAR-m2.R ../model/PVAR-m2.Rout &
nohup R CMD BATCH ../model/PVAR-m3.R ../model/PVAR-m3.Rout &
nohup R CMD BATCH ../model/PVAR-q1.R ../model/PVAR-q1.Rout &


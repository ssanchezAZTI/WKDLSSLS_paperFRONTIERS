
library(FLBEIA)
library(dplyr)

# library(ggplot2)
# library(gridExtra)
# library(grid)
# library(cowplot)

rm(list = ls())

plot.dir <- "./plots/brps"
out.dir  <- "./output/brps"

source(file.path("./R/fun","brps_seasonal.R"))

# Populations type 1: anchovies + Norway pout

stkn <- "STK1"
sc   <- "bc"      # steppness = 0.75


load( #stk, sr_model, sr_params, ref.pts,
      file.path("./input",paste(stkn,sc,"_dataLH.RData",sep="")))



stk <- iter(window(stk, start = 2, end = 2), 1)

B0        <- 1e+05    # B.initial
R0        <- 27489766 # R.initial
rec.ss    <- 2
ssb.ss    <- 2
sr_model  <- "bevholt"
sr_params <- c( a = 29988835.109, b = 9090.909)
# Fprop     <- c(0.30,0.70)
# Fscan <- seq(0,4,by=0.01)
# oldest <- 100
# nrun <- 200
# tol <- 1e-8


runs.file <- file.path(out.dir,paste(stkn,sc,"_msyruns.csv",sep=""))
brps.file <- file.path(out.dir,paste(stkn,sc,"_brps.csv",sep=""))

if (file.exists(runs.file)) file.remove(runs.file)
if (file.exists(brps.file)) file.remove(brps.file)


# Loop by percentage of catches in each season

for (p in seq(0.1,0.9,0.1)) {
  
  print(paste("p =",p))
  
  fruns <- brpsson( stk, B0=1e+05, R0=27489766, rec.ss=2, ssb.ss=2, 
                    sr_model="bevholt", sr_params=c( a = 29988835.109, b = 9090.909), 
                    Fprop = c(p, 1-p))
  
  # simulations
  
  if (file.exists(runs.file)) {
    write.table(fruns$runs, file=runs.file, sep=";", row.names=F, append=T, col.names=F)
  } else {
    write.table(fruns$runs, file=runs.file, sep=";", row.names=F, append=F)
  }
  
  
  # reference points
  
  if (file.exists(brps.file)) {
    write.table(fruns$refpts, file=brps.file, sep=";", row.names=F, append=T, col.names=F)
  } else {
    write.table(fruns$refpts, file=brps.file, sep=";", row.names=F, append=F)
  }
  
}


# Fbar vs. %SPR & catch (plots)

brpsson_plot( read.csv(runs.file, sep=";"), pdfflnm=file.path(plot.dir,paste(stkn,sc,"_Fbar_vs_SPR.pdf",sep="")) )


# BRPS: 50% catches in each semester

aux <- read.csv(brps.file, sep=";")

# unique(aux$refpt)
aux$refpt <- factor( aux$refpt, 
                     levels = c("virgin", "msy", "f0.1", "spr.20", "spr.30", "spr.35", "spr.40", "spr.50", 
                                "F.20B0", "F.30B0", "F.35B0", "F.40B0", "F.50B0", "F.50R0", "F.90msy"))

aux <- aux %>% 
  group_by(refpt) %>% 
  filter(abs(pY_s1 - 0.5) == min(abs(pY_s1 - 0.5), na.rm = TRUE)) %>%
  arrange(refpt) %>% 
  filter(refpt != "virgin")

as.data.frame(aux)


## LOAD packages & model output ####
library(stockassessment)
library(ggplot2)
load("./output/output.RData")


## FORECAST settings ####

#  Reference points
F_MSY <- 0.21
F_MSY_lower <- 0.16
F_MSY_upper <- 0.314
Fpa <- 0.78
Flim <- 1.062
Blim <- 1670
Bpa <- 2322
MSY_Btrigger <- 2322

# TAC & advice for previous year
prev.advice <- 0 
prev.TAC <- 721

# Number of simulations
numberOfSims <- 1001
# Note - the number of iterations 10001, up from 1001.
# set to 101 for quick checking
# set to 1001 for routine checking
# set to 10001 preference


##  FORECAST assumptions ####

# Final year in the assessment
final.yr <- max(fit$data$years)

# Resample R from 2004 Rec onwards
R.range <-2004:(final.yr-1)

# Use five year average for mean weights & L-D partition 
av.yrs <- (final.yr-4):(final.yr)

# Use 5 year average selection patter
sel.yrs <- (final.yr-4):(final.yr)

# F status quo is last observed value
f_sq <- fbartable(fit)[as.character(final.yr), "Estimate"]

# F mean over selected years
f_mean <- mean(fbartable(fit)[as.character(sel.yrs),"Estimate"])

# Final assessment year F - needed to run the forecast through next year to get numbers/SSB at start of intermediate year
f_final <- f_sq



## dummy forecast
# 2022F=Fsq then F_MSY - run a dummy forecast to get the SSB at start of TAC year
# RNGkind(sample.kind = "Rounding")
set.seed(1337)
dummy.forecast <- forecast(fit,
                           fscale = c(NA,NA,NA,NA),
                           fval = c(f_final,f_sq,F_MSY,F_MSY),
                           year.base = final.yr,
                           label = "",
                           rec.years = R.range,
                           ave.years = av.yrs,
                           processNoiseF = FALSE,
                           overwriteSelYears = sel.yrs,
                           splitLD = TRUE,
                           nosim = numberOfSims,
                           savesim = TRUE)

# forecast proper
SSBnext <- attributes(dummy.forecast)$shorttab[3,3]

attributes(dummy.forecast)$tab # show int year low and high ssb and rec

scen <- list(
  "MSY approach: F_MSY" = list(fval = c(f_final, f_sq, F_MSY, F_MSY)),
  "Precautionary approach: Fpa" = list(fval = c(f_final, f_sq, Fpa, Fpa)),
  "FMSY upper" = list(fval = c(f_final, f_sq, F_MSY_upper, F_MSY_upper)),
  "FMSY lower" = list(fval = c(f_final, f_sq, F_MSY_lower, F_MSY_lower)),
  "F = 0" = list(fval = c(f_final, f_sq, NA, NA), fscale=c(NA,NA,0.0,0.0)),
  "F = Flim" = list(fval = c(f_final, f_sq, Flim, Flim)),
  "Fsq"  = list(fval = c(f_final, f_sq, f_sq, f_sq)),
  "Ftac"  = list(fval = c(f_final, f_sq, NA, NA), catchval = c(NA, NA, prev.TAC, prev.TAC)),
  # "0.05*Fsq" = list(fval = c(f_final, f_sq, 0.05*f_sq,0.05*f_sq)),
  # "0.25*Fsq" = list(fval = c(f_final, f_sq, 0.25*f_sq,0.25*f_sq)),
  # "0.5*Fsq" = list(fval = c(f_final, f_sq, 0.5*f_sq,0.5*f_sq)),
  # "0.75*Fsq" = list(fval = c(f_final, f_sq, 0.75*f_sq,0.75*f_sq)),
  "hit Blim" = list(fval = c(f_final, f_sq, NA,NA), nextssb = c(NA, NA, Blim, Blim)),
  # "hit Bpa" = list(fval = c(f_final, f_sq, NA,NA), nextssb = c(NA, NA, Bpa, Bpa)),
  # "hit MSY Btrigger" = list(fval = c(f_final, f_sq, NA, NA), nextssb = c(NA, NA, MSY_Btrigger, MSY_Btrigger)),
  # Hit MSYBtrigger not possible in 2025
    "SSB Y+2 = SSB Y+1"  = list(fval = c(f_final, f_sq, NA, NA), nextssb = c(NA, NA, SSBnext, SSBnext))
  
)


# Forecast storage list
FC <- list()
#FC <- vector("list", length(scen))
#names(FC) <- names(scen)

# RUN F scenarios ####
for(i in seq(scen)){
  
  # RNGkind(sample.kind = "Rounding")
  ARGS <- scen[[i]]
  ARGS <- c(ARGS,
            list(fit = fit, 
                 ave.years = av.yrs, 
                 rec.years = R.range, 
                 year.base = final.yr,
                 label = names(scen)[i], 
                 overwriteSelYears = sel.yrs, 
                 splitLD = TRUE,
                 processNoiseF = FALSE,
                 nosim = numberOfSims,
                 savesim = TRUE))  
  set.seed(1337)
  try(FC[[i]] <- do.call(forecast, ARGS))
  
  print(paste0("forecast : ", "'", names(scen)[i], "'", " is complete"))
}


# FUNCTION: ICES advice rule for MSY ####
MSYappr <- function(fit, 
                    Fmsy = NULL, 
                    Btrig = NULL, 
                    fscale_init = 1, 
                    fval_init =NA, 
                    catchval_init = NA, 
                    label = NA){
  
  fscale <- fscale_init
  fval <- fval_init
  catchval <- catchval_init
  
  fscale[2] <- fscale
  fval[2] <- fval
  catchval[2] <- catchval
  
  
  for (i in 3:4){
    
    fscale[i] <- NA
    fval[i] <- Fmsy
    catchval[i] <- NA
    
    set.seed(1337)
    f1 <- forecast(fit, fscale = fscale, fval = fval, catchval = catchval, ave.years = av.yrs, rec.years = R.range, overwriteSelYears = sel.yrs)
    SSB <- attr(f1, "shorttab")[3,i]
    
    fval[i] <- min(Fmsy*SSB/Btrig, Fmsy)
    
    # a <-  (F_MSY / (Btrig-Blim))
    # b <- -a*Blim
    # if(SSB < Btrig && SSB > Blim) fval[i] <- a*SSB + b
    # if(SSB < Blim) fval[i] <- 0.000001
  }
  
  set.seed(1337)
  f_final <- forecast(fit, 
                      fscale = fscale, 
                      fval = fval, 
                      catchval = catchval, 
                      ave.years = av.yrs, 
                      rec.years = R.range, 
                      label = label, 
                      overwriteSelYears = sel.yrs, 
                      splitLD = TRUE, 
                      nosim = numberOfSims,
                      savesim = TRUE)
  return(f_final)
}


# RUN ICES HCR for MSY ####
FC[[length(FC)+1]] <- MSYappr(fit, Fmsy = F_MSY, Btrig = MSY_Btrigger, fscale_init = NA, fval_init = f_sq, catchval_init = NA, label = paste0(final.yr, "F=Fsq then Fmsy HCR"))
FC[[length(FC)+1]] <- MSYappr(fit, Fmsy = F_MSY_lower, Btrig = MSY_Btrigger, fscale_init = NA, fval_init = f_sq, catchval_init = NA, label = paste0(final.yr, "F=Fsq then Fmsy HCR lower"))
FC[[length(FC)+1]] <- MSYappr(fit, Fmsy = F_MSY_upper, Btrig = MSY_Btrigger, fscale_init = NA, fval_init = f_sq, catchval_init = NA, label = paste0(final.yr, "F=Fsq then Fmsy HCR upper"))



save(FC, file = "./output/forecast.RData")

#### Forecast Outputs ####

# load("./output/forecast.RData")

## Standard forecast plots
# FMSY
jpeg("./report/FMSY_forecast.jpeg", height = 210, width = 210, units = "mm", res = 350)
plot(FC[[1]])
dev.off()

# FSQ
jpeg("./report/FSQ_forecast.jpeg", height = 210, width = 210, units = "mm", res = 350)
plot(FC[[7]])
dev.off()

# HCR
jpeg("./report/HCR_forecast.jpeg", height = 210, width = 210, units = "mm", res = 350)
plot(FC[[10]])
dev.off()

### Catch table with risk to blim (need to have defined SSBnext, prev.advice and Blim)

cs <- data.frame(Basis = NA,
                 catchTACyr = NA,
                 Landings = NA,
                 Discards = NA,
                 Ftotal = NA,
                 Fland = NA,
                 Fdiscard = NA,
                 ssbTACyrplus1 = NA,
                 ssbChange = NA,
                 adviceChange = NA,
                 Risk = NA)

# the 3rd year in the forecast is the advice year because in this stock the last year with data is included in the forecast
for (i in c(1:length(FC))){
  cs[i,1] = attributes(FC[[i]])$label
  cs[i,2] <- attr(FC[[i]],"shorttab")[4,3]
  cs[i,3] <- attr(FC[[i]],"shorttab")[7,3]
  cs[i,4] <- attr(FC[[i]],"shorttab")[8,3]
  cs[i,5] <- attr(FC[[i]],"shorttab")[1,3]
  cs[i,6] <- attr(FC[[i]],"shorttab")[5,3]
  cs[i,7] <- attr(FC[[i]],"shorttab")[6,3]
  cs[i,8] <- attr(FC[[i]],"shorttab")[3,4]
  cs[i,9] <- 100*attr(FC[[i]],"shorttab")[3,4]/SSBnext-100
  cs[i,10] <- 100*attr(FC[[i]],"shorttab")[4,3]/prev.advice-100
}
cs[,11] <- 100*unlist(lapply(FC, function(x) sum(x[[4]]$ssb < Blim) / length(x[[4]]$ssb)))

write.csv(cs, file = "./output/Catch_Option_Table.csv")




#### Contribution of recruitment assumption to catch and SSB ####
# FMSY approach
lapply(FC, function(x){return(attributes(x)$label)})
i <- 1
attr(FC[[i]], "shorttab")

# attributes(FC[[i]])$label
FC[[i]][[3]]$year
catch <- apply(FC[[i]][[3]]$catchatage, 1, median) * colMeans(tail(fit$data$catchMeanWeight,1))      #catchn
FC[[i]][[4]]$year
#SSB <- exp(apply(FC[[i]][[3]]$sim, 2, median)) [1:8]  * colMeans(tail(fit$data$catchMeanWeight * fit$data$propMat[-dim(fit$data$propMat)[1],],1))   #stockn


SSB <- exp(apply(FC[[i]][[4]]$sim, 2, median) [1:7])  * colMeans(tail(fit$data$catchMeanWeight[,,] * fit$data$propMat,1))   #stockn # pb edit


land <- apply(FC[[i]][[2]]$catchatage, 1, median)*colMeans(tail(fit$data$landMeanWeight,1))*colMeans(tail(fit$data$landFrac,1))
dis <- apply(FC[[i]][[2]]$catchatage, 1, median)*colMeans(tail(fit$data$disMeanWeight,1))*(1-colMeans(tail(fit$data$landFrac,1)))


year <- 2025
plot.data <- rbind(
  data.frame(recruitment = year + 1:-5, val = catch[,1], type = paste(year+1, "Catch")),
  data.frame(recruitment = year + 1:-5, val = SSB, type = paste(year+2, "SSB"))
)

plot.data$est <- plot.data$recruitment >= year

ggplot(plot.data, aes(x=factor(recruitment), y=val, fill=est)) +
  facet_wrap(~type, scales="free_y") + geom_col() +
  xlab("Recruitment year") +ylab("Contribution (tonnes)") +
  scale_fill_discrete(name="Forecast assumption")+ 
  theme_bw()+
  ggtitle("Forecast contribution") +
  theme(legend.position = "none")

ggsave("report/Forecast makeup.png",
       width = 20, height = 15, units = "cm", dpi = 300, type = "cairo-png")

# % contributions to SSB
rev(plot.data$val[plot.data$type == "2027 SSB"])[6]/sum(rev(plot.data$val[plot.data$type == "2027 SSB"]))
rev(plot.data$val[plot.data$type == "2027 SSB"])[7]/sum(rev(plot.data$val[plot.data$type == "2027 SSB"]))

# % contributions to Catch
rev(plot.data$val[plot.data$type == "2026 Catch"])[6]/sum(rev(plot.data$val[plot.data$type == "2026 Catch"]))
rev(plot.data$val[plot.data$type == "2026 Catch"])[7]/sum(rev(plot.data$val[plot.data$type == "2026 Catch"]))
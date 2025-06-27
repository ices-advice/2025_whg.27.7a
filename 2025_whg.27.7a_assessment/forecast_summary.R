# Process SAM forecast outputs ####

library(stockassessment)
library(TMB)
library(icesAdvice)
library(ggplot2)

rm(list=ls()) 

root.dir <- getwd()

#sourceDir("utils")

Resdir <- "output"

# load SAM outputs and FLStock object
load("./output/output.RData")
load("./output/forecast.RData")

# Load change in advice data
# adviceChange22 <- read.csv("./results/n-at-age_WGCSE_2022.csv")
# adviceChange23 <- read.csv("./results/n-at-age_WGCSE_2023.csv")
# adviceChangeN <- rbind(adviceChange22, adviceChange23)
# adviceChangeN$type <- as.factor(adviceChangeN$type)
# 
# adviceChange22 <- read.csv("./results/B-at-age_WGCSE_2022.csv")
# adviceChange23 <- read.csv("./results/B-at-age_WGCSE_2023.csv")
# adviceChangeB <- rbind(adviceChange22, adviceChange23)
# adviceChangeB$type <- as.factor(adviceChangeB$type)
# 
# adviceChange22 <- read.csv("./results/catch-at-age_WGCSE_2022.csv")
# adviceChange23 <- read.csv("./results/catch-at-age_WGCSE_2023.csv")
# adviceChangeC <- rbind(adviceChange22, adviceChange23)
# adviceChangeC$type <- as.factor(adviceChangeC$type)

# Check list object indices
lapply(FC, function(x){return(attributes(x)$label)})
attr(FC[[10]], "shorttab")


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
plot(FC[[11]])
dev.off()



# Some summary values
assessment_summary <- as.data.frame(summary(fit))
assessment_summary  #For Standard Graph input

# Previous Advice
prev.advice <- 0
prev.TAC <- 721


# Create the tables ####

# Final year of assessment
final.yr <- max(fit$data$years)
# If the assessment includes the int year (and this is the 1st year in forecast 
# then set int.yr=1)
int.yr <- 2

Table2 <- NULL
Table3 <- NULL
for (i in 1:length(FC)){#i=1
  Table2 <- rbind(Table2, data.frame(Basis = attributes(FC[[i]])$label,
                                     Ftot = attributes(FC[[i]])$shorttab[1,int.yr],
                                     SSB = attributes(FC[[i]])$shorttab[3,int.yr+1],
                                     R1 = attributes(FC[[i]])$shorttab[2,int.yr],
                                     R2 = attributes(FC[[i]])$shorttab[2,int.yr+1],
                                     TotCatch = attributes(FC[[i]])$shorttab[7,int.yr]+attributes(FC[[i]])$shorttab[8,int.yr],
                                     Landings = attributes(FC[[i]])$shorttab[7,int.yr],
                                     Discards = attributes(FC[[i]])$shorttab[8,int.yr]
  ))
  
  Table3 <- rbind(Table3, data.frame(Basis = attributes(FC[[i]])$label,
                                     TotCatch = attributes(FC[[i]])$shorttab[7,int.yr+1]+attributes(FC[[i]])$shorttab[8,int.yr+1],
                                     WCatch = attributes(FC[[i]])$shorttab[7,int.yr+1],
                                     UCatch = attributes(FC[[i]])$shorttab[8,int.yr+1],
                                     Ftot = attributes(FC[[i]])$shorttab[1,int.yr+1],
                                     Fw = attributes(FC[[i]])$shorttab[5,int.yr+1],
                                     Fu = attributes(FC[[i]])$shorttab[6,int.yr+1],
                                     SSB = attributes(FC[[i]])$shorttab[3,int.yr+2],
                                     SSBpc = NA,
                                     TACpc = NA,
                                     Advicepc = NA
                                     
  ))
}

# Calculate Discards
Table2$Discards <- Table2$TotCatch - Table2$Landings
Table2 # check rows are all the same

# Catch scenario summary table
Table3
Table3$Fu <- Table3$Ftot - Table3$Fw
Table3$UCatch <-Table3$TotCatch - Table3$WCatch
Table3$SSBpc <- 100 * (Table3$SSB/Table2$SSB -1)
Table3$TACpc <- 100 * (Table3$TotCatch/prev.TAC - 1)
Table3$Advicepc <- 100 * (Table3$TotCatch/prev.advice - 1)
# write.csv(Table2, file = "./results/Table2.csv")

# Create ICES rounded table
Tab3 <- Table3
Tab3$Ftot <- icesRound(Table3$Ftot)
Tab3$Fw <- icesRound(Table3$Fw)
Tab3$Fu <- icesRound(Table3$Fu)
Tab3$TotCatch <- round(Table3$TotCatch)
Tab3$WCatch <- round(Table3$WCatch)
Tab3$UCatch <- round(Table3$UCatch)
Tab3$SSB <- round(Table3$SSB)
Tab3$SSBpc <- icesRound(Table3$SSBpc,percent=TRUE,sign=FALSE)
Tab3$TACpc <- icesRound(Table3$TACpc,percent=TRUE,sign=FALSE)
# Tab3$Advicepc <- icesRound(Table3$Advicepc,percent=TRUE,sign=FALSE)
# write.csv(Tab3, file = "./output/Table3_Catch_Options.csv")

# Getting median values where rounding to 4 places is required
round(median(FC[[7]][[3]]$fbarL), 4)
round(median(FC[[12]][[3]]$fbarL), 4)


## Contribution of recruitment assumption to catch and SSB
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

# same as above plot but I like it less
# 
# jpeg("report/forecast_contribution.jpeg", 
#      height = 100, width = 250, units = "mm", res = 350)
# par(mfrow = c(1, 2), mar = c(0, 4.1, 1.1, 1.1), oma = c(5.1, 0, 0, 0))
# barplot(height = rev(plot.data$val[plot.data$type == "2026 Catch"]),
#         names.arg = rev(plot.data$recruitment[plot.data$type == "2026 Catch"]),
#         col = c("#56B4E9", "#E69F00")[as.factor(rev(plot.data$est[plot.data$type == "2026 Catch"]))],
#         ylab = "Contribution (tonnes)",
#         xpd = FALSE)
# legend("topright",
#        legend = "2026 Catch",
#        cex = 1.5,
#        text.font = 2,
#        bty = "n")
# barplot(height = rev(plot.data$val[plot.data$type == "2027 SSB"]),
#         names.arg = rev(plot.data$recruitment[plot.data$type == "2027 SSB"]),
#         col = c("#56B4E9", "#E69F00")[as.factor(rev(plot.data$est[plot.data$type == "2027 SSB"]))],
#         xpd = FALSE)
# legend("topleft",
#        legend = "2027 SSB",
#        cex = 1.5,
#        text.font = 2,
#        bty = "n")
# mtext("Recruitment Year", side = 1, outer = TRUE, line = 4, cex = 1.5)
# dev.off()

# % contributions to SSB
rev(plot.data$val[plot.data$type == "2027 SSB"])[6]/sum(rev(plot.data$val[plot.data$type == "2027 SSB"]))
rev(plot.data$val[plot.data$type == "2027 SSB"])[7]/sum(rev(plot.data$val[plot.data$type == "2027 SSB"]))

# % contributions to Catch
rev(plot.data$val[plot.data$type == "2026 Catch"])[6]/sum(rev(plot.data$val[plot.data$type == "2026 Catch"]))
rev(plot.data$val[plot.data$type == "2026 Catch"])[7]/sum(rev(plot.data$val[plot.data$type == "2026 Catch"]))


# Intermediate year table

int_table <- data.frame(F = attributes(FC[[1]])$shorttab[1,2],
                        SSB = attributes(FC[[1]])$shorttab[3,3],
                        R1 = attributes(FC[[1]])$shorttab[2,2],
                        TotCatch = attributes(FC[[1]])$shorttab[7,2]+attributes(FC[[1]])$shorttab[8,2],
                        Landings = attributes(FC[[1]])$shorttab[7,2],
                        Discards = attributes(FC[[1]])$shorttab[8,2])

write.csv(int_table, file = "./output/intermediate_year_table.csv")

# get some additional digits for advice rule roundings
median(FC[[11]][[3]]$fbarL)
median(FC[[4]][[3]]$fbar) - median(FC[[4]][[3]]$fbarL)
median(FC[[12]][[3]]$fbarL)
median(FC[[13]][[3]]$fbar) - median(FC[[12]][[3]]$fbarL)

# write data tables for report
fage <- data.frame(faytable(fit))
write.csv(fage, file = "./output/f_at_age_table.csv", row.names = TRUE)

sn <- ntable(fit)
sn <- ntable(FC[[1]])
write.csv(sn, file = "./output/Stock_numbers_table.csv", row.names = TRUE)

# LOAD pks & data ####
library(ggplot2)
library(icesSAG)
library(stockassessment)
library(tidyverse)
source("funcs.R")

## ◦ GET ASAP assessment output ####
sag23 <- read.csv("./data/sag2023.csv")


## ◦ DATA import ####
cn <- read.ices("./data/cn.dat")
cw <- read.ices("./data/cw.dat")
dw <- read.ices("./data/dw.dat")
lf <- read.ices("./data/lf.dat")
lw <- read.ices("./data/lw.dat")
mo <- read.ices("./data/mo.dat")
nm <- read.ices("./data/nm.dat")
pf <- read.ices("./data/pf.dat")
pm <- read.ices("./data/pm.dat")
sw <- read.ices("./data/sw.dat")
surveys <- read.ices("./data/survey_2025.dat")

# Survey CVs
scv <- stockassessment:::read.surveys("./data/WHG7aSurveyCv.txt")
W1 <- 1/log(scv[[1]]^2+1)
W2 <- 1/log(scv[[2]]^2+1)
attr(surveys[[1]],"weight") <- W1
attr(surveys[[2]],"weight") <- W2


# Create collected data object for SAM
dat <- setup.sam.data(surveys = surveys,
                      residual.fleet = cn, 
                      prop.mature = mo, 
                      stock.mean.weight = sw, 
                      catch.mean.weight = cw, 
                      dis.mean.weight = dw, 
                      land.mean.weight = lw,
                      prop.f = pf, 
                      prop.m = pm, 
                      natural.mortality = nm, 
                      land.frac = lf)


# CONFIGURE model ####

# Generate a default model configuration
# This configuration can be changed by modifying/overwriting the elements in the list. 
conf <- defcon(dat)

# Coupling of the fishing mortality states (normally only first row is used).
conf$keyLogFsta <- matrix(c(
  0,    1,    2,    3,    4,    5,    6,
  -1,   -1,   -1,   -1,   -1,   -1,   -1,
  -1,   -1,   -1,   -1,   -1,   -1,   -1,
  -1,   -1,   -1,   -1,   -1,   -1,   -1), nrow = 4, byrow=TRUE)

# Coupling of the survey catchability parameters 
# (normally first row is not used, as that is covered by fishing mortality).
conf$keyLogFpar <- matrix(c(-1,   -1,   -1,   -1,   -1,   -1,   -1,
                            -1,    0,    1,    1,    1,    1,    2,
                            3,    4,    4,    4,    5,    6,    7,
                            8,   -1,   -1,   -1,   -1,   -1,   -1), nrow = 4, byrow=TRUE)

# Covariance structure for each fleet ("ID" independent, "AR" AR(1), or "US" for unstructured).
# Possible values are: "ID" "AR" "US"
conf$obsCorStruct <- factor(c("ID", "AR", "AR", "ID"), levels = c("ID", "AR", "US"))

# Coupling of correlation parameters can only be specified if the AR(1) structure is chosen above.
# NA's indicate where correlation parameters can be specified (-1 where they cannot).
cns <- colnames(conf$keyCorObs)
conf$keyCorObs <- matrix(c(NA,  NA,  NA,  NA,  NA,  NA,
                           -1,   0,   1,   1,   2,   2,
                           3,   4,   4,   4,   4,   4,
                           -1,  -1,  -1,  -1,  -1,  -1), nrow = 4, byrow = TRUE)
colnames(conf$keyCorObs) <- cns

# lowest and highest age included in Fbar
conf$fbarRange <- c(1, 3)

# Save model configuration
saveConf(conf, "./output/model.cfg", overwrite = TRUE)
# loadConf("./input/model.cfg")




# PARAMETER initialisation ####
par <- defpar(dat, conf)
par$logSdLogN[2] <- log(0.05)


# FIT SAM, or load existing output ####
if(file.exists("./output/output.RData")){
  
  load("./output/output.RData")
  
} else {
  
  # Optimize the model fit
  set.seed(1337)
  fit <- sam.fit(dat, conf, par, map = list(logSdLogN = as.factor(c(1,NA))))
  
  # Calculate residuals, and fit retros & leave-one-out
  res <- residuals(fit)
  resp <- procres(fit)
  mretro <- retro(fit, year = 5)
  lo <- leaveout(fit)
  
  # Save Model Output & Diagnostics
  save(fit,
       res,
       resp,
       mretro,
       lo,
       file = "./output/output.RData")
  
}




# RESULTS ####

## • QUALITY ####
modeltable(fit)


## • SUMMARY of SAM fit ####
fitsum <- as.data.frame(cbind(summary(fit), catchtable(fit)))
fitsum$year <- as.numeric(rownames(fitsum))
names(fitsum) <- c("Rec","RecLow", "RecHigh", 
                   "SSB", "SSBLow", "SSBHigh", 
                   "Fbar", "FLow", "FHigh",     
                   "Catch", "CatchLow", "CatchHigh", "year")
write.table(fitsum, "./output/summary.txt", row.names = FALSE)
write.csv(fitsum, "./output/summary.csv", row.names = FALSE)


## • FITPLOT ####
jpeg("./report/fitplot.jpeg", height = 160, width = 300, units = "mm", res = 350)
fitplot(fit)
dev.off()


## • STOCK development ####
# SSB
plssb <- ggplot(sag23, aes(x = Year, y = SSB)) +
  geom_ribbon(aes(ymin = Low.SSB, ymax = High.SSB), fill = "#eaeaea") +
  geom_line(lty = 3) +
  geom_ribbon(aes(x = year, ymin = SSBLow, ymax = SSBHigh), 
              data = fitsum, fill = "#ffd2ae", alpha = 0.5) +
  geom_line(aes(x = year, y = SSB), data = fitsum, col = "#D55E00", lwd = 1.5) +
  labs(x = "Year", y = "SSB (t)") +
  theme_classic()
plssb

# F
plf <- ggplot(sag23, aes(x = Year, y = Fbar)) +
  geom_ribbon(aes(ymin = Low.F, ymax = High.F), fill = "#eaeaea") +
  geom_line(lty = 3) +
  geom_ribbon(aes(x = year, ymin = FLow, ymax = FHigh), 
              data = fitsum, fill = "#bee2f7", alpha = 0.5) +
  geom_line(aes(x = year, y = Fbar), data = fitsum, col = "#56B4E9", lwd = 1.5) +
  labs(x = "Year", y = expression(italic(bar("F"))["1-3"])) +
  theme_classic()
plf

# RECRUITMENT
plr <- ggplot(sag23, aes(x = Year, y = Recruitment)) +
  geom_ribbon(aes(ymin = Low.Recruitment, ymax = High.Recruitment), fill = "#eaeaea") +
  geom_line(lty = 3) +
  geom_ribbon(aes(x = year, y = Rec, ymin = RecLow, ymax = RecHigh), 
              data = fitsum, fill = "#eccedf", alpha = 0.5) +
  geom_line(aes(x = year, y = Rec), data = fitsum, col = "#CC79A7", lwd = 1.5) +
  labs(x = "Year", y = "Recruitment-at-age 0 ('000)") +
  theme_classic()
plr

# CATCH
plc <- ggplot(fitsum, aes(x = year, y = Catch)) +
  geom_ribbon(aes(ymin = CatchLow, ymax = CatchHigh), fill = "#b2ffea") +
  geom_line(aes(x = year, y = Catch, group = 1), col = "#009E73", lwd = 1.5) +
  geom_point(aes(x = Year, y = Catches), data = sag23, pch = 8, cex = 0.5) +
  labs(x = "Year", y = "Catch ('000 t)") +
  theme_classic()
plc

# EXPORT
s_graph <- ggpubr::ggarrange(plc, plr, plf, plssb,
                             labels = c("", "", "", ""),
                             ncol = 2, nrow = 2)
ggsave("./report/standard_graph.jpeg",
       height = 7, width = 9)


## • SELECTIVITY-at-age ####
jpeg(paste0("./report/Selectivity_at_age_rec.jpeg"), 
     height = 210, width = 210, units = "mm", res = 350)
matplot(fit$data$years, faytable(fit)/rowSums(faytable(fit)), 
        type = "b", lwd = 2, pch = 1:7, col = 1:7, 
        xlab = "Year", ylab = "Selectivity-at-age")
legend("topright", legend =colnames(faytable(fit)), 
       lwd = 2, col=1:7, pch=1:7, lty=1:5, ncol=3)
dev.off()


## • SRR ####
colfunc <- colorRampPalette(c("#7f7f7f", "#E69F00"))

### ~ FULL time series ####
jpeg("./report/srr_random_walk.jpeg", width = 15, height = 7.5, res = 300, units = "in")
par(mar = c(5.1, 4.1, 1.1, 1.1))
plot(fitsum$SSB/1e3, fitsum$Rec/1e3,
     type = "n",
     xlab = "SSB ('000 t)",
     ylab = "Recruitment Age 0 (millions)",
     col = "#000000",
     xlim = c(0, 70),
     ylim = c(0, 3000),
     lwd = 2)
segments(x0 = head(fitsum$SSB/1e3,-1),
         y0 = head(fitsum$Rec/1e3,-1),
         x1 = tail(fitsum$SSB/1e3,-1),
         y1 = tail(fitsum$Rec/1e3,-1),
         lwd = 2,
         col = colfunc(nrow(fitsum)-1))
points(fitsum$SSB/1e3, fitsum$Rec/1e3,
       col = colfunc(nrow(fitsum)),
       pch = 19)
plotrix::plotCI(fitsum$SSB/1e3, fitsum$Rec/1e3, 
                ui = fitsum$RecHigh/1e3, 
                li = fitsum$RecLow/1e3,
                col = paste0(colfunc(nrow(fitsum)), "70"),
                add = TRUE)
plotrix::plotCI(fitsum$SSB/1e3, fitsum$Rec/1e3, 
                ui = fitsum$SSBHigh/1e3, 
                li = fitsum$SSBLow/1e3,
                err = "x",
                col = paste0(colfunc(nrow(fitsum)), "70"),
                add = TRUE)
text(fitsum$SSB/1e3, fitsum$Rec/1e3, 
     substr(as.character(fitsum$year), 3, 4), 
     # col = colfunc(nrow(fitsum)),
     cex = 0.75,
     pos = 4)
dev.off()


### ~ TRUNCATED time series ####
fitsumshort <- fitsum[fitsum$year %in% c(1992:2023),]
jpeg("./report/srr_random_walk_short.jpeg", width = 15, height = 7.5, res = 300, units = "in")
par(mar = c(5.1, 4.1, 1.1, 1.1))
plot(fitsumshort$SSB/1e3, fitsumshort$Rec/1e3,
     type = "n",
     xlab = "SSB ('000 t)",
     ylab = "Recruitment Age 0 (millions)",
     col = "#000000",
     xlim = c(0, 12),
     ylim = c(170, 1200),
     lwd = 2)
segments(x0 = head(fitsumshort$SSB/1e3,-1),
         y0 = head(fitsumshort$Rec/1e3,-1),
         x1 = tail(fitsumshort$SSB/1e3,-1),
         y1 = tail(fitsumshort$Rec/1e3,-1),
         lwd = 2,
         col = colfunc(nrow(fitsumshort)-1))
points(fitsumshort$SSB/1e3, fitsumshort$Rec/1e3,
       col = colfunc(nrow(fitsumshort)),
       pch = 19)
plotrix::plotCI(fitsumshort$SSB/1e3, fitsumshort$Rec/1e3, 
                ui = fitsumshort$RecHigh/1e3, 
                li = fitsumshort$RecLow/1e3,
                col = paste0(colfunc(nrow(fitsumshort)), "70"),
                add = TRUE)
plotrix::plotCI(fitsumshort$SSB/1e3, fitsumshort$Rec/1e3, 
                ui = fitsumshort$SSBHigh/1e3, 
                li = fitsumshort$SSBLow/1e3,
                err = "x",
                col = paste0(colfunc(nrow(fitsumshort)), "70"),
                add = TRUE)
text(fitsumshort$SSB/1e3, fitsumshort$Rec/1e3, 
     substr(as.character(fitsumshort$year), 3, 4), 
     cex = 0.75,
     pos = 4)
dev.off()


## • RESIDUALS ####

### ~ OOA residuals ####
jpeg("./report/res.jpeg", height = 210, width = 210, units = "mm", res = 350)
plot(res)
dev.off()

### ~ PROCESS residuals ####
jpeg("./report/resp.jpeg", height = 210, width = 210, units = "mm", res = 350)
plot(resp)
dev.off()

### ~ FULL time series ####
jpeg("./report/residplot.jpeg", height = 400, width = 400, units = "mm", res = 350)
residplot(fit)
dev.off()


## • RETROSPECTIVE ####
(mrho <- mohn(mretro))

### ~ STANDARD plot ####
jpeg("./report/retro.jpeg", height = 210, width = 210, units = "mm", res = 350)
plot(mretro)
dev.off()


### ~ SHORT time series ####
# SSB
rssb <- lapply(mretro, ssbtable)
rssb <- lapply(rssb, as.data.frame)
rssb <- lapply(rssb, rownames_to_column, var = "year")
rssb <- rssb %>% 
  bind_rows(.id = "retro") %>%
  mutate(year = as.numeric(year))
ssbcols <- colorRampPalette(c("#D55E00", "#7f7f7f"))(length(mretro))
rlab <- paste("\u03C1 =", round(as.numeric(mrho["SSB"]), 3))
rssb <- ggplot(fitsum, aes(x = year, y = SSB)) +
  geom_ribbon(aes(ymin = SSBLow, ymax = SSBHigh), fill = "#eaeaea") +
  geom_line(lty = 3) +
  geom_line(aes(x = year, y = Estimate, colour = retro), 
            data = rssb, lwd = 1.5, show.legend = FALSE) + 
  scale_color_manual(values = sprintf(paste0(ssbcols, "%s"), seq(95, 75, length.out = length(mretro)))) +
  labs(x = "Year", y = "SSB (t)") +
  theme_classic() +
  scale_x_continuous(limits = c(2010, 2023)) +
  scale_y_continuous(limits = c(250, 2300)) + 
  annotate("text", x = Inf, y = Inf, label = rlab, hjust = 1, vjust = 1)
rssb

# F
rfbar <- lapply(mretro, fbartable)
rfbar <- lapply(rfbar, as.data.frame)
rfbar <- lapply(rfbar, rownames_to_column, var = "year")
rfbar <- rfbar %>% 
  bind_rows(.id = "retro") %>%
  mutate(year = as.numeric(year))
fcols <- colorRampPalette(c("#56B4E9", "#7f7f7f"))(length(mretro))
rlab <- paste("\u03C1 =", round(as.numeric(mrho["Fbar(1-3)"]), 2))
rfbar <- ggplot(fitsum, aes(x = year, y = Fbar)) +
  geom_ribbon(aes(ymin = FLow, ymax = FHigh), fill = "#eaeaea") +
  geom_line(lty = 3) +
  geom_line(aes(x = year, y = Estimate, colour = retro), 
            data = rfbar, lwd = 1.5, show.legend = FALSE) + 
  scale_color_manual(values = sprintf(paste0(fcols, "%s"), seq(95, 75, length.out = length(mretro)))) +
  labs(x = "Year", y = expression(italic(bar("F"))["1-3"])) +
  theme_classic() +
  scale_x_continuous(limits = c(2010, 2023)) +
  scale_y_continuous(limits = c(0.7, 1.6)) + 
  annotate("text", x = Inf, y = Inf, label = rlab, hjust = 1, vjust = 1)
rfbar

# Recruitment
rrec <- lapply(mretro, rectable)
rrec <- lapply(rrec, as.data.frame)
rrec <- lapply(rrec, rownames_to_column, var = "year")
rrec <- rrec %>% 
  bind_rows(.id = "retro") %>%
  mutate(year = as.numeric(year))
reccols <- colorRampPalette(c("#CC79A7", "#7f7f7f"))(length(mretro))
rlab <- paste("\u03C1 =", round(as.numeric(mrho["R(age 0)"]), 2))
rrec <- ggplot(fitsum, aes(x = year, y = Rec)) +
  geom_ribbon(aes(ymin = RecLow, ymax = RecHigh), fill = "#eaeaea") +
  geom_line(lty = 3) +
  geom_line(aes(x = year, y = Estimate, colour = retro), 
            data = rrec, lwd = 1.5) + 
  scale_color_manual(values = sprintf(paste0(reccols, "%s"), seq(95, 75, length.out = length(mretro)))) +
  labs(x = "Year", y = "Recruitment-at-age 0 ('000)") +
  theme_classic() +
  scale_x_continuous(limits = c(2010, 2023)) +
  scale_y_continuous(limits = c(2e5, 6e5)) + 
  annotate("text", x = Inf, y = Inf, label = rlab, hjust = 1, vjust = 1)
rrec

# EXPORT
short_retro <- ggpubr::ggarrange(rssb, rfbar, rrec,
                                 labels = c("", "", "", ""),
                                 ncol = 2, nrow = 2)
ggsave("./report/short_retro.jpeg",
       height = 7, width = 9)


## • LEAVE-ONE-OUT ####
jpeg("./report/loo.jpeg", height = 210, width = 210, units = "mm", res = 350)
plot(lo)
dev.off()


## • JITTER analysis ####
set.seed(1337)
(fit.jit <- jitfix(fit, nojit = 1e3))


## • SIMULATION validation ####
set.seed(17)
sim <- simstudy(fit , nsim = 20)
jpeg(paste0("./report/simstudy.jpeg"), 
     height = 210, width = 210, units = "mm", res = 350)
plot(sim)
dev.off()

# SSB plots
ggplot(fitsum, aes(year, SSB))+
  geom_line()+
  geom_hline(yintercept = 1670, lty = 5, colour = "red", linewidth = 1) +
  geom_hline(yintercept = 2322, lty = 3,colour = "orange", linewidth = 1) +
  geom_ribbon(aes(ymin = SSBLow, ymax = SSBHigh), alpha = 0.2)+
  ggtitle("WHG.27.7a SSB")
ggsave("./report/SSB.jpeg", height = 7, width = 9)

ggplot(fitsum %>% filter(year >1995), aes(year, SSB))+
  geom_line()+
  geom_hline(yintercept = 1670, lty = 5, colour = "red", linewidth = 2) +
  geom_hline(yintercept = 2322, lty = 3, colour = "orange", linewidth = 2) +
  geom_ribbon(aes(ymin = SSBLow, ymax = SSBHigh), alpha = 0.2)+
  ggtitle("WHG.27.7a SSB")
ggsave("./report/SSB short.jpeg", height = 7, width = 9)

# fishing mortality plots
ggplot(fitsum, aes(year, Fbar))+
  geom_line()+
  geom_hline(yintercept = 0.21, lty = 5, colour = "red", linewidth = 1) +
  geom_hline(yintercept = 0.78, lty = 3,colour = "orange", linewidth = 1) +
  geom_hline(yintercept = 1.062, lty = 3,colour = "green", linewidth = 1) +
  geom_ribbon(aes(ymin = FLow, ymax = FHigh), alpha = 0.2)+
  ggtitle("WHG.27.7a Fishing Mortality")
ggsave("./report/Fbar.jpeg", height = 7, width = 9)





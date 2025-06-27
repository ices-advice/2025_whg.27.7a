# LOAD pks & data ####
library(ggplot2); theme_set(theme_bw())
library(icesSAG)
library(stockassessment)
library(tidyverse)

options(scipen = 999)

last.yr <- 2024

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

#### cn plot

cn_df <- data.frame(Year = rownames(cn), as.data.frame(cn), row.names = NULL)

cn_df <- pivot_longer(
  cn_df, 
  cols = -Year,           # all columns except Year
  names_to = "Age",  # name for former column names
  values_to = "Value"     # name for the values
)
cn_df$Age <- sub("^X", "", cn_df$Age)

ggplot(cn_df, aes(x = Year, y = Value, colour = Age, group = Age)) +
  geom_line() +
  labs(title = "Catch numbers by age", x = "Year", y = "Catch numbers")+
  theme(axis.text = element_text(angle = 90, hjust = 1))
ggsave("report/Input_Plots/cn.png", width = 10, height = 6, dpi = 300)

ggplot(cn_df %>% filter (Year > last.yr - 10), aes(x = Year, y = Value, colour = Age, group = Age)) +
  geom_line() +
  labs(title = "Catch numbers by age", x = "Year", y = "Catch numbers")+
  theme(axis.text = element_text(angle = 90, hjust = 1))
ggsave("report/Input_Plots/cn_last10.png", width = 10, height = 6, dpi = 300)


#### Catch proportion by age

cn_prop <- cn_df %>% 
  group_by(Year) %>% 
  mutate(Total = sum(Value)) %>% 
  mutate(Prop = Value/Total)

ggplot(cn_prop, aes(x = Year, y = Prop, colour = Age, group = Age)) +
  geom_line() +
  labs(title = "Catch proportion by age", x = "Year", y = "Catch proportion")+
  theme(axis.text = element_text(angle = 90, hjust = 1))
ggsave("report/Input_Plots/cn_prop.png", width = 10, height = 6, dpi = 300)


ggplot(cn_prop %>% filter (Year > last.yr - 10), aes(x = Year, y = Prop, colour = Age, group = Age)) +
  geom_line() +
  labs(title = "Catch proportion by age", x = "Year", y = "Catch proportion")+
  theme(axis.text = element_text(angle = 90, hjust = 1))
ggsave("report/Input_Plots/cn_prop_last10.png", width = 10, height = 6, dpi = 300)

ggplot(cn_prop, aes(x = Year, y = Prop, fill = Age, group = Age)) +
  geom_bar(stat = "identity")+
  labs(title = "Catch proportion by age", x = "Year", y = "Catch proportion")+
  theme(axis.text = element_text(angle = 90, hjust = 1))
ggsave("report/Input_Plots/cn_prop_bar.png", width = 10, height = 6, dpi = 300)



#### cw plot

cw_df <- data.frame(Year = rownames(cw), as.data.frame(cw), row.names = NULL)

cw_df <- pivot_longer(
  cw_df, 
  cols = -Year,           # all columns except Year
  names_to = "Age",  # name for former column names
  values_to = "Value"     # name for the values
)
cw_df$Age <- sub("^X", "", cw_df$Age)

ggplot(cw_df, aes(x = Year, y = Value, colour = Age, group = Age)) +
  geom_line() +
  labs(title = "Catch weight by age", x = "Year", y = "Catch weight")+
  theme(axis.text = element_text(angle = 90, hjust = 1))
ggsave("report/Input_Plots/cw.png", width = 10, height = 6, dpi = 300)

ggplot(cw_df %>% filter (Year > last.yr - 10), aes(x = Year, y = Value, colour = Age, group = Age)) +
  geom_line() +
  labs(title = "Catch weight by age", x = "Year", y = "Catch weight")+
  theme(axis.text = element_text(angle = 90, hjust = 1))
ggsave("report/Input_Plots/cw_last10.png", width = 10, height = 6, dpi = 300)


#### dw plot

dw_df <- data.frame(Year = rownames(dw), as.data.frame(dw), row.names = NULL)

dw_df <- pivot_longer(
  dw_df, 
  cols = -Year,           # all columns except Year
  names_to = "Age",  # name for former column names
  values_to = "Value"     # name for the values
)
dw_df$Age <- sub("^X", "", dw_df$Age)

ggplot(dw_df, aes(x = Year, y = Value, colour = Age, group = Age)) +
  geom_line() +
  labs(title = "Discard weight by age", x = "Year", y = "Discard weight")+
  theme(axis.text = element_text(angle = 90, hjust = 1))
ggsave("report/Input_Plots/dw.png", width = 10, height = 6, dpi = 300)

ggplot(dw_df %>% filter (Year > last.yr - 10), aes(x = Year, y = Value, colour = Age, group = Age)) +
  geom_line() +
  labs(title = "Discard weight by age", x = "Year", y = "Discard weight")+
  theme(axis.text = element_text(angle = 90, hjust = 1))
ggsave("report/Input_Plots/dw_last10.png", width = 10, height = 6, dpi = 300)

#### lf plot

lf_df <- data.frame(Year = rownames(lf), as.data.frame(lf), row.names = NULL)

lf_df <- pivot_longer(
  lf_df, 
  cols = -Year,           # all columns except Year
  names_to = "Age",  # name for former column names
  values_to = "Value"     # name for the values
)
lf_df$Age <- sub("^X", "", lf_df$Age)

lf_df$DiscFrac = 1- lf_df$Value

ggplot(lf_df, aes(x = Year, y = Value, colour = Age, group = Age)) +
  geom_line() +
  labs(title = "Discard Fraction", x = "Year", y = "Landings")+
  theme(axis.text = element_text(angle = 90, hjust = 1))
ggsave("report/Input_Plots/lf.png", width = 10, height = 6, dpi = 300)

ggplot(lf_df %>% filter (Year > last.yr - 10), aes(x = Year, y = Value, colour = Age, group = Age)) +
  geom_line() +
  labs(title = "Discard Fraction", x = "Year", y = "Landings")+
  theme(axis.text = element_text(angle = 90, hjust = 1))
ggsave("report/Input_Plots/lf_last10.png", width = 10, height = 6, dpi = 300)

#### lw plot

lw_df <- data.frame(Year = rownames(lw), as.data.frame(lw), row.names = NULL)
lw_df <- pivot_longer(
  lw_df, 
  cols = -Year,           # all columns except Year
  names_to = "Age",  # name for former column names
  values_to = "Value"     # name for the values
)
lw_df$Age <- sub("^X", "", lw_df$Age)

ggplot(lw_df, aes(x = Year, y = Value, colour = Age, group = Age)) +
  geom_line() +
  labs(title = "Landings weight by age", x = "Year", y = "Landings weight")+
  theme(axis.text = element_text(angle = 90, hjust = 1))
ggsave("report/Input_Plots/lw.png", width = 10, height = 6, dpi = 300)

ggplot(lw_df %>% filter (Year > last.yr - 10), aes(x = Year, y = Value, colour = Age, group = Age)) +
  geom_line() +
  labs(title = "Landings weight by age", x = "Year", y = "Landings weight")+
  theme(axis.text = element_text(angle = 90, hjust = 1))
ggsave("report/Input_Plots/lw_last10.png", width = 10, height = 6, dpi = 300)

#### mo plot

mo_df <- data.frame(Year = rownames(mo), as.data.frame(mo), row.names = NULL)
mo_df <- pivot_longer(
  mo_df, 
  cols = -Year,           # all columns except Year
  names_to = "Age",  # name for former column names
  values_to = "Value"     # name for the values
)
mo_df$Age <- sub("^X", "", mo_df$Age)

ggplot(mo_df, aes(x = Year, y = Value, colour = Age, group = Age)) +
  geom_line() +
  labs(title = "Maturity ogive by age", x = "Year", y = "Maturity")+
  theme(axis.text = element_text(angle = 90, hjust = 1))
ggsave("report/Input_Plots/mo.png", width = 10, height = 6, dpi = 300)


#### sw plot

sw_df <- data.frame(Year = rownames(sw), as.data.frame(sw), row.names = NULL)
sw_df <- pivot_longer(
  sw_df, 
  cols = -Year,           # all columns except Year
  names_to = "Age",  # name for former column names
  values_to = "Value"     # name for the values
)
sw_df$Age <- sub("^X", "", sw_df$Age)

ggplot(sw_df, aes(x = Year, y = Value, colour = Age, group = Age)) +
  geom_line() +
  labs(title = "Survey weight by age", x = "Year", y = "Survey weight")+
  theme(axis.text = element_text(angle = 90, hjust = 1))
ggsave("report/Input_Plots/sw.png", width = 10, height = 6, dpi = 300)

ggplot(sw_df %>% filter (Year > last.yr - 10), aes(x = Year, y = Value, colour = Age, group = Age)) +
  geom_line() +
  labs(title = "Survey weight by age", x = "Year", y = "Survey weight")+
  theme(axis.text = element_text(angle = 90, hjust = 1))
ggsave("report/Input_Plots/sw_last10.png", width = 10, height = 6, dpi = 300)


# forecast assumption exploration

# calculate the mean of the last values in each column
# 
# mo_opt <- data.frame("Years_5" = colMeans(tail(mo, 5)),
#                      "Years_3" = colMeans(tail(mo, 3)),
#                      "Years_1" = colMeans(tail(mo, 1)))
# 
# ggplot(mo_df %>% filter (Year > last.yr - 5), aes(x = Year, y = Value, colour = Age, group = Age)) +
#   geom_line() +
#   labs(title = "Maturity ogive by age", x = "Year", y = "Maturity")+
#   theme(axis.text = element_text(angle = 90, hjust = 1))
# 
# sw_opt <- data.frame("Years_5" = colMeans(tail(sw, 5)),
#                      "Years_3" = colMeans(tail(sw, 3)),
#                      "Years_1" = colMeans(tail(sw, 1)))
# 
# ggplot(sw_df %>% filter (Year > last.yr - 5), aes(x = Year, y = Value, colour = Age, group = Age)) +
#   geom_line() +
#   labs(title = "Stock weight by age", x = "Year", y = "Survey weight")+
#   theme(axis.text = element_text(angle = 90, hjust = 1))
# 
# 
# cw_opt <- data.frame("Years_5" = colMeans(tail(cw, 5)),
#                      "Years_3" = colMeans(tail(cw, 3)),
#                      "Years_1" = colMeans(tail(cw, 1)))
# 
# ggplot(cw_df %>% filter (Year > last.yr - 5), aes(x = Year, y = Value, colour = Age, group = Age)) +
#   geom_line() +
#   labs(title = "Catch weight by age", x = "Year", y = "Catch weight")+
#   theme(axis.text = element_text(angle = 90, hjust = 1))
# 
# library(kableExtra)
# kable(mo_opt, digits =3)
# kable(cw_opt, digits =3)
# kable(sw_opt, digits =3)

q1 <- surveys[[1]]
q4 <- surveys[[2]]
mik <- surveys[[3]]

NIGFS_q1 <- data.frame(Year = rownames(q1),
                       A1 = q1[, 1],
                       A2 = q1[, 2],
                       A3 = q1[, 3],
                       A4 = q1[, 4],
                       A5 = q1[, 5],
                       A6 = q1[, 6],
                       Survey = "NIGFS_q1")
# long format
NIGFS_q1_long <- NIGFS_q1 %>%
  pivot_longer(cols = -c(Year, Survey),
               names_to = "Age",
               values_to = "Abundance") 

NIGFS_q4 <- data.frame(Year = rownames(q4),
                       A0 = q4[, 1],
                       A1 = q4[, 2],
                       A2 = q4[, 3],
                       A3 = q4[, 4],
                       A4 = q4[, 5],
                       A5 = q4[, 6],
                       A6 = q4[, 7],
                       Survey = "NIGFS_q4")

# long format
NIGFS_q4_long <- NIGFS_q4 %>%
  pivot_longer(cols = -c(Year, Survey),
               names_to = "Age",
               values_to = "Abundance")

mik <- data.frame(Year = rownames(mik),
                  A0 = mik[, 1],
                  Survey = "NI_mik")
# long format
mik_long <- mik %>%
  pivot_longer(cols = -c(Year, Survey),
               names_to = "Age",
               values_to = "Abundance")

surveys_long <- rbind(NIGFS_q1_long, NIGFS_q4_long, mik_long)

# standardised index

df_std <- surveys_long %>%
  group_by(Age, Survey) %>%
  mutate(std_data = Abundance / mean(Abundance, na.rm = TRUE)) %>%
  ungroup()

df_std$Year <- as.numeric(df_std$Year)

ggplot(df_std, aes(x = Year, y = std_data, group = Age, color = Age)) +
  geom_line() +
  facet_wrap(~ Survey) +
  labs(x = "Year",
       y = "Standardised Abundance") +
  scale_x_continuous(breaks = seq(1995, 2025, by = 5))+
  theme(axis.text = element_text(angle = 90, hjust = 1))

ggsave("report/Input_Plots/age_composition.png", width = 10, height = 6)

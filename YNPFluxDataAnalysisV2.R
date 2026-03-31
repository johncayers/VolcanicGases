library(tidyverse)
library(EnvStats)
library(psych)
library(ggrepel)
setwd(file.path("c:", "Users", "ayersj", "Box Sync", "Personal", "Projects", "VolcanicGases", "Yellowstone", "Data", "Summary_Data"))

# Picarro Gas Flux Measurements -------------------------------------------

Picarro <- read.csv("YNP_Picarro_Data_2018.csv")
Picarro$Site <- factor(Picarro$Site)
hist(Picarro$CO2_Flux, xlab = "CO2 Flux (g m-2 h-1)", main = "Histogram of Picarro 2018 CO2 Fluxes")
hist(Picarro$CH4_Flux, xlab = "CH4 Flux (g m-2 h-1)", main = "Histogram of Picarro 2018 CH4 Fluxes")
# Shift negative flux values - see https://en.wikipedia.org/wiki/Probability_distribution_fitting#Shifting_of_distributions
Picarro_CH4_Flux_Shift <- 1e-2 - min(Picarro$CH4_Flux)
Picarro$CH4_Flux_Shift <- Picarro$CH4_Flux + Picarro_CH4_Flux_Shift
# Make Boxcox plots using EnvStats (see Millard 2013 pp. 75-77) to ID distribution type 
boxcox.list <- boxcox(Picarro$CH4_Flux_Shift)
plot(boxcox.list, main = "CH4 Flux (g m-2 h-1)")
# Probability Plot Correlation Coefficient for CH4 peaks at lambda = 0, indicating a log transform
Picarro_CO2_Flux_Shift <- 1e-2 - min(Picarro$CO2_Flux)
Picarro$CO2_Flux_Shift <- Picarro$CO2_Flux + Picarro_CO2_Flux_Shift
boxcox.list <- boxcox(Picarro$CO2_Flux_Shift)
plot(boxcox.list, main = "CO2 Flux (g m-2 h-1)")
# Probability Plot Correlation Coefficient for CO2 peaks at a value of lambda = 0, indicating a log transform is appropriate for CO2.
Picarro$LogCO2_Flux_Shift <- log10(Picarro$CO2_Flux_Shift)
Picarro$LogCH4_Flux_Shift <- log10(Picarro$CH4_Flux_Shift)
# One-Sample Goodness of Fit Tests for Normality (Millard 2013 sect. 7.2.1)
# Note that estimate of mean shifted
Flux.lnorm <- gofTest(Picarro$LogCO2_Flux_Shift)
Flux.lnorm
# P < 0.05, so reject the null hypothesis that log10 CO2 Flux distribution is normal
plot(Flux.lnorm, digits = 3)
Flux.lnorm <- gofTest(Picarro$LogCH4_Flux_Shift)
Flux.lnorm
# P < 0.05, so reject the null hypothesis that log10 CH4 Flux distribution is normal
# Note that null hypothesis of normal distribution is nearly always rejected for large sample sizes (Millard 2013, pg. 150) 
plot(Flux.lnorm, digits = 3)
qplot(Picarro$CO2_Flux, Picarro$CH4_Flux)
# CO2 and CH4 fluxes not correlated
p = ggplot(Picarro, aes(x = Site, y = Picarro$LogCO2_Flux_Shift)) +
  geom_boxplot(na.rm = TRUE, alpha = 0.3) +
  theme_bw() +
  scale_fill_grey(start = 0, end = .9) +
  labs(y = "log10 fCO2 (mg m-2 h-1)", caption = "Picarro 2018 data - shifted values")
print(p)
ggsave("log10fCO2BoxplotPicarro2018.png", plot = p)
p = ggplot(Picarro, aes(x = Site, y = Picarro$LogCH4_Flux_Shift)) +
  geom_boxplot(na.rm = TRUE, alpha = 0.3) +
  theme_bw() +
  scale_fill_grey(start = 0, end = .9) +
  labs(y = "log10 fCH4 (mg m-2 h-1)", caption = "Picarro 2018 data - shifted values")
print(p)
ggsave("log10fCH4BoxplotPicarro2018.png", plot = p)
describe(Picarro$CO2_Flux)
describe(Picarro$CH4_Flux)
Fluxes <- select(Picarro, Site, CO2_Flux, CO2_Flux_Shift, CH4_Flux, CH4_Flux_Shift)
FluxesSumm <- Fluxes %>%
  group_by(Site) %>%
  dplyr::summarize(
    count = n(),
    avg = round(mean(CO2_Flux, na.rm = TRUE), digits = 3),
    stdev = round(sd(CO2_Flux, na.rm = TRUE), digits = 3),
    min = min(CO2_Flux, na.rm = TRUE),
    max = max(CO2_Flux, na.rm = TRUE),
    geomean = geoMean(CO2_Flux_Shift) - Picarro_CO2_Flux_Shift,
    geosd = geoSD(CO2_Flux_Shift)
  )
# Summary of CO2 Fluxes
FluxesSumm
write.csv(FluxesSumm, file = "SummaryCO2SiteFluxesPicarro2018.csv")
FluxesSumm <- Fluxes %>%
  group_by(Site) %>%
  dplyr::summarize(
    count = n(),
    avg = round(mean(CH4_Flux, na.rm = TRUE), digits = 3),
    stdev = round(sd(CH4_Flux, na.rm = TRUE), digits = 3),
    min = min(CH4_Flux, na.rm = TRUE),
    max = max(CH4_Flux, na.rm = TRUE),
    geomean = geoMean(CH4_Flux_Shift) - Picarro_CO2_Flux_Shift,
    geosd = geoSD(CH4_Flux_Shift)
  )
# Summary of CH4 Fluxes
FluxesSumm
write.csv(FluxesSumm, file = "SummaryCH4SiteFluxesPicarro2018.csv")
Picarro$SiteAndNumber <- paste(Picarro$Site, Picarro$Site_Number, sep = "_")

# EGM CO2 Flux Measurements -----------------------------------------------

EGMCO2Fluxes <- read.csv("EGM_CO2FluxesAllData.csv")
EGMCO2Fluxes$Site <- factor(EGMCO2Fluxes$Site)
hist(EGMCO2Fluxes$fCO2, xlab = "CO2 Flux (g m-2 h-1)", main = "Histogram of EGM CO2 Fluxes")
# Make Boxcox plots for fCO2 using EnvStats (see Millard 2013 pp. 75-77) to ID distribution type 
EGM_CO2_Flux_Shift <- 1e-2 - min(EGMCO2Fluxes$fCO2)
EGMCO2Fluxes$fCO2_Shift <- EGMCO2Fluxes$fCO2 + EGM_CO2_Flux_Shift
boxcox.list <- boxcox(EGMCO2Fluxes$fCO2_Shift)
plot(boxcox.list, main = "CO2 Flux (g m-2 h-1)")
# Probability Plot Correlation Coefficient for CO2 peaks at a value of lambda = 0, indicating a log transform is appropriate.
EGMCO2Fluxes$log10fCO2_Shift <- log10(EGMCO2Fluxes$fCO2_Shift)
Flux.lnorm <- gofTest(EGMCO2Fluxes$log10fCO2_Shift)
Flux.lnorm
# P < 0.05, so reject the null hypothesis that log10 CO2 Flux distribution is normal
plot(Flux.lnorm, digits = 3)
# Since no other data transformation is better, use nonparametric tests on log transformed values.
describe(EGMCO2Fluxes$fCO2, na.rm = TRUE)
EGMCO2Fluxes$Year <- factor(EGMCO2Fluxes$Year)
p = ggplot(EGMCO2Fluxes, aes(x = Site, y = log10fCO2_Shift, fill = Year, position = Year)) +
  geom_boxplot(na.rm = TRUE, alpha = 0.3) +
  theme_bw() +
  scale_fill_grey(start = 0, end = .9) +
  labs(y = "log fCO2 (g m-2 h-1)", caption = "EGM 2018-2019 shifted values")
print(p)
ggsave("log10fCO2BoxplotEGM2018-19.png", plot = p)
# CO2 Fluxes by Site
FluxesSumm <- EGMCO2Fluxes %>%
  group_by(Site) %>%
  dplyr::summarize(
    count = n(),
    avg = round(mean(fCO2, na.rm = TRUE), digits = 3),
    stdev = round(sd(fCO2, na.rm = TRUE), digits = 3),
    min = min(fCO2, na.rm = TRUE),
    max = max(fCO2, na.rm = TRUE),
    geomean = geoMean(fCO2_Shift) - EGM_CO2_Flux_Shift,
    geosd = geoSD(fCO2_Shift)
  )
FluxesSumm
write.csv(FluxesSumm, file = "SummaryCO2SiteEGM_Fluxes.csv")

# Compare Picarro and EGM CO2 fluxes --------------------------------------

EGMCO2Fluxes$SiteAndNumber <- paste(EGMCO2Fluxes$Site, EGMCO2Fluxes$Site_Number, sep = "_")
EGMCO2FluxAvg <- EGMCO2Fluxes %>%
  group_by(SiteAndNumber) %>%
  dplyr::summarize(
    geomeanEGMfCO2 = geoMean(fCO2_Shift, na.rm = TRUE) - EGM_CO2_Flux_Shift,
    geosdEGMfCO2 = geoSD(fCO2_Shift, na.rm = TRUE),
    )
PicarroCO2Flux <- select(Picarro, SiteAndNumber, CO2_Flux)
# Convert Picarro CO2 fluxes to g m-2 h-1.
PicarroCO2Flux$CO2_Flux = PicarroCO2Flux$CO2_Flux/1000
# Combine datasets using inner join with key = "SiteAndNumber"
CO2Fluxes <- inner_join(EGMCO2FluxAvg, PicarroCO2Flux, by = "SiteAndNumber")
fitparameters <- coefficients(lm(log10(CO2_Flux) ~ log10(geomeanEGMfCO2), data = CO2Fluxes))
fitparameters <- round(fitparameters, digits = 3)
names(fitparameters) <- NULL
# Error bars not plotted because xmin formula produced mostly NAN values
p = ggplot(CO2Fluxes, aes(x = log10(geomeanEGMfCO2), y = log10(CO2_Flux))) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  theme_bw() +
  geom_smooth(method = 'lm', formula = y ~ x, color = "black") +
  labs(x = "Picarro log10 CO2 flux (g m-2 h-1)") +
  labs(y = "EGM-5 log10 CO2 flux (g m-2 h-1)") +
  scale_x_continuous(limits = c(-2.5, 1.5)) +
  scale_y_continuous(limits = c(-2.5, 1.5)) +
  # geom_label_repel(aes(label = SiteAndNumber)) +
  labs(subtitle = str_c("y = ", fitparameters[2], "*x +", fitparameters[1]))
print(p)
ggsave("EGMVersusPicarroCO2FluxComparison.png", plot = p)
             
# ============================================================
# AJR Empirical Analysis
# Relationship between institutions, settler mortality and GDP
# Structure follows the PDF questions:
#   Question 1: Baseline relationship and OLS
#   Question 2: Robust standard errors and Breusch-Pagan test
#   Question 3: OLS with additional controls
#   Question 4: Endogeneity discussion only, no code
#   Question 5: IV strategy, first stage, reduced form and 2SLS
#   Question 6: Robustness checks and additional controls
# ============================================================


# ============================================================
# 0. SETUP
# ============================================================

rm(list = ls())        # clear all objects from the R environment
graphics.off()         # close all open plots

# Required packages
library(AER)           # ivreg() for instrumental-variable regressions
library(ggplot2)       # graphs
library(lmtest)        # Breusch-Pagan test
library(sandwich)      # robust standard errors
library(stargazer)     # regression tables
library(modelsummary)  # optional table package
library(openxlsx)      # read Excel files

# Set working directory to the folder containing this script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  # useful when running from RStudio

# Load dataset
AJR <- read.xlsx("AJR.xlsx")  # AJR data file must be in the same folder as this script

# Quick checks
head(AJR)      # preview first rows
names(AJR)     # check variable names


# ============================================================
# 1. QUESTION 1: BASELINE RELATIONSHIP BETWEEN RISK AND LOGGDP
# ============================================================

# ------------------------------------------------------------
# 1.1 Build continent variable for the scatter plot
# ------------------------------------------------------------

AJR$africa  <- ifelse(is.na(AJR$africa),  0, AJR$africa)   # replace missing Africa dummy with 0
AJR$asia    <- ifelse(is.na(AJR$asia),    0, AJR$asia)     # replace missing Asia dummy with 0
AJR$neoeuro <- ifelse(is.na(AJR$neoeuro), 0, AJR$neoeuro)  # replace missing Neo-Europe dummy with 0

AJR$continent <- "other"                         # default group
AJR$continent[AJR$africa == 1]  <- "africa"      # classify African countries
AJR$continent[AJR$asia == 1]    <- "asia"        # classify Asian countries
AJR$continent[AJR$neoeuro == 1] <- "neoeuro"     # classify Neo-European countries

AJR$continent <- factor(
  AJR$continent,
  levels = c("africa", "asia", "neoeuro", "other")  # keep legend/order stable
)

# ------------------------------------------------------------
# 1.2 Figure 1: Scatter plot of risk and log GDP
# ------------------------------------------------------------

ggplot(AJR, aes(x = risk, y = loggdp)) +                         # risk on x-axis, loggdp on y-axis
  geom_point(aes(color = continent), alpha = 0.8, size = 2.2) +  # points by continent
  geom_smooth(method = "lm", se = FALSE, color = "black",
              linewidth = 1) +                                  # fitted OLS line
  scale_color_manual(
    values = c(
      "africa"  = "green",
      "asia"    = "blue",
      "neoeuro" = "red",
      "other"   = "#333333"
    )
  ) +
  labs(
    x = "risk",
    y = "log gdp",
    color = "continent"
  ) +
  theme_minimal()                                                # clean plot theme

# ------------------------------------------------------------
# 1.3 Table 1: Baseline OLS regression
# Model: loggdp = alpha + beta*risk + u
# ------------------------------------------------------------

reg1 <- lm(loggdp ~ risk, data = AJR)  # baseline OLS model
summary(reg1)                          # full R output

stargazer(
  reg1,
  type = "text",                       # print table in console
  title = "Table 1: OLS regression of log GDP per capita on institutional risk",
  dep.var.labels = "Log GDP per capita",
  covariate.labels = c("Risk"),
  omit.stat = c("ser")            # omit F-statistic and residual SE from this table
)


# ============================================================
# 2. QUESTION 2: ROBUST STANDARD ERRORS AND HETEROSKEDASTICITY
# ============================================================

# ------------------------------------------------------------
# 2.1 Compare conventional OLS SE with robust HC0 SE
# ------------------------------------------------------------

se_ols    <- sqrt(diag(vcov(reg1)))                  # conventional OLS standard errors
se_robust <- sqrt(diag(vcovHC(reg1, type = "HC0")))  # heteroskedasticity-robust SE

stargazer(
  reg1, reg1,                                      # same model shown twice
  type = "text",
  se = list(se_ols, se_robust),                    # column 1: OLS SE, column 2: robust SE
  column.labels = c("OLS SE", "Robust SE (HC0)"),
  title = "Table 2: OLS regression with conventional and robust standard errors",
  dep.var.labels = "Log GDP per capita",
  covariate.labels = c("Risk"),
  omit.stat = c("ser"),
  notes = "Column (1) reports OLS standard errors. Column (2) reports heteroskedasticity-robust standard errors (HC0)."
)

# ------------------------------------------------------------
# 2.2 Table 3: Breusch-Pagan test for heteroskedasticity
# ------------------------------------------------------------

bptest(reg1)  # null hypothesis: homoskedasticity


# ============================================================
# 3. QUESTION 3: OLS WITH LATITUDE AND AFRICA DUMMY
# ============================================================

# ------------------------------------------------------------
# 3.1 Estimate baseline and controlled OLS models
# ------------------------------------------------------------

reg1 <- lm(loggdp ~ risk, data = AJR)                     # baseline model
reg2 <- lm(loggdp ~ risk + latitude + africa, data = AJR) # add geography and Africa dummy

summary(reg2)  # inspect controlled model

# ------------------------------------------------------------
# 3.2 Table 4: Compare OLS specifications
# ------------------------------------------------------------

stargazer(
  reg1, reg2,
  type = "text",
  title = "Table 4: OLS regression of loggdp",
  dep.var.labels = "Log GDP per capita",
  covariate.labels = c("Risk", "Latitude", "Africa dummy"),
  omit.stat = c("ser")
)


# ============================================================
# 4. QUESTION 4: ENDOGENEITY OF INSTITUTIONS
# ============================================================

# No code is required here.
# This question is discussed conceptually in the PDF:
# reverse causality, omitted variables, measurement error,
# perception bias and selection bias.


# ============================================================
# 5. QUESTION 5: IV STRATEGY USING LOG SETTLER MORTALITY
# ============================================================

# ------------------------------------------------------------
# 5.1 OLS benchmark
# ------------------------------------------------------------

ols1 <- lm(loggdp ~ risk + latitude + africa, data = AJR)  # OLS version of the IV model
summary(ols1)

# ------------------------------------------------------------
# 5.2 Table 5: First stage
# Model: risk = alpha + beta*logmort + controls + v
# ------------------------------------------------------------

fs <- lm(risk ~ logmort + latitude + africa, data = AJR)  # first-stage regression
summary(fs)                                               # check instrument relevance

stargazer(
  fs,
  type = "text",
  title = "Table 5: First stage: determinants of institutions (risk)",
  dep.var.labels = "risk",
  covariate.labels = c("Log settler mortality", "Latitude", "Africa dummy"),
  omit.stat = c("ser")
)

# ------------------------------------------------------------
# 5.3 Table 6: Reduced form
# Model: loggdp = alpha + beta*logmort + controls + e
# ------------------------------------------------------------

redform <- lm(loggdp ~ logmort + latitude + africa, data = AJR)  # reduced-form regression
summary(redform)                                                 # direct link between instrument and outcome

stargazer(
  redform,
  type = "text",
  title = "Table 6: Reduced form: effect of settler mortality on income",
  dep.var.labels = "log(gdp)",
  covariate.labels = c("Log settler mortality", "Latitude", "Africa dummy"),
  omit.stat = c("ser")
)

# ------------------------------------------------------------
# 5.4 Tables 7 and 8: Instrumental variable regression
# Instrument risk with log settler mortality
# ------------------------------------------------------------

regiv1 <- ivreg(
  loggdp ~ risk + latitude + africa |       # second-stage equation
    logmort + latitude + africa,            # instruments and exogenous controls
  data = AJR
)

summary(regiv1)                             # main 2SLS output
summary(regiv1, diagnostics = TRUE)         # weak instrument and Wu-Hausman tests

stargazer(
  ols1, regiv1,
  type = "text",
  column.labels = c("OLS", "2SLS"),
  title = "Table 7: Dependent variable loggdp",
  dep.var.labels = "loggdp",
  covariate.labels = c("Risk", "Latitude", "Africa dummy"),
  omit.stat = c("ser")
)


# ============================================================
# 6. QUESTION 6: ROBUSTNESS CHECKS AND ADDITIONAL CONTROLS
# ============================================================

# ------------------------------------------------------------
# 6.1 Clean malaria variable
# The dataset stores missing malaria values as "."
# ------------------------------------------------------------

AJR$malfal94 <- as.character(AJR$malfal94)  # convert to character to replace "."
AJR$malfal94[AJR$malfal94 == "."] <- NA     # define "." as missing
AJR$malfal94 <- as.numeric(AJR$malfal94)    # convert malaria variable back to numeric


# ------------------------------------------------------------
# 6.2 Specification 1: Risk + latitude
# ------------------------------------------------------------

AJR_10 <- AJR[
  complete.cases(AJR[, c("loggdp", "risk", "logmort", "latitude")]),  # keep complete observations
]

ols_10 <- lm(loggdp ~ risk + latitude, data = AJR_10)  # OLS comparison
fs_10  <- lm(risk ~ logmort + latitude, data = AJR_10) # first stage

iv_10 <- ivreg(
  loggdp ~ risk + latitude |       # outcome equation
    logmort + latitude,            # risk instrumented by logmort
  data = AJR_10
)

summary(iv_10, diagnostics = TRUE)  # IV output with diagnostic tests

stargazer(
  ols_10, iv_10,
  type = "text",
  column.labels = c("OLS", "2SLS"),
  title = "Specification 1: loggdp on risk and latitude"
)


# ------------------------------------------------------------
# 6.3 Specification 2: Risk + malaria
# ------------------------------------------------------------

AJR_11 <- AJR[
  complete.cases(AJR[, c("loggdp", "risk", "logmort", "malfal94")]),  # drop missing malaria
]

ols_11 <- lm(loggdp ~ risk + malfal94, data = AJR_11)   # OLS with malaria
fs_11  <- lm(risk ~ logmort + malfal94, data = AJR_11)  # first stage with malaria

iv_11 <- ivreg(
  loggdp ~ risk + malfal94 |       # include malaria as control
    logmort + malfal94,            # instrument risk using logmort
  data = AJR_11
)

summary(iv_11, diagnostics = TRUE)

stargazer(
  ols_11, iv_11,
  type = "text",
  column.labels = c("OLS", "2SLS"),
  title = "Specification 2: loggdp on risk and malaria"
)


# ------------------------------------------------------------
# 6.4 Specification 3: Risk + latitude + malaria
# ------------------------------------------------------------

AJR_12 <- AJR[
  complete.cases(AJR[, c("loggdp", "risk", "logmort", "latitude", "malfal94")]),  # complete sample
]

ols_12 <- lm(loggdp ~ risk + latitude + malfal94, data = AJR_12)   # OLS with both controls
fs_12  <- lm(risk ~ logmort + latitude + malfal94, data = AJR_12)  # first stage with both controls

iv_12 <- ivreg(
  loggdp ~ risk + latitude + malfal94 |       # second stage
    logmort + latitude + malfal94,            # first stage instruments
  data = AJR_12
)

summary(iv_12, diagnostics = TRUE)

stargazer(
  ols_12, iv_12,
  type = "text",
  column.labels = c("OLS", "2SLS"),
  title = "Specification 3: loggdp on risk, latitude and malaria"
)


# ------------------------------------------------------------
# 6.5 Specification 4: Risk + continent dummies
# ------------------------------------------------------------

AJR_13 <- AJR[
  complete.cases(AJR[, c("loggdp", "risk", "logmort", "africa", "asia", "other")]),  # complete sample
]

ols_13 <- lm(loggdp ~ risk + africa + asia + other, data = AJR_13)   # OLS with region dummies
fs_13  <- lm(risk ~ logmort + africa + asia + other, data = AJR_13)  # first stage with dummies

iv_13 <- ivreg(
  loggdp ~ risk + africa + asia + other |       # include continent dummies
    logmort + africa + asia + other,            # instrument risk with logmort
  data = AJR_13
)

summary(iv_13, diagnostics = TRUE)

stargazer(
  ols_13, iv_13,
  type = "text",
  column.labels = c("OLS", "2SLS"),
  title = "Specification 4: loggdp on risk and continent dummies"
)


# ------------------------------------------------------------
# 6.6 Specification 5: Excluding contested West and Central Africa
# ------------------------------------------------------------

AJR_14_base <- AJR[
  complete.cases(AJR[, c("loggdp", "risk", "logmort", "latitude", "malfal94", "wacacontested")]),
]

AJR_14 <- AJR_14_base[
  AJR_14_base$wacacontested == 0,  # exclude contested WCA observations
]

ols_14 <- lm(loggdp ~ risk + latitude + malfal94, data = AJR_14)   # OLS on restricted sample
fs_14  <- lm(risk ~ logmort + latitude + malfal94, data = AJR_14)  # first stage on restricted sample

iv_14 <- ivreg(
  loggdp ~ risk + latitude + malfal94 |       # same controls as specification 3
    logmort + latitude + malfal94,            # risk instrumented by logmort
  data = AJR_14
)

summary(iv_14, diagnostics = TRUE)

stargazer(
  ols_14, iv_14,
  type = "text",
  column.labels = c("OLS", "2SLS"),
  title = "Specification 5: loggdp excluding contested West and Central Africa"
)


# ------------------------------------------------------------
# 6.7 Table 9: IV robustness regressions
# Order follows the PDF:
#   (1) Risk + latitude
#   (2) Risk + malaria
#   (3) Risk + latitude + malaria
#   (4) Exclude WCA
#   (5) Risk + continent dummies
# ------------------------------------------------------------

stargazer(
  iv_10, iv_11, iv_12, iv_14, iv_13,  # order chosen to match the PDF table
  type = "text",
  title = "Table 9: IV regressions of log GDP per capita",
  column.labels = c(
    "Base sample",
    "Base sample",
    "Base sample",
    "Exclude WCA",
    "Base sample"
  ),
  dep.var.labels = "Log GDP per capita",
  covariate.labels = c(
    "Risk",
    "Latitude",
    "Malaria index 1994",
    "Africa dummy",
    "Asia dummy",
    "Other continent dummy"
  ),
  keep = c("risk", "latitude", "malfal94", "africa", "asia", "other"),
  omit.stat = c("f", "ser"),
  notes = "In column (4), contested observations in West and Central Africa are excluded."
)


# ------------------------------------------------------------
# 6.8 Table 10: First stage robustness regressions
# Same column order as Table 9
# ------------------------------------------------------------

stargazer(
  fs_10, fs_11, fs_12, fs_14, fs_13,  # same order as the IV robustness table
  type = "text",
  title = "Table 10: First stage regressions",
  column.labels = c(
    "Base sample",
    "Base sample",
    "Base sample",
    "Exclude WCA",
    "Base sample"
  ),
  dep.var.labels = "Risk",
  covariate.labels = c(
    "Log settler mortality",
    "Latitude",
    "Malaria",
    "Africa dummy",
    "Asia dummy",
    "Other continent dummy"
  ),
  keep = c("logmort", "latitude", "malfal94", "africa", "asia", "other"),
  omit.stat = c("f", "ser")
)


# ============================================================
# END OF SCRIPT
# ============================================================

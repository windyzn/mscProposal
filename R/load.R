#######################################################################################
## Load and Clean Data [https://bitbucket.org/promise-cohort/promise]
#######################################################################################
install.packages(c("tidyverse", "neprho"))

library(nephro)
library(carpenter)
library(ggplot2)
library(knitr)
library(plyr)
library(dplyr)
library(tidyr)
library(pander)
library(captioner)
library(knitr)
library(mason)
library(msm)
library(magrittr)
library(geepack)


# No need to run unless data has changed

ds <- PROMISE::PROMISE %>%
  dplyr::filter(UDBP < 10000) %>%
  dplyr::mutate(
    UDBP = ifelse(UDBP == 0.01, NA, UDBP),
    UrineCreatinine = ifelse(SID == 2028, 9, UrineCreatinine),
    ACR = round(UrineMicroalbumin / UrineCreatinine, digits = 2),
    Ethnicity = as.character(Ethnicity),
    isAfrican = ifelse(Ethnicity == 'African', 1, 0),
    Ethnicity = ifelse(
      Ethnicity %in% c('African', 'First Nations', 'Other'),
      'Other',
      Ethnicity
    ),
    fVN = factor(
      VN,
      levels = c(1, 3, 6),
      labels = c("Baseline", "3Year", "6Year"),
      ordered = TRUE
    ),
    MedsBP = ifelse(is.na(MedsBloodPressure), 0, MedsBloodPressure),
    fMedsBP = factor(
      MedsBP,
      levels = c(0, 1),
      labels = c("No", "Yes")
    ),
    dm_status = ifelse(DM == 1, 'DM',
                       ifelse(IFG == 1 |
                                IGT == 1, 'Prediabetes',
                              'NGT')),
    acr_status = ifelse(
      ACR < 2,
      'Normoalbuminuria',
      ifelse(ACR > 20, 'Macroalbuminuria',
             "Microalbuminuria")
    ),
    acr_status2 = ifelse(
      acr_status == "Normoalbuminuria",
      "Normoalbuminuria",
      "Albuminuria"
    ),
    creat.mgdl = Creatinine * 0.011312,
    eGFR = nephro::CKDEpi.creat(creat.mgdl, as.numeric(Sex) -
                                  1, Age, isAfrican),
    eGFR_status = ifelse(
      eGFR >= 90,
      'Normal',
      ifelse(eGFR >= 60 &
               eGFR < 90, 'Mild',
             'Moderate')
    ),
    UDBP_status = ifelse(UDBP < 1.23, 'Trace',
                         ifelse(UDBP > 60, 'High', 'Normal')),
    udbpCrRatio = UDBP / UrineCreatinine
  ) %>%
  dplyr::filter(eGFR < 200) %>%
  dplyr::filter(Creatinine < 200) %>%
  dplyr::filter(!(SID == 1131 & VN == 1)) %>%
  dplyr::filter(!(SID == 1444 & VN == 6)) %>%
  dplyr::filter(!(SID == 2042 & VN == 3)) %>%
  dplyr::filter(!(SID == 2124 & VN == 1)) %>%
  dplyr::filter(!(SID == 3025 & VN == 6)) %>%
  dplyr::filter(!(SID == 4016 & VN == 1)) %>%
  dplyr::mutate(
    eGFR_status = factor(
      eGFR_status,
      levels = c('Normal', 'Mild', 'Moderate'),
      ordered = TRUE
    ),
    acr_status = factor(
      acr_status,
      levels = c("Normoalbuminuria", "Microalbuminuria", "Macroalbuminuria"),
      ordered = TRUE
    ),
    acr_status2 = factor(
      acr_status2,
      levels = c("Normoalbuminuria", "Albuminuria"),
      ordered = TRUE
    ),
    dm_status = factor(
      dm_status,
      levels = c('NGT', 'Prediabetes', 'DM'),
      ordered = TRUE
    ),
    UDBP_status = factor(
      UDBP_status,
      levels = c('Trace', 'Normal', 'High'),
      ordered = TRUE
    )
  ) %>%
  dplyr::select(
    SID,
    BMI,
    Waist,
    Age,
    Sex,
    Ethnicity,
    VN,
    fVN,
    Glucose0,
    Glucose120,
    DM,
    IFG,
    IGT,
    dm_status,
    acr_status,
    acr_status2,
    eGFR_status,
    UDBP_status,
    MicroalbCreatRatio,
    eGFR,
    UrineMicroalbumin,
    UrineCreatinine,
    Creatinine,
    ACR,
    UDBP,
    udbpCrRatio,
    VitaminD,
    MeanArtPressure,
    Systolic,
    Diastolic,
    PTH,
    ALT,
    UrinaryCalcium,
    fMedsBP,
    SmokeCigs,
    Canoe,
    dplyr::matches("meds")
  )


# Save the data ===================================================================================

saveRDS(ds, file='data/ds.Rds')

library(tidyverse)
library(lubridate)
library(edwr)

dir_raw <- "ed_steroids/data/raw"
tzone <- "US/Central"

# Current inclusion would be:
#     •	Patients >= 18 yo
# •	Use of any oral or IV steroids while in the emergency department
# •	ICD code for asthma exacerbation or COPD exacerbation
# •	Over the last 3 years (4/1/2015 – 3/31/2018)

# ICD 10 codes:
#     J45.901 (acute asthma exacerbation)
# J44.1 (acute COPD exacerbation)
# 
# MRN
# Age
# gender
# Date of admission
# Which steroid received
# Dose of steroid
# Route of steroid
# ICD 10 code used (so can be separated into asthma and COPD)

# run MBO query
#   * Patients - by ICD
#       - Facility (Curr): HH HERMANN
#       - Admit Date: 10/1/2015 - 3/31/2018
#       - Diagnosis Code: J44.1;J45.21;J45.31;J45.41;J45.51;J45.901
#       - Diagnosis Type: ADMIT;WORKING;FINAL;BILLING;DISCHARGE

pts_icd <- read_data(dir_raw, "patients", FALSE) %>%
    as.patients() %>%
    filter(
        age >= 18,
        discharge.datetime <= mdy("3/31/2018", tz = tzone)
    )

mbo_pts <- concat_encounters(pts_icd$millennium.id)

# steroids <- med_lookup("glucocorticoids")

# run MBO queries
#   * Demographics
#   * Diagnosis - ICD-9/10-CM
#   * Medications - Inpatient - Prompt
#       - Medication Generic: dexamethasone;methylPREDNISolone;prednisoLONE;predniSONE;hydrocortisone

diagnosis <- read_data(dir_raw, "diagnosis", FALSE) %>%
    as.diagnosis() %>%
    filter(
        diag.type == "FINAL",
        diag.code %in% c(
            "J44.1",
            "J45.21",
            "J45.31",
            "J45.41",
            "J45.51",
            "J45.901"
        )
    ) %>%
    mutate(
        disease = if_else(
            diag.code == "J44.1", 
            "copd", 
            "asthma"
        ),
        val = TRUE
    ) %>%
    distinct(millennium.id, disease, val) %>%
    spread(disease, val) %>%
    mutate_at(
        c("asthma", "copd"),
        funs(coalesce(., FALSE))
    )

meds_ed <- read_data(dir_raw, "meds-inpt", FALSE) %>%
    as.meds_inpt() %>%
    filter(
        med.location %in% c(
            "HH VUHH",
            "HH EDHH",
            "HH EDTR",
            "HH EREV",
            "HH OBEC",
            "HC EDPD"
        )
    ) %>%
    semi_join(diagnosis, by = "millennium.id")

mbo_id <- concat_encounters(meds_ed$millennium.id)

# run MBO query
#   * Encounters
#   * Identifiers - by Millennium Encounter Id

demog <- read_data(dir_raw, "demographics", FALSE) %>%
    as.demographics() %>%
    semi_join(meds_ed, by = "millennium.id")

id <- read_data(dir_raw, "identifiers", FALSE) %>%
    as.id()

visits <- read_data(dir_raw, "encounters", FALSE) %>%
    as.encounters() %>%
    select(millennium.id, admit.datetime)

data_patients <- demog %>%
    left_join(id, by = "millennium.id") %>%
    left_join(visits, by = "millennium.id") %>%
    left_join(diagnosis, by = "millennium.id") %>%
    select(
        fin,
        admit.datetime,
        age,
        gender,
        asthma,
        copd
    )

data_meds <- meds_ed %>%
    left_join(id, by = "millennium.id") %>%
    select(
        fin,
        med.datetime,
        med,
        med.dose,
        med.dose.units,
        route,
        med.location
    )

write.csv(
    data_patients,
    "ed_steroids/data/external/data_patients.csv",
    row.names = FALSE
)

write.csv(
    data_meds,
    "ed_steroids/data/external/data_steroids.csv",
    row.names = FALSE
)

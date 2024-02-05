
# Diabetes

#Creating dataset with participants who have one or more of diagnoses "E10" to "E14" in icd10 and "250" in ICD9.
# (tried different filter codes, e.g. filtering for "E11.1", and compared with UKB data showcase.
# Some values don't add up. Some participants probably have more than one diabetes ICD10-code.)
data_dia <- data %>%
    filter(str_detect(icd10, "E10")|
               str_detect(icd10, "E11")|
               str_detect(icd10, "E12")|
               str_detect(icd10, "E13")|
               str_detect(icd10, "E14")|
               str_detect(icd9, "250")) %>%
    select(icd10, icd9)

# dataset contains 8066 observations, i.e, 8066 participants with at least one diabetes ICD10-code.
# to get percentage of whole cohort:

(8100/126812)*100


# Same for liver cancer data:
data_dia_liver <- data_liver %>%
    filter(str_detect(icd10, "E10")|
               str_detect(icd10, "E11")|
               str_detect(icd10, "E12")|
               str_detect(icd10, "E13")|
               str_detect(icd10, "E14")|
               str_detect(icd9, "250")) %>%
    select(icd10, icd9)

(40/115)*100


# For cholelithiasis

data_lith <- data %>%
    filter(str_detect(icd10, "K80")|
               str_detect(icd9, "574")) %>%
    select(icd10, icd9)

    (5880/126812)*100

data_lith_liver <- data_liver %>%
    filter(str_detect(icd10, "K80")|
               str_detect(icd9, "574")) %>%
    select(icd10, icd9)

(26/115)*100

# Alcoholic liver disease

data_alc <- data %>%
    filter(str_detect(icd10, "K70")) %>%
    select(icd10) # no data from icd9

(221/126812)*100

data_alc_liver <- data_liver %>%
    filter(str_detect(icd10, "K70")) %>%
    select(icd10)

(10/115)*100


# NAFLD:
# K76.0 Fatty (change of) liver, not elsewhere classified

data_nafld <- data %>%
    filter(str_detect(icd10, "K76.0")) %>%
    select(icd10) # no data from icd9

(1389/126812)*100

data_nafld_liver <- data_liver %>%
    filter(str_detect(icd10, "K76.0")) %>%
    select(icd10)

(14/115)*100


# Diabetes

#Creating dataset with participants who have one or more of diagnoses "E10" to "E14" in icd10 and "250" in ICD9.
# (tried different filter codes, e.g. filtering for "E11.1", and compared with UKB data showcase.
# Some values don't add up. Some participants probably have more than one diabetes ICD10-code.)
data_dia <- data %>%
    filter(str_detect(icd10, "E11")) %>%
    select(icd10, icd9)

# dataset contains 8100 observations, i.e, 8100 participants with at least one diabetes ICD10-code.
# to get percentage of whole cohort:

nrow(data_dia) / nrow(data) * 100


# Same for liver cancer data:
data_dia_liver <- data_liver %>%
    filter(str_detect(icd10, "E11")) %>%
    select(icd10, icd9)

nrow(data_dia_liver) / nrow(data_liver) * 100


# For cholelithiasis

data_lith <- data %>%
    filter(str_detect(icd10, "K80")|
               str_detect(icd9, "574")) %>%
    select(icd10, icd9)

nrow(data_lith) / nrow(data) * 100

data_lith_liver <- data_liver %>%
    filter(str_detect(icd10, "K80")|
               str_detect(icd9, "574")) %>%
    select(icd10, icd9)

nrow(data_lith_liver) / nrow(data_liver) * 100


# Alcoholic liver disease

data_alc <- data %>%
    filter(str_detect(icd10, "K70")) %>%
    select(icd10) # no data from icd9

nrow(data_alc) / nrow(data) * 100

data_alc_liver <- data_liver %>%
    filter(str_detect(icd10, "K70")) %>%
    select(icd10)

nrow(data_alc_liver) / nrow(data_liver) * 100



# NAFLD:
# K76.0 Fatty (change of) liver, not elsewhere classified

data_nafld <- data %>%
    filter(str_detect(icd10, "K76.0")) %>%
    select(icd10) # no data from icd9

nrow(data_nafld) / nrow(data) * 100


data_nafld_liver <- data_liver %>%
    filter(str_detect(icd10, "K76.0")) %>%
    select(icd10)

nrow(data_nafld_liver) / nrow(data_liver) * 100


# cholecystectomy

data_cyst <- data %>%
    filter(str_detect(opcs4, "J18")|
               str_detect(opcs3, "522")) %>%
    select(opcs4, opcs3)

nrow(data_cyst) / nrow(data) * 100

data_cyst_liv <- data_liver %>%
    filter(str_detect(opcs4, "J18")|
               str_detect(opcs3, "522")) %>%
    select(opcs4, opcs3)

nrow(data_cyst_liv) / nrow(data_liver) * 100

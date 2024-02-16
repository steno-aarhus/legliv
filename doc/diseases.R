# Use template to determine number with disease at baseline:
# Change betweem data and data_liver
# Replace icd10 code

data_disease <- data %>%
    filter(grepl("K70", icd10_a0) & icd10d_a0 < baseline_start_date|
               grepl("K70", icd10_a1) & icd10d_a1 < baseline_start_date|
               grepl("K70", icd10_a2) & icd10d_a2 < baseline_start_date|
               grepl("K70", icd10_a3) & icd10d_a3 < baseline_start_date|
               grepl("K70", icd10_a4) & icd10d_a4 < baseline_start_date|
               grepl("K70", icd10_a5) & icd10d_a5 < baseline_start_date|
               grepl("K70", icd10_a6) & icd10d_a6 < baseline_start_date|
               grepl("K70", icd10_a7) & icd10d_a7 < baseline_start_date|
               grepl("K70", icd10_a8) & icd10d_a8 < baseline_start_date|
               grepl("K70", icd10_a9) & icd10d_a9 < baseline_start_date|
               grepl("K70", icd10_a10) & icd10d_a10 < baseline_start_date|
               grepl("K70", icd10_a11) & icd10d_a11 < baseline_start_date|
               grepl("K70", icd10_a12) & icd10d_a12 < baseline_start_date|
               grepl("K70", icd10_a13) & icd10d_a13 < baseline_start_date|
               grepl("K70", icd10_a14) & icd10d_a14 < baseline_start_date|
               grepl("K70", icd10_a15) & icd10d_a15 < baseline_start_date|
               grepl("K70", icd10_a16) & icd10d_a16 < baseline_start_date|
               grepl("K70", icd10_a17) & icd10d_a17 < baseline_start_date|
               grepl("K70", icd10_a18) & icd10d_a18 < baseline_start_date|
               grepl("K70", icd10_a19) & icd10d_a19 < baseline_start_date|
               grepl("K70", icd10_a20) & icd10d_a20 < baseline_start_date|
               grepl("K70", icd10_a21) & icd10d_a21 < baseline_start_date|
               grepl("K70", icd10_a22) & icd10d_a22 < baseline_start_date|
               grepl("K70", icd10_a23) & icd10d_a23 < baseline_start_date|
               grepl("K70", icd10_a24) & icd10d_a24 < baseline_start_date|
               grepl("K70", icd10_a25) & icd10d_a25 < baseline_start_date|
               grepl("K70", icd10_a26) & icd10d_a26 < baseline_start_date|
               grepl("K70", icd10_a27) & icd10d_a27 < baseline_start_date|
               grepl("K70", icd10_a28) & icd10d_a28 < baseline_start_date|
               grepl("K70", icd10_a29) & icd10d_a29 < baseline_start_date|
               grepl("K70", icd10_a30) & icd10d_a30 < baseline_start_date|
               grepl("K70", icd10_a31) & icd10d_a31 < baseline_start_date|
               grepl("K70", icd10_a32) & icd10d_a32 < baseline_start_date|
               grepl("K70", icd10_a33) & icd10d_a33 < baseline_start_date|
               grepl("K70", icd10_a34) & icd10d_a34 < baseline_start_date|
               grepl("K70", icd10_a35) & icd10d_a35 < baseline_start_date|
               grepl("K70", icd10_a36) & icd10d_a36 < baseline_start_date|
               grepl("K70", icd10_a37) & icd10d_a37 < baseline_start_date|
               grepl("K70", icd10_a38) & icd10d_a38 < baseline_start_date|
               grepl("K70", icd10_a39) & icd10d_a39 < baseline_start_date|
               grepl("K70", icd10_a40) & icd10d_a40 < baseline_start_date|
               grepl("K70", icd10_a41) & icd10d_a41 < baseline_start_date|
               grepl("K70", icd10_a42) & icd10d_a42 < baseline_start_date|
               grepl("K70", icd10_a43) & icd10d_a43 < baseline_start_date|
               grepl("K70", icd10_a44) & icd10d_a44 < baseline_start_date|
               grepl("K70", icd10_a45) & icd10d_a45 < baseline_start_date|
               grepl("K70", icd10_a46) & icd10d_a46 < baseline_start_date|
               grepl("K70", icd10_a47) & icd10d_a47 < baseline_start_date|
               grepl("K70", icd10_a48) & icd10d_a48 < baseline_start_date|
               grepl("K70", icd10_a49) & icd10d_a49 < baseline_start_date|
               grepl("K70", icd10_a50) & icd10d_a50 < baseline_start_date|
               grepl("K70", icd10_a51) & icd10d_a51 < baseline_start_date|
               grepl("K70", icd10_a52) & icd10d_a52 < baseline_start_date|
               grepl("K70", icd10_a53) & icd10d_a53 < baseline_start_date|
               grepl("K70", icd10_a54) & icd10d_a54 < baseline_start_date|
               grepl("K70", icd10_a55) & icd10d_a55 < baseline_start_date|
               grepl("K70", icd10_a56) & icd10d_a56 < baseline_start_date|
               grepl("K70", icd10_a57) & icd10d_a57 < baseline_start_date|
               grepl("K70", icd10_a58) & icd10d_a58 < baseline_start_date|
               grepl("K70", icd10_a59) & icd10d_a59 < baseline_start_date|
               grepl("K70", icd10_a60) & icd10d_a60 < baseline_start_date|
               grepl("K70", icd10_a61) & icd10d_a61 < baseline_start_date|
               grepl("K70", icd10_a62) & icd10d_a62 < baseline_start_date|
               grepl("K70", icd10_a63) & icd10d_a63 < baseline_start_date|
               grepl("K70", icd10_a64) & icd10d_a64 < baseline_start_date|
               grepl("K70", icd10_a65) & icd10d_a65 < baseline_start_date|
               grepl("K70", icd10_a66) & icd10d_a66 < baseline_start_date|
               grepl("K70", icd10_a67) & icd10d_a67 < baseline_start_date|
               grepl("K70", icd10_a68) & icd10d_a68 < baseline_start_date|
               grepl("K70", icd10_a69) & icd10d_a69 < baseline_start_date|
               grepl("K70", icd10_a70) & icd10d_a70 < baseline_start_date|
               grepl("K70", icd10_a71) & icd10d_a71 < baseline_start_date|
               grepl("K70", icd10_a72) & icd10d_a72 < baseline_start_date|
               grepl("K70", icd10_a73) & icd10d_a73 < baseline_start_date|
               grepl("K70", icd10_a74) & icd10d_a74 < baseline_start_date|
               grepl("K70", icd10_a75) & icd10d_a75 < baseline_start_date|
               grepl("K70", icd10_a76) & icd10d_a76 < baseline_start_date|
               grepl("K70", icd10_a77) & icd10d_a77 < baseline_start_date|
               grepl("K70", icd10_a78) & icd10d_a78 < baseline_start_date|
               grepl("K70", icd10_a79) & icd10d_a79 < baseline_start_date|
               grepl("K70", icd10_a80) & icd10d_a80 < baseline_start_date|
               grepl("K70", icd10_a81) & icd10d_a81 < baseline_start_date|
               grepl("K70", icd10_a82) & icd10d_a82 < baseline_start_date|
               grepl("K70", icd10_a83) & icd10d_a83 < baseline_start_date|
               grepl("K70", icd10_a84) & icd10d_a84 < baseline_start_date|
               grepl("K70", icd10_a85) & icd10d_a85 < baseline_start_date|
               grepl("K70", icd10_a86) & icd10d_a86 < baseline_start_date|
               grepl("K70", icd10_a87) & icd10d_a87 < baseline_start_date|
               grepl("K70", icd10_a88) & icd10d_a88 < baseline_start_date|
               grepl("K70", icd10_a89) & icd10d_a89 < baseline_start_date|
               grepl("K70", icd10_a90) & icd10d_a90 < baseline_start_date|
               grepl("K70", icd10_a91) & icd10d_a91 < baseline_start_date|
               grepl("K70", icd10_a92) & icd10d_a92 < baseline_start_date|
               grepl("K70", icd10_a93) & icd10d_a93 < baseline_start_date|
               grepl("K70", icd10_a94) & icd10d_a94 < baseline_start_date|
               grepl("K70", icd10_a95) & icd10d_a95 < baseline_start_date|
               grepl("K70", icd10_a96) & icd10d_a96 < baseline_start_date|
               grepl("K70", icd10_a97) & icd10d_a97 < baseline_start_date|
               grepl("K70", icd10_a98) & icd10d_a98 < baseline_start_date|
               grepl("K70", icd10_a99) & icd10d_a99 < baseline_start_date|
               grepl("K70", icd10_a100) & icd10d_a100 < baseline_start_date
    )

nrow(data_disease) / nrow(data) * 100
nrow(data_disease) / nrow(data_liver) * 100

data_disease %>%
    select()

# Diabetes #"E.11"

# For cholelithiasis # K.80

# Alcoholic liver disease # k.70

# NAFLD:
# K.76.0 Fatty (change of) liver, not elsewhere classified


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


data_surg <- data %>%
    filter(grepl("J18", opcs4_a0) & p41282_a0 < baseline_start_date|
               grepl("J18", opcs4_a1) & p41282_a1 < baseline_start_date|
               grepl("J18", opcs4_a2) & p41282_a2 < baseline_start_date|
               grepl("J18", opcs4_a3) & p41282_a3 < baseline_start_date|
               grepl("J18", opcs4_a4) & p41282_a4 < baseline_start_date|
               grepl("J18", opcs4_a5) & p41282_a5 < baseline_start_date|
               grepl("J18", opcs4_a6) & p41282_a6 < baseline_start_date|
               grepl("J18", opcs4_a7) & p41282_a7 < baseline_start_date|
               grepl("J18", opcs4_a8) & p41282_a8 < baseline_start_date|
               grepl("J18", opcs4_a9) & p41282_a9 < baseline_start_date|
               grepl("J18", opcs4_a10) & p41282_a10 < baseline_start_date|
               grepl("J18", opcs4_a11) & p41282_a11 < baseline_start_date|
               grepl("J18", opcs4_a12) & p41282_a12 < baseline_start_date|
               grepl("J18", opcs4_a13) & p41282_a13 < baseline_start_date|
               grepl("J18", opcs4_a14) & p41282_a14 < baseline_start_date|
               grepl("J18", opcs4_a15) & p41282_a15 < baseline_start_date|
               grepl("J18", opcs4_a16) & p41282_a16 < baseline_start_date|
               grepl("J18", opcs4_a17) & p41282_a17 < baseline_start_date|
               grepl("J18", opcs4_a18) & p41282_a18 < baseline_start_date|
               grepl("J18", opcs4_a19) & p41282_a19 < baseline_start_date|
               grepl("J18", opcs4_a20) & p41282_a20 < baseline_start_date|
               grepl("J18", opcs4_a21) & p41282_a21 < baseline_start_date|
               grepl("J18", opcs4_a22) & p41282_a22 < baseline_start_date|
               grepl("J18", opcs4_a23) & p41282_a23 < baseline_start_date|
               grepl("J18", opcs4_a24) & p41282_a24 < baseline_start_date|
               grepl("J18", opcs4_a25) & p41282_a25 < baseline_start_date|
               grepl("J18", opcs4_a26) & p41282_a26 < baseline_start_date|
               grepl("J18", opcs4_a27) & p41282_a27 < baseline_start_date|
               grepl("J18", opcs4_a28) & p41282_a28 < baseline_start_date|
               grepl("J18", opcs4_a29) & p41282_a29 < baseline_start_date|
               grepl("J18", opcs4_a30) & p41282_a30 < baseline_start_date|
               grepl("J18", opcs4_a31) & p41282_a31 < baseline_start_date|
               grepl("J18", opcs4_a32) & p41282_a32 < baseline_start_date|
               grepl("J18", opcs4_a33) & p41282_a33 < baseline_start_date|
               grepl("J18", opcs4_a34) & p41282_a34 < baseline_start_date|
               grepl("J18", opcs4_a35) & p41282_a35 < baseline_start_date|
               grepl("J18", opcs4_a36) & p41282_a36 < baseline_start_date|
               grepl("J18", opcs4_a37) & p41282_a37 < baseline_start_date|
               grepl("J18", opcs4_a38) & p41282_a38 < baseline_start_date|
               grepl("J18", opcs4_a39) & p41282_a39 < baseline_start_date|
               grepl("J18", opcs4_a40) & p41282_a40 < baseline_start_date|
               grepl("J18", opcs4_a41) & p41282_a41 < baseline_start_date|
               grepl("J18", opcs4_a42) & p41282_a42 < baseline_start_date|
               grepl("J18", opcs4_a43) & p41282_a43 < baseline_start_date|
               grepl("J18", opcs4_a44) & p41282_a44 < baseline_start_date|
               grepl("J18", opcs4_a45) & p41282_a45 < baseline_start_date|
               grepl("J18", opcs4_a46) & p41282_a46 < baseline_start_date|
               grepl("J18", opcs4_a47) & p41282_a47 < baseline_start_date|
               grepl("J18", opcs4_a48) & p41282_a48 < baseline_start_date|
               grepl("J18", opcs4_a49) & p41282_a49 < baseline_start_date|
               grepl("J18", opcs4_a50) & p41282_a50 < baseline_start_date|
               grepl("J18", opcs4_a51) & p41282_a51 < baseline_start_date|
               grepl("J18", opcs4_a52) & p41282_a52 < baseline_start_date|
               grepl("J18", opcs4_a53) & p41282_a53 < baseline_start_date|
               grepl("J18", opcs4_a54) & p41282_a54 < baseline_start_date|
               grepl("J18", opcs4_a55) & p41282_a55 < baseline_start_date|
               grepl("J18", opcs4_a56) & p41282_a56 < baseline_start_date|
               grepl("J18", opcs4_a57) & p41282_a57 < baseline_start_date|
               grepl("J18", opcs4_a58) & p41282_a58 < baseline_start_date|
               grepl("J18", opcs4_a59) & p41282_a59 < baseline_start_date|
               grepl("J18", opcs4_a60) & p41282_a60 < baseline_start_date|
               grepl("J18", opcs4_a61) & p41282_a61 < baseline_start_date|
               grepl("J18", opcs4_a62) & p41282_a62 < baseline_start_date|
               grepl("J18", opcs4_a63) & p41282_a63 < baseline_start_date|
               grepl("J18", opcs4_a64) & p41282_a64 < baseline_start_date|
               grepl("J18", opcs4_a65) & p41282_a65 < baseline_start_date|
               grepl("J18", opcs4_a66) & p41282_a66 < baseline_start_date|
               grepl("J18", opcs4_a67) & p41282_a67 < baseline_start_date|
               grepl("J18", opcs4_a68) & p41282_a68 < baseline_start_date|
               grepl("J18", opcs4_a69) & p41282_a69 < baseline_start_date|
               grepl("J18", opcs4_a70) & p41282_a70 < baseline_start_date|
               grepl("J18", opcs4_a71) & p41282_a71 < baseline_start_date|
               grepl("J18", opcs4_a72) & p41282_a72 < baseline_start_date
    )

nrow(data_surg) / nrow(data) * 100

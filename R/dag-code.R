library(tidyverse)
library(ggdag)

legliv <- dagitty::dagitty(
'dag {
bb="-7.026,-4.028,6.224,4.491"
"Intake of other foods (g)" [pos="-5.742,-1.085"]
"Legume intake (g)" [pos="-5.691,3.519"]
"Liver cancer" [outcome,pos="5.094,1.535"]
"Living alone" [pos="0.921,-3.639"]
"Metabolic syndrome" [pos="-2.327,3.117"]
"Physical activity" [pos="2.141,-3.406"]
"Replacing meat with legumes (g)" [exposure,pos="-5.023,1.535"]
"Socioeconomic status" [pos="0.703,-1.850"]
"Total food intake" [pos="-6.076,0.147"]
"Waist circumference" [pos="-4.689,-3.457"]
"red meat intake (g)" [pos="-6.063,2.287"]
Adiposity [pos="-4.689,-2.070"]
Age [pos="-1.479,-1.940"]
Alcohol [pos="4.606,-3.341"]
Diabetes [pos="2.924,2.468"]
Education [pos="-0.709,-3.756"]
Lifestyle [pos="3.361,-1.979"]
NAFLD [pos="2.937,3.700"]
Sex [pos="-3.315,-2.239"]
Smoking [pos="3.451,-3.354"]
tdi [pos="-1.967,-3.730"]
"Intake of other foods (g)" -> "Liver cancer"
"Intake of other foods (g)" -> "Replacing meat with legumes (g)"
"Legume intake (g)" -> "Replacing meat with legumes (g)"
"Living alone" -> "Socioeconomic status"
"Metabolic syndrome" -> Diabetes
"Metabolic syndrome" -> NAFLD
"Physical activity" -> Lifestyle
"Replacing meat with legumes (g)" -> "Liver cancer"
"Replacing meat with legumes (g)" -> "Metabolic syndrome"
"Socioeconomic status" -> "Liver cancer"
"Socioeconomic status" -> "Replacing meat with legumes (g)"
"Total food intake" -> "Liver cancer"
"Total food intake" -> "Replacing meat with legumes (g)"
"Waist circumference" -> Adiposity
"red meat intake (g)" -> "Replacing meat with legumes (g)"
Adiposity -> "Liver cancer"
Adiposity -> "Replacing meat with legumes (g)"
Age -> "Liver cancer"
Age -> "Replacing meat with legumes (g)"
Alcohol -> Lifestyle
Diabetes -> "Liver cancer"
Education -> "Socioeconomic status"
Lifestyle -> "Liver cancer"
Lifestyle -> "Replacing meat with legumes (g)"
NAFLD -> "Liver cancer"
Sex -> "Liver cancer"
Sex -> "Replacing meat with legumes (g)"
Smoking -> Lifestyle
tdi -> "Socioeconomic status"
}'
)

legliv_tidy <- tidy_dagitty(legliv) %>% dag_adjustment_sets(exposure = "Replacing meat with legumes (g)", outcome = "Liver cancer") %>%
  mutate(colour = case_when(
    name == "Liver cancer" ~ "outcome",
    name == "Replacing meat with legumes (g)" ~ "exposure",
    name == "Legume intake (g)" | name == "red meat intake (g)" ~ "food",
    name == "Metabolic syndrome" | name == "NAFLD" | name == "Diabetes" ~ "met",
    name != "Liver cancer" & name != "Replacing red meat with legumes (g) ~ exposure" ~ "adjusted"
  ))
legliv_tidy |>
  ggdag(text = FALSE) +
  geom_dag_point(aes(colour = colour)) +
  theme_dag() +
  theme(legend.position = "none") +
  geom_dag_label(color="black", size = 2.5) +
  scale_adjusted()


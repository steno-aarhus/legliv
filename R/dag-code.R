library(tidyverse)
library(ggdag)

legliv <- dagitty::dagitty('dag {
bb="-7.026,-4.028,6.224,4.491"
"Intake of other foods (g)" [pos="-5.800,-3.000"]
"Legume intake (g)" [pos="-5.959,2.549"]
"Liver cancer" [outcome,pos="5.000,0.450"]
"Metabolic syndrome" [pos="0.245,2.071"]
"Physical activity" [pos="2.256,-3.000"]
"Replacing meat with legumes (g)" [exposure,pos="-5.000,0.450"]
"Socioeconomic status" [pos="-0.111,-3.000"]
"Total food intake" [pos="-5.903,-0.593"]
"Waist circumference" [pos="-2.834,2.061"]
"red meat intake (g)" [pos="-5.903,1.427"]
Age [pos="-2.048,-3.000"]
Alcohol [pos="5.000,-3.000"]
Diabetes [pos="3.136,1.622"]
NAFLD [pos="3.145,2.647"]
Sex [pos="-3.386,-3.000"]
Smoking [pos="4.006,-3.000"]
"Intake of other foods (g)" -> "Liver cancer"
"Intake of other foods (g)" -> "Replacing meat with legumes (g)"
"Intake of other foods (g)" -> "Total food intake"
"Legume intake (g)" -> "Replacing meat with legumes (g)"
"Metabolic syndrome" -> Diabetes
"Metabolic syndrome" -> NAFLD
"Physical activity" -> "Liver cancer"
"Physical activity" -> "Replacing meat with legumes (g)"
"Replacing meat with legumes (g)" -> "Liver cancer"
"Replacing meat with legumes (g)" -> "Total food intake"
"Replacing meat with legumes (g)" -> "Waist circumference"
"Socioeconomic status" -> "Liver cancer"
"Socioeconomic status" -> "Replacing meat with legumes (g)"
"Waist circumference" -> "Metabolic syndrome"
"red meat intake (g)" -> "Replacing meat with legumes (g)"
Age -> "Liver cancer"
Age -> "Replacing meat with legumes (g)"
Alcohol -> "Liver cancer"
Alcohol -> "Replacing meat with legumes (g)"
Diabetes -> "Liver cancer"
NAFLD -> "Liver cancer"
Sex -> "Liver cancer"
Sex -> "Replacing meat with legumes (g)"
Smoking -> "Liver cancer"
Smoking -> "Replacing meat with legumes (g)"
}
'
)
legliv <- tidy_dagitty(legliv)
legliv |>
  ggdag(text = FALSE) +
  geom_dag_node(color="darkgreen") +
  theme_dag() + theme(legend.position = "none") +
  geom_dag_label(color="black", size = 2.5) +
  scale_adjusted()

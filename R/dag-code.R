library(tidyverse)
library(ggdag)
library(dagitty)

legliv <- dagitty(
  'dag {
bb="-8,-5,6,5"
"Intake of other foods (g)" [pos="4.000,0.000"]
"Legume intake (g)" [pos="-1.000,4.000"]
"Liver cancer" [outcome,pos="0.000,-4.000"]
"Living alone" [pos="-3.500,0.000"]
"Physical activity" [pos="-3.500,-1.000"]
"Replacing meat with legumes (g)" [exposure,pos="0.000,2.000"]
"Socioeconomic status" [pos="-6.000,1.000"]
"Total food intake (g)" [pos="4.000,-1.000"]
"Waist circumference" [pos="-3.500,-4.000"]
"red meat intake (g)" [pos="1.000,4.000"]
Adiposity [pos="-6.000,-4.000"]
Age [pos="-3.500,4.000"]
Alcohol [pos="-3.500,-2.000"]
Education [pos="-3.500,2.000"]
Lifestyle [pos="-6.000,-2.000"]
Sex [pos="-3.500,3.000"]
Smoking [pos="-3.500,-3.000"]
tdi [pos="-3.500,1.000"]
"Intake of other foods (g)" -> "Liver cancer"
"Intake of other foods (g)" -> "Replacing meat with legumes (g)"
"Legume intake (g)" -> "Replacing meat with legumes (g)"
"Living alone" -> "Liver cancer"
"Living alone" -> "Replacing meat with legumes (g)"
"Physical activity" -> "Liver cancer"
"Physical activity" -> "Replacing meat with legumes (g)"
"Replacing meat with legumes (g)" -> "Liver cancer"
"Socioeconomic status" -> "Living alone"
"Socioeconomic status" -> Education
"Socioeconomic status" -> tdi
"Total food intake (g)" -> "Liver cancer"
"Total food intake (g)" -> "Replacing meat with legumes (g)"
"Waist circumference" -> "Liver cancer"
"Waist circumference" -> "Replacing meat with legumes (g)"
"red meat intake (g)" -> "Replacing meat with legumes (g)"
Adiposity -> "Waist circumference"
Age -> "Liver cancer"
Age -> "Replacing meat with legumes (g)"
Alcohol -> "Liver cancer"
Alcohol -> "Replacing meat with legumes (g)"
Education -> "Liver cancer"
Education -> "Replacing meat with legumes (g)"
Lifestyle -> "Physical activity"
Lifestyle -> Alcohol
Lifestyle -> Smoking
Sex -> "Liver cancer"
Sex -> "Replacing meat with legumes (g)"
Smoking -> "Liver cancer"
Smoking -> "Replacing meat with legumes (g)"
tdi -> "Liver cancer"
tdi -> "Replacing meat with legumes (g)"
}

'
)

legliv_tidy <- tidy_dagitty(legliv) %>%
  dag_adjustment_sets(type = "minimal",
                      effect = "total") %>%
  mutate(colour = case_when(
    x == "1" | x == "-1" ~ "exposure",
    x == "0" & y == "2" ~ "exposure",
    x == "4" | x == "-6" | x == "-3.5" ~ "confounders",
    x == "0" & y == "-4" ~ "liver cancer",
  ))

dag <- legliv_tidy |>
  ggdag_adjustment_set(text = FALSE,
                       exposure = "Replacing meat with legumes (g)",
                       outcome = "Liver cancer",
                       shadow = TRUE,
                       stylized = TRUE,
                       type = "minimal",
                       effect = "total",
                       node_size = 12,
                       expand_x = expansion(c(0.1, 0.1)),
                       expand_y = expansion(c(0.1, 0.1))) +
  geom_dag_node(aes(colour = colour)) +
  scale_color_manual(values = c("black", "darkred", "darkgreen", "darkblue", "black")) +
  theme_dag(base_size = 0) +
  theme(legend.position = "none") +
  geom_dag_label(color="black", size = 2) +
  scale_adjusted()

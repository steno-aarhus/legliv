library(tidyverse)
library(ggdag)
library(dagitty)

legliv <- dagitty( # Har først lavet grafen i dagitty og så kopieret koden ind her. Her er det relativt let at justere koordinaterne så det står helt skarpt.
  'dag {
bb="-8,-5,6,5"
"Geographical region" [pos="3.500,-3.000"]
"Intake of other foods (g)" [pos="3.500,-1.000"]
"Legume intake (g)" [pos="-1.000,2.000"]
"Liver cancer" [outcome,pos="0.000,-4.000"]
"Living alone" [pos="-3.500,0.000"]
"Physical activity" [pos="-3.500,-1.000"]
"Red meat intake (g)" [pos="1.000,2.000"]
"Replacing meat with legumes (g)" [exposure,pos="0.000,1.000"]
"Socioeconomic status" [pos="-6.000,1.000"]
"Waist circumference" [pos="-3.500,-4.000"]
Adiposity [pos="-6.000,-4.000"]
Age [pos="3.500,0.000"]
Alcohol [pos="-3.500,-2.000"]
Education [pos="-3.500,2.000"]
Lifestyle [pos="-6.000,-2.000"]
Sex [pos="3.500,-2.000"]
Smoking [pos="-3.500,-3.000"]
TDI [pos="-3.500,1.000"]
"Geographical region" -> "Liver cancer"
"Geographical region" -> "Replacing meat with legumes (g)"
"Intake of other foods (g)" -> "Liver cancer"
"Intake of other foods (g)" -> "Replacing meat with legumes (g)"
"Legume intake (g)" -> "Replacing meat with legumes (g)"
"Living alone" -> "Liver cancer"
"Living alone" -> "Replacing meat with legumes (g)"
"Physical activity" -> "Liver cancer"
"Physical activity" -> "Replacing meat with legumes (g)"
"Red meat intake (g)" -> "Replacing meat with legumes (g)"
"Replacing meat with legumes (g)" -> "Liver cancer"
"Socioeconomic status" -> "Living alone"
"Socioeconomic status" -> Education
"Socioeconomic status" -> TDI
"Waist circumference" -> "Liver cancer"
"Waist circumference" -> "Replacing meat with legumes (g)"
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
TDI -> "Liver cancer"
TDI -> "Replacing meat with legumes (g)"
}
'
)

legliv_tidy <- tidy_dagitty(legliv) %>%
  dag_adjustment_sets(type = "minimal", # Dette er default settings, men de kan ændres, f.eks. hvis jeg skriver "canonical" i stedet for "minimal" vil ancestors til confoundere også blive square nodes.
                      effect = "total") %>% # Brug "direct" hvis du også har mediatorer og vil justere for disse.
  mutate(colour = case_when( # Her laver jeg en ny kolonne som jeg bruger til at farve nodes'ene med. Kategorierne er baseret på koordinaterne, men kan i princippet baseres på navn eller retning (brug print som nedenfor at at se kolonnerne).
    x == "1" | x == "-1" ~ "exposure",
    x == "0" & y == "1" ~ "exposure",
    x == "3.5" | x == "-3.5" | x == "-6" ~ "confounders",
    x == "0" & y == "-4" ~ "liver cancer",
  ))

# legliv_tidy %>% print() # Tjek kolonner

dag <- legliv_tidy |>
  ggdag_adjustment_set( # ggdag_adjustment_set og dag_adjustment_sets gør egentlig det samme, så det meste her er egentlig dobbeltkonfekt. Jeg bruger begge, fordi jeg kan definere grupper til farvekodning med dag_adjustment_sets og manipulere udseendet af plottet med ggdag_adjustment_set.
    text = FALSE, # vigtigt, da grafen ellers bliver grim
    exposure = "Replacing meat with legumes (g)",
    outcome = "Liver cancer", # Egentlig irrelevant, da tidy_dagitty() selv definerer exposure og outcome.
    shadow = TRUE, # For at vise pile for biasing paths
    stylized = TRUE, # Formentlig irrelevant
    type = "minimal",
    effect = "total",
    node_size = 12, # bliver overruled a geom_dag_node() nedenunder, så man kan ikke både ændre størrelsen på nodes og selv definere farverne...
    expand_x = expansion(c(0.1, 0.13)),
    expand_y = expansion(c(0.075, 0.07)) # Udnytter hele griddet
  ) +
  geom_dag_node(aes(colour = colour)) + # se under node_size. "size =" argument får nodes'ene til at se mærkelige ud.
  scale_color_manual(values = c("black", "darkred", "darkgreen", "darkblue", "black")) + # "black" er plot fill-in fordi default farvekoden er defineret ud fra "adjusted" og "unadjusted" i adjusted-kolonnen.
  theme_dag(base_size = 0) + # Fjerner grim title der ellers ville komme med i plottet. den fjernes ikke helt i selve plots-outputtet, men hvis man gemmer som pdf eller render den i r-markdown eller quarto, så forsvinder den.
  theme(legend.position = "none") + # f.eks "right" hvis du vil se legend (kan give et overblik ift. farvningen af nodes).
  geom_dag_label(color="black", size = 2) +
  scale_adjusted() # tror ikke det gør nogen forskel om den er her eller ej, i hvert fald ikke for min DAG.

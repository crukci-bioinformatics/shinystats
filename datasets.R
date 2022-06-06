#!/usr/bin/env Rscript

library(tidyverse)

disease_x <- read_csv(
  "datasets/DiseaseX.csv",
  col_types = "cd"
)

blood_vessel_formation_1 <- read_csv(
  "datasets/BloodVesselFormation1.csv",
  col_types = "cd"
)

biological_process_durations <- read_csv(
  "datasets/BiologicalProcessDurations.csv",
  col_types = "fd"
)

blood_vessel_formation_2 <- read_csv(
  "datasets/BloodVesselFormation2.csv",
  col_types = "cdd"
)

nibp_expression <- read_csv(
  "datasets/NIBPExpression.csv",
  col_types = "fd"
)

fibrosis_vitamin_d <- read_csv(
  "datasets/FibrosisVitaminD.csv",
  col_types = "fd"
)

sids_twin_birth_weight <- read_csv(
  "datasets/SIDSTwinBirthWeight.csv",
  col_types = "dd"
)

plant_growth_fertilization <- read_csv(
  "datasets/PlantGrowthFertilization.csv",
  col_types = "dd"
)

hygiene_regime <- read_csv(
  "datasets/FlorenceNightingaleHygieneRegime.csv",
  col_types = "fd"
)

bran_diverticulosis <- read_csv(
  "datasets/BranDiverticulosis.csv",
  col_types = "fd"
)

autism_drug <- read_csv(
  "datasets/AutismDrugRepetitiveBehaviour.csv",
  col_types = "dd"
)

hiv_drug_cd4_counts <- read_csv(
  "datasets/HIVDrugCD4Counts.csv",
  col_types = "dd"
)

drink_driving_reaction_times <- read_csv(
  "datasets/DrinkDrivingReactionTimes.csv",
  col_types = "dd"
)

tree_pollution <- read_csv(
  "datasets/TreePollution.csv",
  col_types = "dd"
)

professorial_salaries <- read_csv(
  "datasets/ProfessorialSalaries.csv",
  col_types = "fd"
)

chick_weights <- read_csv(
  "datasets/chick_weights.csv",
  col_types = "cdd"
)

save.image("datasets.RData")


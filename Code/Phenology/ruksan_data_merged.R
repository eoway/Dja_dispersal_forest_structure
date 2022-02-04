setwd("/Users/sophieroberts/Downloads/elsa_lab/crown_delineation/ruksan_data")
getwd()

library(readxl)
library(tidyverse)
# install.packages("writexl")
library(writexl)
# install.packages("openxlsx")
library(openxlsx)
library(readr)

Pheno_Bouamir_Tree_Census_clean <- read_csv("clean_data/Pheno_Bouamir_Tree_Census_clean.csv")

Pheno_Bouamir_Tree_Metadata_clean <- read_csv("clean_data/Pheno_Bouamir_Tree_Metadata_clean.csv")

Pheno_Bouamir_Tree_merged <- merge(Pheno_Bouamir_Tree_Census_clean, Pheno_Bouamir_Tree_Metadata_clean, by = "tree_ID")

write_xlsx(Pheno_Bouamir_Tree_merged, "/Users/sophieroberts/Downloads/elsa_lab/crown_delineation/ruksan_data/Pheno_Bouamir_Tree_merged.xlsx")


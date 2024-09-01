rm(list = ls())
ls()

library(tidyverse)
library(readxl)

dadosA_Produtividade <- read_excel("dados/Produtividade/aprod.xlsx")

dadosA_Produtividade |> 
  ggplot(aes(x = Prod))+
  geom_histogram()

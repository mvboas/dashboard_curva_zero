#################################################################################################################
# Script para coletar curvas zero diariamente
# Marcelo Vilas Boas de Castro
# Data: 12-11-2021
#################################################################################################################

# Pacotes
library(GetTDData)
library(tidyr)
library(rio)
library(dplyr)
library(taskscheduleR)



# Coletando dados do dia
df.yield <- get.yield.curve()
years <- data.frame(anos = seq(0.5,10,0.5), vertices = c(126, 252, 378, 504, 630, 756, 882, 1008, 1134, 1260, 1386, 1512, 1638, 1764, 1890, 2016, 2142, 2268, 2394, 2520))

value.nominal <- df.yield %>% filter(type == "nominal_return") %>% select(vertices = n.biz.days, nominal = value) %>% merge(years, by = "vertices")
value.real <- df.yield %>% filter(type == "real_return") %>% select(vertices = n.biz.days, real = value) %>% merge(years, by = "vertices")
value.imp <- df.yield %>% filter(type == "implicit_inflation") %>% select(vertices = n.biz.days, implicita = value) %>% merge(years, by = "vertices")

df.incorporar <- value.nominal %>%
  merge(value.real, by = "vertices") %>%
  merge(value.imp, by = "vertices") %>%
  select(anos, vertices, real, nominal, implicita)

df.incorporar$data <- as.character(df.yield[1,"current.date"])



# Juntando com o resto dos dados
dados_real <- readRDS("dados_real")
dados_real <- df.incorporar %>% select(data, anos, real) %>% rbind(dados_real) %>% distinct()
saveRDS(dados_real, "dados_real")

dados_nominal <- readRDS("dados_nominal")
dados_nominal <- df.incorporar %>% select(data, anos, nominal) %>% rbind(dados_nominal) %>% distinct()
saveRDS(dados_nominal, "dados_nominal")

dados_implicita <- readRDS("dados_implicita")
dados_implicita <- df.incorporar %>% select(data, anos, implicita) %>% rbind(dados_implicita) %>% distinct()
saveRDS(dados_implicita, "dados_implicita")
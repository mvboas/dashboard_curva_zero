


dados <- import("curva.xlsx", col_names = F)
data_recortada <- as.Date(as.numeric(dados[1,1]), origin = "1899-12-30")
dados <- dados[6:22,1:4]
dados <- dados[-1,] %>% mutate_all(function(x) as.numeric(as.character(x))) %>%
    as.data.frame() %>% drop_na()
names(dados) <- c("vertices", "real", "nominal", "implicita")
years <- data.frame(anos = seq(0.5,10,0.5), vertices = c(126, 252, 378, 504, 630, 756, 882, 1008, 1134, 1260, 1386, 1512, 1638, 1764, 1890, 2016, 2142, 2268, 2394, 2520))
dados <- dados %>%
  merge(years, by = "vertices") %>%
  select(anos, vertices, real, nominal, implicita)
dados["data"] <- data_recortada

# Juntando com o resto dos dados
dados_real <- readRDS("dados_real")
dados_real <- dados %>% select(data, anos, real) %>% rbind(dados_real) %>% distinct()
# saveRDS(dados_real, "dados_real")

dados_nominal <- readRDS("dados_nominal")
dados_nominal <- dados %>% select(data, anos, nominal) %>% rbind(dados_nominal) %>% distinct()
# saveRDS(dados_nominal, "dados_nominal")

dados_implicita <- readRDS("dados_implicita")
dados_implicita <- dados %>% select(data, anos, implicita) %>% rbind(dados_implicita) %>% distinct()
# saveRDS(dados_implicita, "dados_implicita")
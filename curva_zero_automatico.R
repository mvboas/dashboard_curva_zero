# Pacotes
library(GetTDData)
library(tidyr)
library(rio)
library(dplyr)

# Coletando dados
df.yield <- get.yield.curve()
years <- data.frame(anos = seq(0.5,10,0.5), vertices = c(126, 252, 378, 504, 630, 756, 882, 1008, 1134, 1260, 1386, 1512, 1638, 1764, 1890, 2016, 2142, 2268, 2394, 2520))

value.nominal <- df.yield %>% filter(type == "nominal_return") %>% select(vertices = n.biz.days, nominal = value) %>% merge(years, by = "vertices")
value.real <- df.yield %>% filter(type == "real_return") %>% select(vertices = n.biz.days, real = value) %>% merge(years, by = "vertices")
value.imp <- df.yield %>% filter(type == "implicit_inflation") %>% select(vertices = n.biz.days, implicita = value) %>% merge(years, by = "vertices")

values <- value.nominal %>%
  merge(value.real, by = "vertices") %>%
  merge(value.imp, by = "vertices") %>%
  select(anos, vertices, real, nominal, implicita)

values$data <- as.character(df.yield[1,"current.date"])

# Nomeando (colocando a data)
nome <- as.character(df.yield[1,"current.date"])
assign(nome, values)

# Exportando arquivo
rio::export(get(nome), paste(nome,"xlsx",sep = "."))

# Open the scheduler: START -> All Programs -> Accesories -> System Tools -> Scheduler
# Create a new Task
# under tab Action, create a new action
# choose Start Program
# browse to Rscript.exe which should be placed e.g. here:
#   "C:\Program Files\R\R-3.0.2\bin\x64\Rscript.exe"
# input the name of your file in the parameters field
# input the path where the script is to be found in the Start in field
# go to the Triggers tab
# create new trigger
# choose that task should be done each day, month, ... repeated several times, or whatever you like
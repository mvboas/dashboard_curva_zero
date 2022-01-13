#Script para dados da curva zero
#Feito por: Marcelo Vilas Boas de Castro
#última atualização: 25/05/2021

##Carregando pacotes que serão utilizados
library(tidyverse)
library(rio)
library(openxlsx)
# library(shiny)
# library(ggrepel)
library(scales)
library(plotly)

##Definindo caminho
setwd("D:/Documentos/GitHub/dashboard_curva_zero")



#Importando planilha
#Definindo as datas
datas <- import("curva_zero.xlsx", col_names = F)
datas <- datas[1,]
seleciona_datas <- seq(2,length(datas), 8)
datas_tratadas <- NA
for (i in 1:length(seleciona_datas)){
  datas_tratadas[i] <- datas[1,seleciona_datas[i]]
}
datas_tratadas <- as.vector(as.character((convertToDate(datas_tratadas))))

#Coletando o resto dos dados
dados <- import("curva_zero.xlsx", skip = 5, col_names = F)
seleciona_dados <- seq(1,length(dados), 8)
for (i in 1:length(datas_tratadas)){
  dados_dia <- dados[0:22,seleciona_dados[i]:(seleciona_dados[i]+4)]
  names(dados_dia) <- dados_dia[1,]
  dados_dia <- dados_dia[-1,] %>% mutate_all(function(x) as.numeric(as.character(x))) %>%
    as.data.frame() %>% drop_na()
  names(dados_dia) <- c("anos", "vertices", "real", "nominal", "implícita")
  nome_arquivo <- as.character(datas_tratadas[i])
  assign(nome_arquivo, dados_dia) #Nomeando arquivos
  print(paste(i, length(datas_tratadas), sep = '/')) #Printa o progresso da repetição
}


#Selecionando data
usuario_data <- function(){
  resposta_dados <- readline(prompt = "Escolha um dado (nominal, real ou implícita): ")
  opcoes_dados <- c("nominal", "real", "implícita")
  while(is.element(resposta_dados, opcoes_dados) == F){
    resposta_dados <- readline(prompt = "Escolha um dado (nominal, real ou implícita): ")
  }
  
  resposta_data <- readline(prompt = "Escolha uma data no formato YYYY-MM-DD: ")
  while(is.element(resposta_data, datas_tratadas) == F){
    print(datas_tratadas)
    resposta_data <- readline(prompt = "Escolha uma data no formato YYYY-MM-DD: ")
  }
  
  dado_graf <- get(resposta_data) %>% select(anos, all_of(resposta_dados)) %>% rename(!!resposta_data := resposta_dados)

  graf <- ggplot(dado_graf, aes(x = anos, y = get(!!resposta_data), label = sprintf("%0.2f", round(dado_graf[,2],2)))) + 
    geom_line() + geom_point() +
    scale_x_continuous(breaks = round(seq(min(dado_graf$anos), max(dado_graf$anos), by = 0.5),1)) + 
    scale_y_continuous(breaks = round(seq(min(dado_graf[,2]), max(dado_graf[,2]), by = 0.5),1)) + 
    theme(axis.text.x=element_text(angle=90, hjust=1)) + 
    labs(title = paste("Curva zero", resposta_data, sep = " "), subtitle = resposta_dados,
         caption = "Fonte: Ambima") + ylab("%") + xlab("Anos")
  
  # show(graf)
  graf <- graf %>% ggplotly() %>% style(hoverinfo = "y")
  show(graf)
  
  for (i in 1:length(datas_tratadas)){
    resposta_data_nova <- readline(prompt = "Escolha uma data no formato YYYY-MM-DD: ")
    while(is.element(resposta_data_nova, datas_tratadas) == F){
      print(datas_tratadas)
      resposta_data_nova <- readline(prompt = "Escolha uma data no formato YYYY-MM-DD: ")
    }
    
    dado_graf <- merge(dado_graf, select(get(resposta_data_nova), anos, resposta_dados), by = "anos", all = T) %>%
      rename(!!resposta_data_nova := resposta_dados)
    
    dado_graf_mult <- dado_graf %>% na.omit() %>% pivot_longer(-1)
    
    graf <- ggplot(dado_graf_mult, aes(x = anos, y = value, colour = name, label = sprintf("%0.2f", round(value,2)))) +
      geom_line() + geom_point() +
      scale_x_continuous(breaks = round(seq(min(dado_graf_mult$anos), max(dado_graf_mult$anos), by = 0.5),1)) + 
      scale_y_continuous(breaks = round(seq(min(dado_graf_mult$value), max(dado_graf_mult$value), by = 0.5),1)) + 
      theme(axis.text.x=element_text(angle=90, hjust=1), legend.position = "bottom") + 
      scale_colour_discrete(name = "Datas") + 
      labs(title = "Curva zero", subtitle = resposta_dados,
           caption = "Fonte: Ambima") + ylab("%") + xlab("Anos")
    # show(graf)
    graf <- graf %>% ggplotly() %>% style(hoverinfo = "y")
    show(graf)
  }
}

usuario_data()

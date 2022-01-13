#################################################################################################################
# Shiny app com dados da estrutura a termo da taxa de juros
# Marcelo Vilas Boas de Castro
# Data: 24-06-2021
#################################################################################################################



# Pacotes
library(shiny)
library(tidyverse)
library(rio)
library(openxlsx)
library(plotly)
library(shinydashboard)
library(stringi)



# Importando planilha
# Definindo as datas
datas <- import("curva_zero.xlsx", col_names = F)
datas <- datas[1,]
seleciona_datas <- seq(2,length(datas), 8)
datas_tratadas <- NA
for (i in 1:length(seleciona_datas)){
    datas_tratadas[i] <- datas[1,seleciona_datas[i]]
}
datas_tratadas <- as.vector(as.character((convertToDate(datas_tratadas))))



# Coletando o resto dos dados
dados <- import("curva_zero.xlsx", skip = 5, col_names = F)
seleciona_dados <- seq(1,length(dados), 8)
for (i in 1:length(datas_tratadas)){
    dados_dia <- dados[0:22,seleciona_dados[i]:(seleciona_dados[i]+4)]
    names(dados_dia) <- dados_dia[1,]
    dados_dia <- dados_dia[-1,] %>% mutate_all(function(x) as.numeric(as.character(x))) %>%
        as.data.frame() %>% drop_na()
    names(dados_dia) <- c("anos", "vertices", "real", "nominal", "implicita")
    dados_dia["data"] <- datas_tratadas[i]
    nome_arquivo <- as.character(datas_tratadas[i])
    assign(nome_arquivo, dados_dia) #Nomeando arquivos
    print(paste(i, length(datas_tratadas), sep = '/')) #Printa o progresso
}

dados_real <- data.frame()
for (i in 1:length(datas_tratadas)){
    dados_a_incorporar <- select(get(datas_tratadas[i]), data, anos, real)
    dados_real <- rbind(dados_real, dados_a_incorporar)
    print(paste(i, length(datas_tratadas), sep = '/')) #Printa o progresso
}

dados_nominal <- data.frame()
for (i in 1:length(datas_tratadas)){
    dados_a_incorporar <- select(get(datas_tratadas[i]), data, anos, nominal)
    dados_nominal <- rbind(dados_nominal, dados_a_incorporar)
    print(paste(i, length(datas_tratadas), sep = '/')) #Printa o progresso
}

dados_implicita <- data.frame()
for (i in 1:length(datas_tratadas)){
    dados_a_incorporar <- select(get(datas_tratadas[i]), data, anos, implicita)
    dados_implicita <- rbind(dados_implicita, dados_a_incorporar)
    print(paste(i, length(datas_tratadas), sep = '/')) #Printa o progresso
}


dados_lista <- list(implicita = dados_implicita, nominal = dados_nominal, real = dados_real)



# Servidor
server <- function(input, output, session){

    resposta_data1 <- reactive({
        as.character(input$datas1)
    })
    
    resposta_dados1 <- reactive({
        as.character(input$dados1)
    }) 
    
    dados_graf1 <- reactive({
        req(as.data.frame(dados_lista[[resposta_dados1()]])[as.data.frame(dados_lista[[resposta_dados1()]])$data %in% resposta_data1(),])
    })
    
    tabela1 <- reactive({
        arrange(pivot_wider(dados_graf1(), names_from = data, values_from = !!sym(resposta_dados1())), anos)
    })
    
    opcao_dados1 <- reactive({
        if (resposta_dados1() == "real")
            return("Juros reais")
        if (resposta_dados1() == "nominal")
            return("Juros nominais")
        if (resposta_dados1() == "implicita")
            return("Inflação implícita")
        })
    
    limites1 <- reactive({
        dados_graf1() %>% select(-anos, -data) %>% replace(is.na(.), 0)
    })
    
    n_ticks_y <- reactive({
        if (as.numeric(length(round(seq(min(limites1()), max(limites1()), by = 0.1),1))) <= 20)
            return(round(seq(min(limites1()), max(limites1()), by = 0.1),1))
        if (as.numeric(length(round(seq(min(limites1()), max(limites1()), by = 0.1),1))) <= 40 & as.numeric(length(round(seq(min(limites1()), max(limites1()), by = 0.1),1))) > 20)
            return(round(seq(min(limites1()), max(limites1()), by = 0.2),1))
        if (as.numeric(length(round(seq(min(limites1()), max(limites1()), by = 0.1),1))) < 60 & as.numeric(length(round(seq(min(limites1()), max(limites1()), by = 0.1),1))) > 40)
            return(round(seq(min(limites1()), max(limites1()), by = 0.3),1))
        else
            return(round(seq(min(limites1()), max(limites1()), by = 0.4),1))
    })
    
    
    output$plot1 <- renderPlotly({
        graf1 <- dados_graf1() %>% 
            ggplot(aes(x = anos, y = !!sym(resposta_dados1()), color = data, label = sprintf("%0.2f", round(!!sym(resposta_dados1()),2)))) + 
            geom_line() + geom_point() +
            theme(axis.text.x=element_text(angle=90, hjust=1)) +
            labs(title = "Curvas", subtitle = opcao_dados1(), 
                 caption = "Fonte: Anbima") + ylab("%") + xlab("Anos") + 
            scale_x_continuous(breaks = seq(0.5, 10.5, 0.5)) + 
            scale_y_continuous(breaks = n_ticks_y()) + 
            scale_color_discrete(name = "Datas")
        graf1 <- graf1 %>% ggplotly(height = 700) %>%
            layout(title = list(text = paste0('Estrutura a termo da taxa de juros',
                                              '<br>',
                                              '<sup>',
                                              stri_enc_toutf8(opcao_dados1(), is_unknown_8bit = FALSE, validate = FALSE),
                                              '</sup>'))) %>%
            layout(annotations = list(x = 1.05, y = -0.05, text = "Fonte: Anbima", 
                            showarrow = F, xref='paper', yref='paper', 
                            xanchor='left', yanchor='top', xshift=0, yshift=0,
                            font=list(size=12, color="black"))) %>% 
            style(hoverinfo = "y")
            })
    
    output$table1 <- renderTable({
        # arrange(pivot_wider(dados_graf1(), names_from = data, values_from = !!sym(resposta_dados())), anos)
        tabela1()
        })
    
    output$download1 <- downloadHandler(
        filename = function(){"dados.csv"}, 
        content = function(fname){
            write.csv2(tabela1(),fname, row.names = F)
        })
    
    
    
    resposta_data2 <- reactive({
        as.character(input$datas2)
    })
    
    resposta_data3 <- reactive({
        as.character(input$datas3)
    })
    
    resposta_dados2 <- reactive({
        as.character(input$dados2)
    }) 
    
    dados_graf2 <- reactive({
        req(as.data.frame(dados_lista[[resposta_dados2()]])[as.data.frame(dados_lista[[resposta_dados2()]])$data %in% c(resposta_data2(), resposta_data3()),])
    })
    
    tabela2 <- reactive({
        arrange(pivot_wider(dados_graf2(), names_from = data, values_from = !!sym(resposta_dados2())), anos) %>% transmute(anos = anos, diferenca = !!sym(resposta_data2()) - !!sym(resposta_data3()))
    })
    
    opcao_dados2 <- reactive({
        if (resposta_dados2() == "real")
            return("Juros reais")
        if (resposta_dados2() == "nominal")
            return("Juros nominais")
        if (resposta_dados2() == "implicita")
            return("Inflação implícita")
    })
    
    limites2 <- reactive({
        tabela2() %>% replace(is.na(.), 0)
    })
    
    output$plot2 <- renderPlotly({
        graf2 <- tabela2() %>% 
            ggplot(aes(x = anos, y = diferenca, label = sprintf("%0.2f", diferenca, 2))) + 
            geom_line() + geom_point() +
            theme(axis.text.x=element_text(angle=90, hjust=1)) + 
            labs(title = "Diferenças", subtitle = opcao_dados1(), 
                 caption = "Fonte: Anbima") + ylab("%") + xlab("Anos") + 
            scale_x_continuous(breaks = seq(0.5, 10.5, 0.5)) + 
            scale_y_continuous(breaks = round(seq(min(limites2()), max(limites2()), by = 0.1),1)) + 
            scale_color_discrete(name = "Datas")
        graf2 <- graf2 %>% ggplotly(height = 700) %>% 
            layout(title = list(text = paste0('Diferença entre vértices de datas diferentes',
                                              '<br>',
                                              '<sup>',
                                              stri_enc_toutf8(opcao_dados2(), is_unknown_8bit = FALSE, validate = FALSE),
                                              '</sup>'))) %>%
            layout(annotations = list(x = 1, y = -0.06, text = "Fonte: Anbima", 
                                      showarrow = F, xref='paper', yref='paper', 
                                      xanchor='right', yanchor='top', xshift=0, yshift=0,
                                      font=list(size=12, color="black"))) %>% 
            style(hoverinfo = "y")
    })
    
    output$table2 <- renderTable({
        # arrange(pivot_wider(dados_graf1(), names_from = data, values_from = !!sym(resposta_dados())), anos)
        tabela2()
    })
    
    output$download2 <- downloadHandler(
        filename = function(){"dados.csv"}, 
        content = function(fname){
            write.csv2(tabela2(),fname, row.names = F)
        })
}



# "User interface"

ui <- dashboardPage(
    dashboardHeader(),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Curvas", tabName = "first_app"),
            menuItem("Diferenças", tabName = "second_app")
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "first_app",
                    fluidPage(
                        titlePanel("Estrutura a termo da taxa de juros"),
                        sidebarLayout(
                            sidebarPanel(
                                selectInput(inputId = "datas1",
                                            label = "Escolha alguma(s) data(s):",
                                            choices = datas_tratadas,
                                            multiple = T,
                                            selected = head(datas_tratadas,1)),
                                width = 2,
                                br(),
                                selectInput(inputId = "dados1",
                                            label = "Escolha um tipo de dado:",
                                            choices = c("Juros nominais" = "nominal",
                                                        "Juros reais" = "real",
                                                        "Inflação implícita" = "implicita")),
                                downloadButton('download1',"Download dos dados")),
                            mainPanel(tabsetPanel(type = "tabs",
                                                  tabPanel("Gráfico", plotlyOutput("plot1")),
                                                  tabPanel("Tabela", tableOutput("table1"))),
                                      width = 10)))
            ),
            tabItem(tabName = "second_app",
                    fluidPage(
                        titlePanel("Diferença entre vértices de datas diferentes"),
                        sidebarLayout(
                            sidebarPanel(
                                selectInput(inputId = "datas2",
                                            label = "Escolha uma data:",
                                            choices = datas_tratadas,
                                            multiple = F,
                                            selected = head(datas_tratadas,1)),
                                width = 2,
                                br(),
                                selectInput(inputId = "datas3",
                                            label = "Escolha outra data:",
                                            choices = datas_tratadas,
                                            selected = tail(datas_tratadas,1)),
                                br(),
                                selectInput(inputId = "dados2",
                                            label = "Escolha um tipo de dado:",
                                            choices = c("Juros nominais" = "nominal",
                                                        "Juros reais" = "real",
                                                        "Inflação implícita" = "implicita")),
                                downloadButton('download2',"Download dos dados")),
                            mainPanel(tabsetPanel(type = "tabs",
                                                  tabPanel("Gráfico", plotlyOutput("plot2")),
                                                  tabPanel("Tabela", tableOutput("table2"))),
                                      width = 10))))
        )
    )
)



# Run the application 
shinyApp(ui = ui, server = server)
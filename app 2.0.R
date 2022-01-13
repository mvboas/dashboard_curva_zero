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
library(shinydashboard)
library(GetTDData)



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



# Coletando o resto dos dados
dados_real <- readRDS("dados_real")
dados_real <- df.incorporar %>% select(data, anos, real) %>% rbind(dados_real) %>% distinct()
saveRDS(dados_real, "dados_real")

dados_nominal <- readRDS("dados_nominal")
dados_nominal <- df.incorporar %>% select(data, anos, nominal) %>% rbind(dados_nominal) %>% distinct()
saveRDS(dados_nominal, "dados_nominal")

dados_implicita <- readRDS("dados_implicita")
dados_implicita <- df.incorporar %>% select(data, anos, implicita) %>% rbind(dados_implicita) %>% distinct()
saveRDS(dados_implicita, "dados_implicita")



# Definindo as datas
datas_tratadas <- unique(dados_real$data)



# Unindo dados
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
            return("Real")
        if (resposta_dados1() == "nominal")
            return("Nominal")
        if (resposta_dados1() == "implicita")
            return("InflaÃ§Ã£o implÃ­cita")
        })
    
    limites1 <- reactive({
        dados_graf1() %>% select(-anos, -data) %>% replace(is.na(.), 0)
    })
    
    
    output$plot1 <- renderPlotly({
        graf1 <- dados_graf1() %>% 
            ggplot(aes(x = anos, y = !!sym(resposta_dados1()), color = data, label = sprintf("%0.2f", round(!!sym(resposta_dados1()),2)))) + 
            geom_line() + geom_point() +
            theme(axis.text.x=element_text(angle=90, hjust=1)) + 
            labs(title = "Curvas", subtitle = opcao_dados1(), 
                 caption = "Fonte: Anbima") + ylab("%") + xlab("Anos") + 
            scale_x_continuous(breaks = seq(0.5, 10.5, 0.5)) + 
            scale_y_continuous(breaks = round(seq(min(limites1()), max(limites1()), by = 0.5),1)) + 
            scale_color_discrete(name = "Datas")
        graf1 <- graf1 %>% ggplotly() %>% style(hoverinfo = "y")
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
            return("Real")
        if (resposta_dados2() == "nominal")
            return("Nominal")
        if (resposta_dados2() == "implicita")
            return("InflaÃ§Ã£o implícita")
    })
    
    limites2 <- reactive({
        tabela2() %>% replace(is.na(.), 0)
    })
    
    output$plot2 <- renderPlotly({
        graf2 <- tabela2() %>% 
            ggplot(aes(x = anos, y = diferenca, label = sprintf("%0.2f", diferenca, 2))) + 
            geom_line() + geom_point() +
            theme(axis.text.x=element_text(angle=90, hjust=1)) + 
            labs(title = "Diferença", subtitle = opcao_dados2(), 
                 caption = "Fonte: Anbima") + ylab("%") + xlab("Anos") + 
            scale_x_continuous(breaks = seq(0.5, 10.5, 0.5)) + 
            scale_y_continuous(breaks = round(seq(min(limites2()), max(limites2()), by = 0.5),1)) + 
            scale_color_discrete(name = "Datas")
        graf2 <- graf2 %>% ggplotly() %>% style(hoverinfo = "y")
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
                                br(),
                                selectInput(inputId = "dados1",
                                            label = "Escolha um tipo de dado:",
                                            choices = c("Nominal" = "nominal",
                                                        "Real" = "real",
                                                        "Inflação implícita" = "implicita")),
                                downloadButton('download1',"Download dos dados")),
                            mainPanel(tabsetPanel(type = "tabs",
                                                  tabPanel("Gráfico", plotlyOutput("plot1")),
                                                  tabPanel("Tabela", tableOutput("table1"))))))
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
                                br(),
                                selectInput(inputId = "datas3",
                                            label = "Escolha outra data:",
                                            choices = datas_tratadas,
                                            selected = tail(datas_tratadas,1)),
                                br(),
                                selectInput(inputId = "dados2",
                                            label = "Escolha um tipo de dado:",
                                            choices = c("Nominal" = "nominal",
                                                        "Real" = "real",
                                                        "Inflação implícita" = "implicita")),
                                downloadButton('download2',"Download dos dados")),
                            mainPanel(tabsetPanel(type = "tabs",
                                                  tabPanel("Gráfico", plotlyOutput("plot2")),
                                                  tabPanel("Tabela", tableOutput("table2")))))))
        )
    )
)



# Run the application 
shinyApp(ui = ui, server = server)
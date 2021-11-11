
library(shiny)
library(gsheet)
library(tidyverse)
library(shinyWidgets)
library(corrplot)
library(plotly)
library(FactoMineR)
library(ggrepel)
library(psych)
library(NbClust)
library(shinydashboard)
library(viridis)
library(cluster)
library(factoextra)
library(ggfortify)
library(GGally)
library(ggpubr)



# Import and cleaning

DF <- gsheet2tbl(url = "https://docs.google.com/spreadsheets/d/1IQ_RxxTSmBKHTExlxboIRNlMov_F6RyqdcOPrflCv_w/edit?usp=sharing")



DF <- DF %>% drop_na() %>% filter(between(x = `Escribe tu edad exacta`, 15, 25))



# Statements

statements <- colnames(x = DF %>%
                           select(starts_with(match = "Según tu forma")))



statements <- as.data.frame(x = statements) %>%
    
    separate(col = statements,
             into = c("[", "statements"),
             sep = "\\[") %>%
    
    separate(col = 2,
             into = c("statements", "]"),
             sep = "\\]")  %>%
    
    select(statements)



statements <- as.character(x = statements[,"statements"])



colnames(DF)[6:29] <- statements







for(statement in statements) { 
    
    DF[, statement] <- ifelse(test = DF[, statement] == "Totalmente verdadero",
                              yes = 5,
                              ifelse(test = DF[, statement] == "Un poco verdadero",
                                     yes = 4,
                                     ifelse(test = DF[, statement] == "No lo sé",
                                            yes = 3,
                                            ifelse(test = DF[, statement] == "Un poco falso",
                                                   yes = 2,
                                                   ifelse(test = DF[, statement] == "Totalmente falso",
                                                          yes = 1,
                                                          no = 0)))))
    
}



DF$`Cuando tenga 30 años me gustaría:` <- ifelse(
    
    test = DF$`Cuando tenga 30 años me gustaría:` == "Formar una familia, pues me daría una motivación extra para enfocarme en mis logros personales",
    yes = 1,
    no = 0
    
)



colnames(DF)[30] <- "Cuando tenga 30 años me gustaría formar una familia"



statements <- c(statements,
                "Cuando tenga 30 años me gustaría formar una familia")



# Time in apps

apps <- colnames(x = DF %>%
                     select(starts_with(match = "¿Cuánto tiempo")))



for(app in apps) { 
    
    DF[, app] <- as.numeric(x = unlist(x = DF[, app])) / 60^2
    
}



colnames(DF)[31:34] <- c("TikTok", "Instagram", "Facebook", "YouTube")



ui <- function(request) {
    
    dashboardPage(
        
        dashboardHeader(title = "Segmentación psico-conductual",
                        titleWidth = 350),
        
        
        
        dashboardSidebar(
            
            sidebarMenu(
                
                id = "sidebar",
                
                menuItem(text = "Main dashboard",
                         tabName = "clustering",
                         icon = icon(name = "fas fa-tachometer-alt")),
                
                menuItem(text = "Frases",
                         tabName = "statements_list",
                         icon = icon(name ="far fa-check-square"),
                         startExpanded = T,
                         
                         menuSubItem(text = "Análisis factorial",
                                     tabName = "Relaciones",
                                     icon = icon(name = "fas fa-angle-double-right")),
                         
                         menuSubItem(text = "Agrupación con PCA",
                                     tabName = "PCA",
                                     icon = icon(name = "fas fa-angle-double-right"),
                                     selected = T)),
                
                hr(),
                
                pickerInput(inputId = "FRASES",
                            label = "Frases",
                            choices = statements,
                            selected = statements,
                            multiple = T,
                            options = pickerOptions(actionsBox = T,
                                                    liveSearch = T)),
                
                sliderInput(inputId = "EDAD",
                            label = "Edad",
                            min = min(DF$`Escribe tu edad exacta`,
                                      na.rm = T),
                            max = max(DF$`Escribe tu edad exacta`,
                                      na.rm = T),
                            value = c(min(DF$`Escribe tu edad exacta`,
                                          na.rm = T),
                                      max(DF$`Escribe tu edad exacta`,
                                          na.rm = T)),
                            step = 1,
                            ticks = F),
                
                checkboxGroupButtons(inputId = "SEXO",
                                     label = "Sexo",
                                     choices = levels(x = factor(x = DF$Sexo)),
                                     selected = levels(x = factor(x = DF$Sexo)),
                                     direction = "horizontal",
                                     justified = T),
                
                br(),
                
                bookmarkButton(label = "Save")
                
            )),
        
        
        
        
        dashboardBody(
            
            tags$style(type = "text/css",
                       ".shiny-output-error { visibility: hidden; }",
                       ".shiny-output-error:before { visibility: hidden; }"),
            
            tabItems(
                
                tabItem(tabName = "clustering",
                        h2("Clustering"),
                        
                        fluidRow(
                            
                            column(width = 3,
                                   
                                   box(title = "Definición de segmentos",
                                       status = "warning",
                                       width = NULL,
                                       height = 820,
                                       
                                       sliderInput(inputId = "N.CLUSTERS",
                                                   label = "Cantidad de segmentos",
                                                   min = 3,
                                                   max = 10,
                                                   value = 3,
                                                   step = 1,
                                                   ticks = F),
                                       
                                       fluidRow(valueBoxOutput(outputId = "CLUS.INDEX",
                                                               width = 12)),
                                       
                                       plotlyOutput(outputId = "SILHOUETTE"))),
                            
                            column(width = 3,
                                   
                                   box(title = "Tamaño de los segmentos",
                                       status = "warning",
                                       width = NULL,
                                       height = 400,
                                       
                                       plotlyOutput(outputId = "PIE",
                                                    width = "310px",
                                                    height = "310px")),
                                   
                                   tabBox(title = "Demográficos",
                                          width = NULL,
                                          height = 400,
                                          
                                          tabPanel(title = "Edad",
                                                   plotlyOutput(outputId = "EDAD.PLOT",
                                                                width = "350px",
                                                                height = "325px")),
                                          
                                          tabPanel(title = "Sexo",
                                                   plotlyOutput(outputId = "SEXO.PLOT",
                                                                width = "350px",
                                                                height = "325px")))),
                            
                            column(width = 6,
                                   
                                   tabBox(title = "Contraste entre segmentos",
                                          width = 12,
                                          height = 820,
                                          
                                          tabPanel(title = "Estilo de vida",
                                                   
                                                   br(), br(),
                                                   
                                                   plotOutput(outputId = "LIFESTYLE",
                                                              height = "700px")),
                                          
                                          tabPanel(title = "Frases",
                                                   
                                                   br(), br(),
                                                   
                                                   plotlyOutput(outputId = "FRASES.PLOT",
                                                                height = "700px")),
                                          
                                          tabPanel(title = "Uso de APPS",
                                                   
                                                   br(), br(), br(), br(), br(),
                                                   
                                                   plotlyOutput(outputId = "SPIDER",
                                                                height = "550px"))))
                            
                        )),
                
                tabItem(tabName = "Relaciones",
                        h2("Relaciones entre frases"),
                        
                        br(),
                        
                        tabBox(width = 12,
                               height = 800,
                               
                               tabPanel(title = "Matriz de correlaciones",
                                        plotlyOutput(outputId = "Rmatrix")),
                               
                               tabPanel(title = "Análisis factorial",
                                        
                                        sliderInput(inputId = "N.GRUPOS",
                                                    label = "Cantidad de grupos",
                                                    min = 1,
                                                    max = length(x = statements),
                                                    value = 4,
                                                    step = 1,
                                                    ticks = F),
                                        
                                        verbatimTextOutput(outputId = "Factorial")))
                        
                ),
                
                tabItem(tabName = "PCA",
                        h2("Creación de dimensiones"),
                        
                        fluidRow(
                            
                            column(width = 4,
                                   
                                   box(title = "Definición de dimensiones",
                                       status = "warning",
                                       width = NULL,
                                       
                                       sliderInput(inputId = "N.GRUPOS.2",
                                                   label = "Cantidad de grupos",
                                                   min = 3,
                                                   max = length(x = statements),
                                                   value = 4,
                                                   step = 1,
                                                   ticks = F)),
                                   
                                   fluidRow(
                                       
                                       column(width = 6,
                                              
                                              box(status = "warning",
                                                  width = NULL,
                                                  
                                                  uiOutput("GRUPOS"))),
                                       
                                       column(width = 6,
                                              
                                              box(status = "warning",
                                                  width = NULL,
                                                  
                                                  uiOutput("NAMES")))
                                       
                                   )),
                            
                            column(width = 8,
                                   
                                   box(title = "Principal Component Analysis",
                                       status = "primary",
                                       solidHeader = T,
                                       width = NULL,
                                       
                                       uiOutput(outputId = "GRUPO"),
                                       
                                       verbatimTextOutput(outputId = "PCA.EIGEN")),
                                   
                                   fluidRow(
                                       
                                       column(width = 6,
                                              
                                              tabBox(title = "Primera componente",
                                                     width = NULL,
                                                     height = 450,
                                                     
                                                     tabPanel(title = "Correlaciones",
                                                              plotlyOutput(outputId = "PCA.COR",
                                                                           width = "490px",
                                                                           height = "400px")),
                                                     
                                                     tabPanel(title = "Contribuciones",
                                                              plotlyOutput(outputId = "PCA.WEIGHT",
                                                                           width = "490px",
                                                                           height = "400px")))),
                                       
                                       column(width = 6,
                                              
                                              box(title = "Matriz de correlaciones",
                                                  status = "primary",
                                                  width = NULL,
                                              
                                              plotlyOutput(outputId = "Rmatrix.2")))
                                       
                                       ))
                            
                        )))))
    
}



server <- function(input, output, session) {
    
    DF1 <- reactive({
        
        DF %>%
            
            dplyr::filter(`Escribe tu edad exacta` >= input$EDAD[1] & `Escribe tu edad exacta` <= input$EDAD[2],
                          Sexo %in% input$SEXO)
        
    })
    
    
    
    m <- reactive({
        
        cor(x = DF1()[, input$FRASES],
            use = "pairwise.complete.obs",
            method = "spearman")
        
    })
    
    
    
    observe({
        
        updateSliderInput(session = getDefaultReactiveDomain(),
                          inputId = "N.GRUPOS",
                          label = "Cantidad de grupos",
                          min = 1,
                          max = nrow(x = m()),
                          value = 4,
                          step = 1)
        
        updateSliderInput(session = getDefaultReactiveDomain(),
                          inputId = "N.GRUPOS.2",
                          label = "Cantidad de grupos",
                          min = 3,
                          max = nrow(x = m()),
                          step = 1)
        
    })
    
    
    
    output$Rmatrix <- renderPlotly({
        
        m  <- data.frame(rownames(x = m()), m())
        
        rownames(m) <- NULL
        
        colnames(m)[1] <- "Frase"
        
        
        m <- m %>% pivot_longer(cols = !Frase,
                                names_to = "Frase2",
                                values_to = "cor")
        
        m$Frase2 <- str_replace_all(string = m$Frase2,
                                    pattern = "\\.",
                                    replacement = " ")
        
        m$cor <- round(x = m$cor,
                       digits = 5)
        
        
        
        ggplotly(
            
            ggplot(data = m,
                   aes(x = Frase,
                       y = Frase2,
                       fill = cor)) +
                
                geom_raster() +
                
                scale_fill_viridis_c() +
                
                theme_minimal() +
                
                theme(axis.title = element_blank(),
                      axis.text.x = element_blank(),
                      axis.ticks.x = element_blank(),
                      panel.grid = element_blank()),
            
            autosize = F,
            width = 1300,
            height = 725)
        
        
    })
    
    
    
    output$Factorial <- renderPrint({
        
        fa.sort(fa.results = principal(r = m(),
                                       nfactors = input$N.GRUPOS,
                                       rotate = "varimax",
                                       method = "Thurstone",
                                       n.obs = nrow(x = m())))$loadings
        
    })
    
    
    
    output$GRUPOS <- renderUI({
        
        lapply(X = 1:input$N.GRUPOS.2,
               
               FUN = function(i) {
                   
                   pickerInput(inputId = paste("D",
                                               i,
                                               sep = ""),
                               label = paste("Dimensión",
                                             i,
                                             sep = " "),
                               choices = rownames(x = m()),
                               selected = NULL,
                               multiple = T,
                               options = pickerOptions(actionsBox = T,
                                                       liveSearch = T))
                               
               })
        
    })
    
    
    
    output$NAMES <- renderUI({
        
        lapply(X = 1:input$N.GRUPOS.2,
               
               FUN = function(i) {
                   
                   textInput(inputId = paste("N",
                                             i,
                                             sep = ""),
                             label = paste("Nombre Dim.",
                                           i,
                                           sep = " "))
                   
               })
        
    })
    
    
    
    output$GRUPO <- renderUI({
        
        radioGroupButtons(inputId = "GRUPO.SELECT",
                          label = "Grupo",
                          choices = seq(from = 1,
                                        to = input$N.GRUPOS.2,
                                        by = 1),
                          selected = 1,
                          direction = "horizontal")
        
    })
    
    
    
    PCA <- reactive({
        
        DIM <- as.numeric(x = input$GRUPO.SELECT)
        
        DIMS <- input$N.GRUPOS.2
        
        DIM.MATRIX <- lapply(X = 1:DIMS,
                             FUN = function(i){
                                 
                                 input[[paste("D",
                                              i,
                                              sep = "")]]
                                 
                             })
        
        FactoMineR::PCA(X = DF1()[, DIM.MATRIX[[DIM]]],
                        scale.unit = T,
                        ncp = 1,
                        graph = F)
        
    }) 
    
    
    
    output$PCA.EIGEN <- renderPrint({
        
        cat("*** AUTOVALORES Y VARIACIÓN EXPLICADA ***",
            "\n",
            "\n")
        
        print(PCA()$eig)
        
    })
    
    
    
    output$PCA.COR <- renderPlotly({
        
        COR <- as.data.frame(x = PCA()$var$cor)
        colnames(COR) <- "Corr"
        COR <- data.frame(Frase = stringr::str_wrap(string = rownames(x = COR),
                                                    width = 12),
                          COR)
        
        ggplotly(
            
            ggplot(data = COR,
                   mapping = aes(x = Frase,
                                 y = Corr)) +
                
                geom_bar(stat = "identity",
                         alpha = .75,
                         fill = "#22A884") +
                
                theme_minimal() +
                
                theme(axis.title = element_blank(),
                      axis.text.x = element_text(size = 8),
                      panel.grid.major.x = element_blank(),
                      panel.grid.minor.x = element_blank(),
                      legend.position = "none")
            
        )
        
    })
    
    
    
    output$PCA.WEIGHT <- renderPlotly({
        
        WEIGHT <- as.data.frame(x = PCA()$var$contrib)
        colnames(WEIGHT) <- "Contrib"
        WEIGHT <- data.frame(Frase = stringr::str_wrap(string =rownames(x = WEIGHT),
                                                    width = 12),
                             WEIGHT)
        
        ggplotly(
            
            ggplot(data = WEIGHT,
                   mapping = aes(x = Frase,
                                 y = Contrib,
                                 fill = Frase)) +
                
                geom_bar(stat = "identity",
                         alpha = .75,
                         fill = "#2A788E") +
                
                theme_minimal() +
                
                theme(axis.title = element_blank(),
                      axis.text.x = element_text(size = 8),
                      panel.grid.major.x = element_blank(),
                      panel.grid.minor.x = element_blank(),
                      legend.position = "none")
            
        )
        
    })
    
    
    
    m2 <- reactive({
        
        DIM <- as.numeric(x = input$GRUPO.SELECT)
        
        DIMS <- input$N.GRUPOS.2
        
        DIM.MATRIX <- lapply(X = 1:DIMS,
                             FUN = function(i){
                                 
                                 input[[paste("D",
                                              i,
                                              sep = "")]]
                                 
                             })
        
        cor(x = DF1()[, DIM.MATRIX[[DIM]]],
            use = "pairwise.complete.obs",
            method = "spearman")
        
    })
    
    
    
    output$Rmatrix.2 <- renderPlotly({
        
        m  <- data.frame(rownames(x = m2()), m2())
        
        rownames(m) <- NULL
        
        colnames(m)[1] <- "Frase"
        
        
        m <- m %>% pivot_longer(cols = !Frase,
                                names_to = "Frase2",
                                values_to = "cor")
        
        m$Frase2 <- str_replace_all(string = m$Frase2,
                                    pattern = "\\.",
                                    replacement = " ")
        
        m$cor <- round(x = m$cor,
                       digits = 5)
        
        
        
        ggplotly(
            
            ggplot(data = m,
                   aes(x = Frase,
                       y = Frase2,
                       fill = cor)) +
                
                geom_raster() +
                
                scale_fill_viridis_c(alpha = 1) +
                
                theme_minimal() +
                
                theme(axis.title = element_blank(),
                      axis.text = element_blank(),
                      axis.ticks.x = element_blank(),
                      panel.grid = element_blank())
            
        ) %>%
            
            layout(legend = list(orientation = 'h',
                                 title = "none",
                                 xanchor = "center",
                                 x = .5,
                                 y = 1.2,
                                 font = list(size = 12)))
        
    })
    
    
    
    DF.CLUS <- reactive({
        
        DIMS <- input$N.GRUPOS.2
        
        DIM.MATRIX <- lapply(X = 1:DIMS,
                             FUN = function(i){
                                 
                                 input[[paste("D",
                                              i,
                                              sep = "")]]
                                 
                             })
        
        COORDS <- matrix(nrow = nrow(x = DF1()))
        
        for (DIM in DIM.MATRIX) {
            
            COORD <- if(FactoMineR::PCA(X = DF1()[, DIM],
                                        scale.unit = T,
                                        ncp = 1,
                                        graph = F)$var$cor[1] > 0) {
                
                FactoMineR::PCA(X = DF1()[, DIM],
                                scale.unit = T,
                                ncp = 1,
                                graph = F)$ind$coord
                
            } else {
                
                FactoMineR::PCA(X = DF1()[, DIM],
                                scale.unit = T,
                                ncp = 1,
                                graph = F)$ind$coord * -1
                
            }
            
            COORDS <- data.frame(COORDS, COORD)
            
        }
        
        COORDS <- COORDS[, -1]
        
        colnames(COORDS) <- lapply(X = 1:DIMS,
                                   FUN = function(i){
                                       
                                       input[[paste("N",
                                                    i,
                                                    sep = "")]]
                                       
                                   })
        
        COORDS
        
    })
    
    
    
    CLUSTERING <- reactive({
        
        NbClust(data = scale(x = DF.CLUS()),
                distance = "euclidean",
                method = "ward.D2",
                index = "dunn",
                min.nc = input$N.CLUSTERS,
                max.nc = input$N.CLUSTERS)
        
    })
    
    
    
    DF2 <- reactive({
        
        data.frame(DF.CLUS(),
                   CLUSTER.N = CLUSTERING()$Best.partition,
                   CLUSTER.F = as.factor(x = paste("Cluster",
                                                   as.character(x = CLUSTERING()$Best.partition))),
                   DF1() %>% dplyr::select(TikTok:YouTube))
        
    })
    
    
    
    output$PIE <- renderPlotly({
        
        table <- as.data.frame(x = table(as.factor(x = DF2()$CLUSTER.F)))
        
        colnames(table) <- c("Cluster",
                             "Freq")
        
        plot_ly(data = table,
                labels = ~ Cluster,
                values = ~ Freq,
                type = "pie",
                textposition = "inside",
                textinfo = "label+percent",
                marker = list(colors = viridis(n = input$N.CLUSTERS,
                                               alpha = .75),
                              line = list(color = '#FFFFFF',
                                          width = 5)),
                showlegend = F)
        
    })
    
    
    
    output$EDAD.PLOT <- renderPlotly({
        
        DF <- data.frame(Segmento = DF2()$CLUSTER.F,
                         Edad = DF1()$`Escribe tu edad exacta`)
        
        ggplotly(
            
            ggplot(data = DF,
                   mapping = aes(x = Segmento,
                                 y = Edad,
                                 fill = Segmento)) +
                
                geom_boxplot() +
                
                scale_fill_viridis_d(alpha = .75) +
                
                theme_minimal() +
                
                theme(axis.title = element_blank(),
                      axis.text.x = element_text(angle = 90),
                      panel.grid.major.x = element_blank(),
                      legend.position = "none")
            
        )
        
    })
    
    
    
    output$SEXO.PLOT <- renderPlotly({
        
        DF <- data.frame(Segmento = DF2()$CLUSTER.F,
                         Sexo = DF1()$Sexo)
        
        DF <- DF %>% dplyr::count(Segmento, Sexo)
        
        ggplotly(
            
            ggplot(data = DF,
                   mapping = aes(x = Segmento,
                                 y = n,
                                 fill = Sexo)) +
                
                geom_bar(stat = "identity") +
                
                scale_fill_manual(values = c("#22A884",
                                             "#2A788E")) +
                
                theme_minimal() +
                
                theme(axis.title = element_blank(),
                      axis.text.x = element_text(angle = 90),
                      panel.grid.major.x = element_blank())
            
        ) %>%
            
            layout(legend = list(orientation = 'h',
                                 title = "none",
                                 xanchor = "center",
                                 x = .5,
                                 y = 1.2,
                                 font = list(size = 12)))
        
    })
    
    
    
    output$SILHOUETTE <- renderPlotly({
        
        ggplotly(
            
            ggplot2::autoplot(object = silhouette(x = DF2()$CLUSTER.N,
                                                  dist = dist(x = DF2() %>% dplyr::select(!(CLUSTER.N:YouTube)),
                                                              method = "euclidean"))) +
                
                scale_fill_viridis_d(alpha = .75) +
                
                theme_minimal() +
                
                theme(legend.position = "none",
                      panel.grid.major.y = element_blank(),
                      axis.text.y = element_blank(),
                      axis.title = element_text(size = 10)),
            
            height = 550
            
        ) %>%
            
            layout(xaxis = list(autorange = T),
                   yaxis = list(autorange = T))
        
    })
    
    
    
    output$CLUS.INDEX <- renderValueBox({
        
        valueBox(value = CLUSTERING()$All.index,
                 subtitle = "Dunn index - Mientras mayor sea, mejor",
                 icon = icon("fas fa-users"),
                 color = "purple")
    })
    
    
    
    table1 <- reactive({
        
        DF <- DF2() %>%
            
            dplyr::select(!c(CLUSTER.N,
                             TikTok:YouTube)) %>%
            
            dplyr::mutate(across(where(is.numeric),
                                 function(x) {
                                     
                                     (x-min(x))/(max(x)-min(x))
                                     
                                 }))
        
        
        as.data.frame(x = t(x = rowsum(x = DF %>% select(-CLUSTER.F),
                                       group = DF$CLUSTER.F,
                                       na.rm = T)))
        
    })
    
    
    
    output$LIFESTYLE <- renderPlot({
        
        ca <- CA(X = table1(),
                 ncp = 2)
        
        dim <- data.frame(rownames(x = ca$row$coord),
                          rep(x = "Dimensión",
                              times = nrow(x = ca$row$coord)),
                          ca$row$coord)
        
        colnames(dim)[1:2] <- c("DIMENSIÓN",
                                "TIPO")
        rownames(dim) <- NULL
        
        cluster <- data.frame(rownames(x = ca$col$coord),
                              rep(x = "Segmento",
                                  times = nrow(x = ca$col$coord)),
                              ca$col$coord)
        
        colnames(cluster)[1:2] <- c("DIMENSIÓN",
                                    "TIPO")
        rownames(cluster) <- NULL
        
        DF <- rbind(dim,
                    cluster)
        
        ggplot(data = DF,
               
               mapping = aes(x = Dim.1,
                             y = Dim.2,
                             label = `DIMENSIÓN`)) +
            
            geom_hline(aes(yintercept = 0),
                       linetype = "dashed",
                       color = "#787878") +
            
            geom_vline(aes(xintercept = 0),
                       linetype = "dashed",
                       color = "#787878") +
            
            geom_point(aes(colour = TIPO),
                       size = 4) +
            
            xlim(c(min(min(DF$Dim.1),
                       min(DF$Dim.2)),
                   max(max(DF$Dim.1),
                       max(DF$Dim.2)))) +
            
            ylim(c(min(min(DF$Dim.1),
                       min(DF$Dim.2)),
                   max(max(DF$Dim.1),
                       max(DF$Dim.2)))) +
            
            coord_fixed(ratio = 1) +
            
            scale_colour_manual(values = c("#22A884",
                                           "#2A788E")) +
            
            geom_label_repel(label.size = NA) +
            
            labs(title = element_blank(),
                 x = element_blank(),
                 y = element_blank()) +
            
            theme(legend.position = "none",
                  panel.background = element_rect(fill = "white",
                                                  colour = "black"),
                  panel.grid = element_blank())
        
    })
    
    
    
    output$FRASES.PLOT <- renderPlotly({
        
        data <- data.frame(DF1()[, input$FRASES],
                           DF2()[, "CLUSTER.F"])
        
        colnames(data)[ncol(x = data)] <- "CLUSTER.F" 
        
        data <- data %>%
            dplyr::group_by(CLUSTER.F) %>%
            dplyr::mutate(across(where(is.numeric),
                                 mean))
        
        ggplotly(
            
            ggparcoord(data = data,
                       columns = 1:(ncol(data) - 1),
                       scale = "std",
                       groupColumn = ncol(data),
                       showPoints = TRUE) +
                
                scale_color_viridis(discrete = TRUE) +
                
                theme_minimal() +
                
                theme(axis.text.y = element_text(size = 8),
                      axis.title = element_blank()) +
                
                rotate()
            
        ) %>%
            
            layout(legend = list(title = "none",
                                 orientation = 'h',
                                 xanchor = "center",
                                 x = .5))
        
    })
    
    
    
    output$SPIDER <- renderPlotly({
        
        data <- DF2() %>%
            
            dplyr::group_by(CLUSTER.F) %>%
            
            dplyr::summarize(TikTok = mean(TikTok, na.rm = T),
                             Instagram = mean(Instagram, na.rm = T),
                             Facebook = mean(Facebook, na.rm = T),
                             YouTube = mean(YouTube, na.rm = T))
        
        
        
        plot_ly(type = 'scatterpolar',
                fill = 'toself') %>%
            
            add_trace(r = data$TikTok,
                      theta = data$CLUSTER.F,
                      name = "TikTok")%>%
            
            add_trace(r = data$Instagram,
                      theta = data$CLUSTER.F,
                      name = "Instagram")%>%
            
            add_trace(r = data$Facebook,
                      theta = data$CLUSTER.F,
                      name = "Facebook")%>%
            
            add_trace(r = data$YouTube,
                      theta = data$CLUSTER.F,
                      name = "YouTube") %>%
            
            layout(legend = list(orientation = 'h',
                                 xanchor = "center",
                                 x = .5))
        
    })
    
}



shinyApp(ui = ui,
         server = server,
         enableBookmarking = "url")













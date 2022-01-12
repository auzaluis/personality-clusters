
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

DF <- gsheet2tbl(url = "https://docs.google.com/spreadsheets/d/1JDiRx-UdMTUz7XcEuwUpsXWV0UqyZo2DS5etYE9BJFw/edit?usp=sharing")



DF <- DF %>% drop_na() %>% filter(between(x = `Type your age`, 15, 25))



# Statements

statements <- colnames(x = DF %>%
                           select(starts_with(match = "According to your way of being")))



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
    
    DF[, statement] <- ifelse(test = DF[, statement] == "Strongly agree",
                              yes = 5,
                              ifelse(test = DF[, statement] == "Agree",
                                     yes = 4,
                                     ifelse(test = DF[, statement] == "Neither agree nor disagree",
                                            yes = 3,
                                            ifelse(test = DF[, statement] == "Disagree",
                                                   yes = 2,
                                                   ifelse(test = DF[, statement] == "Strongly disagree",
                                                          yes = 1,
                                                          no = 0)))))
    
}



DF$`When I am 30 years old I would like to...` <- ifelse(
    
    test = DF$`When I am 30 years old I would like to...` == "Have a family, because they would give me extra motivation to focus on my personal goals",
    yes = 1,
    no = 0
    
)



colnames(DF)[30] <- "When I am 30 years old I would like to have a family"



statements <- c(statements,
                "When I am 30 years old I would like to have a family")



# Time in apps

apps <- colnames(x = DF %>%
                     select(starts_with(match = "How much time")))



for(app in apps) { 
    
    DF[, app] <- as.numeric(x = unlist(x = DF[, app])) / 60^2
    
}



colnames(DF)[31:34] <- c("TikTok", "Instagram", "Facebook", "YouTube")



ui <- function(request) {
    
    dashboardPage(
        
        dashboardHeader(title = "Psychographic Segmentation",
                        titleWidth = 350),
        
        
        
        dashboardSidebar(
            
            sidebarMenu(
                
                id = "sidebar",
                
                menuItem(text = "Main dashboard",
                         tabName = "clustering",
                         icon = icon(name = "fas fa-tachometer-alt"),
                         
                         menuSubItem(text = "Definition",
                                     tabName = "Definition",
                                     icon = icon(name = "fas fa-angle-double-right")),
                         
                         menuSubItem(text = "Quotes",
                                     tabName = "Quotes",
                                     icon = icon(name = "fas fa-angle-double-right"))),
                
                menuItem(text = "Reductions",
                         tabName = "statements_list",
                         icon = icon(name ="far fa-check-square"),
                         startExpanded = T,
                         
                         menuSubItem(text = "Factor Analysis",
                                     tabName = "Relationships",
                                     icon = icon(name = "fas fa-angle-double-right")),
                         
                         menuSubItem(text = "PCA",
                                     tabName = "PCA",
                                     icon = icon(name = "fas fa-angle-double-right"),
                                     selected = T)),
                
                hr(),
                
                pickerInput(inputId = "QUOTES",
                            label = "Quotes",
                            choices = statements,
                            selected = statements,
                            multiple = T,
                            options = pickerOptions(actionsBox = T,
                                                    liveSearch = T,
                                                    size = 8)),
                
                sliderInput(inputId = "AGE",
                            label = "Age",
                            min = min(DF$`Type your age`,
                                      na.rm = T),
                            max = max(DF$`Type your age`,
                                      na.rm = T),
                            value = c(min(DF$`Type your age`,
                                          na.rm = T),
                                      max(DF$`Type your age`,
                                          na.rm = T)),
                            step = 1,
                            ticks = F),
                
                checkboxGroupButtons(inputId = "GENDER",
                                     label = "Gender",
                                     choices = levels(x = factor(x = DF$Gender)),
                                     selected = levels(x = factor(x = DF$Gender)),
                                     direction = "horizontal",
                                     justified = T),
                
                br(),
                
                bookmarkButton(label = "Save")
                
            )),
        
        
        
        
        dashboardBody(
            
            tags$style(type = "text/css",
                       ".shiny-output-error { visibility: hidden; }",
                       ".shiny-output-error:before { visibility: hidden; }"),
            
            fluidRow(
            
              tabItems(
                
                tabItem(tabName = "Definition",
                        h2("Clustering"),
                        
                        column(width = 5,
                               
                               tabPanel(title = "Definition",
                                        
                                        sliderInput(inputId = "N.CLUSTERS",
                                                    label = "Number of clusters",
                                                    min = 3,
                                                    max = 10,
                                                    value = 3,
                                                    step = 1,ticks = F),
                                        
                                        fluidRow(valueBoxOutput(outputId = "CLUS.INDEX",
                                                                width = 12)))),
                        
                        column(width = 7,
                               
                               tabBox(width = 12,
                                      
                                      tabPanel(title = "Silhouttes",
                                               plotlyOutput(outputId = "SILHOUETTE")),
                                      
                                      tabPanel(title = "Size",
                                               plotlyOutput(outputId = "PIE")),
                                      
                                      tabPanel(title = "Lifestyle",
                                               plotOutput(outputId = "LIFESTYLE")),
                                      
                                      tabPanel(title = "App usage",
                                               plotlyOutput(outputId = "SPIDER")),
                                      
                                      tabPanel(title = "Age",
                                               plotlyOutput(outputId = "AGE.PLOT")),
                                      
                                      tabPanel(title = "Gender",
                                               plotlyOutput(outputId = "GENDER.PLOT"))))),
                
                tabItem(tabName = "Quotes",
                        br(),
                        
                        column(width = 12,
                               plotlyOutput(outputId = "QUOTES.PLOT"))),
                
                tabItem(tabName = "Relationships",
                        h2("Relationships among quotes"),
                        
                        tabBox(width = 12,
                               
                               tabPanel(title = "Correlation Matrix",
                                        plotlyOutput(outputId = "Rmatrix")),
                               
                               tabPanel(title = "Factor Analysis",
                                        
                                        sliderInput(inputId = "N.GROUPS",
                                                    label = "Number of groups",
                                                    min = 1,
                                                    max = length(x = statements),
                                                    value = 4,
                                                    step = 1,
                                                    ticks = F),
                                        
                                        verbatimTextOutput(outputId = "Factor")))),
                
                tabItem(tabName = "PCA",
                        h2("Dimensions"),
                        
                        column(width = 5,
                               
                               column(width = 12,
                                      
                                      box(title = "Dimensionality reduction",
                                          status = "warning",
                                          width = NULL,
                                          
                                          sliderInput(inputId = "N.GROUPS.2",
                                                      label = "Number of groups",
                                                      min = 3,
                                                      max = length(x = statements),
                                                      value = 4,
                                                      step = 1,
                                                      ticks = F))),
                               
                               
                               
                               column(width = 6,
                                      
                                      box(status = "warning",
                                          width = NULL,
                                          
                                          uiOutput("GROUPS"))),
                               
                               column(width = 6,
                                      
                                      box(status = "warning",
                                          width = NULL,
                                          
                                          uiOutput("NAMES")))),
                        
                        column(width = 7,
                               
                               column(width = 12,
                                      
                                      box(title = "Principal Component Analysis",
                                          status = "primary",
                                          solidHeader = T,
                                          width = NULL,
                                          
                                          uiOutput(outputId = "GROUP"),
                                          
                                          verbatimTextOutput(outputId = "PCA.EIGEN"))),
                               
                               column(width = 12,
                                      
                                      tabBox(title = "First component",
                                             width = NULL,
                                             height = 450,
                                             
                                             #tabPanel(title = "Correlation matrix",
                                             #plotlyOutput(outputId = "Rmatrix.2")),
                                             
                                             tabPanel(title = "Correlations",
                                                      plotlyOutput(outputId = "PCA.COR")),
                                             
                                             tabPanel(title = "Contributions",
                                                      plotlyOutput(outputId = "PCA.WEIGHT")))))
                        
                )))))
    
}



server <- function(input, output, session) {
    
    DF1 <- reactive({
        
        DF %>%
            
            dplyr::filter(`Type your age` >= input$AGE[1] & `Type your age` <= input$AGE[2],
                          Gender %in% input$GENDER)
        
    })
    
    
    
    m <- reactive({
        
        cor(x = DF1()[, input$QUOTES],
            use = "pairwise.complete.obs",
            method = "spearman")
        
    })
    
    
    
    observe({
        
        updateSliderInput(session = getDefaultReactiveDomain(),
                          inputId = "N.GROUPS",
                          label = "Number of groups",
                          min = 1,
                          max = nrow(x = m()),
                          value = 4,
                          step = 1)
        
        updateSliderInput(session = getDefaultReactiveDomain(),
                          inputId = "N.GROUPS.2",
                          label = "Number of groups",
                          min = 3,
                          max = nrow(x = m()),
                          step = 1)
        
    })
    
    
    
    output$Rmatrix <- renderPlotly({
        
        m  <- data.frame(rownames(x = m()), m())
        
        rownames(m) <- NULL
        
        colnames(m)[1] <- "Quote"
        
        
        m <- m %>% pivot_longer(cols = !Quote,
                                names_to = "Quote2",
                                values_to = "cor")
        
        m$Quote2 <- str_replace_all(string = m$Quote2,
                                    pattern = "\\.",
                                    replacement = " ")
        
        m$cor <- round(x = m$cor,
                       digits = 5)
        
        
        
        ggplotly(
            
            ggplot(data = m,
                   aes(x = Quote,
                       y = Quote2,
                       fill = cor)) +
                
                geom_raster() +
                
                scale_fill_viridis_c() +
                
                theme_minimal() +
                
                theme(axis.title = element_blank(),
                      axis.text.x = element_blank(),
                      axis.ticks.x = element_blank(),
                      panel.grid = element_blank()))
        
        
    })
    
    
    
    output$Factor <- renderPrint({
        
        fa.sort(fa.results = principal(r = m(),
                                       nfactors = input$N.GROUPS,
                                       rotate = "varimax",
                                       method = "Thurstone",
                                       n.obs = nrow(x = m())))$loadings
        
    })
    
    
    
    output$GROUPS <- renderUI({
        
        lapply(X = 1:input$N.GROUPS.2,
               
               FUN = function(i) {
                   
                   pickerInput(inputId = paste("D",
                                               i,
                                               sep = ""),
                               label = paste("Dimension",
                                             i,
                                             sep = " "),
                               choices = rownames(x = m()),
                               selected = NULL,
                               multiple = T,
                               options = pickerOptions(actionsBox = T,
                                                       liveSearch = T,
                                                       size = 5))
                               
               })
        
    })
    
    
    
    output$NAMES <- renderUI({
        
        lapply(X = 1:input$N.GROUPS.2,
               
               FUN = function(i) {
                   
                   textInput(inputId = paste("N",
                                             i,
                                             sep = ""),
                             label = paste("Name of Dim.",
                                           i,
                                           sep = " "))
                   
               })
        
    })
    
    
    
    output$GROUP <- renderUI({
        
        radioGroupButtons(inputId = "SELECT.GROUP",
                          label = "Group",
                          choices = seq(from = 1,
                                        to = input$N.GROUPS.2,
                                        by = 1),
                          selected = 1,
                          direction = "horizontal")
        
    })
    
    
    
    PCA <- reactive({
        
        DIM <- as.numeric(x = input$SELECT.GROUP)
        
        DIMS <- input$N.GROUPS.2
        
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
        
        cat("*** EIGENVALUES AND VARIANCE ***",
            "\n",
            "\n")
        
        print(PCA()$eig)
        
    })
    
    
    
    output$PCA.COR <- renderPlotly({
        
        COR <- as.data.frame(x = PCA()$var$cor)
        colnames(COR) <- "Corr"
        COR <- data.frame(Quote = stringr::str_wrap(string = rownames(x = COR),
                                                    width = 18),
                          COR)
        
        ggplotly(
            
            ggplot(data = COR,
                   mapping = aes(x = Quote,
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
        WEIGHT <- data.frame(Quote = stringr::str_wrap(string =rownames(x = WEIGHT),
                                                    width = 18),
                             WEIGHT)
        
        ggplotly(
            
            ggplot(data = WEIGHT,
                   mapping = aes(x = Quote,
                                 y = Contrib,
                                 fill = Quote)) +
                
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
        
        DIM <- as.numeric(x = input$SELECT.GROUP)
        
        DIMS <- input$N.GROUPS.2
        
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
        
        colnames(m)[1] <- "Quote"
        
        
        m <- m %>% pivot_longer(cols = !Quote,
                                names_to = "Quote2",
                                values_to = "cor")
        
        m$Frase2 <- str_replace_all(string = m$Quote2,
                                    pattern = "\\.",
                                    replacement = " ")
        
        m$cor <- round(x = m$cor,
                       digits = 5)
        
        
        
        ggplotly(
            
            ggplot(data = m,
                   aes(x = Quote,
                       y = Quote2,
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
        
        DIMS <- input$N.GROUPS.2
        
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
    
    
    
    output$AGE.PLOT <- renderPlotly({
        
        DF <- data.frame(Cluster = DF2()$CLUSTER.F,
                         Edad = DF1()$`Type your age`)
        
        ggplotly(
            
            ggplot(data = DF,
                   mapping = aes(x = Cluster,
                                 y = Edad,
                                 fill = Cluster)) +
                
                geom_boxplot() +
                
                scale_fill_viridis_d(alpha = .75) +
                
                theme_minimal() +
                
                theme(axis.title = element_blank(),
                      axis.text.x = element_text(angle = 90),
                      panel.grid.major.x = element_blank(),
                      legend.position = "none")
            
        )
        
    })
    
    
    
    output$GENDER.PLOT <- renderPlotly({
        
        DF <- data.frame(Cluster = DF2()$CLUSTER.F,
                         Gender = DF1()$Gender)
        
        DF <- DF %>% dplyr::count(Cluster, Gender)
        
        ggplotly(
            
            ggplot(data = DF,
                   mapping = aes(x = Cluster,
                                 y = n,
                                 fill = Gender)) +
                
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
                axis.title = element_text(size = 10))
        
      ) %>%
        
        layout(xaxis = list(autorange = T),
               yaxis = list(autorange = T))
      
    })
    
    
    
    output$CLUS.INDEX <- renderValueBox({
      
        valueBox(value = CLUSTERING()$All.index,
                 subtitle = "Dunn index - The higher the better",
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
                          rep(x = "Dimension",
                              times = nrow(x = ca$row$coord)),
                          ca$row$coord)
        
        colnames(dim)[1:2] <- c("DIMENSION",
                                "TYPE")
        rownames(dim) <- NULL
        
        cluster <- data.frame(rownames(x = ca$col$coord),
                              rep(x = "Cluster",
                                  times = nrow(x = ca$col$coord)),
                              ca$col$coord)
        
        colnames(cluster)[1:2] <- c("DIMENSION",
                                    "TYPE")
        rownames(cluster) <- NULL
        
        DF <- rbind(dim,
                    cluster)
        
        ggplot(data = DF,
               
               mapping = aes(x = Dim.1,
                             y = Dim.2,
                             label = `DIMENSION`)) +
            
            geom_hline(aes(yintercept = 0),
                       linetype = "dashed",
                       color = "#787878") +
            
            geom_vline(aes(xintercept = 0),
                       linetype = "dashed",
                       color = "#787878") +
            
            geom_point(aes(colour = TYPE),
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
    
    
    
    output$QUOTES.PLOT <- renderPlotly({
        
        data <- data.frame(DF1()[, input$QUOTES],
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













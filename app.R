options(encoding = "UTF-8")
options(OutDec = ",")

source("dependencies.R") # https://github.com/rstudio/shinydashboard/issues/190
source("aux_shiny.R")
load("pre_shiny.RData")

# https://fontawesome.com/icons?from=io

flags_df = tibble(Pais = c("Argentina", "Bolívia", "Brasil", "Chile", "Colômbia", "Equador",
                           "Paraguai", "Peru", "Uruguai", "Venezuela", "Continental"),
                  flag = c('<img src="https://flagpedia.net/data/flags/mini/ar.png" width="29" height="20" /></a>',
                           '<img src="https://flagpedia.net/data/flags/mini/bo.png" width="29" height="20" /></a>',
                           '<img src="https://flagpedia.net/data/flags/mini/br.png" width="29" height="20" /></a>',
                           '<img src="https://flagpedia.net/data/flags/mini/cl.png" width="29" height="20" /></a>',
                           '<img src="https://flagpedia.net/data/flags/mini/co.png" width="29" height="20" /></a>',
                           '<img src="https://flagpedia.net/data/flags/mini/ec.png" width="29" height="20" /></a>',
                           '<img src="https://flagpedia.net/data/flags/mini/py.png" width="29" height="20" /></a>',
                           '<img src="https://flagpedia.net/data/flags/mini/pe.png" width="29" height="20" /></a>',
                           '<img src="https://flagpedia.net/data/flags/mini/uy.png" width="29" height="20" /></a>',
                           '<img src="https://flagpedia.net/data/flags/mini/ve.png" width="29" height="20" /></a>',
                           '<img src="https://upload.wikimedia.org/wikipedia/en/thumb/a/a8/CONMEBOL_logo_%282017%29.svg/278px-CONMEBOL_logo_%282017%29.svg.png" width="29" height="20" /></a>'))

#################################################################################

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Introdução", icon = icon("youtube"), tabName = "int"), 
    menuItem("Dados", icon = icon("database"), tabName = "dados",
             menuSubItem("Fonte dos dados", tabName = "fonte"),
             menuSubItem("Tratamento na base", tabName = "tratamento")),
    menuItem("Análise exploratória", icon = icon("chart-bar"), tabName = "eda",
             menuSubItem("Partidas por confederação", tabName = "partidas"),
             menuSubItem("Placares mais comuns", tabName = "placares"),
             menuSubItem("Mando de campo", tabName = "mando"),
             menuSubItem("Confrontos internacionais", tabName = "confrontos"),
             menuSubItem("Estatísticas dos clubes", tabName = "estatisticas")),
    menuItem("Modelagem", icon = icon("question"), tabName = "model"),
    menuItem("Rankings", icon = icon("trophy"), tabName = "rank"),
    menuItem("Histórico", icon = icon("chart-line"), tabName = "hist"),
    menuItem("Resultados", icon = icon("poll-h"), tabName = "result",
             menuSubItem("Maiores pontuações", tabName = "maiores_pontuacoes"),
             menuSubItem("Mais tempo na liderança", tabName = "tempo_lideranca")),
    menuItem("Próximos passos", icon = icon("forward"), tabName = "next"),
    menuItem("Github", icon = icon("github"), href = "https://github.com/luizfgnmaia/FDS-Final-Project")
  )
)

#################################################################################

body <- dashboardBody(
  tabItems(
    
    # Introdução
    #################################################################################

    tabItem(tabName = "int",
            HTML('<iframe width="600" height="400" src="https://www.youtube.com/embed/AREB7MCGaUY" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
            h2("Introdução")),
    
    #################################################################################
    
    # Rankings
    #################################################################################
    
    tabItem(tabName = "rank",
            h2("Ranking mensal geral ou por país"),
            mydateInput("rank_data", 
                        "Mês", 
                        format = "mm-yyyy", 
                        language = "pt-BR",
                        width = 78,
                        min = datas[2],
                        max = datas[len_datas],
                        value = datas[len_datas]), 
            uiOutput("rank_ui"),
            tableOutput("rank_table")
    ),
    
    #################################################################################
    
    # Histórico
    #################################################################################
    
    tabItem(h2("Série temporal da pontuação dos clubes"),
            tabName = "hist",
            uiOutput("hist_ui"), # https://stackoverflow.com/questions/40996536/shiny-r-select-input-is-not-working
            plotlyOutput("hist_plot"),
            textOutput("debug")
    ),
    
    #################################################################################
    
    # Análise exploratória 
    #################################################################################
    
    tabItem(tabName = "partidas",
            h2("Quantidade de partidas por confederação"),
            plotlyOutput("partidas")),
    
    tabItem(tabName = "placares",
            h2("Placares mais comuns"),
            plotlyOutput("placares")),
    
    tabItem(tabName = "mando",
            h2("Influência do mando de campo por confederação"),
            plotlyOutput("mando")),
    
    tabItem(tabName = "confrontos",
            h2("Aproveitamento nos confrontos internacionais"),
            HTML('A entrada <b>(i,j)</b> desta matriz representa o aproveitamento dos clubes do país <b>i</b>  contra adversários do país <b>j</b>.'),
            plotlyOutput("df_confrontos"),
            plotlyOutput("plot_confrontos")),
    
    tabItem(tabName = "estatisticas",
            h2("Estatísticas dos clubes"),
            uiOutput("estatisticas_ui"),
            dataTableOutput("estatisticas")),
    
    #################################################################################
    
    # Dados
    #################################################################################
    
    tabItem(tabName = "fonte",
            h2("Fonte dos dados"),
            HTML('Os dados foram coletados do site <a href="https://www.flashscore.com/">FlashScore.com</a>
                 utilizando o pacote <a href="https://pypi.org/project/selenium/">Selenium</a> do Python.'),
            h2("Base de dados"),
            HTML('A base de dados utilizado para a construção do ranking é composta pelos resultados das seguintes competições:'),
            tableOutput("competicoes")
            ),
    
    tabItem(tabName = "tratamento"),
    
    #################################################################################
    
    # Modelagem
    #################################################################################
    
    tabItem(tabName = "model"),
    
    #################################################################################
    
    # Resultados
    #################################################################################
    
    tabItem(tabName = "maiores_pontuacoes",
            h2("Melhores pontuações da medida Elo"),
            tableOutput("maiores_pontuacoes")),
    
    tabItem(tabName = "tempo_lideranca",
            h2("Clubes que permaneceram mais meses consecutivos no primeiro lugar do ranking"),
            tableOutput("tempo_lideranca")),
    
    # Próximos passos
    #################################################################################
    
    tabItem(tabName = "next")
    
    #################################################################################
    
            )
    )

ui <- dashboardPage(skin = "red",
                    dashboardHeader(title = ""),
                    sidebar,
                    body
)

server <- function(input, output) {
  
  output$hist_plot <- renderPlotly({
     
    validate(need(!is.na(input$hist_clube), message = "")) # https://stackoverflow.com/questions/42789819/prevent-error-in-shiny-app-render-plot
    
    p = dados %>%
      filter(Clube %in% input$hist_clube) %>%
      mutate(text = paste0("Clube: ", Clube, "<br>", 
                           "Mês: ", str_to_lower(format(Data, "%b %Y")), "<br>",
                           "Elo: ", round(Elo, 2))) %>%
      ggplot(aes(x = Data, y = Elo, group = 1, text = text)) + # https://stackoverflow.com/questions/45948926/ggplotly-text-aesthetic-causing-geom-line-to-not-display
      geom_line(aes(color = Clube), size = 0.75) +
      tema +
      xlab("")
    
    p %>%
      ggplotly(tooltip = c("text")) %>% 
      layout(plot_bgcolor = 'rgba(0, 0, 0, 0)', # https://community.plot.ly/t/create-plots-with-transparent-background/14658
             paper_bgcolor = 'rgba(0, 0, 0, 0)')
    

  })
  
  output$hist_ui <- renderUI({
    selectInput("hist_clube",
                "Clube",
                choices = clubes,
                selected = clubes[1],
                multiple = TRUE)
  })
  
  output$rank_ui <- renderUI({
    selectInput("rank_pais",
                "País",
                choices = paises,
                selected = NULL,
                multiple = TRUE)
  })
  
  output$rank_table <- renderTable({
    
    reac_rank_table = reactive({
      
      data_atual = datas[which(abs(datas-input$rank_data) == min(abs(datas-input$rank_data)))]
      data_anterior = datas[which(abs(datas-input$rank_data) == min(abs(datas-input$rank_data)))-1]

      if(is.null(input$rank_pais)) {
        
        atual = dados %>%
          filter(Data == data_atual) %>%
          arrange(desc(Elo)) %>%
          mutate(new_pos = row_number())
        
        anterior = dados %>%
          filter(Data == data_anterior) %>%
          arrange(desc(Elo)) %>%
          mutate(old_pos = row_number()) %>%
          rename(old_elo = Elo) %>%
          select(Clube, old_pos, old_elo)
        
        atual %>%
          inner_join(anterior) %>%
          mutate(dif_pos = ifelse((old_pos - new_pos) != 0, as.character(old_pos - new_pos), ""),
                 tmp_pos = up_or_down(dif_pos),
                 Clube = paste(Clube, tmp_pos, ifelse(dif_pos == "", "", as.character(abs(as.integer(dif_pos))))),
                 dif_elo = Elo - old_elo,
                 Elo = paste(round(Elo, 2), delta(dif_elo))) %>%
          rename('Elo <font style="opacity:.5"> (variação)' = Elo) %>%
          inner_join(flags_df) %>%
          mutate(Clube = paste(flag, Clube)) %>%
          select(Clube, 'Elo <font style="opacity:.5"> (variação)')
        
      } else {
        
        atual = dados %>%
          filter(Data == data_atual,
                 Pais %in% input$rank_pais) %>%
          arrange(desc(Elo)) %>%
          mutate(new_pos = row_number())
        
        anterior = dados %>%
          filter(Data == data_anterior,
                 Pais %in% input$rank_pais) %>%
          arrange(desc(Elo)) %>%
          mutate(old_pos = row_number()) %>%
          rename(old_elo = Elo) %>%
          select(Clube, old_pos, old_elo)
        
        atual %>%
          inner_join(anterior) %>%
          mutate(dif_pos = ifelse((old_pos - new_pos) != 0, as.character(old_pos - new_pos), ""),
                 tmp_pos = up_or_down(dif_pos),
                 Clube = paste(Clube, tmp_pos, ifelse(dif_pos == "", "", as.character(abs(as.integer(dif_pos))))),
                 dif_elo = Elo - old_elo,
                 Elo = paste(round(Elo, 2), delta(dif_elo))) %>%
          rename('Elo <font style="opacity:.5"> (variação)' = Elo) %>%
          inner_join(flags_df) %>%
          mutate(Clube = paste(flag, Clube)) %>%
          select(Clube, 'Elo <font style="opacity:.5"> (variação)')

      }
    })
      
    reac_rank_table()
    
  }, rownames = TRUE, striped = TRUE, hover = TRUE, width = 700,
  sanitize.text.function = function(x) x) # https://www.oipapio.com/question-8798651
  
  output$maiores_pontuacoes <- renderTable(maiores_pontuacoes, rownames = TRUE, 
                                        striped = TRUE, hover = TRUE, width = 700,
                                        sanitize.text.function = function(x) x)
  
  output$tempo_lideranca <- renderTable(maiores_streaks %>%
                                                rename(Até = Ate), 
                                              rownames = TRUE, 
                                              striped = TRUE, hover = TRUE, width = 700,
                                              sanitize.text.function = function(x) x,
                                              na = " ")
  
  output$competicoes <- renderTable(tabela_dados %>%
                                            rename(Competição = Competicao),
                                          striped = TRUE, hover = TRUE, width = 700,
                                          sanitize.text.function = function(x) x)
  
  output$df_confrontos <- renderPlotly({
    
    plot_ly(x = df_confrontos$Origem, y = df_confrontos$Adversario, z = df_confrontos$Aproveitamento, type = "heatmap", colorscale = "RdBu",
            reversescale = TRUE, hoverinfo = "none") %>%
      layout(plot_bgcolor = 'rgba(0, 0, 0, 0)',
             paper_bgcolor = 'rgba(0, 0, 0, 0)',
             xaxis = list(title = FALSE, fixedrange = TRUE),
             yaxis = list(title = FALSE, fixedrange = TRUE)) %>% 
      add_annotations(x = df_confrontos$Origem, y = df_confrontos$Adversario, text = df_confrontos$text,
                      showarrow = FALSE, font = list(color = 'black'))
    
    })
  
  output$partidas <- renderPlotly({
    p = df_partidas %>%
      rename(Confederação = Confederacao) %>%
      ggplot(aes(x = Confederação, y = Partidas, text = text)) +
      geom_bar(stat = "identity", position = "dodge", fill = "#F8766D") +
      tema +
      xlab("") +
      ylab("Partidas")
    
    p %>%
      ggplotly(tooltip = c("text")) %>%
      layout(plot_bgcolor = 'rgba(0, 0, 0, 0)',
             paper_bgcolor = 'rgba(0, 0, 0, 0)')
  })
  
  output$mando <- renderPlotly({
    
    p = df_mandante %>%
      ggplot(aes(x = Confederacao, y = Freq, fill = Vencedor, text = text)) +
      geom_bar(stat = "identity", position = "dodge") +
      tema +
      xlab("") +
      ylab("Resultado (%)") +
      scale_y_continuous(breaks = c(10, 20, 30, 40, 50))

    p %>%
      ggplotly(tooltip = c("text")) %>%
      layout(plot_bgcolor = 'rgba(0, 0, 0, 0)',
             paper_bgcolor = 'rgba(0, 0, 0, 0)')
  })
  
  output$placares <- renderPlotly({
    p = placares %>%
      ggplot(aes(x = Placar, y = n, text = text)) +
      geom_bar(stat = "identity", position = "dodge", fill = "#F8766D") +
      tema +
      xlab("") +
      ylab("Quantidade")
    
    p %>%
      ggplotly(tooltip = c("text")) %>%
      layout(plot_bgcolor = 'rgba(0, 0, 0, 0)',
             paper_bgcolor = 'rgba(0, 0, 0, 0)')
  })
  
  output$plot_confrontos <- renderPlotly({
    p = confrontos %>%
      rename(País = Pais) %>%
      ggplot(aes(x = País, y = Aproveitamento, text = text)) +
      geom_bar(stat = "identity", position = "dodge", fill = "#F8766D") +
      tema +
      xlab("") +
      ylab("Aproveitamento (%)")
    
    p %>%
      ggplotly(tooltip = c("text")) %>%
      layout(plot_bgcolor = 'rgba(0, 0, 0, 0)',
             paper_bgcolor = 'rgba(0, 0, 0, 0)')
  })
  
  output$estatisticas_ui <- renderUI({
    selectInput("estatisticas_ui",
                "País",
                choices = paises,
                selected = NULL,
                multiple = TRUE)
  })
  
  output$estatisticas <- renderDataTable({
    
    reac_estatisticas = reactive({
      
      if(is.null(input$estatisticas_ui)) {
        
        estatisticas %>% # https://datatables.net/examples/basic_init/language.html
          rename(Vitórias = Vitorias) %>%
          select(-Pais)
        
      } else {
        
        estatisticas %>% 
          rename(Vitórias = Vitorias) %>%
          filter(Pais %in% input$estatisticas_ui) %>%
          select(-Pais)
        
      }
      
    })
    
    reac_estatisticas()

    # https://datatables.net/examples/basic_init/language.html
    # https://stackoverflow.com/questions/25205410/r-shiny-set-datatable-column-width
  }, escape = FALSE, options = list(language = list(search = "Procurar:",
                                                    info = "Mostrando página _PAGE_ de _PAGES_",
                                                    lengthMenu = "Mostrar _MENU_ clubes por página",
                                                    zeroRecords = "Nenhum clube encontrado :(",
                                                    infoEmpty = "",
                                                    infoFiltered = "",
                                                    paginate = list("next" = "Próximo",
                                                                    previous = "Anterior")),
                                    columnDefs = list(list(width = '200px', targets = 0)))) 
  
  #output$debug = renderText(input$rank_pais)
}

shinyApp(ui, server)








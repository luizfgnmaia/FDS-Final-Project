source("dependencies.R") # https://github.com/rstudio/shinydashboard/issues/190

options(OutDec = ",")

text_size = 10
line_size = 0.75

# Funções e objetos auxiliares
#################################################################################

load("dados.RData")

clubes = dados %>%
  filter(Data == max(dados$Data)) %>%
  arrange(desc(Elo)) %>%
  .$Clube
paises = sort(unique(dados$Pais))
datas = sort(unique(dados$Data))
len_datas = length(datas)

# https://stackoverflow.com/questions/31152960/display-only-months-in-daterangeinput-or-dateinput-for-a-shiny-app-r-programmin/32171132

mydateInput <- function(inputId, label, value = NULL, min = NULL, max = NULL,
                        format = "yyyy-mm-dd", startview = "month", weekstart = 0, 
                        language = "en", minviewmode = "months", width = NULL) {
  
  # If value is a date object, convert it to a string with yyyy-mm-dd format
  # Same for min and max
  if (inherits(value, "Date"))  value <- format(value, "%Y-%m-%d")
  if (inherits(min,   "Date"))  min   <- format(min,   "%Y-%m-%d")
  if (inherits(max,   "Date"))  max   <- format(max,   "%Y-%m-%d")
  
  htmltools::attachDependencies(
    tags$div(id = inputId,
             class = "shiny-date-input form-group shiny-input-container",
             style = if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";"),
             
             controlLabel(inputId, label),
             tags$input(type = "text",
                        # datepicker class necessary for dropdown to display correctly
                        class = "form-control datepicker",
                        `data-date-language` = language,
                        `data-date-weekstart` = weekstart,
                        `data-date-format` = format,
                        `data-date-start-view` = startview,
                        `data-date-min-view-mode` = minviewmode,
                        `data-min-date` = min,
                        `data-max-date` = max,
                        `data-initial-date` = value
             )
    ),
    datePickerDependency
  )
}

`%AND%` <- function(x, y) {
  if (!is.null(x) && !is.na(x))
    if (!is.null(y) && !is.na(y))
      return(y)
  return(NULL)
}

controlLabel <- function(controlName, label) {
  label %AND% tags$label(class = "control-label", `for` = controlName, label)
}

# the datePickerDependency is taken from https://github.com/rstudio/shiny/blob/master/R/input-date.R
datePickerDependency <- htmltools::htmlDependency(
  "bootstrap-datepicker", "1.6.4", c(href = "shared/datepicker"),
  script = "js/bootstrap-datepicker.min.js",
  stylesheet = "css/bootstrap-datepicker3.min.css",
  # Need to enable noConflict mode. See #1346.
  head = "<script>
  (function() {
  var datepicker = $.fn.datepicker.noConflict();
  $.fn.bsDatepicker = datepicker;
  })();
  </script>")

tema = theme(
  panel.grid.minor = element_blank(), 
  panel.grid.major = element_blank(),
  panel.background = element_blank(),
  plot.background = element_blank(),
  legend.background = element_blank(),
  legend.key = element_blank(),
  text = element_text(size = text_size)
)

up_or_down = function(x) {
  ifelse(x == "", as.character(icon("horizontal-rule")),
         ifelse(as.integer(x) > 0, as.character(icon("angle-up")),
                as.character(icon("angle-down"))))
}

delta <- function(x) {
  sinal = ifelse(x >= 0, "+", "-")
  ret = paste0("(", sinal, round(abs(x), 2), ")")
  paste0('<font style="opacity:.5">', ret) # https://stackoverflow.com/questions/10835500/how-to-change-text-transparency-in-html-css
}

flags_df = tibble(Pais = c("Argentina", "Bolívia", "Brasil", "Chile", "Colômbia", "Equador",
                            "Paraguai", "Peru", "Uruguai", "Venezuela"),
                  flag = c('<img src="https://flagpedia.net/data/flags/mini/ar.png" width="29" height="20" /></a>',
                           '<img src="https://flagpedia.net/data/flags/mini/bo.png" width="29" height="20" /></a>',
                           '<img src="https://flagpedia.net/data/flags/mini/br.png" width="29" height="20" /></a>',
                           '<img src="https://flagpedia.net/data/flags/mini/cl.png" width="29" height="20" /></a>',
                           '<img src="https://flagpedia.net/data/flags/mini/co.png" width="29" height="20" /></a>',
                           '<img src="https://flagpedia.net/data/flags/mini/ec.png" width="29" height="20" /></a>',
                           '<img src="https://flagpedia.net/data/flags/mini/py.png" width="29" height="20" /></a>',
                           '<img src="https://flagpedia.net/data/flags/mini/pe.png" width="29" height="20" /></a>',
                           '<img src="https://flagpedia.net/data/flags/mini/uy.png" width="29" height="20" /></a>',
                           '<img src="https://flagpedia.net/data/flags/mini/ve.png" width="29" height="20" /></a>'))

#################################################################################

# sidebar
#################################################################################

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Introdução", icon = icon("youtube"), tabName = "int"), # https://fontawesome.com/icons?from=io
    menuItem("Rankings", icon = icon("trophy"), tabName = "rank"),
    menuItem("Histórico", icon = icon("chart-line"), tabName = "hist"),
    menuItem("Estatísticas", icon = icon("chart-bar"), tabName = "stats"),
    menuItem("Metodologia", icon = icon("question"), tabName = "metod"),
    menuItem("Github", icon = icon("github"), href = "https://github.com/luizfgnmaia/FDS-Final-Project")
  )
)

#################################################################################

body <- dashboardBody(
  tabItems(
    
    # Introdução
    #################################################################################

    tabItem(tabName = "int",
            HTML('<iframe width="1200" height="600" src="https://www.youtube.com/embed/AREB7MCGaUY" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
            h2("Introdução"),
            HTML("<p>Uma vez Flamengo, sempre Flamengo</p>
                 <p>Flamengo sempre eu hei de ser</p>
                 <p>É meu maior prazer vê-lo brilhar</p>
                 <p>Seja na terra, seja no mar</p>
                 <p>Vencer, vencer, vencer!</p>
                 <p>Uma vez Flamengo, Flamengo até morrer!</p>
                 
                 <p>Na regata, ele me mata</p>
                 <p>Me maltrata, me arrebata</p>
                 <p>Que emoção no coração!</p>
                 <p>Consagrado no gramado</p>
                 <p>Sempre amado, o mais cotado</p>
                 <p>Nos Fla-Flus é o Ai, Jesus!</p>
                 
                 <p>Eu teria um desgosto profundo</p>
                 <p>Se faltasse o Flamengo no mundo</p>
                 <p>Ele vibra, ele é fibra</p>
                 <p>Muita libra já pesou</p>
                 <p>Flamengo até morrer eu sou!</p>")
            ),
    
    #################################################################################
    
    # Rankings
    #################################################################################
    
    tabItem(tabName = "rank",
            uiOutput("rank_ui"),
            mydateInput("rank_data", 
                        "Mês", 
                        format = "mm-yyyy", 
                        language = "pt-BR",
                        width = 78,
                        min = datas[2],
                        max = datas[len_datas],
                        value = datas[len_datas]), 
            tableOutput("rank_table")
    ),
    
    #################################################################################
    
    # Histórico
    #################################################################################
    
    tabItem(tabName = "hist",
            uiOutput("hist_ui"), # https://stackoverflow.com/questions/40996536/shiny-r-select-input-is-not-working
            plotlyOutput("hist_plot"),
            textOutput("debug")
    ),
    
    #################################################################################
    
    # Estatísticas
    #################################################################################
    
    tabItem(tabName = "stats",
            "Obina > Eto'o"),
    
    #################################################################################
    
    # Metodologia
    #################################################################################
    
    tabItem(tabName = "metod")
    
    #################################################################################
    
            )
    )

ui <- dashboardPage(skin = "red",
                    dashboardHeader(title = "Elo da paz"),
                    sidebar,
                    body
)

server <- function(input, output) {
  
  output$hist_plot <- renderPlotly({
     
    validate(need(!is.na(input$hist_clube), message = "")) # https://stackoverflow.com/questions/42789819/prevent-error-in-shiny-app-render-plot
    
    p = dados %>%
      filter(Clube %in% input$hist_clube) %>%
      mutate(Mes = paste0("Mês: ", format(Data, "%b/%Y"))) %>%
      ggplot(aes(x = Data, y = Elo, group = 1, text = Mes)) + # https://stackoverflow.com/questions/45948926/ggplotly-text-aesthetic-causing-geom-line-to-not-display
      geom_line(aes(color = Clube), size = line_size) +
      tema +
      xlab("")
    
    p %>%
      ggplotly(tooltip = c("Clube", "Mes", "Elo")) %>%
      layout(plot_bgcolor = 'rgba(0, 0, 0, 0)') %>% # https://community.plot.ly/t/create-plots-with-transparent-background/14658
      layout(paper_bgcolor = 'rgba(0, 0, 0, 0)')
  })
  
  output$hist_ui <- renderUI({
    selectInput("hist_clube",
                "Clube",
                choices = clubes,
                selected = "Flamengo RJ",
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
  
  #output$debug = renderText(input$rank_pais)
}

shinyApp(ui, server)








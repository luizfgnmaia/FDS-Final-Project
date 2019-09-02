source("dependencies.R") # https://github.com/rstudio/shinydashboard/issues/190

options(OutDec = ",")

text_size = 10
plot_res = 150
line_size = 0.75

#################################################################################

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

dateRangeMonthsInput <- function(inputId, label, start = NULL, end = NULL,
                                 min = NULL, max = NULL, format = "yyyy-mm-dd", startview = "month",
                                 minviewmode = "months", # added manually
                                 weekstart = 0, language = "en", separator = " to ", width = NULL) {
  
  # If start and end are date objects, convert to a string with yyyy-mm-dd format
  # Same for min and max
  if (inherits(start, "Date"))  start <- format(start, "%Y-%m-%d")
  if (inherits(end,   "Date"))  end   <- format(end,   "%Y-%m-%d")
  if (inherits(min,   "Date"))  min   <- format(min,   "%Y-%m-%d")
  if (inherits(max,   "Date"))  max   <- format(max,   "%Y-%m-%d")
  
  htmltools::attachDependencies(
    div(id = inputId,
        class = "shiny-date-range-input form-group shiny-input-container",
        style = if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";"),
        
        controlLabel(inputId, label),
        # input-daterange class is needed for dropdown behavior
        div(class = "input-daterange input-group",
            tags$input(
              class = "input-sm form-control",
              type = "text",
              `data-date-language` = language,
              `data-date-weekstart` = weekstart,
              `data-date-format` = format,
              `data-date-start-view` = startview,
              `data-date-min-view-mode` = minviewmode, # added manually
              `data-min-date` = min,
              `data-max-date` = max,
              `data-initial-date` = start
            ),
            span(class = "input-group-addon", separator),
            tags$input(
              class = "input-sm form-control",
              type = "text",
              `data-date-language` = language,
              `data-date-weekstart` = weekstart,
              `data-date-format` = format,
              `data-date-start-view` = startview,
              `data-date-min-view-mode` = minviewmode, # added manually
              `data-min-date` = min,
              `data-max-date` = max,
              `data-initial-date` = end
            )
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

#################################################################################

load("exemplo.RData")
exemplo = exemplo %>%
  rename(País = Pais)
clubs = unique(exemplo$Clube)
paises = unique(exemplo$País)
datas = sort(unique(exemplo$Data))


#################################################################################

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

body <- dashboardBody(
  tabItems(
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
    
    tabItem(tabName = "rank",
            uiOutput("rank_ui"),
            mydateInput("rank_data", 
                        "Data", 
                        format = "mm-yyyy", 
                        language = "pt-BR",
                        width = 78,
                        min = datas[1],
                        max = datas[length(datas)],
                        value = datas[length(datas)]), 
            tableOutput("rank_table")
    ),
    
    tabItem(tabName = "hist",
            uiOutput("hist_ui"), # https://stackoverflow.com/questions/40996536/shiny-r-select-input-is-not-working
            dateRangeMonthsInput("hist_data", 
                                 "Intervalo de tempo",
                                 format = "mm-yyyy",
                                 language = "pt-BR", 
                                 separator = "até",
                                 start = "2000-01-01",
                                 min = datas[1],
                                 max = datas[length(datas)]),
            plotOutput("hist_plot")#, #hover = hoverOpts("hist_plot_hover", delay = 100))
            #uiOutput("hist_hover")
            #textOutput("debug")
    ),  
    
    tabItem(tabName = "stats",
            "Obina > Eto'o"),
    
    tabItem(tabName = "metod")
            )
    )

#################################################################################

ui <- dashboardPage(skin = "red",
                    dashboardHeader(title = "Elo da paz"),
                    sidebar,
                    body
)

server <- function(input, output) {
  
  hist_min = reactive({
    ifelse(is.na(input$hist_data[1]), datas[1], input$hist_data[1])
  })
  hist_max = reactive({
    ifelse(is.na(input$hist_data[2]), datas[length(datas)], input$hist_data[2])
  })
  
  output$hist_plot <- renderPlot({
    exemplo %>%
      filter(Data >= hist_min(),
             Data <= hist_max(),
             Clube %in% input$hist_clube) %>%
      ggplot(aes(x = Data, y = Elo)) +
      geom_line(aes(color = Clube), size = line_size) +
      tema +
      xlab("")
  }, bg = "transparent", res = plot_res)
  
  output$hist_ui <- renderUI({
    selectInput("hist_clube",
                "Clube",
                choices = clubs,
                selected = "Flamengo",
                multiple = TRUE)
  })
  
  output$rank_ui <- renderUI({
    selectInput("rank_pais",
                "País",
                choices = paises,
                selected = NULL,
                multiple = TRUE)
  })
  
  #####
  # output$hist_hover <- renderUI({ # https://gitlab.com/snippets/16220
  #   hover = input$hist_plot_hover
  #   point = nearPoints(exemplo, hover, maxpoints = 1)
  # 
  #   if (nrow(point) == 0) return(NULL)
  #   # calculate point position INSIDE the image as percent of total dimensions
  #   # from left (horizontal) and from top (vertical)
  #   left_pct = (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
  #   top_pct = (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
  # 
  #   # calculate distance from left and bottom side of the picture in pixels
  #   left_px = hover$range$left + left_pct * (hover$range$right - hover$range$left)
  #   top_px = hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
  # 
  #   style = paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
  #                   "left:", left_px + 100, "px; top:", top_px + 100, "px;")
  # 
  #   wellPanel(
  #     style = style,
  #     p(HTML(paste0("<b> Clube: </b>", point$Clube, "<br/>",
  #                   "<b> Data: </b>", format(point$Data, "%m-%Y"), "<br/>",
  #                   "<b> Elo: </b>", round(point$Elo, 0), "<br/>")))
  #     )
  # })
  #####
  
  output$rank_table <- renderTable({
    
    if(is.null(input$rank_pais)) {
      
      if(input$rank_data == datas[1]) {
        exemplo %>%
          filter(Data == input$rank_data) %>%
          arrange(desc(Elo)) %>%
          select(-Data)
        
      } else {
        
        atual = exemplo %>%
          filter(Data == input$rank_data) %>%
          arrange(desc(Elo)) %>%
          mutate(new_pos = row_number())
        
        anterior = exemplo %>%
          filter(Data == datas[which(datas == input$rank_data)-1]) %>%
          arrange(desc(Elo)) %>%
          mutate(old_pos = row_number()) %>%
          select(Clube, old_pos)
        
        atual %>%
          inner_join(anterior) %>%
          mutate(dif = ifelse((old_pos - new_pos) != 0, as.character(old_pos - new_pos), ""),
                 tmp = up_or_down(dif),
                 Clube = paste(Clube, tmp, ifelse(dif == "", "", as.character(abs(as.integer(dif)))))) %>%
          select(Clube, Elo, País)
      }
      
    } else {
      
      if(input$rank_data == datas[1]) {
        exemplo %>%
          filter(Data == input$rank_data) %>%
          arrange(desc(Elo)) %>%
          select(-Data) 
        
      } else { 
        
        if(input$rank_data == datas[1]) {
          exemplo %>%
            filter(País %in% input$rank_pais,
                   Data == input$rank_data) %>%
            arrange(desc(Elo)) %>%
            select(-Data)
          
        } else {
          
          atual = exemplo %>%
            filter(País %in% input$rank_pais,
                   Data == input$rank_data) %>%
            arrange(desc(Elo)) %>%
            mutate(new_pos = row_number())
          
          anterior = exemplo %>%
            filter(País %in% input$rank_pais,
                   Data == datas[which(datas == input$rank_data)-1]) %>%
            arrange(desc(Elo)) %>%
            mutate(old_pos = row_number()) %>%
            select(Clube, old_pos)
          
          atual %>%
            inner_join(anterior) %>%
            mutate(dif = ifelse((new_pos - old_pos) != 0, as.character(new_pos - old_pos), ""),
                   tmp = up_or_down(dif),
                   Clube = paste(Clube, tmp, ifelse(dif == "", "", as.character(abs(as.integer(dif)))))) %>%
            select(Clube, Elo, País)
        }
      }
    }
  }, rownames = TRUE, sanitize.text.function = function(x) x) # https://www.oipapio.com/question-8798651
  
  #output$debug = renderText(input$hist_data)
}

shinyApp(ui, server)
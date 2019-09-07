# Funções e objetos auxiliares
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
  text = element_text(size = 10)
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

library(dplyr)
library(stringr)

arruma_data <- function(x) {
  if(nchar(x) == 15) {
    ano1 = str_sub(x, start = 7, end = 10)
    ano2 = str_sub(x, start = 12, end = 15)
    if(as.integer(str_sub(x, start = 4, end = 5)) >= 7) {
      x = (paste0(str_sub(x, start = 1, end = 6), ano1))
    } else {
      x = (paste0(str_sub(x, start = 1, end = 6), ano2))
    }
  }
  as.Date(x, "%d.%m.%Y")
}

paises = c("Argentina", "Bolívia", "Brasil", "Chile", "Colômbia", "Equador", "Paraguai", "Peru", "Uruguai", "Venezuela", "Continentais")

for(pais in paises) {
  csv = read.csv(paste0("data/old/", pais, ".csv"), stringsAsFactors = FALSE)
  
  csv_2019 = read.csv(paste0("data/old/", pais, "_2019.csv"), stringsAsFactors = FALSE) 
  
  csv_novo = rbind(csv, csv_2019) %>%
    distinct() %>%
    rowwise() %>%
    mutate(Data = arruma_data(date)) %>%
    filter(Data <= "2019-09-03") %>%
    select(-Data)
  
  write.table(csv_novo, paste0("data/", pais, "_atualizado.csv"), sep = ",", row.names = FALSE, na = "")
}


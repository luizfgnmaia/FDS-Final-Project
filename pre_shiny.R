
library(dplyr)
source("aux_shiny.R")

load("dados.RData")

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

clubes = dados %>%
  filter(Data == max(dados$Data)) %>%
  arrange(desc(Elo)) %>%
  .$Clube
paises = sort(unique(dados$Pais))
datas = sort(unique(dados$Data))
len_datas = length(datas)

# Times com maiores elos na história

melhores_clubes = dados %>%
  mutate(Ano = format(Data, "%Y")) %>%
  group_by(Clube, Pais, Ano) %>%
  summarise(Elo = max(Elo)) %>%
  ungroup() %>%
  arrange(desc(Elo)) %>%
  head(50) %>%
  inner_join(flags_df) %>%
  mutate(Clube = paste(flag, Clube)) %>%
  select(-Pais, -flag)


# Time com mais tempo na primeira posição do ranking

maior_por_mes = dados %>%
  group_by(Data) %>%
  slice(which.max(Elo)) %>%
  ungroup() %>%
  mutate(Data = as.character(Data))

clube = maior_por_mes$Clube[1]
pais = maior_por_mes$Pais[1]
de = maior_por_mes$Data[1]
ate = NULL
meses = NULL
j = 1
m = 1
for(i in 2:nrow(maior_por_mes)) {
  if(!(clube[j] == maior_por_mes$Clube[i] & pais[j] == maior_por_mes$Pais[i])) {
    meses[j] = m
    ate[j] = maior_por_mes$Data[i-1]
    j = j + 1
    m = 1
    clube[j] = maior_por_mes$Clube[i]
    pais[j] = maior_por_mes$Pais[i]
    de[j] = maior_por_mes$Data[i]
  } else {
    m = m + 1
  }
}
ate = c(ate, NA)
meses = c(meses, m)


maiores_streaks = tibble(Clube = clube, Pais = pais, De = de, Ate = ate, Meses = meses) %>% 
  mutate(De = format(as.Date(De), "%b %Y"),
         Ate = format(as.Date(Ate), "%b %Y")) %>%
  arrange(desc(Meses)) %>%
  inner_join(flags_df) %>%
  mutate(Clube = paste(flag, Clube),
         Meses = as.integer(Meses)) %>%
  select(-Pais, -flag)

rm(list = setdiff(ls(), c("dados", "clubes", "paises", "datas", "len_datas", "melhores_clubes", "maiores_streaks")))

save.image("pre_shiny.RData")

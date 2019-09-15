options(encoding = "UTF-8")
options(OutDec = ",")

source("dependencies.R")
source("aux_shiny.R")

dados = read.csv("data/rankings/ranktodos.csv", stringsAsFactors = FALSE) %>%
  select(-X) %>%
  rename(Clube = nome,
         Pais = pais,
         Elo = rate,
         Data = date) %>%
  mutate(Data = as.Date(Data, format = "%Y.%m.%d"))

dados$Clube[which(dados$id == 618)] = "River Plate (Par)"
dados$Clube[which(dados$id == 826)] = "River Plate (Uru)"
dados$Clube[which(dados$id == 414)] = "Fortaleza (Col)"
dados$Clube[which(dados$id == 611)] = "Guarani (Par)"
dados$Clube[which(dados$id == 113)] = "Independiente (Bol)"
dados$Clube[which(dados$id == 928)] = "Portuguesa (Ven)"
dados$Clube[which(dados$id == 620)] = "San Lorenzo (Par)"
dados$Clube[which(dados$id == 124)] = "Santa Cruz (Bol)"
dados$Clube[which(dados$id == 732)] = "Sport Boys (Per)"
dados$Clube[which(dados$id == 525)] = "U. Catolica (Equ)"

dados$Pais[which(dados$Pais == "Bolivia")] = "Bolívia"
dados$Pais[which(dados$Pais == "Colombia")] = "Colômbia"


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

clubes = dados %>%
  filter(Data == max(dados$Data)) %>%
  arrange(desc(Elo)) %>%
  .$Clube
paises = sort(unique(dados$Pais))
datas = sort(unique(dados$Data))
len_datas = length(datas)

# Clubes com as maiores pontuações
################################################################################# 

maiores_pontuacoes = dados %>%
  mutate(Ano = format(Data, "%Y")) %>%
  group_by(Clube, Pais, Ano) %>%
  summarise(Elo = max(Elo)) %>%
  ungroup() %>%
  arrange(desc(Elo)) %>%
  head(50) %>%
  inner_join(flags_df) %>%
  mutate(Clube = paste(flag, Clube)) %>%
  select(-Pais, -flag)

################################################################################# 


# Clubes que permaneceram mais meses consecutivos no primeiro lugar do ranking
################################################################################# 

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

################################################################################# 

# Clubes que permaneceram mais meses no primeiro lugar do ranking
################################################################################# 
mais_meses_lider = maior_por_mes %>%
  count(Clube, Pais) %>%
  arrange(desc(n)) %>%
  rename(Meses = n) %>%
  inner_join(flags_df) %>%
  mutate(Clube = paste(flag, Clube),
         Meses = as.integer(Meses)) %>%
  select(-Pais, -flag)

################################################################################# 

# Tabela dos dados
################################################################################# 

competicao = c("Copa Libertadores", "Copa Sudamericana", "Recopa Sudamericana",
               "Superliga", "Copa Argentina", "Copa de la Superliga", "Super Cup",
               "Division Professional",
               "Série A", "Copa do Brasil",
               "Primera Division", "Chilean Cup", "Super Cup",
               "Liga Aguila", "Copa Aguila", "Super Cup",
               "Liga Pro", "Copa Ecuador", 
               "Primera Division", "Copa Paraguay",
               "Liga 1", "Copa Bicentenario",
               "Primera Division", "Super Cup",
               "Primera Division", "Copa Venezuela")

pais = c("Continental",  "Continental",  "Continental",
         "Argentina", "Argentina", "Argentina", "Argentina",
         "Bolívia",
         "Brasil", "Brasil",
         "Chile", "Chile", "Chile",
         "Colômbia", "Colômbia", "Colômbia",
         "Equador", "Equador",
         "Paraguai", "Paraguai",
         "Peru", "Peru",
         "Uruguai", "Uruguai",
         "Venezuela", "Venezuela")

ano_inicio = c("2002", "2002", "2003",
               "2002", "2011", "2019", "2012",
               "2002", "2002", "2009",
               "2002", "2009", "2013",
               "2002", "2008", "2013",
               "2002", "2019",
               "2002", "2019",
               "2002", "2019",
               "2002", "2018",
               "2002", "2003")

tabela_dados = tibble(Competicao = competicao, Pais = pais, `A partir de` = ano_inicio) %>%
  inner_join(flags_df) %>%
  mutate(Competicao = paste(flag, Competicao)) %>%
  select(-flag, -Pais)

################################################################################# 

# Confrontos internacionais
#################################################################################

sula = read.csv("data/jogossulaid.csv") %>%
  select(-X, -date, -tourn) %>%
  mutate(p1 = NA, p2 = NA) %>%
  mutate(resultado = if_else(g1 > g2, "vit",
                          if_else(g1 == g2, "emp", "der")))
  
sula$p1[which(sula$id1 > 0 & sula$id1 <= 100)] = "Argentina"
sula$p1[which(sula$id1 > 100 & sula$id1 <= 200)] = "Bolívia"
sula$p1[which(sula$id1 > 200 & sula$id1 <= 300)] = "Brasil"
sula$p1[which(sula$id1 > 300 & sula$id1 <= 400)] = "Chile"
sula$p1[which(sula$id1 > 400 & sula$id1 <= 500)] = "Colômbia"
sula$p1[which(sula$id1 > 500 & sula$id1 <= 600)] = "Equador"
sula$p1[which(sula$id1 > 600 & sula$id1 <= 700)] = "Paraguai"
sula$p1[which(sula$id1 > 700 & sula$id1 <= 800)] = "Peru"
sula$p1[which(sula$id1 > 800 & sula$id1 <= 900)] = "Uruguai"
sula$p1[which(sula$id1 > 900 & sula$id1 <= 1000)] = "Venezuela"

sula$p2[which(sula$id2 > 0 & sula$id2 <= 100)] = "Argentina"
sula$p2[which(sula$id2 > 100 & sula$id2 <= 200)] = "Bolívia"
sula$p2[which(sula$id2 > 200 & sula$id2 <= 300)] = "Brasil"
sula$p2[which(sula$id2 > 300 & sula$id2 <= 400)] = "Chile"
sula$p2[which(sula$id2 > 400 & sula$id2 <= 500)] = "Colômbia"
sula$p2[which(sula$id2 > 500 & sula$id2 <= 600)] = "Equador"
sula$p2[which(sula$id2 > 600 & sula$id2 <= 700)] = "Paraguai"
sula$p2[which(sula$id2 > 700 & sula$id2 <= 800)] = "Peru"
sula$p2[which(sula$id2 > 800 & sula$id2 <= 900)] = "Uruguai"
sula$p2[which(sula$id2 > 900 & sula$id2 <= 1000)] = "Venezuela"

mat_confrontos = matrix(0, nrow = 10, ncol = 10)
colnames(mat_confrontos) = paises
rownames(mat_confrontos) = paises

for(i in 1:10) {
  for(j in 1:10) {
    if(i!=j) {
      
      vit = 0
      emp = 0
      
      tmp1 = sula %>%
        filter(p1 == rownames(mat_confrontos)[i],
               p2 == colnames(mat_confrontos)[j])
      
      c1 = count(tmp1, resultado)
      
      n1 = nrow(tmp1)
      
      tmp_vit = c1 %>% 
        filter(resultado == "vit") %>% 
        .$n
      
      if(length(tmp_vit) > 0) {
        vit = vit + tmp_vit
      }
      
      tmp_emp = c1 %>% 
        filter(resultado == "emp") %>% 
        .$n
      
      if(length(tmp_emp) > 0) {
        emp = emp + tmp_emp
      }
      
      tmp2 = sula %>%
        filter(p2 == rownames(mat_confrontos)[i],
               p1 == colnames(mat_confrontos)[j])
      
      c2 = count(tmp2, resultado)
      
      n2 = nrow(tmp2)
      
      tmp_vit = c2 %>% 
        filter(resultado == "der") %>% 
        .$n
      
      if(length(tmp_vit) > 0) {
        vit = vit + tmp_vit
      }
      
      tmp_emp = c2 %>% 
        filter(resultado == "emp") %>% 
        .$n
      
      if(length(tmp_emp) > 0) {
        emp = emp + tmp_emp
      }
      
      n = n1 + n2
      
      mat_confrontos[i, j] = (2*vit + emp) / (n*2)
      
    }
  }
}

diag(mat_confrontos) = NA

df_confrontos = tibble()

for(i in 1:10) {
  for(j in 1:10) {
    tmp = tibble(Origem = rownames(mat_confrontos)[i],
                 Adversario = colnames(mat_confrontos)[j],
                 Aproveitamento = mat_confrontos[j,i])
    df_confrontos = df_confrontos %>%
      rbind(tmp)
  }
}

df_confrontos = df_confrontos %>%
  mutate(Aproveitamento = 100*Aproveitamento,
         text = paste0(as.character(round(Aproveitamento, 2)), "%"))

df_confrontos$text[is.na(df_confrontos$Aproveitamento)] = ""

vitorias = NULL
empates = NULL
partidas = NULL

for(i in 1:length(paises)) {
  
  vit = 0
  emp = 0
  
  tmp1 = sula %>%
    filter(p1 == paises[i], p2 != paises[i])
  
  c1 = count(tmp1, resultado)
  
  n1 = nrow(tmp1)
  
  tmp_vit = c1 %>% 
    filter(resultado == "vit") %>% 
    .$n
  
  if(length(tmp_vit) > 0) {
    vit = vit + tmp_vit
  }
  
  tmp_emp = c1 %>% 
    filter(resultado == "emp") %>% 
    .$n
  
  if(length(tmp_emp) > 0) {
    emp = emp + tmp_emp
  }
  
  tmp2 = sula %>%
    filter(p2 == paises[i], p1 != paises[i])
  
  c2 = count(tmp2, resultado)
  
  n2 = nrow(tmp2)
  
  tmp_vit = c2 %>% 
    filter(resultado == "der") %>% 
    .$n
  
  if(length(tmp_vit) > 0) {
    vit = vit + tmp_vit
  }
  
  tmp_emp = c2 %>% 
    filter(resultado == "emp") %>% 
    .$n
  
  if(length(tmp_emp) > 0) {
    emp = emp + tmp_emp
  }
  
  vitorias[i] = vit
  empates[i] = emp
  partidas[i] = n1 + n2
  
}

confrontos = tibble(Pais = paises, Vitorias = vitorias, Empates = empates, Partidas = partidas) %>%
  mutate(Aproveitamento = 100*(2*Vitorias + Empates) / (Partidas*2),
         text = paste0("País: ", Pais, "<br>",
                       "Aproveitamento: ", round(Aproveitamento, 2), "%<br>",
                       "Vitórias: ", Vitorias, "<br>",
                       "Empates: ", Empates, "<br>",
                       "Partidas: ", Partidas, "<br>")) %>%
  arrange(desc(Aproveitamento)) %>%
  mutate(Pais = factor(Pais, levels = Pais))

################################################################################# 
 
# Vantagem do mandante e partidas por confederação
#################################################################################

files = c("data/jogosarg.csv", "data/jogosbol.csv", "data/jogosbra.csv", "data/jogoschi.csv", "data/jogoscol.csv",
          "data/jogosecu.csv", "data/jogospar.csv", "data/jogosper.csv", "data/jogosuru.csv", "data/jogosven.csv",
          "data/jogossula.csv", "data/jogosid.csv")

conf = c(paises, "Conmebol", "Todas")

df_mandante = data.frame()
df_partidas = data.frame()

for(i in 1:length(files)) {
  jogos = read.csv(files[i]) %>%
    mutate(resultado = if_else(g1 > g2, "Mandante",
                               if_else(g1 == g2, "Empate", "Visitante")))
  n = nrow(jogos)
  c = count(jogos, resultado)
  mandante = c$n[which(c$resultado == "Mandante")]
  empate = c$n[which(c$resultado == "Empate")]
  visitante = c$n[which(c$resultado == "Visitante")]
  
  tmp = tibble(Confederacao = c(conf[i], conf[i], conf[i]),
               Vencedor = c("Mandante", "Empate", "Visitante"),
               Freq = c(100*mandante/n, 100*empate/n, 100*visitante/n),
               n = c(mandante, empate, visitante))
  
  df_mandante = df_mandante %>%
    rbind(tmp)
  
  tmp2 = tibble(Confederacao = conf[i],
                Partidas = n)
  
  df_partidas = df_partidas %>%
    rbind(tmp2)
}

ordem_conf = c("Todas", "Conmebol", paises)
ordem_vencedor = c("Mandante", "Empate", "Visitante")

df_mandante = df_mandante %>%
  mutate(Confederacao = factor(Confederacao, levels = ordem_conf),
         Vencedor = factor(Vencedor, levels = ordem_vencedor),
         text = paste0(n, " (", round(Freq, 2), "%)"))

total = max(df_partidas$Partidas)
  
df_partidas = df_partidas %>%
  mutate(Confederacao = factor(Confederacao, levels = ordem_conf),
         porc = round(100*Partidas/total, 2),
         text = paste0(Partidas, " (", porc, "%)"))
#################################################################################

# Placares mais comuns
#################################################################################

tmp = read.csv("data/jogosid.csv") %>%
  mutate(Placar = paste(g1, "x", g2)) %>%
  count(Placar) %>%
  arrange(desc(n))

placares = tmp %>%
  head(20)

outros = sum(tmp$n) - sum(placares$n)

total = sum(tmp$n)

placares = placares %>%
  rbind(data.frame(Placar = "Outros", n = outros)) %>%
  mutate(Placar = factor(Placar, levels = Placar),
         porc = round(100*n/total, 2),
         text = paste0(n, " (", porc, "%)"))

#################################################################################

# Estatísticas
#################################################################################

jogos = read.csv("data/jogosid.csv") %>%
  mutate(resultado = if_else(g1 > g2, "vit",
                             if_else(g1 == g2, "emp", "der")))

ids = unique(c(jogos$id1, jogos$id2))

vitorias = NULL
empates = NULL
partidas = NULL
gols_pro = NULL
gols_contra = NULL

for(i in 1:length(ids)) {
  
  vit = 0
  emp = 0
  g_pro = 0
  g_contra = 0
  
  tmp1 = jogos %>%
    filter(id1 == ids[i])
  
  c1 = count(tmp1, resultado)
  
  n1 = nrow(tmp1)
  
  tmp_vit = c1 %>% 
    filter(resultado == "vit") %>% 
    .$n
  
  if(length(tmp_vit) > 0) {
    vit = vit + tmp_vit
  }
  
  tmp_emp = c1 %>% 
    filter(resultado == "emp") %>% 
    .$n
  
  if(length(tmp_emp) > 0) {
    emp = emp + tmp_emp
  }
  
  g_pro = g_pro + sum(tmp1$g1)
  g_contra = g_contra + sum(tmp1$g2)
  
  tmp2 = jogos %>%
    filter(id2 == ids[i])
  
  c2 = count(tmp2, resultado)
  
  n2 = nrow(tmp2)
  
  tmp_vit = c2 %>% 
    filter(resultado == "der") %>% 
    .$n
  
  if(length(tmp_vit) > 0) {
    vit = vit + tmp_vit
  }
  
  tmp_emp = c2 %>% 
    filter(resultado == "emp") %>% 
    .$n
  
  if(length(tmp_emp) > 0) {
    emp = emp + tmp_emp
  }
  
  g_pro = g_pro + sum(tmp2$g2)
  g_contra = g_contra + sum(tmp2$g1)
  
  vitorias[i] = vit
  empates[i] = emp
  partidas[i] = n1 + n2
  gols_pro[i] = g_pro
  gols_contra[i] = g_contra
}

tmp_dados = dados %>%
  distinct(id, .keep_all = TRUE) %>%
  select(id, Clube, Pais)

estatisticas = tibble(id = ids, Vitorias = vitorias, Empates = empates, Partidas = partidas,
                     GP = gols_pro, GC = gols_contra) %>%
  inner_join(tmp_dados) %>%
  inner_join(flags_df) %>%
  mutate(Clube = paste(flag, Clube)) %>%
  select(Clube, Partidas, Vitorias, Empates, GP, GC, Pais) %>%
  mutate(SG = GP - GC,
         Aproveitamento = round((2*Vitorias + Empates) / (Partidas*2), 4),
         Partidas = as.integer(Partidas),
         Vitorias = as.integer(Vitorias),
         Empates = as.integer(Empates),
         GP = as.integer(GP),
         GC = as.integer(GC),
         SG = as.integer(SG)) %>%
  arrange(desc(Aproveitamento))
  
#################################################################################

# Elo médio
#################################################################################
elo_medio = dados %>%
  group_by(Clube, Pais) %>%
  summarise(Elo = mean(Elo)) %>%
  arrange(desc(Elo)) %>%
  ungroup() %>%
  inner_join(flags_df) %>%
  mutate(Clube = paste(flag, Clube)) %>%
  select(-Pais, -flag)
#################################################################################


      
rm(list = setdiff(ls(), c("dados", "clubes", "paises", "datas", "len_datas", 
                          "maiores_pontuacoes", "maiores_streaks", "tabela_dados", 
                          "df_confrontos", "df_mandante", "df_partidas",
                          "placares", "confrontos", "estatisticas", "mais_meses_lider",
                          "elo_medio")))

save.image("pre_shiny.RData")

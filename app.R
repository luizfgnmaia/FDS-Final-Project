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
    menuItem("Modelagem", icon = icon("question"), tabName = "model",
             menuSubItem("Processo de modelagem", tabName = "processo"),
             menuSubItem("Método Elo", tabName = "elo")),
    menuItem("Rankings", icon = icon("trophy"), tabName = "rank"),
    menuItem("Histórico", icon = icon("chart-line"), tabName = "hist"),
    menuItem("Resultados", icon = icon("poll-h"), tabName = "result",
             menuSubItem("Mais meses na liderança", tabName = "tempo_lideranca"),
             menuSubItem("Meses consecutivos no topo", tabName = "tempo_lideranca_consec"),
             menuSubItem("Maiores pontuações", tabName = "maiores_pontuacoes"),
             menuSubItem("Elo médio por clube", tabName = "elo_medio")),
    menuItem("Conclusão e próximos passos", icon = icon("forward"), tabName = "next"),
    menuItem("Github", icon = icon("github"), href = "https://github.com/luizfgnmaia/FDS-Final-Project")
  )
)

#################################################################################

body <- dashboardBody(
  tabItems(
    
    # Introdução
    #################################################################################

    tabItem(tabName = "int",
            HTML('<iframe width="600" height="400" src="https://www.youtube.com/embed/Ip1Qr3tsh44" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
            h2("Introdução"),
            HTML('<p> Ranquear equipes de futebol é uma pratica muito antiga e, atualmente, as principais ligas e federações do mundo já possuem algum tipo de raqueamento para seus times. O ranking mais famoso, é o Ranking Mundial da Fifa. O sistema de ranking da Fifa começou a ser implementado em agosto de 1993 e passou por muitas reformulações desde então. Em 2018 o conselho Fifa no intuito de deixar o ranking mais justo e diminuir a chance de manipulação alterou o seu modelo de cálculo, inspirados no sistema de <a href="https://www.flashscore.com/">Ranking Elo</a>.'),
            HTML("<p> Arpad Elo, professor de física e mestre em xadrez, propôs um modelo estatístico para tentar solucionar o problema de qualificar jogadores de xadrez da forma mais justa e razoável possível. Neste trabalho, um dos grandes desafios é estabelecer a maneira, o modelo, e os critérios que possam se aproximar ao máximo da real força de um time para determinada partida, supondo que tal força exista. Outro desafio, consiste em adaptar um modelo de ranqueamento para as equipes da América do Sul que, além de poder ser calibrado em um espaço de tempo mais curto, contenha as forças relativas entres as equipes, um modelo alternativo ao apresentado pela federação máxima de futebol no continente, a Conmebol. Tendo como inspiração o sistema de Ranking Elo e da FIFA, para sanar estes problemas, foi necessária uma abordagem mais científica para redistribuição das equipes no ranking e assim chegar próximo a um resultado satisfatório."),
            h2("Objetivos"),
            HTML("<p> Os objetivos deste trabalho foram:"),
            HTML("<p> &#8226; Construir um ranking mensal das equipes da América do sul, utilizando dados históricos desde 2002 aplicando a metodologia Elo;"),
            HTML("<p> &#8226; Construir interface que possibilite ao usuário buscar informações sobre um clube em determinado período de tempo."),
            h2("Abordagem"),
            HTML('<p> Iniciamos nosso projeto raspando dados dos confrontos de equipes da América do Sul no site <a href="https://www.flashscore.com/">FlashScore.com</a>. O serviço de resultados do site FlashScore.com oferece resultados de futebol de mais de 1000 ligas. Além do futebol, é possível acompanhar mais de 30 esportes no FlashScore.'),
            HTML("<p> Depois de extrair e organizar os dados, utilizamos esses dados para realizarmos uma série de analises exploratórias sobre resultados que pensávamos intuitivamente ter sobre o comportamento das equipes sul-americanas. Essa exploração mostrou vantagens de equipes brasileiras e argentinas em confrontos diretos com times de outros países e um “empate” no histórico de confrontos de equipes dessa nacionalidade. Além de placares que mais ocorreram, e estatísticas gerais sobre os times que compõem o ranking. Foi estabelecido modelo de ranqueamento guiado no Ranking Elo, considerando na base do projeto somente equipes que jogaram pelo menos uma temporada inteira na primeira divisão de seu país, e estabelecendo uma atualização do ranking toda primeira terça-feira de cada mês. Desta maneira organizamos as series históricas de confrontos e assim foi possível criar gráficos que continham informações por períodos, colocados de maneira pratica e intuitiva para o usuário buscar os elementos que precisa.")
    ),
    
    #################################################################################
    
    # Rankings
    #################################################################################
    
    tabItem(tabName = "rank",
            h2("Ranking mensal geral ou por país"),
            HTML("Apresentamos aqui os rankings mês a mês considerando todos os clubes ou apenas os clubes de países específicos."),
            mydateInput("rank_data", 
                        "Mês", 
                        format = "mm-yyyy", 
                        language = "pt-BR",
                        width = 78,
                        min = datas[2],
                        max = datas[len_datas],
                        value = datas[len_datas]), 
            uiOutput("rank_ui"),
            tableOutput("rank_table"),
            h4("Vídeo demonstrativo:")
            #HTML('<iframe width="600" height="400" src="https://www.youtube.com/embed/FakUq5S9q_Y" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
    ),
    
    #################################################################################
    
    # Histórico
    #################################################################################
    
    tabItem(h2("Série temporal da pontuação dos clubes"),
            tabName = "hist",
            HTML("Temos nesse gráfico uma coleção de observações feitas sequencialmente ao longo dos anos para cada uma das equipes do ranking. Podemos analisar e comparar o comportamento dos times anos a ano, identificar períodos de ascensão e queda de rendimento, e também picos históricos."),
            uiOutput("hist_ui"), # https://stackoverflow.com/questions/40996536/shiny-r-select-input-is-not-working
            plotlyOutput("hist_plot"),
            h4("Vídeo demonstrativo:"),
            HTML('<iframe width="600" height="400" src="https://www.youtube.com/embed/8y8pDuuAot8" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
    ),
    
    #################################################################################
    
    # Análise exploratória 
    #################################################################################
    
    tabItem(tabName = "partidas",
            h2("Quantidade de partidas por confederação"),
            plotlyOutput("partidas"),
            HTML("Neste gráfico pode-se observar uma vantagem para as principais nações dentro do cenário do futebol sul-americano, com destaque para a Colômbia com 7508 jogos.")),
    
    tabItem(tabName = "placares",
            h2("Placares mais comuns"),
            plotlyOutput("placares"),
            HTML("As barras deste gráfico mostram as quantidade de jogos que terminaram com um determinado placar. Vemos que na base de dados, os quatro placares mais comuns são de empates ou vitórias do time mandante por uma margem de apenas um gol.")),
    
    tabItem(tabName = "mando",
            h2("Influência do mando de campo por confederação"),
            plotlyOutput("mando"),
            HTML("A influência do mando de campo no futebol sempre foi alvo de discussões entre especialistas e amantes do esporte. Neste gráfico estão dispostos os resultados das partidas considerando o mando de campo. Em todas as federações este fator vem se mostrando decisivo com peso de até 56,68%, como é o caso da Bolívia.")),
    
    tabItem(tabName = "confrontos",
            h2("Aproveitamento nos confrontos internacionais"),
            HTML('A entrada <b>(i,j)</b> desta matriz representa o aproveitamento dos clubes do país <b>i</b>  contra adversários do país <b>j</b>.'),
            plotlyOutput("df_confrontos"),
            plotlyOutput("plot_confrontos"),
            HTML("Podemos ver que o Brasil é o país com melhor aproveitamento nos confrontos internacionais; os clubes brasileiros apresentam um equilíbro em confrontos com equipes da Argentina e levam vantagem contra clubes dos demais países. 
Acima do 50% de aproveitamento, além do Brasil, temos a Argentina, Colômbia e Equador. Em contrapartida, as equipes da Venezuela são as que demonstram o pior desempenho nos confrontos internacionais com 30,89% no geral e apenas 10,83% em confrontos contra clubes brasileiros.")),
    
    tabItem(tabName = "estatisticas",
            h2("Estatísticas dos clubes"),
            HTML("Podemos observar estatísticas dos clubes participantes do ranking. Número de partidas computadas, vitórias, empates, gols feitos (GP), gols sofridos (GC) e o aproveitamento histórico das equipes."),
            uiOutput("estatisticas_ui"),
            dataTableOutput("estatisticas"),
            h4("Vídeo demonstrativo:"),
            HTML('<iframe width="600" height="400" src="https://www.youtube.com/embed/N0i1gpY7-NA" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')),
    
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
    
    tabItem(tabName = "tratamento",
            h2("Tratamento na base de dados"),
            HTML("<p> Obtidos os dados, o primeiro passo foi decidir o momento inicial do nosso ranking. Para alguns países, dados estavam disponíveis desde 1998 ou 1999, enquanto, para outros, somente a partir de 2002. Visando a uniformizar o momento de entrada dos times no ranking, escolhemos janeiro de 2002 como nosso tempo inicial."),
            HTML("<p> Outra decisão de projeto foi quais times entrariam na base. Considerando competições como as copas nacionais, nossa massa de jogos continha diversos times que disputaram somente um punhado de partidas. Caso esses times fossem incluídos, seu rating ficaria necessariamente próximo ao rating inicial, acima de times “mais fortes”, que disputaram muitas partidas, o que levaria a um desequilíbrio do modelo como um todo."),
            HTML("<p> Para minimizar o problema, decidimos incluir na base somente os times que disputaram ao menos uma temporada inteira na primeira divisão de seu país, num total de 341 times dos 10 países. E mantendo a coerência, deixamos na base de jogos somente aqueles em que ambas as equipes estivessem na nossa base de times, restando, assim, 60900 partidas na base."),
            HTML("<p> Encontramos ainda uma última dificuldade: embora nossa base apresentasse, dentro de cada país, nomes “exclusivos” para cada time, o mesmo não ocorria entre os diversos países. Assim, era possível diferir o Atlético de Minas do de Goiás, mas não, por exemplo, o River Plate argentino do seu homônimo uruguaio. Para resolver esse problema, tivemos que acessar manualmente as partidas internacionais das equipes com nomes duplicados e identificar de qual time se tratava.")
            ),
    
    #################################################################################
    
    # Modelagem
    #################################################################################
    
    tabItem(tabName = "processo",
            h2("Processo de modelagem"),
            HTML("<p> O cerne do modelo é muito simples: processar a base de jogos em ordem cronológica, atualizando, a cada partida, o rating Elo das equipes envolvidas, utilizando um algoritmo para calcular esse valor. Contudo, para podermos chegar a esse passo, precisamos estabelecer alguns parâmetros para o modelo."),
            HTML("<p> Primeiramente, o ranking Elo trabalha com um algoritmo de “soma zero”, i.e., a cada partida, os pontos que uma equipe ganha são equivalentes aos pontos que a outra equipe perde. Isso significa dizer que existe um rating médio, que deve corresponder a um patamar de times “nem bons nem ruins”. No nosso modelo, escolhemos o rating médio de 1500, que deve se manter ao longo do tempo."),
            HTML("<p> Para inicializar o ranking, no tempo inicial (janeiro de 2002), pensamos em avaliar todos os times com o rating médio de 1500, já que não estávamos considerando informações precedentes àquele momento. Não obstante, observamos que a prática levaria a uma certa distorção nos ratings atualizados. Os primeiros colocados no ranking seriam os times com maior sucesso em seus torneios locais, que, observamos, não correspondiam aos times de maior sucesso nos torneios continentais."),
            HTML("<p> Nossa interpretação para o fenômeno foi de que alguns torneios nacionais são mais desequilibrados que outros e, portanto, um time com supremacia local não seria necessariamente melhor que um time de outro país que estivesse “no bolo” dos concorrentes ao título. Para corroborar nossa tese, computamos o aproveitamento dos times de cada país em confrontos internacionais e verificamos que os países de melhor desempenho são alguns dos que têm os torneios nacionais mais equilibrados e, com isso, acabavam subestimados no ranking."),
            HTML("<p> As seções 'Confrontos internacionais' e 'Estatísticas dos clubes' na aba 'Análise exploratória' podem dar uma boa noção do quão diferentes são a lista dos países com melhor aproveitamento internacional e a dos países dos clubes com melhor aproveitamento."),
            HTML("<p> A forma encontrada para solucionar o problema foi inicializar o ranking com valores distintos para times de países diferentes. Foi calculado um rating “para cada país”, em função do desempenho de seus times nas três edições da Copa Libertadores imediatamente anteriores ao nosso tempo inicial. Depois disso, ponderamos esse “rating nacional” em função de número de times do país na base e inicializamos cada time com esse valor ponderado, de modo a manter o rating médio de 1500."),
            HTML("<p> Reprocessando o ranking ao longo de todo o período, pudemos observar que esse ajuste nos ratings iniciais propiciou uma melhor correlação entre os líderes do ranking mensal e os times considerados mais fortes no respectivo mês. Como exemplo, vale observar que os quatro semifinalistas da atual edição da Copa Libertadores se encontram entre os cinco primeiros colocados do último ranking.")
            ),
    
    tabItem(tabName = "elo",
            h2("Rating Elo"),
            HTML("<p> O método base para essa pontuação está o sistema Elo, um método muito famoso usado em larga escala para medir os níveis de força relativa. A vantagem da implementação do Elo está na sua grande simplicidade: temos apenas um valor para cada clube no determinado momento e, quanto maior, melhor é a equipe."),
            HTML("<p> A diferença de pontos entre as duas equipes está totalmente ligada as suas chances de vencer o confronto, <i>E</i>."),
            withMathJax("$$ E = {1 \\over {10^{-dr/400}+1}} $$"),
            HTML("<p> Onde <i>dr</i> é a diferença de pontos no ranking entre as duas equipes."),
            HTML("<p> A troca de pontos acontece quando os clubes se enfrentam. A quantidade de pontos trocados é feita para que um coeficiente de vitória entre as duas equipes faça a diferença entre as equipes convergir para a real taxa de vitória entre as equipes."),
            HTML("<p> Usamos a seguinte equacação:"),
            withMathJax("$$ \\Delta Elo = (R-E)K $$"),
            HTML("<p> Em que <i>R</i> é o resultado do jogo (1 para vitória, 0,5 para empate e 0 para derrota)."),
            HTML("<p> <i>K</i> é uma constante que deve ser escolhida: quanto maior o valor de <i>K</i>, mais rapidamente a classificação convergirá para seu real valor, porém teremos variações mais fortes. Um valor menor de <i>K</i> dá mais estabilidade, no entanto, precisa de mais tempo para convergir. Optamos pelo valor de 20 para <i>K</i>."),
            HTML("<p> Uma vitória por um placar mais elástico é considerada mais expressiva do que uma vitória por um placar estreito, levando em consideração quando a quantidade de pontos é trocada e calculada. Os pontos que são trocados aumentam proporcionalmente à raiz quadrada da diferença de gols."),
            HTML("<p> Os pontos trocados em caso de vitória ou derrota são determinados da seguinte maneira:"),
            withMathJax("$$ \\Delta m = {\\Delta Elo \\over {l(\\Theta) + l(\\theta) \\sqrt {\\Delta g}}} \\sqrt {\\Delta g}$$"),
            HTML("<p> Onde <i>l</i>(&#952;) é a chance para uma diferença de gols específica, <i>l</i>(&#920;) é a chance de ganhar ou perder por qualquer margem de gols e &#916;<i>g</i> é a margem de gols da partida."),
            HTML("<p> Como podemos verificar na analise exploratória, as equipes tem uma tendência de ter mais chances de vitória por jogar em casa, ou seja, em média as equipes que jogam em casa ganham mais pontos no ranking. Desta maneira, aumentamos a diferença para uma partida em uma certa quantidade de pontos denominada HFA (Vantagem de jogar em casa)."),
            HTML("<p> Neste projeto optamos por utilizar um HFA constante, igual a 100.")
    ),
    
    #################################################################################
    
    # Resultados
    #################################################################################
    
    tabItem(tabName = "maiores_pontuacoes",
            h2("Melhores pontuações da medida Elo"),
            HTML("Aqui apresentamos os maiores picos de pontuações da série histórica da medida Elo, observe que há equipes que aparecem mais vezes, essa análise é feita independentemente das equipes, visando somente a quantidades de pontos alcançados, restringindo apenas a uma entrada da mesma equipe por ano. Nesta tabela destacam-se os clubes do Palmeiras e Flamengo nos dias atuais, que realmente prevalecem no cenário brasileiro. Também vale a pena ressaltar o São Paulo de 2007 que dominava o campeonato brasileiro e conseguiu a quarta melhor pontuação relativamente cedo no ranking."),
            tableOutput("maiores_pontuacoes")),
    
    tabItem(tabName = "tempo_lideranca",
            h2("Clubes que permaneceram mais meses no primeiro lugar do ranking"),
            HTML("Somando os meses em que cada equipe permaneceu na liderança do ranking, podemos acompanhar os times que mais figuraram entre os melhores da América do Sul. Ênfase para as clubes do São Paulo, Boca Juniors e Cruzeiro que lutaram pelo título de praticamente todos os campeonatos que disputaram neste século e compõem o topo deste ranking."),
            tableOutput("tempo_lideranca")),
    
    tabItem(tabName = "tempo_lideranca_consec",
            h2("Clubes que permaneceram mais meses consecutivos no primeiro lugar do ranking"),
            HTML("Diferente do outro resultado, aqui podemos analisar as equipes que mais permaneceram consecutivamente no topo do ranking, mantendo resultados e padrão por mais tempo, o que permitiu se sustentar na elite do futebol sul-americanos por mais tempo. Destaque aqui para o São Paulo que conseguiu se manter na primeira posição do ranking por 17 meses consecutivos."),
            tableOutput("tempo_lideranca_consec")),
    
    tabItem(tabName = "elo_medio",
            h2("Elo médio por clube"),
            HTML("Nesta tabela podemos verificar as equipes mais consistentes no cenário sul-americano. Novamente, destaque para Boca Juniors e São Paulo que apresentam equipes competitivas em praticamente todas as temporadas deste século."),
            tableOutput("elo_medio")),
    
    # Próximos passos
    #################################################################################
    
    tabItem(tabName = "next")
    
    #################################################################################
    
            )
    )

ui <- dashboardPage(skin = "red",
                    dashboardHeader(title = "ConmebElo"),
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
      scale_x_date(breaks = as.Date(c(paste0(2002:2020, "-01-01"))),
                   labels = c(2002:2020)) +
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
  
  output$tempo_lideranca_consec <- renderTable(maiores_streaks %>%
                                                rename(Até = Ate), 
                                              rownames = TRUE, 
                                              striped = TRUE, hover = TRUE, width = 700,
                                              sanitize.text.function = function(x) x,
                                              na = " ")
  
  output$tempo_lideranca <- renderTable(mais_meses_lider,
                                        rownames = TRUE,
                                        striped = TRUE, hover = TRUE, width = 700,
                                        sanitize.text.function = function(x) x)
  
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
  
  output$elo_medio <- renderTable(elo_medio %>%
                                    rename(`Elo médio` = Elo),
                                  rownames = TRUE,
                                  striped = TRUE, hover = TRUE, width = 700,
                                  sanitize.text.function = function(x) x)
}

shinyApp(ui, server)








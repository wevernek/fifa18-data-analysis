# Limpa todas as variáveis existentes no nosso RStudio
rm(list=ls())

# Carrega todas as nossas bibliotecas necessárias para a análise do dataset
library(DBI)
library(readr)
library(dplyr)
library(plotly)
library(tree)
library(randomForest)
library(corrplot)
library(MLmetrics)
library(DT)
library(data.table)
library(formattable)
library(RPostgres)
library(ggplot2)
library(rworldmap)
options(scipen = 999, digits = 4, repr.plot.width = 12, repr.plot.height = 8)

# Carrega o nosso CSV
# O CSV foi gerado através de inner_joins das consultas do banco de dados cedido pelo do professor ... As tabelas utilizadas foram:
# 'futebol.players', 'futebol.habilities', 'futebol.features' e 'futebol.financial' e o resultado final é lido logo abaixo.
# Observação: É necessário alterar o path do read_csv para apontar corretamente o arquivo .csv de acordo com o seu S. O.
df <- read_csv("/home/pedro/Desktop/fifa18-data-analysis/data/fifa18.csv")
setDT(df) # transforma o dataframe em datatable

# Estatísticas básicas do nosso DF
summary(df)

# Pega todos os jogadores meio-de-campo do nosso DF para criarmos uma variável de análise
center_mid_players <- df %>%
  filter(Position == "Center-mid")

# Histograma de jogadores por overall
# Conseguimos identificar uma grande concentração de jogadores com overall entre 60 e 73
histogram <- plot_ly(x = ~center_mid_players$overall,
                     type = "histogram",
                     marker = list(color = "lightgray",
                                   line = list(color = "darkgray",
                                               width = 1))) %>%
  layout(title = "Histograma por overall dos jogadores (Center-mid)",
         xaxis = list(title = "Overall",
                      zeroline = FALSE),
         yaxis = list(title = "Quantidade",
                      zeroline = FALSE))

histogram # Mostra o histograma

# Top 10 melhores jogadores da posição
center_mid_players %>%
  arrange(-overall) %>% 
  top_n(10, wt = overall) %>% 
  select(full_name, overall, club, eur_value, Position) %>% 
  datatable(class = "nowrap hover row-border", escape = FALSE, options = list(dom = 't',
                                                                              scrollX = TRUE, 
                                                                              autoWidth = TRUE))

# Quantidade de jogadores por seus respectivos clubes do nosso DF (em tabela)
df %>% 
  count(club) %>% 
  formattable(align = 'l')

# Países por continente nas suas respectivas listas
# A partir daqui, iremos definir a nacionalidade de cada jogador de acordo com o a informação
# de continente que temos no nosso DF original. Após aplicar as devidas tratavivas, iremos mostrar
# um gráfico com a quantidade de jogadores por país
Africa <- c('Algeria','Angola','Benin','Botswana','Burkina','Burundi','Cameroon','Cape Verde','Central African Republic','Chad','Comoros','Congo','Congo Democratic Republic of','Djibouti','Egypt','Equatorial Guinea','Eritrea','Ethiopia','Gabon','Gambia','Ghana','Guinea','Guinea-Bissau','Ivory Coast','Kenya','Lesotho','Liberia','Libya','Madagascar','Malawi','Mali','Mauritania','Mauritius','Morocco','Mozambique','Namibia','Niger','Nigeria','Rwanda','Sao Tome and Principe','Senegal','Seychelles','Sierra Leone','Somalia','South Africa','South Sudan','Sudan','Swaziland','Tanzania','Togo','Tunisia','Uganda','Zambia','Zimbabwe','Burkina Faso')
Antarctica <- c('Fiji','Kiribati','Marshall Islands','Micronesia','Nauru','New Zealand','Palau','Papua New Guinea','Samoa','Solomon Islands','Tonga','Tuvalu','Vanuatu')
Asia <- c('Afghanistan','Bahrain','Bangladesh','Bhutan','Brunei','Burma (Myanmar)','Cambodia','China','East Timor','India','Indonesia','Iran','Iraq','Israel','Japan','Jordan','Kazakhstan','North Korea','South Korea','Kuwait','Kyrgyzstan','Laos','Lebanon','Malaysia','Maldives','Mongolia','Nepal','Oman','Pakistan','Philippines','Qatar','Russian Federation','Saudi Arabia','Singapore','Sri Lanka','Syria','Tajikistan','Thailand','Turkey','Turkmenistan','United Arab Emirates','Uzbekistan','Vietnam','Yemen','Russia')
Europe <- c('Albania','Andorra','Armenia','Austria','Azerbaijan','Belarus','Belgium','Bosnia and Herzegovina','Bulgaria','Croatia','Cyprus','Czech Republic','Denmark','Estonia','Finland','France','Georgia','Germany','Greece','Hungary','Iceland','Ireland','Italy','Latvia','Liechtenstein','Lithuania','Luxembourg','Macedonia','Malta','Moldova','Monaco','Montenegro','Netherlands','Norway','Poland','Portugal','Romania','San Marino','Scotland','Serbi','Slovakia','Slovenia','Spain','Sweden','Switzerland','Ukraine','England','Vatican City','Republic of Ireland','Wales')
North_america <- c('Antigua and Barbuda','Bahamas','Barbados','Belize','Canada','Costa Rica','Cuba','Dominica','Dominican Republic','El Salvador','Grenada','Guatemala','Haiti','Honduras','Jamaica','Mexico','Nicaragua','Panama','Saint Kitts and Nevis','Saint Lucia','Saint Vincent and the Grenadines','Trinidad and Tobago','United States')
South_america <- c('Argentina','Bolivia','Brazil','Chile','Colombia','Ecuador','Guyana','Paraguay','Peru','Suriname','Uruguay','Venezuela')

df[, continent :=  df$nationality] # Cria a coluna 'continent' espelhando a coluna 'nationality'
df <- df %>% 
  relocate(continent, .after = nationality) # Move 'continent' para depois da coluna 'nationality'

# Checa a Nacionalidade do jogador e atribui o continente de acordo com o que acha dentro das listas dos continentes
df$continent[df$continent %in% Africa ] <- "Africa"
df$continent[df$continent %in% Antarctica ] <- "Antarctica"
df$continent[df$continent %in% Asia ] <- "Asia"
df$continent[df$continent %in% Europe ] <- "Europe"
df$continent[df$continent %in% North_america ] <- "North_america"
df$continent[df$continent %in% South_america ] <- "South_america"

# Análise da quantidade de jogadores (mapa de calor nos países) do nosso dataset
country <- df[,.N, by = nationality]
fr <- joinCountryData2Map(dF = country, joinCode = "NAME", nameJoinColumn = "nationality", verbose = F)

mapCountryData(mapToPlot = fr,nameColumnToPlot = "N",
               catMethod = "fixedWidth",
               oceanCol = "steelblue1",
               missingCountryCol = "white",
               mapTitle = "Quantidade de jogadores por país",
               aspect = "variable") 

# Mostra o nosso gŕafico de jogadores por país
plot_intro(df)
plot_missing(df)

# Quantidade de jogadores por nacionalidade do nosso DF (em tabela)
df %>% 
  count(nationality) %>% 
  formattable(align = 'l')


# Jogadores meio-campo (Center-mid) que atuam no continente europeu
MID_EUROPE <- df %>%
  filter(Position == "Center-mid" & continent == "Europe")

# Dataframe para validação - Jogadores meio-campo que NÃO jogam na europa
MID_NOT_EUROPE <- df %>%
  filter(Position == "Center-mid" & continent != "Europe")

# Trata os dados removendo valores NA e não numéricos para treinamento do modelo
MID_EUROPE <- MID_EUROPE %>% 
  select_if(~ !any(is.na(.))) %>%
  select_if(~ any(is.numeric(.)))

fifa.18.cm <- MID_NOT_EUROPE %>% 
  select_if(~ !any(is.na(.))) %>%
  select_if(~ any(is.numeric(.)))

# Boxplot dos valores de 'MID_EUROPE' ... Outliers no valor em euros
boxplot(MID_EUROPE)

# Matriz de correlação das habilidades e características dos jogadores selecionados em MID_EUROPE
corrMatrix <- cor(MID_EUROPE)
corrplot.mixed(corrMatrix, 
               lower = "ellipse", 
               upper = "number",
               tl.pos = "lt",
               tl.col = "black",
               order="hclust",
               hclust.method = "ward.D",
               addrect = 3)

reg <- tree(data = MID_EUROPE, formula = eur_value ~ .)
plot(reg)
text(reg)
summary(reg)

set.seed(1)
reg.test <- randomForest(formula = eur_value ~ ., 
                         data = MID_EUROPE, 
                         ntree=100, 
                         proximity=TRUE, 
                         localImp=TRUE)
plot(reg.test)

predito = predict(reg.test, MID_EUROPE)
print(paste("R2: ", R2_Score(predito, MID_EUROPE$eur_value) ) )
print(paste("MSE: ", MSE(predito, MID_EUROPE$eur_value) ) )

MID_EUROPE[, predito:=predito]
MID_EUROPE <- MID_EUROPE %>% relocate(predito, .after = eur_value)
head(MID_EUROPE)

MID_EUROPE %>%
  mutate(predito = predict(reg.test, .)) %>%
  plot_ly(x = ~eur_value,
          y= ~predito,
          type='scatter',
          mode='markers',
          text=~paste0("Valor real: ", currency(eur_value, symbol='€', digits = 0L), 
                       "\nValor predito: ", currency(predito, symbol='€', digits = 0L), 
                       "\nErro: ", (eur_value - predito)),
          name="Dispersão") %>%
  add_segments(x=0, y=0, xend = 80000000, yend = 80000000, name="Equilíbrio")
#-----------------------------------

predito = predict(reg.test, fifa.18.cm)
print(paste("R2: ", R2_Score(predito, fifa.18.cm$eur_value) ) )
print(paste("MSE: ", MSE(predito, fifa.18.cm$eur_value) ) )

fifa.18.cm[, predito:=predito]
fifa.18.cm <- fifa.18.cm %>% relocate(predito, .after = eur_value)
head(fifa.18.cm)

fifa.18.cm %>%
  mutate(predito = predict(reg.test, .)) %>%
  plot_ly(x = ~eur_value,
          y= ~predito,
          type='scatter',
          mode='markers',
          text=~paste0("Valor real: ", currency(eur_value, symbol='€', digits = 0L), 
                       "\nValor predito: ", currency(predito, symbol='€', digits = 0L), 
                       "\nErro: ", (eur_value - predito)),
          name="Dispersão") %>%
  add_segments(x=0, y=0, xend = 60000000, yend = 60000000, name="Equilíbrio")


# Seleciona as variáveis 'Position', 'name', e 'eur_value' de MID_NOT_EUROPE e atribui
# a nossa variável 'output' que servirá de demonstrativo para o nosso cliente final
output <- MID_NOT_EUROPE %>%
  select(Position, name, eur_value) 

# Trata os dados das colunas e seus nomes
output[, eur_value := currency(fifa.18.cm$eur_value, symbol = '€', digits = 0L)]
output[, 'Preço "Calculado" (€)' := currency(fifa.18.cm$predito, symbol = '€', digits = 0L)]
output[, 'Potencial Valorização (€)' := currency((fifa.18.cm$predito - fifa.18.cm$eur_value), symbol='€', digits = 0L) ]
output[, 'Potencial Valorização (%)' := (percent((fifa.18.cm$predito - fifa.18.cm$eur_value) / 100000000)) ]

# rename de algumas colunas
output <- output %>% 
  rename(
    'Posição' = Position,
    'Jogador' = name,
    'Preço de mercado' = eur_value
  )

head(output) # Print no console do output
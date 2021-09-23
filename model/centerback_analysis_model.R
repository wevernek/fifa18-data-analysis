rm(list=ls())
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
#library(ggplot2)
#library(scales)
#library(DataExplorer)
#library(rworldmap)
options(scipen = 999, digits = 4)

# Se conecta ao banco de dados da FIAP para consultar as tabelas
connection <- dbConnect(
  Postgres(),
  host=Sys.getEnv("HOST"),
  port = Sys.getEnv("PORT"),
  dbname= Sys.getEnv("DB_NAME"),
  user= Sys.getEnv("USER"),
  password= Sys.getEnv("PASSWORD"))

# Recuperamos as informações das tabelas do banco para o nosso modelo
df_players <- dbGetQuery(con=connection, "SELECT * FROM futebol.players")
df_habilities <- dbGetQuery(con=connection, "SELECT * FROM futebol.habilities")
df_features <- dbGetQuery(con=connection, "SELECT * FROM futebol.features")
df_financial <- dbGetQuery(con=connection, "SELECT * FROM futebol.financial")

dbDisconnect # Desconecta-se do banco

# Faz os inner_joins dos dados consultados acima no nosso dataframe (df) principal
df = inner_join(df_players, df_financial)
df = inner_join(df, df_habilities)
df = inner_join(df, df_features)

#df <- read_csv("fifa18-data-analysis/model/data/fifa18.csv", locale = locale(encoding = "ISO-8859-1"))
setDT(df) # transforma o dataframe em datatable

# Países por continente nas suas respectivas listas
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

# Jogadores meio-campo (Center-back) que atuam no continente europeu
MID_EUROPE <- df %>%
  filter(Position == "Center-back" & continent == "Europe")

# Dataframe para validação - Jogadores meio-campo que NÃO jogam na europa
MID_NOT_EUROPE <- df %>%
  filter(Position == "Center-back" & continent != "Europe")

# Trata os dados removendo valores NA e não numéricos para treinamento do modelo
MID_EUROPE <- MID_EUROPE %>% 
  select_if(~ !any(is.na(.))) %>%
  select_if(~ any(is.numeric(.)))

fifa.18.cb <- MID_NOT_EUROPE %>% 
  select_if(~ !any(is.na(.))) %>%
  select_if(~ any(is.numeric(.)))

# Boxplot dos valores de 'MID_EUROPE' ... Outliers no valor em euros
boxplot(MID_EUROPE)

# Matrix de corelação das habilidades e características dos jogadores selecionados em MID_EUROPE
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
          text=~paste0("Real value: ", currency(eur_value, symbol='€', digits = 0L), 
                       "\nPredicted value: ", currency(predito, symbol='€', digits = 0L), 
                       "\nError: ", (eur_value - predito)),
          name="Dispersão") %>%
  add_segments(x=0, y=0, xend = 80000000, yend = 80000000, name="Equilíbrio")
#-----------------------------------

predito = predict(reg.test, fifa.18.cb)
print(paste("R2: ", R2_Score(predito, fifa.18.cb$eur_value) ) )
print(paste("MSE: ", MSE(predito, fifa.18.cb$eur_value) ) )

fifa.18.cb[, predito:=predito]
fifa.18.cb <- fifa.18.cb %>% relocate(predito, .after = eur_value)
head(fifa.18.cb)

fifa.18.cb %>%
  mutate(predito = predict(reg.test, .)) %>%
  plot_ly(x = ~eur_value,
          y= ~predito,
          type='scatter',
          mode='markers',
          text=~paste0("Real value: ", currency(eur_value, symbol='€', digits = 0L), 
                       "\nPredicted value: ", currency(predito, symbol='€', digits = 0L), 
                       "\nError: ", (eur_value - predito)),
          name="Dispersão") %>%
  add_segments(x=0, y=0, xend = 60000000, yend = 60000000, name="Equilíbrio")


output <- MID_NOT_EUROPE %>%
  select(Position, name, eur_value) 

output[, eur_value := currency(fifa.18.cb$eur_value, symbol = '€', digits = 0L)]
output[, 'Preço "Calculado" (€)' := currency(fifa.18.cb$predito, symbol = '€', digits = 0L)]
output[, 'Potencial Valorização (€)' := currency((fifa.18.cb$predito - fifa.18.cb$eur_value), symbol='€', digits = 0L) ]
output[, 'Potencial Valorização (%)' := (percent((fifa.18.cb$predito - fifa.18.cb$eur_value) / 100000000)) ]

output <- output %>% 
  rename(
    'Posição' = Position,
    'Jogador' = name,
    'Preço de mercado' = eur_value
  )

head(output)
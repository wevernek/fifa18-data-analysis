rm(list=ls())
library(readr)
library(dplyr)
library(plotly)
library(tree)
library(randomForest)
library(corrplot)
library(MLmetrics)
library(DataExplorer)
library(ggplot2)
library(scales)
library(DT)
library(data.table)
library(rworldmap)
options(scipen = 999, digits = 4)

#// 1) carregar os jogadores 
fifa18 <- read_csv("/media/njaneto/HD1/FIAP/PROGRAMANDO_IA_COM_R/Trabalho_Final/data/fifa_game-2.csv", locale = locale(encoding = "ISO-8859-1")) 
setDT(fifa18)

#--- incluindo coluna de continente
Africa<-c('Algeria','Angola','Benin','Botswana','Burkina','Burundi','Cameroon','Cape Verde','Central African Republic','Chad','Comoros','Congo','Congo Democratic Republic of','Djibouti','Egypt','Equatorial Guinea','Eritrea','Ethiopia','Gabon','Gambia','Ghana','Guinea','Guinea-Bissau','Ivory Coast','Kenya','Lesotho','Liberia','Libya','Madagascar','Malawi','Mali','Mauritania','Mauritius','Morocco','Mozambique','Namibia','Niger','Nigeria','Rwanda','Sao Tome and Principe','Senegal','Seychelles','Sierra Leone','Somalia','South Africa','South Sudan','Sudan','Swaziland','Tanzania','Togo','Tunisia','Uganda','Zambia','Zimbabwe','Burkina Faso')
Antarctica<-c('Fiji','Kiribati','Marshall Islands','Micronesia','Nauru','New Zealand','Palau','Papua New Guinea','Samoa','Solomon Islands','Tonga','Tuvalu','Vanuatu')
Asia<-c('Afghanistan','Bahrain','Bangladesh','Bhutan','Brunei','Burma (Myanmar)','Cambodia','China','East Timor','India','Indonesia','Iran','Iraq','Israel','Japan','Jordan','Kazakhstan','North Korea','South Korea','Kuwait','Kyrgyzstan','Laos','Lebanon','Malaysia','Maldives','Mongolia','Nepal','Oman','Pakistan','Philippines','Qatar','Russian Federation','Saudi Arabia','Singapore','Sri Lanka','Syria','Tajikistan','Thailand','Turkey','Turkmenistan','United Arab Emirates','Uzbekistan','Vietnam','Yemen','Russia')
Europe<-c('Albania','Andorra','Armenia','Austria','Azerbaijan','Belarus','Belgium','Bosnia and Herzegovina','Bulgaria','Croatia','Cyprus','Czech Republic','Denmark','Estonia','Finland','France','Georgia','Germany','Greece','Hungary','Iceland','Ireland','Italy','Latvia','Liechtenstein','Lithuania','Luxembourg','Macedonia','Malta','Moldova','Monaco','Montenegro','Netherlands','Norway','Poland','Portugal','Romania','San Marino','Scotland','Serbi','Slovakia','Slovenia','Spain','Sweden','Switzerland','Ukraine','England','Vatican City','Republic of Ireland','Wales')
North_america<-c('Antigua and Barbuda','Bahamas','Barbados','Belize','Canada','Costa Rica','Cuba','Dominica','Dominican Republic','El Salvador','Grenada','Guatemala','Haiti','Honduras','Jamaica','Mexico','Nicaragua','Panama','Saint Kitts and Nevis','Saint Lucia','Saint Vincent and the Grenadines','Trinidad and Tobago','United States')
South_america<-c('Argentina','Bolivia','Brazil','Chile','Colombia','Ecuador','Guyana','Paraguay','Peru','Suriname','Uruguay','Venezuela')

fifa18[, continente:= fifa18$nationality ]
fifa18 <- fifa18 %>% relocate(continente, .after = nationality)

fifa18$continente[fifa18$continente %in% Africa ] <- "Africa"
fifa18$continente[fifa18$continente %in% Antarctica ] <- "Antarctica"
fifa18$continente[fifa18$continente %in% Asia ] <- "Asia"
fifa18$continente[fifa18$continente %in% Europe ] <- "Europe"
fifa18$continente[fifa18$continente %in% North_america ] <- "North_america"
fifa18$continente[fifa18$continente %in% South_america ] <- "South_america"

#// 2) filtrar somente goleiros
fifa.18.gk_europe <- fifa18 %>%
  filter(positioning == "14" & continente == "Europe" )

#-- base para validacao
fifa.18.gk_nao_europe <- fifa18 %>%
  filter(positioning == "14" & continente != "Europe" )

unique(fifa.18.gk_europe$continente)
unique(fifa.18.gk_nao_europe$continente)


#// 3) graficos

plot_intro(fifa.18.gk_europe)
plot_missing(fifa.18.gk_europe)

#// 4) removendo os NAs e variaves nao numericas
fifa.18.gk_europe <- fifa.18.gk_europe %>% 
  select_if(~ !any(is.na(.))) %>%
  select_if(~ any(is.numeric(.))) %>%
  select(-ID,-positioning)

fifa.18.gk <- fifa.18.gk_nao_europe %>% 
  select_if(~ !any(is.na(.))) %>%
  select_if(~ any(is.numeric(.))) %>%
  select(-ID,-positioning)


#// 5) treinamento e predicao
boxplot(fifa.18.gk_europe)

corrMatrix <- cor(fifa.18.gk_europe)
corrplot.mixed(corrMatrix, 
               lower = "ellipse", upper = "number",
               tl.pos = "lt",
               tl.col = "black",
               order="hclust",
               hclust.method = "ward.D",
               addrect = 3)

reg <- tree(data = fifa.18.gk_europe, formula = eur_value ~ .)
plot(reg)
text(reg)

set.seed(1)
reg.test <- randomForest(formula = eur_value ~ ., data = fifa.18.gk_europe, ntree=100, proximity=TRUE, localImp = TRUE)
plot(reg.test)

predito = predict(reg.test, fifa.18.gk_europe)
print(paste("R2: ", R2_Score(predito, fifa.18.gk_europe$eur_value) ) )
print(paste("MSE: ", MSE(predito, fifa.18.gk_europe$eur_value) ) )

fifa.18.gk_europe[, predito:= predito ] 
fifa.18.gk_europe <- fifa.18.gk_europe %>% relocate(predito, .after = eur_value)
head(fifa.18.gk_europe)

fifa.18.gk_europe %>%
  mutate(predito = predict(reg.test, .)) %>%
  plot_ly(x = ~eur_value,
          y= ~predito,
          type='scatter',
          mode='markers',
          text= ~paste0("Real: ", eur_value, "\nPredito: ", predito, "\nErro: ", eur_value-predito),
          name="Disperscao") %>%
  add_segments(x=0, y=0, xend = 20000000, yend = 20000000, name="Equilibrio")

#-------------------------------------------------------------

predito = predict(reg.test, fifa.18.gk)
print(paste("R2: ", R2_Score(predito, fifa.18.gk$eur_value) ) )
print(paste("MSE: ", MSE(predito, fifa.18.gk$eur_value) ) )

fifa.18.gk[, predito:= predito ]
fifa.18.gk <- fifa.18.gk %>% relocate(predito, .after = eur_value)
head(fifa.18.gk)

fifa.18.gk %>%
  mutate(predito = predict(reg.test, .)) %>%
  plot_ly(x = ~eur_value,
          y= ~predito,
          type='scatter',
          mode='markers',
          text= ~paste0("Real: ", eur_value, "\nPredito: ", predito, "\nErro: ", eur_value-predito),
          name="Disperscao") %>%
  add_segments(x=0, y=0, xend = 20000000, yend = 20000000, name="Equilibrio")

fifa.18.gk_final <- fifa.18.gk_nao_europe %>%
  select(positioning,full_name, nationality, age , eur_value) 

fifa.18.gk_final[,predito:=fifa.18.gk$predito]
fifa.18.gk_final[, potential_value:= (abs(fifa.18.gk$predito - fifa.18.gk$eur_value)) ]
fifa.18.gk_final[, 'potential_value_%' := (percent((fifa.18.gk$predito - fifa.18.gk$eur_value) / 100000000)) ]
fifa.18.gk_final$positioning <- gsub('14', 'GK', fifa.18.gk_final$positioning)




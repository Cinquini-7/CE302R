## Instalando o pacote tidyverse
install.packages("tidyverse")
## Carregando o pacote tidyverse
library(tidyverse)

## Importação de dados ---------------------------------------------------------

## Importando arquivo do tipo .txt
url <- "http://leg.ufpr.br/~wagner/scientificR/anovareg.txt"
dados <- read_tsv(url, col_names = TRUE)
head(dados)

## Importando dados do tipo .csv
url <- "http://leg.ufpr.br/~wagner/scientificR/reglinear.csv"
dados <- read_table(url, col_names = TRUE)
head(dados)

## Baixando e importando uma planilha eletrônica
# Se precisar instalar o readxl ou o httr descomente os códigos abaixo
# install.packages("readxl")
# install.packages("httr")

library(readxl)
library(httr)
url <- "http://leg.ufpr.br/~wagner/scientificR/meus_dados.xlsx"
GET(url, write_disk(tf <- tempfile(fileext = ".xlsx")))
tb <- read_excel(tf, sheet = "mtcars")
View(tb)

## Conexão com banco de dados relacionais
## O banco usado para exemplo é público e pode sair do ar.

## Se não tiver instalado os pacotes DBI e RMySQL descomente as linhas abaixo
 install.packages("DBI")
 install.packages("RMySQL")

library(DBI)
library(RMySQL)

# Criando a conexão.
db <- dbConnect(
    RMySQL::MySQL(),
    user = "rfamro", password = "",
    port = 4497, dbname = "Rfam",
    host = "mysql-rfam-public.ebi.ac.uk")

# Lista as tabelas do BD.
dbListTables(db)

# Listas as colunas em uma tabela.
dbListFields(db, "keywords")

# Importanto a tabela.
tb <- RMySQL::dbFetch(
    RMySQL::dbSendQuery(
        db, "SELECT * FROM keywords"))
str(tb)
View(tb)
# Desconecta
dbDisconnect(db)

## Manipulação de dados---------------------------------------------------------

## Criando um tibble
# Tabela com alunos do curso de Matemática e de Estatística.
df1 <- tibble(
    mat = c(256, 487, 965,
            125, 458, 874, 963),
    nome = c("João", "Vanessa", "Tiago",
             "Luana", "Gisele", "Pedro",
             "André"),
    curso = c("Mat", "Mat", "Est", "Est",
              "Est", "Mat", "Est"),
    prova1 = c(80, 75, 95, 70, 45, 55, 30),
    prova2 = c(90, 75, 80, 85, 50, 75, NA),
    prova3 = c(80, 75, 75, 50, NA, 90, 30),
    faltas = c(4, 4, 0, 8, 16, 0, 20))

# Informações de cadastro dos alunos em outra base de dados.
df_extra <- tribble(
    ~mat,     ~nome, ~idade, ~bolsista,
    256,  'João'  ,     18,       "S",
    965,  'Tiago' ,     18,       "N",
    285,  'Tiago' ,     22,       "N",
    125,  'Luana' ,     21,       "S",
    874,  'Pedro' ,     19,       "N",
    321,  'Mia'   ,     18,       "N",
    669,  'Luana' ,     19,       "S",
    967,  'André' ,     20,       "N",
)


## Combinação de dados----------------------------------------------------------

# Concatenação na vertical (pilha).
bind_rows(df1[1:3, c(1, 3, 5)],
          df1[5:7, c(1, 3, 5, 4)],
          df1[4,   c(1,    5, 4)])

# Concatenação na horizontal (fila).
bind_cols(df1[, c(1:3)],
          df1[, c(6:7)])

# Full join = união.
full_join(df1, df_extra,
          by = c("mat" = "mat", "nome"))

# Inner join = intersecção.
inner_join(df1,
           df_extra,
           by = c("mat" = "mat",
                  "nome"))

# Todos os que estão na 1º tabela
left_join(df1, df_extra,
          by = c("mat" = "mat",
                 "nome"))

# Todos os que estão na 2º tabela
right_join(df1, df_extra,
           by = c("mat" = "mat",
                  "nome"))

# Os da 2º que não aparecem na 1º.
anti_join(df1, df_extra,
          by = c("mat" = "mat",
                 "nome"))

## Exportação de dados----------------------------------------------------------

## Texto pleno
write_csv(df1, 
          file = "Nome_do_arquivo.csv")

## Binário padrão R
save(df1, file = "Nome_do_arquivo.RData")

## Carregando arquivo .RData
load("Nome_do_arquivo.RData")

## Planilha eletrônica
library(writexl)
write_xlsx(df1, "Nome_do_arquivo.xlsx")

## Script : Exemplos de acesso a APIs públicas ---------------------------------

install.packages('httr')
install.packages('jsonlite1')
install.packages('dplyr')
## Baixando Rtools
install.packages("installr")
library(installr)
installr::install.Rtools()
install.packages('pkgbuild')

## Carregando pacotes adicionais
library(httr)
library(jsonlite)
library(dplyr)

# -----------------------------
# 1. API do IBGE - Lista de Estados do Brasil
# Documentação: https://servicodados.ibge.gov.br/api/docs/
# -----------------------------
res_ibge <- GET("https://servicodados.ibge.gov.br/api/v1/localidades/estados")

# Verificando status da resposta
stop_for_status(res_ibge)

# Conteúdo da resposta (parsed = já em lista R)
estados <- content(res_ibge, as = "parsed", encoding = "UTF-8")
estados

# Organizando para ter um data.frame
df_estados <- tibble::tibble(
    id = sapply(estados, `[[`, "id"),
    nome = sapply(estados, `[[`, "nome"),
    sigla = sapply(estados, `[[`, "sigla"),
    regiao = sapply(estados, function(x) x$regiao$nome)
)

head(df_estados)

# -----------------------------
# 2. API Open-Meteo - Previsão do tempo (sem chave)
# Documentação: https://github.com/open-meteo/open-meteo
# -----------------------------

res_meteo <- GET("https://api.open-meteo.com/v1/forecast",
                 query = list(latitude = -23.55, 
                              longitude = -46.63, 
                              hourly = "temperature_2m"))

stop_for_status(res_meteo)

dados_meteo <- content(res_meteo, as = "parsed", encoding = "UTF-8")
dados <- data.frame('tempo' = unlist(dados_meteo$hourly$time), 
                    'temperatura' = unlist(dados_meteo$hourly$temperature_2m))

head(dados)

# -----------------------------
# 3. Exemplo usando a RapidAPI (cuidado isso é um serviço pago que tem algumas requisições grátis!)
# TikTok Scraper: https://rapidapi.com/tikwm-tikwm-default/api/tiktok-scraper7
# -----------------------------

## Exemplo que precisa de chave de API.
## A minha chave está guardada no arquivo .env (não disponibilizado)
## Você pode criar a sua chave no site rapidapi.com API Tiktok Scraper.
## Grátis para até 300 requisições mês

## Carregando a chave de API
library(dotenv)
dotenv::load_dot_env(file = "/home/wagner/gitprojects/R_Avançado_CNJ/Modulo9/.env")
Sys.getenv("TIKTOK_API_KEY")


url <- "https://tiktok-scraper7.p.rapidapi.com/feed/list"

queryString <- list(
    region = "br",
    count = "20"
)

response <- VERB("GET", url, query = queryString, 
                 add_headers('x-rapidapi-key' = Sys.getenv("TIKTOK_API_KEY"), 
                             'x-rapidapi-host' = 'tiktok-scraper7.p.rapidapi.com'), 
                 content_type("application/octet-stream"))

saida <- content(response, "parsed")
saida$code
saida$msg
saida$processed_time
saida$data[[2]]

########----------------------------------------------------------------########
require(tidyverse)

dados <- readr::read_csv("data/Mental Health Dataset.csv")
## Para vermos os dados, podemos utilizar a função head()
head(dados, 2)
glimpse(dados)
x <- c(-2:2)
x

# Operador Pipe %>%
# Opção 1 - Sem identação
sort(cos(unique(x)), decreasing = TRUE)

# Opção 2 - Com identação
sort(
    cos(
        unique(
            x
        )
    ), 
    decreasing = TRUE)


# Opção 3 - Utilizando o operador %>%
require(magrittr)

x %>% 
    unique() %>% 
    cos() %>%
    sort(decreasing = TRUE)

y = x %>% 
    unique() %>% 
    cos() %>%
    sort(decreasing = TRUE)
x
y

x = x %>% 
    unique() %>% 
    cos() %>%
    sort(decreasing = TRUE)
x

## Pipe de atribuição %<>%
x <- 1:10
x 
x %<>% log()
x

# Manipulação de dados com dplyr
## Criar Colunas com Mutate
dados <- dados %>% 
    mutate(mercosul = ifelse(Country %in%
                                 c("Argentina", "Brazil", "Paraguay", "Uruguay"),
                             "Mercosul", "Não Mercosul"))
glimpse(dados)
table(dados$mercosul)
## Selecionar colunas com select()
dados2 <- dados %>% 
    select(Country, Timestamp, Days_Indoors, mercosul)
glimpse(dados2)

dados3 <- dados %>% 
    select(3:5)
glimpse(dados3)

dados4 <- dados %>% 
    select(treatment:Changes_Habits)
glimpse(dados4)

dados6 <- dados %>% 
    select(starts_with("t"))
glimpse(dados6)

dados7 <- dados %>% 
    select(ends_with("s"))

dados %>% 
    select(ends_with("ss")) %>% 
    glimpse()

dados8 <- dados %>% 
    select(contains("ing"))
glimpse(dados8)

dados9 <- dados %>% 
    select(matches("[tT]"))
glimpse(dados9)

dados10 <- dados %>% 
    select(-Country, -Timestamp, -Days_Indoors, -mercosul)
glimpse(dados10)

dados11 <- dados %>% 
    select(-c(Country, Timestamp, Days_Indoors, mercosul))
glimpse(dados11)

dados12 <- dados %>% 
    select_if(is.character)
glimpse(dados12)

dados %>% 
    select_if(is.logical)

dados %>% 
    select_if(is.numeric)

variaveis <- c("Country", "Timestamp", "Days_Indoors", "mercosul")
dados13 <- dados %>% 
    select(all_of(variaveis))
glimpse(dados13)

variaveis <- c("Country", "Timestamp", "Days_Indoors", "mercosul", "Loss_of_Smell")
dados14 <- dados %>% 
    select(any_of(variaveis))
glimpse(dados14)


dados15 <- dados %>% 
    filter(mercosul == "Mercosul")

table(dados15$mercosul)
# Carregando dados comprimidos (.gz)
library(data.table)
car_crash <- fread("data/Brazil Total highway crashes 2010 - 2023.csv.gz")

glimpse(car_crash)

# Filtrando linhas com filter()
car_crash2 <- car_crash %>% 
    filter(tipo_de_ocorrencia == "sem vítima")
glimpse(car_crash2)

## Filtros com múltiplas condições
car_crash3 <- car_crash %>% 
    filter(tipo_de_ocorrencia == "sem vítima" & automovel >= 3)
glimpse(car_crash3)

car_crash4 <- car_crash %>% 
    filter(between(automovel, 3, 5))

tipos <- c("sem vítima", "com vítima")
tipos
car_crash5 <- car_crash %>% 
    filter(tipo_de_ocorrencia %in% tipos)
car_crash5

car_crash6 <- car_crash %>% 
    filter(!tipo_de_ocorrencia %in% c("sem vítima", "com vítima"))
glimpse(car_crash6)

## Usando o operador %ni%
`%ni%` <- Negate(`%in%`)
car_crash7 <- car_crash %>%
    filter(tipo_de_ocorrencia %ni% c("sem vítima", "com vítima"))
glimpse(car_crash7)
## Operador like %like%
car_crash8 <- car_crash %>% 
    filter(tipo_de_ocorrencia %like% "vítima")

glimpse(car_crash8)

car_crash9 =  car_crash %>% 
    filter(grepl("ilesa|fatal", tipo_de_ocorrencia))

glimpse(car_crash9)

## Ordenando linhas com arrange()
car_crash10 = car_crash %>% 
    arrange(desc(automovel))
glimpse(car_crash10)
car_crash11 = car_crash %>% 
    arrange(desc(automovel), mortos) %>%
    select(automovel, mortos) %>% 
    na.exclude()
head(car_crash11)

car_crash_slice1 = car_crash %>% 
    select(1:5) %>%
    slice(3:5)
car_crash_slice1

car_crash_slice2 = car_crash %>% 
    select(1:5) %>%
    slice_head(n = 3)
car_crash_slice2


car_crash_slice3 = car_crash %>% 
    select(1:5) %>%
    slice_tail(n = 3)
car_crash_slice3

## Exercícios
# 1.
storms %>% 
    filter(status == "tropical depression") %>% 
    nrow()
# 2.
storms %>%
    filter(status == "tropical depression" & wind >= 40) %>%
    nrow()
# 3.
storms %>%
    select_if(is.numeric) %>%
    arrange(pressure)

## Slices
car_crash_slice1 = car_crash %>% 
    select(1:5) %>%
    slice(3:5)
car_crash_slice1

car_crash_slice2 = car_crash %>% 
    select(1:5) %>%
    slice_head(n = 3)
car_crash_slice2

car_crash_slice3 = car_crash %>% 
    select(1:5) %>%
    slice_tail(n = 3)
car_crash_slice3

# Rename
car_carsh12 = car_crash %>% 
    rename(numero_automoveis = automovel)
glimpse(car_carsh12)

# Relocate
car_crash_relocate = car_crash %>% 
    relocate(automovel, .before = 1)
glimpse(car_crash_relocate)

car_crash_relocate2 = car_crash %>% 
    relocate(mortos, .after = last_col())
glimpse(car_crash_relocate2)

# Transmute
car_crash_transmute <- car_crash %>% 
    transmute(automovel_10 = automovel / 10)
glimpse(car_crash_transmute)

# NA Replace
car_crash_replace_na <- car_crash %>%
    mutate(mortos = replace_na(mortos, 0))
glimpse(car_crash_replace_na)

# Cut
car_crash_cut <- car_crash %>%
    mutate(automovel = replace_na(automovel, 0)) %>%
    mutate(automovel_cat = cut(automovel,
                               breaks = c(-Inf, 0, 3, Inf),
                               labels = c("sem automóveis",
                                          "entre 1 e 3 automóveis",
                                          "mais do que três")))
glimpse(car_crash_cut)
table( car_crash_cut$automovel, 
       car_crash_cut$automovel_cat)

## Summarise
car_crash13 = car_crash %>% 
    summarise(total_automoveis = sum(automovel, na.rm = TRUE))
car_crash13
sum(car_crash$automovel, na.rm = T)

car_crash14 = car_crash %>% 
    summarise(total_automoveis = sum(automovel, na.rm = TRUE),
              total_mortos = sum(mortos, na.rm = TRUE), 
              n = n(), 
              media_mortos = mean(mortos, na.rm = TRUE))
car_crash14
sum(car_crash$mortos, na.rm = T)
nrow(car_crash)
mean(car_crash$mortos, na.rm = T)

require(lubridate)
## Agrupamento com group_by()
car_crash15 = car_crash %>% 
    mutate(ano = year(dmy(data))) %>%
    group_by(ano)
glimpse(car_crash15)

car_crash16 = car_crash %>% 
    mutate(ano = year(dmy(data))) %>%
    group_by(ano) %>%
    summarise(total_automoveis = sum(automovel, na.rm = TRUE),
              total_mortos = sum(mortos, na.rm = TRUE))
head(car_crash16)

car_crash17 = car_crash %>% 
    filter(tipo_de_ocorrencia == "com vítima") %>%
    summarise(total_automoveis = sum(automovel, na.rm = TRUE),
              total_mortos = sum(mortos, na.rm = TRUE))
car_crash17

## Exercícios 
# 1.
starwars %>%
    summarise(n_especies = n_distinct(species))

# 2.
starwars %>%
    group_by(species) %>%
    summarise(freq_especies = n()) %>%
    arrange(desc(freq_especies))

# 3.
starwars %>% 
    filter(sex %in% c("female", "male")) %>% 
    group_by(sex) %>%
    summarise(media_altura = mean(height, na.rm = TRUE))

# 4.
starwars %>% 
    filter(sex == "male") %>%
    group_by(species) %>%
    summarise(media_peso = mean(mass, na.rm = TRUE))
# 5.
starwars %>% 
    group_by(species) %>%
    filter(mass == max(mass, na.rm = TRUE)) %>%
    select(species, name, mass)


## Trabalhando com datas - lubridate
car_crash %>% 
    mutate(data = dmy(data)) %>%
    select(data) %>%
    glimpse()

car_crash %>% 
    mutate(data = dmy(data)) %>%
    mutate(ano = year(data),
           mes = month(data),
           dia = day(data)) %>%
    select(data, ano, mes, dia) %>%
    glimpse()

car_crash %>% 
    mutate(data = dmy(data)) %>%
    mutate(dias_desde_acidente = difftime(Sys.Date(), data, units = "days")) %>%
    select(data, dias_desde_acidente) %>%
    head()

car_crash %>% 
    mutate(data = dmy(data)) %>%
    mutate(data_mais_10_dias = data + lubridate::days(10)) %>%
    select(data, data_mais_10_dias) %>%
    head()

## Extraindo componentes de data e hora
data <- ymd_hms("2023-08-21 15:30:45")
ano <- year(data)
mes <- month(data)
dia <- day(data)
hora <- hour(data)
minuto <- minute(data)
segundo <- second(data)

print(ano)
print(mes)
print(dia)
print(hora)
print(minuto)
print(segundo)

# Data original no fuso horário de Nova Iorque
data_ny <- ymd_hms("2025-10-21 12:00:00", tz = "America/New_York")

# Converter para o fuso horário de Londres
data_london <- with_tz(data_ny, tz = "Europe/London")

print(data_ny)
print(data_london)

## Exercícios com datas
car_crash %>% 
    mutate(data = dmy(data)) %>%
    mutate(ano = year(data),
           mes = month(data)) %>%
    select(data, ano, mes, mortos) %>%
    filter(mortos > 0) %>% 
    group_by(mes) %>%
    summarise(total_mortos = sum(mortos)) %>% 
    arrange(desc(total_mortos))

car_crash %>% 
    mutate(data = dmy(data)) %>%
    mutate(dia_semana = lubridate::wday(data, label = T, abbr = F)) %>%
    select(dia_semana, mortos) %>%
    filter(mortos > 0) %>% 
    group_by(dia_semana) %>%
    summarise(total_mortos_dia = sum(mortos)) %>% 
    arrange(desc(total_mortos_dia))


# Tidy data

# Carregar pacotes necessários
library(tidyverse)

# Visualizar o dataset table1
table1

# Transformar dados long -> wide usando pivot_wider()
table1 %>% 
    select(-population) %>%
    pivot_wider(names_from = year, 
                values_from = cases)

# Pivotar com mais de uma variável
table1 %>% 
    pivot_wider(names_from = year, 
                values_from = c(cases, population))

# Transformar dados wide -> long usando pivot_longer()
table1 %>% 
    pivot_longer(cols = c(cases, population), 
                 names_to = "variable", 
                 values_to = "total")


# Separando e juntando observações


# Visualizar o dataset table3
table3

# Separar coluna 'rate' em 'cases' e 'population'
table3 %>% 
    separate(rate, into = c("cases", "population"))

# Juntar colunas 'cases' e 'population' em uma única coluna 'rate'
table1 %>% 
    unite(rate, cases, population, sep = "/")


# Exercício: número de voos entre aeroportos


# Utilizando os dados do pacote nycflights13
require(nycflights13)
flights %>% 
    count(origin, dest) %>% 
    pivot_wider(names_from = origin, 
                values_from = n, 
                values_fill = 0)


# Manipulação de Strings em R

library(stringr)
texto <- "Olá, Mundo!"

# Comprimento da string
str_length(texto)

# Converter para minúsculas
str_to_lower(texto)

# Converter para maiúsculas
str_to_upper(texto)

str_to_title(texto)

str_to_sentence(texto)

# Extrair substring
str_sub(texto, 1, 3)

# Substituir parte da string
str_replace(texto, "Mundo", "R")

# Verificar se a string contém um padrão
str_detect(texto, "Mundo")
str_detect(texto, "R!")

# Expressões Regulares (Regex)

# Corresponder qualquer caractere
str_detect("abc", "a.c") # TRUE

# Início da string
str_detect("abc", "^a")   # TRUE

# Fim da string
str_detect("abc", "c$")   # TRUE

# Zero ou mais ocorrências
str_detect("aaab", "a*b") # TRUE

# Uma ou mais ocorrências
str_detect("aaab", "a+b") # TRUE

# Conjunto de caracteres: corresponde a 'a', 'b' ou 'c'
str_detect("abc", "[abc]") # TRUE

# Operador "ou": corresponde a 'cat' ou 'dog'
str_detect("I have a cat", "cat|dog") # TRUE

## Operador Pipe %>%
# Opção 1 - Sem identação
sort(cos(unique(x)), decreasing = TRUE)

# Opção 2 - Com identação
sort(
    cos(
        unique(
            x
        )
    ),
    decreasing = TRUE)

# Opção 3 - Utilizando o operador %>%
require(magrittr)

x %>%
    unique() %>%
    cos() %>%
    sort(decreasing = TRUE)
## Pipe de atribuição %<>%
# O operador %<>% é utilizado para atribuir o resutlado de uma operação ao objeto original
# Isto é, x %<>% f() é equivalente a x <- f(x) 
x <- 1:10
x
x %<>% log()
<<<<<<< HEAD
#<<<<<<< HEAD-----------------------------------------------------------------------------------------------------------------
=======
<<<<<<< HEAD
>>>>>>> 6d8eb1a48946a3126865462fca20a4a1cf1c76be

#####========== Exercício 1---------------------
# --- 1. CONFIGURAÇÃO: Limpar Ambiente e Carregar Pacotes ---
# Limpa o ambiente para garantir que não há dataframes antigos
rm(list = ls())

# Carregar pacotes
library(tidyverse)
library(readr)

# --- 2. FUNÇÃO HAVERSINE ---
# (Implementação robusta para calcular distância em km)
haversine_distance <- function(lon1, lat1, lon2, lat2, r = 6371) {
    # Função interna para converter graus em radianos
    to_radians <- function(degrees) degrees * pi / 180
    
    # Converter todas as coordenadas para radianos
    lon1_rad <- to_radians(lon1)
    lat1_rad <- to_radians(lat1)
    lon2_rad <- to_radians(lon2)
    lat2_rad <- to_radians(lat2)
    
    # Diferenças de longitude e latitude
    dlon <- lon2_rad - lon1_rad
    dlat <- lat2_rad - lat1_rad
    
    # Fórmula de Haversine
    a <- sin(dlat/2)^2 + cos(lat1_rad) * cos(lat2_rad) * sin(dlon/2)^2
    c <- 2 * asin(sqrt(a))
    distance <- r * c
    
    return(distance)
}

# --- 3. CARREGAR DADOS ---
# !!! ATENÇÃO: Ajuste o caminho para o local do seu arquivo "michelin.csv" !!!
caminho_arquivo <- "C:\\Users\\cinqu\\Downloads\\michelin.csv" 
tryCatch({
    dados_michelin <- read_csv(caminho_arquivo, show_col_types = FALSE)
}, error = function(e) {
    stop(paste("Erro ao ler o arquivo:", caminho_arquivo, ". Verifique o caminho."))
})

# --- 4. DEFINIR PONTO DE PARTIDA ---
# (Conforme o enunciado: "Flocons de Sel" em "Megève, France")
restaurante_inicial <- dados_michelin %>%
    filter(
        grepl("Flocons de Sel", Name, ignore.case = TRUE) &
            grepl("Megève, France", Location, ignore.case = TRUE)
    ) %>%
    slice(1) # Garante que temos apenas uma linha

# Verificar se o restaurante foi encontrado
if(nrow(restaurante_inicial) == 0) {
    stop("RESTAURANTE INICIAL 'Flocons de Sel' em 'Megève, France' NÃO ENCONTRADO. Verifique o dataset.")
}

# Armazenar os dados iniciais
lon_inicial <- restaurante_inicial$Longitude
lat_inicial <- restaurante_inicial$Latitude
location_inicial <- restaurante_inicial$Location

# --- 5. PRÉ-PROCESSAMENTO: Estrelas e Distâncias ---

# Processar coluna 'Award' para extrair o número de estrelas
dados_processados <- dados_michelin %>%
    mutate(
        estrelas = case_when(
            grepl("3.*[Ss]tar", Award) ~ 3,
            grepl("2.*[Ss]tar", Award) ~ 2,
            grepl("1.*[Ss]tar", Award) ~ 1,
            TRUE ~ 0
        )
    )

# Calcular a distância de TODOS os restaurantes até o ponto inicial
dados_com_distancia <- dados_processados %>%
    mutate(
        distancia_inicial = haversine_distance(
            lon_inicial, lat_inicial, Longitude, Latitude
        )
    )

# --- 6. RESPONDER ÀS PERGUNTAS ---

# Q1: Distância (km) para visitar o próximo restaurante com 1 Star?
# (Excluímos o próprio restaurante inicial, que tem distância 0)
resposta1_data <- dados_com_distancia %>%
    filter(
        estrelas == 1,
        distancia_inicial > 0 # Exclui o próprio restaurante
    ) %>%
    arrange(distancia_inicial)

resposta1 <- round(resposta1_data$distancia_inicial[1], 2)

# Q2: Restaurantes (1, 2 ou 3 estrelas) a uma distância de 100 km?
# (Parâmetro novo: 100 km)
resposta2_data <- dados_com_distancia %>%
    filter(
        estrelas %in% c(1, 2, 3), 
        distancia_inicial <= 100
    )

resposta2 <- nrow(resposta2_data)

# Q3: Aniversário (>= 1 estrela, até 3 dinheiros, <= 1500 km, fora da cidade inicial)
# (Parâmetros novos: 3 dinheiros, 1500 km)
resposta3_data <- dados_com_distancia %>%
    filter(
        estrelas >= 1,
        distancia_inicial <= 1500,
        Location != location_inicial, # Fora de "Megève, France"
        !is.na(Price), 
        nchar(Price) <= 3  # Interpretação de "até 3 dinheiros" (ex: "€", "€€", "€€€")
    )

resposta3 <- nrow(resposta3_data)

# Q4: Distância mínima (km) para culinária "Modern Cuisine"?
# (Parâmetro novo: "Modern Cuisine")
resposta4_data <- dados_com_distancia %>%
    filter(
        Cuisine == "Modern Cuisine",
        distancia_inicial > 0 # Exclui o próprio restaurante
    ) %>%
    arrange(distancia_inicial)

resposta4 <- round(resposta4_data$distancia_inicial[1], 2)

# --- 7. EXIBIR RESULTADOS FINAIS ---
cat("\n")
cat("==============================================================\n")
cat("RESULTADOS DA JORNADA MICHELIN (Início: Flocons de Sel)\n")
cat("==============================================================\n")
cat("Restaurante inicial:", restaurante_inicial$Name, "\n")
cat("Localização:", restaurante_inicial$Location, "\n\n")

cat("Resposta 1 (Próximo 1 Estrela):", resposta1, "km\n")
cat("Resposta 2 (Restaurantes 1-3 estrelas <= 100km):", resposta2, "restaurantes\n")
cat("Resposta 3 (Opções de Aniversário):", resposta3, "restaurantes\n")
cat("Resposta 4 (Dist. Mín. 'Modern Cuisine'):", resposta4, "km\n")
cat("==============================================================\n")

#####========== Exercício 2---------------------
# --- 1. LIMPEZA E CONFIGURAÇÃO ---
# Limpa o ambiente para evitar erros de dataframes antigos
rm(list = ls())

# Carregar bibliotecas necessárias
library(tidytuesdayR)
library(tidyverse)
library(lubridate)

# --- 2. CARREGAR DADOS ---
# Carrega a lista de dataframes
tuesdata <- tidytuesdayR::tt_load(2021, week = 48)

# Extrai os dataframes base "limpos"
episodes <- tuesdata$episodes
directors <- tuesdata$directors
writers <- tuesdata$writers

# --- 3. RESPONDER ÀS PERGUNTAS (Com os parâmetros NOVOS) ---

# Q1: Dupla de diretor (Joe Ahearne) e escritor (Russell T Davies)
# Etapa A: Encontrar as histórias de cada um
ahearne_stories <- directors %>% 
    filter(director == "Joe Ahearne")

davies_stories <- writers %>% 
    filter(writer == "Russell T Davies")

# Etapa B: Encontrar as 'story_number' em comum
common_stories <- inner_join(ahearne_stories, davies_stories, by = "story_number")

# Etapa C: Juntar com 'episodes' e contar os episódios únicos
resposta_1 <- episodes %>%
    inner_join(common_stories, by = "story_number") %>%
    distinct(episode_number) %>%
    nrow()


# Q2: Episódios do escritor Chris Chibnall em 2018
# Criar um dataframe focado em escritores
data_writers <- episodes %>%
    inner_join(writers, by = "story_number")

resposta_2 <- data_writers %>%
    mutate(year = year(first_aired)) %>%
    filter(
        writer == "Chris Chibnall", # <-- Parâmetro Novo
        year == 2018              # <-- Parâmetro Novo
    ) %>%
    distinct(episode_number) %>%
    nrow()


# Q3: Quantos anos Frank Cottrell-Boyce trabalhou na série?
# (Usando o 'data_writers' que já criamos)
resposta_3 <- data_writers %>%
    filter(writer == "Frank Cottrell-Boyce") %>%
    mutate(year = year(first_aired)) %>%
    filter(!is.na(year)) %>%
    distinct(year) %>% # Contagem de anos únicos
    nrow()


# Q4: Quantos roteiros (episódios) Frank Cottrell-Boyce escreveu?
# (Usando o 'data_writers')
resposta_4 <- data_writers %>%
    filter(writer == "Frank Cottrell-Boyce") %>%
    distinct(episode_number) %>%
    nrow()


# Q5: Duração média dos episódios dirigidos por Joe Ahearne?
# Criar um dataframe focado em diretores
data_directors <- episodes %>%
    inner_join(directors, by = "story_number")

# Etapa A: Isolar os episódios únicos dirigidos por ele
episodios_ahearne <- data_directors %>%
    filter(director == "Joe Ahearne") %>% # <-- Parâmetro Novo
    distinct(episode_number, duration) 

# Etapa B: Calcular a média sobre esses episódios únicos
resposta_5_data <- episodios_ahearne %>%
    filter(!is.na(duration)) %>%
    summarise(mean_duration = mean(duration))

resposta_5 <- resposta_5_data$mean_duration


# --- 4. EXIBIR RESULTADOS FINAIS ---
# Função para formatar com duas casas decimais
format_decimal <- function(x) {
    format(round(x, 2), nsmall = 2)
}

cat("\n")
cat("==================================================\n")
cat("RESULTADOS (DR. WHO - Conjunto 5)\n")
cat("==================================================\n")

cat("Resposta 1 (Dupla Ahearne/Davies):", 
    format_decimal(resposta_1), "episódios\n")

cat("Resposta 2 (Chris Chibnall em 2018):", 
    format_decimal(resposta_2), "episódios\n")

cat("Resposta 3 (Anos Frank Cottrell-Boyce):", 
    format_decimal(resposta_3), "anos\n")

cat("Resposta 4 (Roteiros Frank Cottrell-Boyce):", 
    format_decimal(resposta_4), "roteiros (episódios)\n")

cat("Resposta 5 (Média Joe Ahearne):", 
    format_decimal(resposta_5), "minutos\n")

cat("==================================================\n")
<<<<<<< HEAD

=======
=======
>>>>>>> 14408292171c3f2a6aad9251db9f06f504350f59
>>>>>>> 6d8eb1a48946a3126865462fca20a4a1cf1c76be

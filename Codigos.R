# --- 1. CONFIGURAÇÃO ---
rm(list = ls()) # Limpa o ambiente
library(tidyverse)
library(readr)

# --- 2. FUNÇÃO HAVERSINE ---
haversine_distance <- function(lon1, lat1, lon2, lat2, r = 6371) {
    to_radians <- function(degrees) degrees * pi / 180
    
    lon1_rad <- to_radians(lon1)
    lat1_rad <- to_radians(lat1)
    lon2_rad <- to_radians(lon2)
    lat2_rad <- to_radians(lat2)
    
    dlon <- lon2_rad - lon1_rad
    dlat <- lat2_rad - lat1_rad
    
    a <- sin(dlat/2)^2 + cos(lat1_rad) * cos(lat2_rad) * sin(dlon/2)^2
    c <- 2 * asin(sqrt(a))
    
    return(r * c)
}

# --- 3. CARREGAR DADOS ---
# !!! ATENÇÃO: Verifique se o caminho do arquivo está correto para o seu computador !!!
caminho_arquivo <- "C:\\Users\\cinqu\\Downloads\\michelin.csv" 

tryCatch({
    dados_michelin <- read_csv(caminho_arquivo, show_col_types = FALSE)
}, error = function(e) {
    stop("Erro ao ler o arquivo. Verifique o caminho no script.")
})

# --- 4. DEFINIR PONTO DE PARTIDA ---
# Novo Alvo: "Stucki - Tanja Grandits" em "Basel, Switzerland"
restaurante_inicial <- dados_michelin %>%
    filter(
        grepl("Stucki", Name, ignore.case = TRUE) & grepl("Grandits", Name, ignore.case = TRUE),
        grepl("Basel", Location, ignore.case = TRUE)
    ) %>%
    slice(1)

if(nrow(restaurante_inicial) == 0) stop("Restaurante inicial 'Stucki - Tanja Grandits' não encontrado.")

lon_inicial <- restaurante_inicial$Longitude
lat_inicial <- restaurante_inicial$Latitude
location_inicial <- restaurante_inicial$Location

# --- 5. PRÉ-PROCESSAMENTO ---
dados_processados <- dados_michelin %>%
    mutate(
        # Extrair número das estrelas
        estrelas = case_when(
            grepl("3", Award) ~ 3,
            grepl("2", Award) ~ 2,
            grepl("1", Award) ~ 1,
            TRUE ~ 0
        ),
        # Calcular distâncias
        distancia_inicial = haversine_distance(
            lon_inicial, lat_inicial, Longitude, Latitude
        )
    )

# --- 6. RESOLUÇÃO DAS QUESTÕES ---

# Q1: Próximo restaurante com 1 Star (FORA da cidade atual)
# Filtro: Estrelas == 1, Location != Inicial
res1_df <- dados_processados %>%
    filter(
        estrelas == 1,
        Location != location_inicial # Saiu de Basel
    ) %>%
    arrange(distancia_inicial)

resp1 <- if(nrow(res1_df) > 0) round(res1_df$distancia_inicial[1], 2) else NA

# Q2: Restaurantes 1, 2 ou 3 estrelas a até 1000 KM (Total)
# Filtro: Estrelas 1-3, Distância <= 1000
res2_df <- dados_processados %>%
    filter(
        estrelas %in% c(1, 2, 3),
        distancia_inicial <= 1000
    )
resp2 <- nrow(res2_df)

# Q3: Aniversário (>= 1 estrela, Preço até 2 ($$), <= 3000 km, Fora da cidade)
# Filtro: Estrelas >= 1, Dist <= 3000, Price length <= 2, Location != Inicial
res3_df <- dados_processados %>%
    filter(
        estrelas >= 1,
        distancia_inicial <= 3000,
        Location != location_inicial,
        !is.na(Price),
        nchar(Price) <= 2 # Preço até 2 cifrões
    )
resp3 <- nrow(res3_df)

# Q4: Distância mínima para "Classic Cuisine"
# Filtro: Cuisine contém "Classic", Dist > 0
res4_df <- dados_processados %>%
    filter(
        grepl("Classic Cuisine", Cuisine, ignore.case = TRUE),
        distancia_inicial > 0 
    ) %>%
    arrange(distancia_inicial)

resp4 <- if(nrow(res4_df) > 0) round(res4_df$distancia_inicial[1], 2) else NA


# --- 7. EXIBIR RESULTADOS ---
cat("\n=== RESULTADOS: JORNADA STUCKI (Basel) ===\n")
cat("Restaurante Inicial:", restaurante_inicial$Name, "\n\n")

cat("Resposta 1 (Km próx. 1 Estrela fora da cidade):", resp1, "km\n")
cat("Resposta 2 (Total até 1000km):", resp2, "restaurantes\n")
cat("Resposta 3 (Aniversário - Preço máx 2):", resp3, "restaurantes\n")
cat("Resposta 4 (Km Classic Cuisine):", resp4, "km\n")
#--------------------------------------------------
# --- 1. LIMPEZA E CONFIGURAÇÃO ---
rm(list = ls())
library(tidytuesdayR)
library(tidyverse)
library(lubridate)

# --- 2. CARREGAR DADOS ---
# Carrega os dados da semana 48 de 2021
tuesdata <- tidytuesdayR::tt_load(2021, week = 48)

# Extrai os dataframes
episodes <- tuesdata$episodes
directors <- tuesdata$directors
writers <- tuesdata$writers

# --- PREPARAÇÃO DOS DADOS ---
# Converter story_number para texto para evitar erros no join
episodes$story_number <- as.character(episodes$story_number)
directors$story_number <- as.character(directors$story_number)
writers$story_number <- as.character(writers$story_number)

# Criar dataset de episódios regulares (excluindo especiais sem número de temporada)
# Isso é importante para a contagem correta de roteiros em alguns gabaritos
episodes_regular <- episodes %>%
    filter(!is.na(season_number))

# --- 3. RESPONDER ÀS PERGUNTAS ---

# Q1: Dupla Graeme Harper (Diretor) e Russell T Davies (Escritor)
# Procura episódios onde ambos trabalharam juntos na mesma história
q1_join <- directors %>%
    filter(director == "Graeme Harper") %>%
    inner_join(writers %>% filter(writer == "Russell T Davies"), by = "story_number") %>%
    inner_join(episodes, by = "story_number") %>%
    distinct(episode_number)

resposta_1 <- nrow(q1_join)


# Q2: Russell T Davies em 2008 (Escritor)
resposta_2 <- episodes %>%
    mutate(year = year(first_aired)) %>%
    inner_join(writers, by = "story_number") %>%
    filter(
        writer == "Russell T Davies",
        year == 2008
    ) %>%
    distinct(episode_number) %>%
    nrow()


# --- Dados para Tom MacRae (Q3 e Q4) ---
macrae_data <- episodes_regular %>%
    inner_join(writers, by = "story_number") %>%
    filter(writer == "Tom MacRae")

# Q3: Quantos anos Tom MacRae trabalhou?
# Lógica: Ano Final - Ano Inicial (Intervalo de tempo)
anos_trabalho <- macrae_data %>%
    mutate(year = year(first_aired)) %>%
    summarise(
        inicio = min(year, na.rm = TRUE),
        fim = max(year, na.rm = TRUE)
    )

resposta_3 <- anos_trabalho$fim - anos_trabalho$inicio


# Q4: Quantos roteiros Tom MacRae escreveu?
# Conta histórias únicas em episódios regulares
resposta_4 <- macrae_data %>%
    distinct(story_number, episode_number) %>%
    nrow()


# Q5: Média Ashley Way (Diretor)
way_data <- episodes %>%
    inner_join(directors, by = "story_number") %>%
    filter(director == "Ashley Way") %>%
    distinct(episode_number, duration)

resposta_5 <- mean(way_data$duration, na.rm = TRUE)


# --- 4. EXIBIR RESULTADOS ---
format_decimal <- function(x) format(round(x, 2), nsmall = 2)

cat("\n=== RESULTADOS (Graeme Harper / Russell T Davies / Tom MacRae / Ashley Way) ===\n")
cat("Q1 (Dupla Harper/Davies):", resposta_1, "episódios\n")
cat("Q2 (Davies em 2008):", resposta_2, "episódios\n")
cat("Q3 (Anos Tom MacRae - Intervalo):", resposta_3, "anos\n")
cat("Q4 (Roteiros Tom MacRae):", resposta_4, "roteiros\n")
cat("Q5 (Média Ashley Way):", format_decimal(resposta_5), "minutos\n")
cat("==============================================================================\n")
a
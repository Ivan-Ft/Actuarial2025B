# Paquetes
install.packages(c("quantmod","tidyquant","tidyverse","lubridate"))
library(quantmod)
library(tidyquant)
library(tidyverse)
library(lubridate)
library(ggplot2)

# =============================
# Activos y fechas
# =============================
etfs <- c(
  JonsonyJonson = "JNJ",
  ORO = "GLD",
  Amazon = "AMZN",
  CocaCola = "KO",
  NVIDIA = "NVDA"
)

start_date <- as.Date("2015-01-01")
end_date   <- as.Date("2025-01-01")

# =============================
# Descargar precios ajustados
# =============================
precios <- etfs %>% 
  tq_get(get = "stock.prices", from = start_date, to = end_date) %>% 
  rename(symbol = JonsonyJonson) %>% 
  group_by(symbol) %>% 
  select(date, adjusted) %>% 
  rename(price = adjusted)

# =============================
# Retornos mensuales
# =============================
precios_mensuales <- precios %>% 
  mutate(year = year(date), month = month(date)) %>% 
  group_by(symbol, year, month) %>% 
  summarize(
    price_month_end = last(price),
    price_month_start = first(price),
    .groups = "drop"
  ) %>% 
  mutate(return_month = (price_month_end / price_month_start) - 1) %>% 
  dplyr::filter(!is.na(return_month)) %>% 
  ungroup()

retornos <- precios_mensuales %>% 
  mutate(date = ymd(paste(year, month, "1", sep = "-"))) %>% 
  select(date, symbol, return_month) %>% 
  tidyr::pivot_wider(names_from = symbol, values_from = return_month)

months_vec <- sort(unique(retornos$date))
retornos_mensuales <- as.matrix(retornos %>% select(-date))

# =============================
# Parámetros base (igual que tu proyecto)
# =============================
capital_inicial <- 100000
retiro_mensual <- 1200
decimo_tercero <- 1200
decimo_cuarto <- 470
infla <- 0.02
costo_txn <- 0.005

# Pesos base (exactamente como en tu código original)
pesos_base <- c(0.10, 0.10, 0.10, 0.10, 0.60)
names(pesos_base) <- names(etfs)

# =============================
# FUNCIÓN SIMULAR CARTERA (mejorada, mantiene API que usan los literales)
# =============================
simular_cartera <- function(retornos, capital_inicial, retiro_mensual, infla, costo_txn, 
                            pesos, tipo_rebalanceo = "frecuencia", frecuencia = 12, umbral = 0.05,
                            con_decimales = TRUE, con_contribuciones = FALSE) {
  
  months_vec <- sort(unique(retornos$date))
  n_meses <- length(months_vec)
  
  capital <- numeric(n_meses + 1)
  capital[1] <- capital_inicial
  retiros <- numeric(n_meses + 1)
  retiros[1] <- 0
  
  cuantias <- matrix(NA, nrow = n_meses + 1, ncol = length(pesos))
  colnames(cuantias) <- names(pesos)
  cuantias[1, ] <- capital_inicial * pesos
  
  retiro_adj <- retiro_mensual
  decimo_tercero_adj <- ifelse(con_decimales, decimo_tercero, 0)
  decimo_cuarto_adj <- ifelse(con_decimales, decimo_cuarto, 0)
  
  for (i in seq_along(months_vec)) {
    date_i <- months_vec[i]
    mes_actual <- month(date_i)
    
    # Ajuste por inflación cada enero (igual que tu código)
    if (mes_actual == 1 && i > 1) {
      retiro_adj <- retiro_adj * (1 + infla)
      decimo_tercero_adj <- decimo_tercero_adj * (1 + infla)
      decimo_cuarto_adj <- decimo_cuarto_adj * (1 + infla)
    }
    
    # Calcular retiro total del mes
    retiro_total <- retiro_adj
    if (mes_actual == 8 && con_decimales) retiro_total <- retiro_total + decimo_cuarto_adj
    if (mes_actual == 12 && con_decimales) retiro_total <- retiro_total + decimo_tercero_adj
    
    contribucion <- 0
    if (con_contribuciones && mes_actual == 1 && i > 1) {
      contribucion <- capital_inicial * 0.05
    }
    
    retiros[i + 1] <- retiro_total
    
    # Si no hay suficiente capital para cubrir retiro
    total <- sum(cuantias[i, ])
    if (retiro_total >= total && contribucion == 0) {
      cuantias[i+1, ] <- rep(0, length(pesos))
      capital[i+1] <- 0
      break
    }
    
    # Aplicar costo de transacción al retirar (proporcional al retiro)
    costo_transaccion_mes <- retiro_total * costo_txn
    retiro_efectivo <- retiro_total + costo_transaccion_mes
    
    # Se retira proporcionalmente entre activos
    cuantias[i, ] <- cuantias[i, ] - (retiro_efectivo * (cuantias[i, ] / total))
    
    # Añadir contribución si corresponde
    if (contribucion > 0) {
      cuantias[i, ] <- cuantias[i, ] + (contribucion * pesos)
    }
    
    # Obtener retornos del mes y aplicarlos
    rets_month <- retornos %>% dplyr::filter(date == date_i) %>% select(-date)
    if (nrow(rets_month) != 1) stop("Falta dato de retorno para mes ", date_i)
    rets_vec <- as.numeric(rets_month[1, ])
    
    cuantias[i+1, ] <- cuantias[i, ] * (1 + rets_vec)
    
    # Actualizar capital antes del rebalanceo
    capital[i+1] <- sum(cuantias[i+1, ])
    
    # ----------------------------
    # BLOQUE DE REBALANCEO MEJORADO
    # ----------------------------
    if (tipo_rebalanceo == "frecuencia") {
      if (i %% frecuencia == 0 && i > 1) {
        # 1) Rebalanceo activo: vender 10% de perdedores y comprar ganadores (según rendimiento del mes)
        rend_mensuales <- rets_vec
        media_rend <- mean(rend_mensuales, na.rm = TRUE)
        ganadores <- which(rend_mensuales > media_rend)
        perdedores <- which(rend_mensuales <= media_rend)
        
        ingreso_venta_net <- 0
        if (length(perdedores) > 0) {
          venta <- cuantias[i+1, perdedores] * 0.10
          ingreso_venta_bruto <- sum(venta)
          # aplicamos costo por venta (comisión)
          ingreso_venta_net <- ingreso_venta_bruto * (1 - costo_txn)
          cuantias[i+1, perdedores] <- cuantias[i+1, perdedores] - venta
          ingreso_venta_net <- ingreso_venta_net
        } else {
          ingreso_venta_net <- 0
        }
        
        if (length(ganadores) > 0 && ingreso_venta_net > 0) {
          # distribuir lo recaudado proporcionalmente entre ganadores (según su tamaño actual)
          total_ganadores <- sum(cuantias[i+1, ganadores])
          if (total_ganadores > 0) {
            cuantias[i+1, ganadores] <- cuantias[i+1, ganadores] + 
              ingreso_venta_net * (cuantias[i+1, ganadores] / total_ganadores)
          } else {
            # si no hay monto en ganadores, distribuir equitativamente
            cuantias[i+1, ganadores] <- cuantias[i+1, ganadores] + ingreso_venta_net / length(ganadores)
          }
        }
        
        # 2) Rebalanceo estructural a pesos objetivo (mantener proporciones exactas)
        cuantias_objetivo <- capital[i+1] * pesos
        cambio <- abs(cuantias_objetivo - cuantias[i+1, ])
        costo_reb <- sum(cambio) * costo_txn
        
        # descontar costo total de rebalanceo del capital
        capital[i+1] <- capital[i+1] - costo_reb
        if (capital[i+1] < 0) capital[i+1] <- 0
        
        # ajustar cuantías a los pesos objetivo en proporción al nuevo capital
        if (sum(cuantias_objetivo) > 0) {
          cuantias[i+1, ] <- (capital[i+1] / sum(cuantias_objetivo)) * cuantias_objetivo
        } else {
          cuantias[i+1, ] <- rep(0, length(pesos))
        }
      }
    } else if (tipo_rebalanceo == "umbral") {
      # Rebalanceo por umbral: si la desviación absoluta supera umbral, rebalancear
      pesos_actuales <- cuantias[i+1, ] / capital[i+1]
      desviacion <- abs(pesos_actuales - pesos)
      if (any(desviacion > umbral) && i > 1) {
        # Primero, hacer la parte activa: vender 10% de perdedores del último mes y comprar ganadores
        rend_mensuales <- rets_vec
        media_rend <- mean(rend_mensuales, na.rm = TRUE)
        ganadores <- which(rend_mensuales > media_rend)
        perdedores <- which(rend_mensuales <= media_rend)
        
        ingreso_venta_net <- 0
        if (length(perdedores) > 0) {
          venta <- cuantias[i+1, perdedores] * 0.10
          ingreso_venta_bruto <- sum(venta)
          ingreso_venta_net <- ingreso_venta_bruto * (1 - costo_txn)
          cuantias[i+1, perdedores] <- cuantias[i+1, perdedores] - venta
        } else {
          ingreso_venta_net <- 0
        }
        if (length(ganadores) > 0 && ingreso_venta_net > 0) {
          total_ganadores <- sum(cuantias[i+1, ganadores])
          if (total_ganadores > 0) {
            cuantias[i+1, ganadores] <- cuantias[i+1, ganadores] + 
              ingreso_venta_net * (cuantias[i+1, ganadores] / total_ganadores)
          } else {
            cuantias[i+1, ganadores] <- cuantias[i+1, ganadores] + ingreso_venta_net / length(ganadores)
          }
        }
        
        # Luego rebalanceo estructural
        cuantias_objetivo <- capital[i+1] * pesos
        cambio <- abs(cuantias_objetivo - cuantias[i+1, ])
        costo_reb <- sum(cambio) * costo_txn
        
        capital[i+1] <- capital[i+1] - costo_reb
        if (capital[i+1] < 0) capital[i+1] <- 0
        
        if (sum(cuantias_objetivo) > 0) {
          cuantias[i+1, ] <- (capital[i+1] / sum(cuantias_objetivo)) * cuantias_objetivo
        } else {
          cuantias[i+1, ] <- rep(0, length(pesos))
        }
      }
    }
  } 
  
  return(list(capital = capital, retiros = retiros, cuantias = cuantias, months = months_vec))
}

# =============================
# FUNCIÓN OBTENER DURACIÓN (igual que tu original)
# =============================
obtener_duracion <- function(capital_ini) {
  sim <- simular_cartera(retornos, capital_ini, retiro_mensual, infla, costo_txn, pesos_base, 
                         tipo_rebalanceo = "frecuencia", frecuencia = 12)
  
  mes_deficit <- which(sim$capital <= 0)[1]
  meses_duracion <- ifelse(is.na(mes_deficit), length(sim$months), mes_deficit - 2)
  return(meses_duracion / 12)
}

# =============================
# LITERAL 1: SIMULACIÓN CON SAMPLES - 1000 CAPITALES ALEATORIOS
# =============================
print("LITERAL 1: Cuantía Inicial para Sostenibilidad 10+ Años")
set.seed(42)
capitales_aleatorios <- runif(100, min = 39600, max = 39700)

duraciones <- sapply(capitales_aleatorios, function(cap) {
  obtener_duracion(cap)
})

resultados_simulaciones <- data.frame(
  Capital = capitales_aleatorios,
  Años = duraciones,
  Sostenible = ifelse(duraciones >= 10, "SÍ", "NO")
)

resultados_simulaciones <- resultados_simulaciones[order(resultados_simulaciones$Capital), ]

# Capital mínimo optimizado
capital_minimo <- resultados_simulaciones %>%
  filter(Sostenible == "SÍ") %>%
  slice(1) %>%
  pull(Capital)

print(paste("CAPITAL MÍNIMO OPTIMIZADO (1000 simulaciones):", round(capital_minimo, 2)))
print(resultados_simulaciones)

# =============================
# LITERAL 2: IMPACTO DE INFLACIÓN, COSTOS Y COMISIONES
# =============================
print("\nLITERAL 2: Impacto de Factores Externos")
print("========================================================================")

# 2A: Inflación
print("2A: INFLACIÓN")
tasas_inflacion <- c(0, 0.02, 0.03, 0.05)
resultados_inflacion <- data.frame()

for (tasa_inf in tasas_inflacion) {
  sim <- simular_cartera(retornos, capital_inicial, retiro_mensual, tasa_inf, costo_txn, pesos_base)
  mes_deficit <- which(sim$capital <= 0)[1]
  meses <- ifelse(is.na(mes_deficit), length(sim$months), mes_deficit - 2)
  
  resultados_inflacion <- rbind(resultados_inflacion, data.frame(
    Inflacion = paste(tasa_inf*100, "%"),
    Años = round(meses/12, 2),
    Capital_Final = round(sim$capital[length(sim$capital)], 2)
  ))
}

print(resultados_inflacion)

# 2B: Costos de Transacción
print("\n2B: COSTOS DE TRANSACCIÓN")
costos_tx <- c(0.001, 0.005, 0.01, 0.02)
resultados_costos <- data.frame()

for (costo in costos_tx) {
  sim <- simular_cartera(retornos, capital_inicial, retiro_mensual, infla, costo, pesos_base)
  mes_deficit <- which(sim$capital <= 0)[1]
  meses <- ifelse(is.na(mes_deficit), length(sim$months), mes_deficit - 2)
  
  resultados_costos <- rbind(resultados_costos, data.frame(
    Costo = paste(costo*100, "%"),
    Años = round(meses/12, 2),
    Capital_Final = round(sim$capital[length(sim$capital)], 2)
  ))
}

print(resultados_costos)

# 2C: Comisiones
print("\n2C: COMISIONES")
comisiones <- c(0, 0.01, 0.02, 0.05)
resultados_comisiones <- data.frame()

for (com in comisiones) {
  retiro_com <- retiro_mensual * (1 + com)
  sim <- simular_cartera(retornos, capital_inicial, retiro_com, infla, costo_txn, pesos_base)
  mes_deficit <- which(sim$capital <= 0)[1]
  meses <- ifelse(is.na(mes_deficit), length(sim$months), mes_deficit - 2)
  
  resultados_comisiones <- rbind(resultados_comisiones, data.frame(
    Comision = paste(com*100, "%"),
    Retiro = round(retiro_com, 2),
    Años = round(meses/12, 2),
    Capital_Final = round(sim$capital[length(sim$capital)], 2)
  ))
}

print(resultados_comisiones)

# =============================
# LITERAL 3: CONTRIBUCIONES Y CAMBIOS DE RETIROS
# =============================
print("\nLITERAL 3: Efectos de Contribuciones y Cambios de Retiros")
print("========================================================================")

sim_sin_dec <- simular_cartera(retornos, capital_inicial, retiro_mensual, infla, costo_txn, pesos_base,
                               con_decimales = FALSE, con_contribuciones = FALSE)
mes_sin_dec <- which(sim_sin_dec$capital <= 0)[1]
meses_sin_dec <- ifelse(is.na(mes_sin_dec), length(sim_sin_dec$months), mes_sin_dec - 2)

sim_con_dec <- simular_cartera(retornos, capital_inicial, retiro_mensual, infla, costo_txn, pesos_base,
                               con_decimales = TRUE, con_contribuciones = FALSE)
mes_con_dec <- which(sim_con_dec$capital <= 0)[1]
meses_con_dec <- ifelse(is.na(mes_con_dec), length(sim_con_dec$months), mes_con_dec - 2)

sim_con_contrib <- simular_cartera(retornos, capital_inicial, retiro_mensual, infla, costo_txn, pesos_base,
                                   con_decimales = TRUE, con_contribuciones = TRUE)
mes_contrib <- which(sim_con_contrib$capital <= 0)[1]
meses_contrib <- ifelse(is.na(mes_contrib), length(sim_con_contrib$months), mes_contrib - 2)

resultados_escenarios <- data.frame(
  Escenario = c("Sin décimos", "Con décimos", "Con contribuciones 5%"),
  Años = round(c(meses_sin_dec/12, meses_con_dec/12, meses_contrib/12), 2),
  Capital_Final = round(c(sim_sin_dec$capital[length(sim_sin_dec$capital)],
                          sim_con_dec$capital[length(sim_con_dec$capital)],
                          sim_con_contrib$capital[length(sim_con_contrib$capital)]), 2)
)

print(resultados_escenarios)

# =============================
# LITERAL 4: ESTRATEGIAS DE REBALANCEO
# =============================
print("\nLITERAL 4: Comparación de Estrategias de Rebalanceo")
print("========================================================================")

# 4A: Frecuencia
print("4A: REBALANCEO POR FRECUENCIA")
frecuencias <- c(1,3,6, 12, 24, 36)
resultados_frecuencia <- data.frame()

for (freq in frecuencias) {
  sim <- simular_cartera(retornos, capital_inicial, retiro_mensual, infla, costo_txn, pesos_base,
                         tipo_rebalanceo = "frecuencia", frecuencia = freq)
  mes_deficit <- which(sim$capital <= 0)[1]
  meses <- ifelse(is.na(mes_deficit), length(sim$months), mes_deficit - 2)
  
  resultados_frecuencia <- rbind(resultados_frecuencia, data.frame(
    Frecuencia = paste("Cada", freq, "meses"),
    Años = round(meses/12, 2),
    Capital_Final = round(sim$capital[length(sim$capital)], 2)
  ))
}

print(resultados_frecuencia)

# 4B: Umbral
print("\n4B: REBALANCEO POR UMBRAL")
umbrales <- c(0.03, 0.05, 0.10, 0.15)
resultados_umbral <- data.frame()

for (umbr in umbrales) {
  sim <- simular_cartera(retornos, capital_inicial, retiro_mensual, infla, costo_txn, pesos_base,
                         tipo_rebalanceo = "umbral", umbral = umbr)
  mes_deficit <- which(sim$capital <= 0)[1]
  meses <- ifelse(is.na(mes_deficit), length(sim$months), mes_deficit - 2)
  
  resultados_umbral <- rbind(resultados_umbral, data.frame(
    Umbral = paste(umbr*100, "%"),
    Años = round(meses/12, 2),
    Capital_Final = round(sim$capital[length(sim$capital)], 2)
  ))
}

print(resultados_umbral)

# 4C: Comparación óptima
print("\n4C: COMPARACIÓN FINAL")
sim_freq <- simular_cartera(retornos, capital_inicial, retiro_mensual, infla, costo_txn, pesos_base,
                            tipo_rebalanceo = "frecuencia", frecuencia = 12)
sim_umb <- simular_cartera(retornos, capital_inicial, retiro_mensual, infla, costo_txn, pesos_base,
                           tipo_rebalanceo = "umbral", umbral = 0.05)

mes_freq <- which(sim_freq$capital <= 0)[1]
mes_umb <- which(sim_umb$capital <= 0)[1]
meses_freq <- ifelse(is.na(mes_freq), length(sim_freq$months), mes_freq - 2)
meses_umb <- ifelse(is.na(mes_umb), length(sim_umb$months), mes_umb - 2)

comparacion_final <- data.frame(
  Estrategia = c("Frecuencia (12m)", "Umbral (5%)"),
  Años = round(c(meses_freq/12, meses_umb/12), 2),
  Capital_Final = round(c(sim_freq$capital[length(sim_freq$capital)],
                          sim_umb$capital[length(sim_umb$capital)]), 2)
)

print(comparacion_final)

# =============================
# GRÁFICOS (manteniendo los tuyos)
# =============================

# G1: ETFs
precios_index <- precios %>%
  group_by(symbol) %>%
  mutate(price_indexed = price / first(price) * 100)

g1 <- ggplot(precios_index, aes(x = date, y = price_indexed, color = symbol)) +
  geom_line(size = 0.8) + 
  labs(title = "Evolución Histórica de Precios ETFs", y = "Precio Indexado", x = "Fecha", color = "Activo") +
  theme_minimal() + theme(legend.position = "bottom")

print(g1)

# ======================================================================
# LITERAL 1 (gráficos comparativos alrededor del óptimo) - conserva tu flujo
# ======================================================================

capital_opt <- 39630.52184

capitales_comparacion <- c(
  capital_opt - 5000,
  capital_opt - 2500,
  capital_opt,
  capital_opt + 2500,
  capital_opt + 5000
)

df_literal1_evoluciones <- data.frame()
duraciones_reales <- c()

for (i in seq_along(capitales_comparacion)) {
  cap <- capitales_comparacion[i]
  sim <- simular_cartera(retornos, cap, retiro_mensual, infla, costo_txn, pesos_base,
                         tipo_rebalanceo = "frecuencia", frecuencia = 12)
  
  mes_deficit <- which(sim$capital <= 0)[1]
  meses_duracion <- ifelse(is.na(mes_deficit), length(sim$months), mes_deficit - 2)
  años_duracion <- meses_duracion / 12
  duraciones_reales <- c(duraciones_reales, años_duracion)
  sostenible <- ifelse(años_duracion >= 10, "SÍ", "NO")
  etiqueta <- paste0("$", format(round(cap, 0), big.mark=","), "\n(", round(años_duracion, 2), " años)")
  
  df_temp <- data.frame(
    date = sim$months,
    capital = sim$capital[-1],
    capital_label = etiqueta,
    sostenible = sostenible
  )
  
  df_literal1_evoluciones <- rbind(df_literal1_evoluciones, df_temp)
}

# Filtrar hasta 13 años
df_literal1_evoluciones <- df_literal1_evoluciones %>%
  filter(date <= months_vec[min(156, length(months_vec))])

g2 <- ggplot(df_literal1_evoluciones, aes(x = date, y = capital, 
                                          color = capital_label, 
                                          linetype = sostenible)) +
  geom_line(size = 0.9) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", alpha = 0.3, linewidth = 0.8) +
  
  scale_linetype_manual(
    name = "Sostenibilidad",
    values = c("SÍ" = "solid", "NO" = "dashed")
  ) +
  
  scale_color_viridis_d(
    name = "Capital Inicial\n(Duración)",
    option = "viridis"
  ) +
  
  labs(
    title = "Evolución del Capital",
    x = "Fecha",
    y = "Capital ($)"
  ) +
  
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    plot.subtitle = element_text(size = 10, color = "gray30")
  )

print(g2)

# ======================================================================
# LITERAL 2 - Impacto de Inflación (gráfico)
# ======================================================================
df_inf <- data.frame()

for (tasa in c(0, 0.02, 0.05)) {
  sim <- simular_cartera(retornos, capital_inicial, retiro_mensual, tasa, costo_txn, pesos_base)
  
  df_inf <- rbind(df_inf, data.frame(
    date = sim$months,
    capital = sim$capital[-1],
    escenario = paste("Inflación:", tasa*100, "%")
  ))
}

g3 <- ggplot(df_inf, aes(x = date, y = capital, color = escenario)) +
  geom_line(size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.5) +
  labs(title = "Impacto de Inflación", 
       x = "Fecha", y = "Capital ($)", color = "Escenario") +
  theme_minimal() + theme(legend.position = "bottom")

print(g3)

# ======================================================================
# LITERAL 3 - Escenarios (gráfico)
# ======================================================================
df_esc <- data.frame()

for (i in 1:3) {
  if (i == 1) sim <- sim_sin_dec
  else if (i == 2) sim <- sim_con_dec
  else sim <- sim_con_contrib
  
  df_esc <- rbind(df_esc, data.frame(
    date = sim$months,
    capital = sim$capital[-1],
    escenario = c("Sin décimos", "Con décimos", "Con contribuciones")[i]
  ))
}

g4 <- ggplot(df_esc, aes(x = date, y = capital, color = escenario)) +
  geom_line(size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.5) +
  labs(title = "Efectos de Contribuciones", 
       x = "Fecha", y = "Capital ($)", color = "Escenario") +
  theme_minimal() + theme(legend.position = "bottom")

print(g4)

# ======================================================================
# LITERAL 4 - GRÁFICAS SEPARADAS
# ======================================================================

# Gráfica 1: Rebalanceo por Frecuencia
df_frecuencia <- data.frame()

frecuencias <- c(1, 3, 6, 12, 24)
for (freq in frecuencias) {
  sim <- simular_cartera(retornos, capital_inicial, retiro_mensual, infla, costo_txn, pesos_base,
                         tipo_rebalanceo = "frecuencia", frecuencia = freq)
  
  df_frecuencia <- rbind(df_frecuencia, data.frame(
    date = sim$months,
    capital = sim$capital[-1],
    estrategia = paste("Cada", freq, ifelse(freq == 1, "mes", "meses"))
  ))
}

g_frecuencia <- ggplot(df_frecuencia, aes(x = date, y = capital, color = estrategia)) +
  geom_line(size = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.5) +
  labs(
    title = "Rebalanceo por Frecuencia",
    x = "Fecha",
    y = "Capital ($)",
    color = "Frecuencia de Rebalanceo"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(g_frecuencia)

# Gráfica 2: Rebalanceo por Umbral
df_umbral <- data.frame()

umbrales <- c(0.03, 0.05, 0.10, 0.15)
for (umbr in umbrales) {
  sim <- simular_cartera(retornos, capital_inicial, retiro_mensual, infla, costo_txn, pesos_base,
                         tipo_rebalanceo = "umbral", umbral = umbr)
  
  df_umbral <- rbind(df_umbral, data.frame(
    date = sim$months,
    capital = sim$capital[-1],
    estrategia = paste("Umbral del", umbr*100, "%")
  ))
}

g_umbral <- ggplot(df_umbral, aes(x = date, y = capital, color = estrategia)) +
  geom_line(size = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.5) +
  labs(
    title = "Rebalanceo por Umbral",
    x = "Fecha",
    y = "Capital ($)",
    color = "Umbral de Desviación"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(g_umbral)

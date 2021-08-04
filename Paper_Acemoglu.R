# Paper Acemoglu
library(tidyverse)
library(ggplot2)
library(haven)
library(xtable)
library(AER)
library(systemfit)

table_1 <- read_dta('C:/Users/lordo/Documents/FEA.dev/R/maketable1.dta')
table_2 <- read_dta('C:/Users/lordo/Documents/FEA.dev/R/maketable2.dta')
table_4 <- read_dta('C:/Users/lordo/Documents/FEA.dev/R/maketable4.dta')

# Base sample -> ex-colonies and for which we have settler mortality, 
#                protection against expropriation risk, and GDP data 
table_1_base_sample <- subset(table_1, table_1['baseco'] == 1)
table_2_base_sample <- subset(table_2, table_2['baseco'] == 1)


# Variável independente
gdp_per_cap <- cbind(table_1 $ logpgp95)
gdp_per_cap_base_sample <- cbind(table_1_base_sample $ logpgp95)

# Variável endógena
instituicoes <- cbind(table_1 $ avexpr)
instituicoes_base_sample <- cbind(table_1_base_sample $ avexpr)

# Variável instrumental
mortalidade <- cbind(table_1 $ logem4)
mortalidade_base_sample <- cbind(table_1_base_sample $ logem4)

# Variáveis exógenas 
latitude  <- cbind(table_2 $ lat_abst)
latitude_base_sample <- cbind(table_2_base_sample $ lat_abst)
africa <- cbind(table_2 $ africa)
africa_base_sample <- cbind(table_2_base_sample $ africa)
asia <- cbind(table_2 $ asia)
asia_base_sample <- cbind(table_2_base_sample $ asia)
outros_paises <- cbind(table_2 $ other)
outros_paises_base_sample <- cbind(table_2_base_sample $ other)


################################# TABELA 2 ###################################
ols_1 <- lm(gdp_per_cap ~ instituicoes)
summary(ols_1)
ols_base_sample_1 <- lm(gdp_per_cap_base_sample ~ instituicoes_base_sample)
summary(ols_base_sample_1)

# Regressão 1 gdp_per_cap ~ latitude (colunas 3 e 5 da tabela 2)
ols_1_lat <- lm(gdp_per_cap ~ instituicoes + latitude)
summary(ols_1_lat)
ols_base_sample_1_lat <- lm(gdp_per_cap_base_sample ~ instituicoes_base_sample + latitude_base_sample)
summary(ols_base_sample_1_lat)

# Regressão 1 gdp_per_cap ~ latitude + continentes (colunas 4 e 6)
ols_1_lat_cont <- lm(gdp_per_cap ~ instituicoes + latitude + africa + asia + outros_paises)
summary(ols_1_lat_cont)
ols_1_lat_cont_base_sample <- lm(gdp_per_cap_base_sample ~ instituicoes_base_sample + latitude_base_sample + africa_base_sample + asia_base_sample + outros_paises_base_sample)
summary(ols_1_lat_cont_base_sample)



################################# TABELA 3 ###################################
# Regressão 2
ols_2 <- lm(instituicoes ~ mortalidade)
summary(ols_2)

# Regressão 2 instituições ~ mortalidade  
ols2_lat_base_sample <- lm(instituicoes_base_sample ~ mortalidade_base_sample)
summary(ols_base_sample_2)

# Regressão 2 instituições ~ mortalidade + latitude
ols2_lat_base_sample <- lm(instituicoes_base_sample ~ mortalidade_base_sample + latitude_base_sample)
summary(ols2_lat_base_sample)


# Regressão com o instrumento
ivreg <- ivreg(gdp_per_cap ~ instituicoes | mortalidade)
summary(ivreg)

regressao <- lm(gdp_per_cap ~ instituicoes + mortalidade)
summary(regressao)


# GRÁFICOS

# Figura 1
ggplot(table_1, aes(x = logem4, y = logpgp95)) +
  labs(x = 'Average settler mortality', y = 'GDP per capita in 1995') +
  ggtitle('GDP per cap 1195 x Average settler mortality') +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

# Figura 2
ggplot(table_2, aes(x = avexpr, y = logpgp95)) +
  ggtitle('Expropriation risk x GDP per capita 1995') +
  labs(x = 'Average expropriation risk 1985-1995', y = 'GDP per capita in 1995') +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

# Figura 3  
ggplot(table_1, aes(x = logem4, y = avexpr)) +
  ggtitle('Settler mortality x Expropriation risk') +
  labs(x = 'Average settler mortality', y = 'Average expropriation risk 1985-1995') +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)
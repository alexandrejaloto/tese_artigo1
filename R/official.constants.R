library (data.table)
library(dplyr)
library(tidyr)

rm(list = ls())

source('R/testlets.codes.R')

# get constants ----

official.constants <- list()
pars_01 <- list()

areas <- c('CH', 'CN', 'LC', 'MT')

# import official parameters
items <- read.table ('D:/Microdados/2020/itens_PROVA_2020.csv', header = TRUE, sep = ';')

for (area in areas)
{

  # create mirt model
  par_mirt <- items %>%
    subset(CO_PROVA == codes[[area]][1]) %>%
    arrange(TP_LINGUA, CO_POSICAO) %>%
    select(NU_PARAM_A, NU_PARAM_B, NU_PARAM_C) %>%
    # excluide NA
    drop_na()

  names (par_mirt) <- c('a1', 'd', 'g')

  par_mirt$d <- - par_mirt$a1 * par_mirt$d

  mod <- mirtCAT::generate.mirt_object(par_mirt, '3PL')

  # import data
  data <- fread(
    paste0('data/data_', area, '.csv'),
    nrow = 300000,
    dec = ',',
    data.table = FALSE
  )

  # the items are the columns starting with 'I'
  item.seq <- names(data) %>%
    stringr::str_starts('I')

  # compute theta
  theta <- data.frame(
    mirt::fscores(
      mod,
      response.pattern = data[,item.seq],
      quadpts = 40,
      theta_lim = c(-4,4)
    )
  )

  # constants are the coefficients of the linear regression model
  fit <- lm (
    data[,paste0('NU_NOTA_', area)] ~ theta$F1
  )

  m <- round (fit$coefficients[1], 3)
  s <- round (fit$coefficients[2], 3)

  # theta transformed to (0,1)
  (theta$F1 * s + m - 500)/100

  official.constants[[area]] <- data.frame(
    m = m,
    s = s
  )

  # transform to (0,1)
  pars_01[[area]] <- items %>%
    subset(CO_PROVA == codes[[area]][1]) %>%
    arrange(TP_LINGUA, CO_POSICAO) %>%
    mutate(NU_PARAM_B = (NU_PARAM_B * s + m - 500)/100) %>%
    mutate (NU_PARAM_A = (NU_PARAM_A/s)*100) %>%
    select(NU_PARAM_A, NU_PARAM_B, NU_PARAM_C)
}

save(official.constants, file = 'rdata/official_constants.RData')
save(pars_01, file = 'rdata/pars_01.RData')

# proof that it is necessary to transform ----------------------------------------------------------------

library(Metrics)

items <- read.table ('D:/Microdados/2020/itens_PROVA_2020.csv', header = TRUE, sep = ';')

area <- 'LC'

par_2020 <- items %>%
  subset(CO_PROVA == codes[[area]][1]) %>%
  arrange(TP_LINGUA, CO_POSICAO) %>%
  select(NU_PARAM_A, NU_PARAM_B, NU_PARAM_C) %>%
  drop_na()

par_mirt_2020 <- par_2020

names (par_mirt_2020) <- c('a1', 'd', 'g')

par_mirt_2020$d <- - par_mirt_2020$a1 * par_mirt_2020$d

mod_official <- mirtCAT::generate.mirt_object(par_mirt_2020, '3PL')

# import data
data <- fread(
  paste0('data/data_', area, '.csv'),
  nrow = 300000,
  dec = ',',
  data.table = FALSE
)

# the items are the columns starting with 'I'
item.seq <- names(data) %>%
  stringr::str_starts('I')

theta <- data.frame(
  mirt::fscores(
    mod_official,
    response.pattern = data[,item.seq],
    quadpts = 40,
    theta_lim = c(-4,4)
  )
)


# theta$F1 <- round(theta$F1, 6)

theta_enem_official <- theta$F1 * 100 + 500
data[,paste0('NU_NOTA_', area)][1:10]
round(theta_enem_official[1:10],1)

cor (data[,paste0('NU_NOTA_', area)], theta_enem_official)
rmse (data[,paste0('NU_NOTA_', area)], theta_enem_official)

# import constants
load('rdata/official_constants.RData')

theta_enem_official <- round (theta$F1*official.constants[[area]]$s+official.constants[[area]]$m, 1)
data[,paste0('NU_NOTA_', area)][1:10]
theta_enem_official[1:10]

cor (data[,paste0('NU_NOTA_', area)], theta_enem_official)
rmse (data[,paste0('NU_NOTA_', area)], theta_enem_official)

which (data[,paste0('NU_NOTA_', area)] != theta_enem_official)
data[which (data[,paste0('NU_NOTA_', area)] != theta_enem_official),paste0('NU_NOTA_', area)]
theta_enem_official[which (data[,paste0('NU_NOTA_', area)] != theta_enem_official)]


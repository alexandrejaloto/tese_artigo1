library(dplyr)
library(tidyr)

source('R/testlets.codes.R')
load('rdata/official_constants.RData')

# data preparation ----

# load gen.resp function
source('R/fct_gen.resp.R')

areas <- c('CH', 'CN', 'LC', 'MT')

for (area in areas)
{
  data <- data.table::fread(
    paste0 ('data/data_', area, '.csv'),
    # nrow = 30000,
    dec = ',',
    data.table = FALSE
  )

  # the number of items correspond to the number of columns starting with "I"
  # first, item sequence
  item.seq <- names(data) %>%
    stringr::str_starts('I')
  n.items <- sum(item.seq)

  # import official parameters
  items_area <- read.table ('D:/Microdados/2020/itens_PROVA_2020.csv', header = TRUE, sep = ';') %>%
    subset(CO_PROVA == codes[[area]][1]) %>%
    arrange(TP_LINGUA, CO_POSICAO) %>%
    select(NU_PARAM_A, NU_PARAM_B, NU_PARAM_C) %>%
    # excluide NA
    drop_na()

  items_area

  # data simulation ----

  # set seed
  set.seed (1000)

  # theta in (0,1)
  thetas <- c((data[[paste0('NU_NOTA_', area)]]-official.constants[[area]]$m)/official.constants[[area]]$s)

  resps <- gen.resp(
    theta = thetas,
    bank = items_area
  ) %>%
    data.frame()

  # if LC, answers in the other language are NA
  if(area == 'LC')
    for(i in 1:10)
      resps[,i] <- ifelse(is.na(data[,which(item.seq)[i]]), NA, resps[,i])

  data[,item.seq] <- resps

  # score CTT
  data$ESCORE_TCT <- rowSums(data[,item.seq], na.rm = TRUE)

  # save data ----

  data.table::fwrite(
    data,
    paste0 ('data/data_sim_', area, '.csv'),
    sep = ';',
    dec = ','
  )

}

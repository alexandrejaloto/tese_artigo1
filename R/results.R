library (dplyr)
library (tidyr)

rm(list = ls())

# testlets code
source('R/testlets.codes.R')

# import official parameters
items <- read.table ('D:/Microdados/2020/itens_PROVA_2020.csv', header = TRUE, sep = ';')
# import parameters (0,1)
load ('rdata/pars_01.RData')

areas <- c('CH', 'CN', 'LC', 'MT')
area <- area_ <- 'CH'
samples <- c('rand', 'unif', 'strat')

# functions ----

# function to compute root mean square error between parameters
fct.rmse <- function(pars1, pars2, par)
{
  # select par from estimated pars object
  lapply(
    pars1,
    function(x) x %>% select(par)
    # compute the square difference between real and estimated in each replication
  ) %>% lapply(
    function(x) (x - pars2[[paste0('NU_PARAM_', stringr::str_to_upper(par))]])^2
    # join all differences and compute the mean for each item
  ) %>%
    do.call(
      cbind,
      .) %>%
    rowMeans() %>%
    # compute the root mean square difference for each item
    sqrt
}

# function to compute mean absolute difference between parameters
fct.mad <- function(pars1, pars2, par)
{
  # select par from estimated pars object
  lapply(
    pars1,
    function(x) x %>% select(par)
  ) %>%
    # compute the difference between real and estimated in each replication
    lapply(
      function(x) (x - pars2[[paste0('NU_PARAM_', stringr::str_to_upper(par))]])
    ) %>%
    # compute the absolute difference
    lapply(
      abs
    ) %>%
    # compute the mean difference
    do.call(
      cbind,
      .
    ) %>%
    rowMeans(
    )
}

# function to compute bias between parameters
fct.bias <- function(pars1, pars2, par)
{
  # select par from estimated pars object
  lapply(
    pars1,
    function(x) x %>% select(par)
    # join all differences and compute the mean for each item
  ) %>%
    do.call(
      cbind,
      .) %>%
    rowMeans(
    ) %>%
    # compute the difference between real and estimated
    `-`(pars2[[paste0('NU_PARAM_', stringr::str_to_upper(par))]])
}

# function to compute mean of estimated parameters
fct.mean <- function(pars1, par)
{
  # select par from estimated pars object
  lapply(
    pars1,
    function(x) x %>% select(par)
    # compute the mean for each item
  ) %>%
    do.call(
      cbind,
      .) %>%
    rowMeans(
    )
}

# function to summarise results
data_summary <- function(data, varname, groupnames){
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- plyr::rename(data_sum, c("mean" = varname))
  return(data_sum)
}

# function to compute correlation between parameters
fct.cor <- function(pars1, pars2, par)
{
  # select par from estimated pars object
  lapply(
    pars1,
    function(x) x %>% select(par)
  ) %>%
    do.call(
      cbind,
      .) %>%
    rowMeans() %>%
    # compute the correlation between the estimation mean and the true parameter
    cor(pars2[[paste0('NU_PARAM_', stringr::str_to_upper(par))]])
}

# pars ----

pars <- data.frame()

for (area in areas)
{

  official_par <- items %>%
    subset(CO_PROVA == codes[[area]][1]) %>%
    arrange(TP_LINGUA, CO_POSICAO) %>%
    mutate(area = area) %>%
    select(area, NU_PARAM_A, NU_PARAM_B, NU_PARAM_C)

  names (official_par) <- c('area', 'a', 'b', 'c')

  # parameters (0,1)
  items_area <- pars_01[[area]] %>%
    select('NU_PARAM_A', 'NU_PARAM_B')

  names (items_area) <- c('a01', 'b01')

  n.deciles <- 10
  b.deciles <- quantile(items_area$b01, probs = seq(.1,1,length.out = n.deciles), na.rm = TRUE)
  levels <- cut(x = items_area$b01, breaks = c(-Inf,b.deciles), labels = 1:10)

  pars. <- data.frame(
    official_par,
    items_area,
    decile = b.deciles[levels]
  )

  pars <- rbind(pars, pars.)
}

write.table(
  pars,
  'results/pars.csv',
  dec = ',',
  sep = ';',
  row.names = FALSE,
  na = ''
)

# end

# raw results ----

pars <- c('a', 'b', 'c')
replications <- 100

raw_results <- data.frame()
for(area_ in areas)
{

  # load RData
  load (paste0('rdata/pars_unif_', area_, '.RData'))
  load (paste0('rdata/pars_strat_', area_, '.RData'))
  load (paste0('rdata/pars_rand_', area_, '.RData'))

  # change name from 'g' to 'c'
  for (i in 1:replications)
    names(pars_unif[[i]])[4] <- 'c'

  for (i in 1:replications)
    names(pars_strat[[i]])[4] <- 'c'

  for (i in 1:replications)
    names(pars_rand[[i]])[4] <- 'c'

  # parameters in metric (0,1)
  for (i in 1:replications)
  {
    pars_unif[[i]]$a <- pars_unif[[i]]$a_transf*100
    pars_unif[[i]]$b <- (pars_unif[[i]]$b_transf-500)/100

    pars_strat[[i]]$a <- pars_strat[[i]]$a_transf*100
    pars_strat[[i]]$b <- (pars_strat[[i]]$b_transf-500)/100

    pars_rand[[i]]$a <- pars_rand[[i]]$a_transf*100
    pars_rand[[i]]$b <- (pars_rand[[i]]$b_transf-500)/100
  }

  items_area <- pars_01[[area_]] %>%
    drop_na()

  for(sample in samples)
    for(par in pars)
    {

      rmse.par <- fct.rmse(
        pars1 = get(
          paste0(
            'pars_',
            sample,
            ''
          )
        ),
        pars2 = items_area,
        par = par
      )

      mad.par <- fct.mad(
        pars1 = get(
          paste0(
            'pars_',
            sample,
            ''
          )
        ),
        pars2 = items_area,
        par = par
      )

      bias.par <- fct.bias(
        pars1 = get(
          paste0(
            'pars_',
            sample,
            ''
          )
        ),
        pars2 = items_area,
        par = par
      )

      mean.par <- fct.mean(
        pars1 = get(
          paste0(
            'pars_',
            sample,
            ''
          )
        ),
        par = par
      )

      cor.par <- fct.cor(
        pars1 = get(
          paste0(
            'pars_',
            sample,
            ''
          )
        ),
        pars2 = items_area,
        par = par
      )

      real.par <- items_area[,paste0('NU_PARAM_', stringr::str_to_upper(par))]

      raw_results. <- data.frame(
        area = area_,
        sample = sample,
        parameter = par,
        rmse = rmse.par,
        mad = mad.par,
        bias = bias.par,
        estimation.mean = mean.par,
        real.par = real.par,
        correlation = cor.par
      )

      raw_results <- rbind(raw_results, raw_results.)
    }
}

rownames(raw_results) <- 1:nrow(raw_results)

raw_results$sample <- dplyr::recode(raw_results$sample, rand = 'random', unif = 'uniform', strat = 'stratified')


write.table(
  raw_results,
  'results/raw_results.csv',
  sep = ';',
  dec = ',',
  row.names = FALSE
)

# conditional results ----

raw_results <- read.table(
  'results/raw_results.csv',
  sep = ';',
  dec = ',',
  header = TRUE
)

pars <- c ('a', 'b', 'c')

mad.conditional <- data.frame()

k <- 1
for (k in 1:4)
{
  area_ <- areas[k]

  # official parameters
  items_area <- pars_01[[area_]] %>%
    drop_na()

  # b parameter deciles
  n.deciles <- 10
  b.deciles <- quantile(items_area$NU_PARAM_B, probs = seq(.1,1,length.out = n.deciles))
  levels <- cut(x = items_area$NU_PARAM_B, breaks = c(-Inf,b.deciles), labels = 1:n.deciles)

  for(par_ in pars)
  {
    raw_cond_results <- raw_results %>%
      subset(area == area_) %>%
      subset (parameter == par_)

    raw_cond_results$levels <- levels
    raw_cond_results$par <- par_

    mad.conditional <- rbind(
      mad.conditional,
      group_by(raw_cond_results, sample, levels) %>%
        dplyr::summarise(cond.mad = mean(mad)) %>%
        data.frame() %>%
        mutate(area = area_, parameter = par_, decile = rep(b.deciles, 3))
    )

  }
}
mad.conditional <- select(mad.conditional, area, parameter, sample, everything())

# one file per area
for (area in c('CH', 'CN', 'LC', 'MT'))
  mad.conditional %>%
  subset(area == area) %>%
  data.frame() %>%
  write.table(
    paste0('results/mad_conditional_results_', area, '.csv'),
    sep = ';',
    dec = ',',
    row.names = FALSE
  )



# summary of results ----

# raw

raw_results <- read.table('results/raw_results.csv',
                          sep = ';',
                          dec = ',',
                          header = TRUE
)

## rmse ----
df_rmse <- data_summary(raw_results,
                        varname = 'rmse',
                        groupnames = c('area', 'sample', 'parameter'))

## mad ----

df_mad <- data_summary(raw_results,
                       varname = 'mad',
                       groupnames = c('area', 'sample', 'parameter'))

## bias ----

df_bias <- data_summary(raw_results,
                        varname = 'bias',
                        groupnames = c('area', 'sample', 'parameter'))

## correlation ----

df_cor <- data.frame()
replications <- 100
pars <- c('a', 'b', 'c')

for (area_ in areas)
{

  items_area <- pars_01[[area_]] %>%
    drop_na()

  # load RData
  load (paste0('rdata/pars_unif_', area_, '.RData'))
  load (paste0('rdata/pars_strat_', area_, '.RData'))
  load (paste0('rdata/pars_rand_', area_, '.RData'))

  # change name from 'g' to 'c'
  for (i in 1:replications)
    names(pars_unif[[i]])[4] <- 'c'

  for (i in 1:replications)
    names(pars_strat[[i]])[4] <- 'c'

  for (i in 1:replications)
    names(pars_rand[[i]])[4] <- 'c'

  for (sample in samples)

    for (par_ in pars)

      # par_ <- 'a'
      df_cor <- rbind(
        df_cor,
        data.frame(
          area = area_,
          sample = sample,
          parameter = par_,
          cor = fct.cor(
            pars1 = get(
              paste0(
                'pars_',
                sample
              )
            ),
            pars2 = items_area,
            par = par_
          )
        )
      )
}

df_cor

## one single file ----

df_bias <- arrange(df_bias, area, sample, parameter)
df_mad <- arrange(df_mad, area, sample, parameter)
df_rmse <- arrange(df_rmse, area, sample, parameter)
df_cor <- arrange(df_cor, area, sample, parameter)

df_bias %>%
  select (-sd) %>%
  cbind (df_mad$mad, df_rmse$rmse, df_cor$cor) %>%
  write.table(
    paste0('results/summary_results.csv'),
    sep = ';',
    dec = ',',
    row.names = FALSE
  )


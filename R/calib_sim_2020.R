library (dplyr)
library (mirt)

rm(list = ls())

area <- 'CH'
area <- 'CN'
area <- 'LC'
area <- 'MT'

for (area in c('CH', 'CN', 'LC', 'MT'))
{

  # data preparation ----

  data <- data.table::fread(
    paste0 ('data/data_sim_', area, '.csv'),
    # nrow = 30000,
    dec = ','
  )

  # the number of items correspond to the number of columns starting with "I"
  # first, item sequence
  item.seq <- names(data) %>%
    stringr::str_starts('I')
  n.items <- sum(item.seq)

  model <- mirt.model(
    paste (
      paste0 ('F1 = 1-', n.items),
      paste0 ('PRIOR = (1-', n.items),
      paste0 (', a1, lnorm, 0, 0.5),(1-', n.items, ',g, expbeta, 7, 28'),
      sep = '\n')
  )

  replications <- 100

  n_sample <- 5040

  # sample with uniform distribution ----

  # strata are score ranges
  range <- 5
  strata_score <- seq (1,45, by = range) + range-1
  # number of strata
  n_strata <- length(strata_score)

  # include strata
  data <- data %>%
    mutate(stratum = cut(
      ESCORE_TCT,
      breaks = c(
        -Inf,
        strata_score
      ),
      labels = 1:n_strata
    )
    )%>%
    # arrange by stratum (important for the sample draw)
    arrange(stratum)

  names(data)
  head(data)

  # language: 1 = spanish (or no language for CH, CN and MT), 2 = english
  data$language <- ifelse(is.na(data$I1), '2', '1')

  data.sample <- list()

  # if CH, CN or MT, length(unique(data$language))==1
  for (lang in 1:length(unique(data$language)))
  {
    data.sample[[lang]] <- subset(data, data$language == lang)
  }

  # sample number of each stratum
  n_sample_stratum <- list()

  # number of participants to pick from each stratum
  n_stratum <- ceiling(n_sample/n_strata)
  # if there is more than one language, half for each
  n_stratum <- ceiling(n_stratum/length(unique(data$language)))

  # loop for each existent language
  for (lang in 1:length(unique(data$language)))
  {

    # how many participants are left to complete the amount of stratum in the sample?
    left <- list()

    for (i in 1:n_strata)
    {
      # the maximum between 0 and what is missing (if it is not missing, it will be negative, hence the 0)
      left[[i]] <- max (0, n_stratum - table(data.sample[[lang]]$stratum)[i])
    }

    # sample number of each stratum
    n_sample_stratum[[lang]] <- list()

    # object with the accumulation of missing persons along the strata
    left_acumulation <- list()

    # verify n_sample of first stratum
    n_sample_stratum[[lang]][[1]] <- min (n_stratum, table(data.sample[[lang]]$stratum)[1])
    left_acumulation[[1]] <- left[[1]]

    # verify n_sample of the other strata
    for (i in 2:n_strata)
    {
      # it is the minimum value between (a) the sum of how much is missing and the n_stratum and (b) the number of people from that stratum in the bank.
      n_sample_stratum[[lang]][[i]] <- min (left_acumulation[[i-1]] + n_stratum, table(data.sample[[lang]]$stratum)[i])

      # check how much is missing in this stratum and accumulate with the previous missing ones
      left_acumulation[[i]] <- n_stratum*i - do.call(sum, n_sample_stratum[[lang]])
    }

    # if it's still missing from the last stratum, take it from the previous one
    if (left_acumulation[[i]] > 0)
    {
      # while the sample sum is less than n_sample
      while (do.call(sum, n_sample_stratum[[lang]]) < n_sample/length(unique(data$language)))
      {
        # the n_sample of the previous stratum will increase. It will be the minimum value between
        # (a) how far is n_sample and (b) the number of people from this stratum in the bank
        n_sample_stratum[[lang]][[i-1]] <- min (n_sample/length(unique(data$language)) - do.call(sum, n_sample_stratum[[lang]]) + n_sample_stratum[[lang]][[i-1]], table(data.sample[[lang]]$stratum)[i-1])

        # go to previous stratum
        i <- i - 1
      }
    }

    if  (any (table(data.sample[[lang]]$stratum) < n_stratum))
    {
      warn <- 'Some strata were grouped.'

      cat(warn, file = paste0('data/warning_uniform_', area, '.txt'))
    }

  }

  pars_unif <- list()
  calib_unif <- list()
  constants_unif <- list()
  samples_unif <- list()

  # set seed
  set.seed (1000)

  rep <- 1
  for (rep in 1:replications)
  {

    print(paste0('Uniform - replication ', rep))

    sample <- data.frame()

    for (lang in 1:length(unique(data$language)))
    {
      sample. <- data.frame()

      # draw the sample of each stratum
      for (i in 1:n_strata)
      {
        sample_stratum <- subset(data.sample[[lang]], stratum == i)

        sample. <- rbind(
          sample.,
          sample_stratum[sample(nrow(sample_stratum), n_sample_stratum[[lang]][[i]])]
        )
      }

      sample. <- data.frame(sample.[,-'stratum'])

      sample <- rbind (sample, sample.)

    }

    # calibrate item parameters
    calib_unif[[paste0(area, rep)]] <- mirt (sample[,item.seq], model, itemtype = '3PL', TOL = .01)

    # equate to Enem metrics

    # 1. check sample mean (mirt mean)
    mirt_scores <- data.frame (fscores (calib_unif[[paste0(area, rep)]], qdpts = 40, theta_lim = c(-4, 4)))

    m <- mean (mirt_scores$F1)
    s <- sd (mirt_scores$F1)

    # 2. check original mean (official mean)
    orig_m <- mean (sample[,paste0 ('NU_NOTA_', area)])
    orig_s <- sd (sample[,paste0 ('NU_NOTA_', area)])

    # 3. transform parameters
    pars. <- data.frame(coef (calib_unif[[paste0(area, rep)]], IRTpars=TRUE, simplify = TRUE)$items)
    pars_unif[[paste0(area, rep)]] <- data.frame (ITEM = rownames (pars.), pars.[,-4]) %>%
      mutate (a_transf = (a*s)/orig_s,
              b_transf = ((b - m)/s) * orig_s + orig_m)

    # constants
    constants_unif[[paste0(area, rep)]] <- data.frame (m = m, s = s, orig_m = orig_m, orig_s = orig_s)

    samples_unif[[paste0(area, rep)]] <- sample

  }

  save(calib_unif, file = paste0('rdata/calib_unif_', area, '.RData'))
  save(constants_unif, file = paste0('rdata/constants_unif_', area, '.RData'))
  save(samples_unif, file = paste0('rdata/samples_unif_', area, '.RData'))
  save(pars_unif, file = paste0('rdata/pars_unif_', area, '.RData'))

  # stratified sample ----

  data <- data %>%
    mutate(stratum = cut(
      ESCORE_TCT,
      breaks = c(
        -Inf,
        quantile (ESCORE_TCT, .25),
        quantile (ESCORE_TCT, .95),
        Inf
      ),
      labels = 1:3
    )
    ) %>%
    # arrange by stratum (important for the sample draw)
    arrange(stratum)

  data.sample <- list()

  for (lang in 1:length(unique(data$language)))
  {
    data.sample[[lang]] <- subset(data, data$language == lang)
  }

  n_strata <- 3

  # sample number of each stratum
  n_sample_stratum <- list()

  # loop for each existent language
  for (lang in 1:length(unique(data$language)))
  {
    n_sample_stratum[[lang]] <- c(.25*n_sample, .50*n_sample, .25*n_sample)
    n_sample_stratum[[lang]] <- ceiling(n_sample_stratum[[lang]]/length(unique(data$language)))
  }

  pars_strat <- list()
  calib_strat <- list()
  constants_strat <- list()
  samples_strat <- list()

  # set seed
  set.seed (1000)

  for (rep in 1:replications)
  {

    print(paste0('Stratified - replication ', rep))

    sample <- data.frame()

    for (lang in 1:length(unique(data$language)))
    {
      sample. <- data.frame()

      # draw the sample of each stratum
      for (i in 1:n_strata)
      {
        sample_stratum <- subset(data.sample[[lang]], stratum == i)

        sample. <- rbind(
          sample.,
          sample_stratum[sample(nrow(sample_stratum), n_sample_stratum[[lang]][[i]])]
        )
      }

      sample. <- data.frame(sample.[,-'stratum'])

      sample <- rbind (sample, sample.)

    }

    # calibrate item parameters
    calib_strat[[paste0(area, rep)]] <- mirt (sample[,item.seq], model, itemtype = '3PL', TOL = .01)

    # equate to Enem metrics

    # 1. check sample mean (mirt mean)
    mirt_scores <- data.frame (fscores (calib_strat[[paste0(area, rep)]], qdpts = 40, theta_lim = c(-4, 4)))

    m <- mean (mirt_scores$F1)
    s <- sd (mirt_scores$F1)

    # 2. check original mean (official mean)
    orig_m <- mean (sample[,paste0 ('NU_NOTA_', area)])
    orig_s <- sd (sample[,paste0 ('NU_NOTA_', area)])

    # 3. transform parameters
    pars. <- data.frame(coef (calib_strat[[paste0(area, rep)]], IRTpars=TRUE, simplify = TRUE)$items)
    pars_strat[[paste0(area, rep)]] <- data.frame (ITEM = rownames (pars.), pars.[,-4]) %>%
      mutate (a_transf = (a*s)/orig_s,
              b_transf = ((b - m)/s) * orig_s + orig_m)

    # constants
    constants_strat[[paste0(area, rep)]] <- data.frame (m = m, s = s, orig_m = orig_m, orig_s = orig_s)

    samples_strat[[paste0(area, rep)]] <- sample

  }

  save(calib_strat, file = paste0('rdata/calib_strat_', area, '.RData'))
  save(constants_strat, file = paste0('rdata/constants_strat_', area, '.RData'))
  save(samples_strat, file = paste0('rdata/samples_strat_', area, '.RData'))
  save(pars_strat, file = paste0('rdata/pars_strat_', area, '.RData'))

  # random sample ----

  pars_rand <- list()
  calib_rand <- list()
  constants_rand <- list()
  samples_rand <- list()

  # set seed
  set.seed (1000)

  for (rep in 1:replications)
  {

    print(paste0('Random - replication ', rep))

    sample <- data.frame()

    for (lang in 1:length(unique(data$language)))
    {

      # draw the sample
      sample. <- data.frame(data.sample[[lang]][sample(1:nrow(data.sample[[lang]]), ceiling(n_sample/length(unique(data$language))), replace = FALSE),])

      sample <- rbind (sample, sample.)

    }

    # calibrate item parameters
    calib_rand[[paste0(area, rep)]] <- mirt (sample[,item.seq], model, itemtype = '3PL', TOL = .01)

    # equate to Enem metrics

    # 1. check sample mean (mirt mean)
    mirt_scores <- data.frame (fscores (calib_rand[[paste0(area, rep)]], qdpts = 40, theta_lim = c(-4, 4)))

    m <- mean (mirt_scores$F1)
    s <- sd (mirt_scores$F1)

    # 2. check original mean (official mean)
    orig_m <- mean (sample[,paste0 ('NU_NOTA_', area)])
    orig_s <- sd (sample[,paste0 ('NU_NOTA_', area)])

    # 3. transform parameters
    pars. <- data.frame(coef (calib_rand[[paste0(area, rep)]], IRTpars=TRUE, simplify = TRUE)$items)
    pars_rand[[paste0(area, rep)]] <- data.frame (ITEM = rownames (pars.), pars.[,-4]) %>%
      mutate (a_transf = (a*s)/orig_s,
              b_transf = ((b - m)/s) * orig_s + orig_m)

    # constants
    constants_rand[[paste0(area, rep)]] <- data.frame (m = m, s = s, orig_m = orig_m, orig_s = orig_s)

    samples_rand[[paste0(area, rep)]] <- sample

  }

  save(calib_rand, file = paste0('rdata/calib_rand_', area, '.RData'))
  save(constants_rand, file = paste0('rdata/constants_rand_', area, '.RData'))
  save(samples_rand, file = paste0('rdata/samples_rand_', area, '.RData'))
  save(pars_rand, file = paste0('rdata/pars_rand_', area, '.RData'))
}

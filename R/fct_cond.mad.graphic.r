fct_cond.mad.graphic <- function(area)
{

  titles <- c('Discriminação', 'Dificuldade', 'Pseudochute')
  titles.sample <- c(' (aleatória)', ' (retangular)', ' (deslocada)')

  # area <- 'CH'
  area_ <- area

  # load samples
  load(paste0('rdata/samples_rand_', area, '.RData'))
  samples_rand <- lapply(
    samples_rand,
    dplyr::mutate,
    across(paste0('NU_NOTA_', area), ~(.-500)/100)
  )

  load(paste0('rdata/samples_unif_', area, '.RData'))
  samples_unif <- lapply(
    samples_unif,
    dplyr::mutate,
    across(paste0('NU_NOTA_', area), ~(.-500)/100)
  )

  load(paste0('rdata/samples_strat_', area, '.RData'))
  samples_strat <- lapply(
    samples_strat,
    dplyr::mutate,
    across(paste0('NU_NOTA_', area), ~(.-500)/100)
  )

  samples <- c('rand', 'unif', 'strat')
  samples_ <- c('random', 'uniform', 'stratified')

  plot <- list()

  for (j in 1:3)
  {

    # j <- 1

    sample <- samples[j]
    sample_ <- samples_[j]

    grafico_theta <- data.frame()


    points <- ggplot_build(
      ggplot() +
        geom_density(aes(get(paste0('samples_', sample))[[1]][,paste0('NU_NOTA_', area)])
        )
    )

    grafico_theta <- data.frame(
      sample = sample_,
      x = points$data[[1]]$x,
      y = points$data[[1]]$y
    )

    plot[[sample_]] <- list()

    for (i in 1:3)
    {

      par_ <- pars[i]

      max.mad <- mad.conditional %>%
        subset (area == area_) %>%
        subset (parameter == par_) %>%
        select (cond.mad) %>%
        max()

      grafico_item <- mad.conditional %>%
        subset (area == area_) %>%
        subset (parameter == par_) %>%
        subset (sample == sample_)

      plot[[sample_]][[par_]] <- ggplot_build(
        ggplot(grafico_item, mapping = aes(x = decile, y = cond.mad)) +
          geom_point() +
          geom_line() +
          geom_line(data = grafico_theta, aes(x = x, y = y*((max.mad)/max(y))), linetype = 2, inherit.aes = FALSE) +
          xlim(min(grafico_item$decile), max(grafico_item$decile)) +
          ylim(0, max.mad*1.01) +
          labs(title=paste0(titles[i], titles.sample[j]), x="b", y = "MDA") +
          theme_bw()
      )

    }
  }

  return(plot)
}


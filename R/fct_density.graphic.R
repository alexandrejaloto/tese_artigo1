source('R/fct_calc.info.R')

fct_density.graphic <- function(area, ylim)
{

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

  max.theta <- lapply(samples_strat, select, paste0('NU_NOTA_', area)) %>%
    do.call(rbind, .) %>%
    max()

  thetas <- seq (-3, max.theta, .01)

  # parameters (0,1)
  items_area <- pars_01[[area]] %>%
    drop_na()

  info <- lapply(thetas, function(x) sum(calc.info(items_area, x))) %>%
    do.call(c, .)

  p <- list()

  for(k in 1:3)
  {
    # k <- 1
    sample <- sample.type[k]

    p[[sample]] <- ggplot()
    for(i in 1:100)
      p[[sample]] <-  p[[sample]] +
      geom_density(
        aes_string(
          get(paste0('samples_', sample))[[i]][,paste0('NU_NOTA_', area)]
        )
      ) +
      geom_line(aes(x = thetas, y = info/(max(info)/ylim[2])), linetype = 2) +
      labs(title=paste0('Amostra ',  sample.title[k], ' de ', area), x= "theta", y = "densidade") +
      ylim(ylim) +
      scale_y_continuous(sec.axis = sec_axis(~./(max(info)/ylim[2]), name = 'informação')) +
      theme_classic()

  }

  jpeg (filename = paste0 ('graphics/density_', area, '.jpg'), width = 800,
        height = 800, units = "px", pointsize = 12, quality = 200,
        bg = "white", res = 100, restoreConsole = TRUE)

  multiplot(plotlist=p, cols = 1)

  dev.off()
}



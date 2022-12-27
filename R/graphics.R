library(ggplot2)
library(Rmisc)
library(ggpubr)
library(dplyr)

rm(list = ls())

# import official parameters
load ('rdata/pars_01.RData')

# sample density ----

source('R/fct_density.graphic.R', encoding = 'UTF-8')

sample.type <- c('rand', 'unif', 'strat')
sample.title <- c('aleatória', 'retangular', 'deslocada')

fct_density.graphic (area = 'CH', ylim = c(0, .45))
fct_density.graphic (area = 'CN', ylim = c(0, .5))
fct_density.graphic (area = 'LC', ylim = c(0, .6))
fct_density.graphic (area = 'MT', ylim = c(0, .4))



# scatter plot ----
areas <- c('CH', 'CN', 'LC', 'MT')
# k <- 1
for (k in 1:4)
{
  area <- areas[k]

  # parameters (0,1)
  items_area <- pars_01[[area]] %>%
    drop_na()

  # load RData
  load (paste0('rdata/pars_unif_', area, '.RData'))
  load (paste0('rdata/pars_strat_', area, '.RData'))
  load (paste0('rdata/pars_rand_', area, '.RData'))

  replications <- 100

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

  plot <- list()

  samples <- c('rand', 'unif', 'strat')
  pars <- c('a', 'b', 'c')

  par.mean <- list()
  for(sample in samples)
  {
    par.mean[[sample]] <- list()
    for(par in pars)
      # select par from estimated pars object
      par.mean[[sample]][[par]] <- lapply(
        get(
          paste0(
            'pars_',
            sample,
            ''
          )
        ),
        function(x) x %>% select(par)
      ) %>%
        # join all estimations
        do.call(
          cbind,
          .) %>%
        # mean
        rowMeans()
  }

  a.min <- 0

  a.max <- items_area$NU_PARAM_A
  for(sample in samples)
    a.max <- max(
      a.max,
      par.mean[[sample]] %>%
        `[[`('a')
    ) %>%
    ceiling()

  b.min <- items_area$NU_PARAM_B
  for(sample in samples)
    b.min <- min(
      b.min,
      par.mean[[sample]] %>%
        `[[`('b')
    ) %>%
    floor()

  b.max <- items_area$NU_PARAM_B
  for(sample in samples)
    b.max <- max(
      b.max,
      par.mean[[sample]] %>%
        `[[`('b')
    ) %>%
    ceiling()

  c.min <- 0

  c.max <- items_area$NU_PARAM_C
  for(sample in samples)
    c.max <- max(
      c.max,
      par.mean[[sample]] %>%
        `[[`('c')
    )

  titles.par <- c('Discriminação', 'Dificuldade', 'Pseudochute')
  titles.sample <- c(' (aleatória)', ' (retangular)', ' (deslocada)')

  graphic <- list()
  plot <- list()
  for(i in 1:3)
  {
    sample <- samples[i]
    sample2 <- paste0('"', sample, '"')
    graphic[[sample]] <- list()
    for(j in 1:3)
    {
      par <- pars[j]
      par2 <- paste0('"', par, '"')

      graphic[[sample]][[par]] <- data.frame(
        reais = items_area[,paste0('NU_PARAM_', stringr::str_to_upper(par))],
        estimados = par.mean[[sample]][[par]]
      )

      eval(
        parse(
          text = paste0(
            "plot[[(i-1)*3 + j]] <- ggplot() +
      geom_point(mapping = aes(x = graphic[[",
            sample2,
            "]][[",
            par2,
            "]]$reais, y = graphic[[",
            sample2,
            "]][[",
            par2,
            "]]$estimados)) +
      stat_regline_equation(mapping = aes(x = graphic[[",
            sample2,
            "]][[",
            par2,
            "]]$reais, y = graphic[[",
            sample2,
            "]][[",
            par2,
            "]]$estimados)) +
            ggpubr::stat_cor(label.y.npc = 'bottom', label.x.npc = 'center', mapping = aes(label = ..r.label.., x = graphic[[",
            sample2,
            "]][[",
            par2,
            "]]$reais, y = graphic[[",
            sample2,
            "]][[",
            par2,
            "]]$estimados)) +
      geom_line(aes(x = seq(-15, 15, .01), y = seq(-15, 15, .01)), linetype = 2) +
      xlim(get(paste0(par, '.min')), get(paste0(par, '.max'))) +
      ylim(get(paste0(par, '.min')), get(paste0(par, '.max'))) +
      labs(title=paste0(titles.par[j], titles.sample[i], ' - ', areas[k]), x='reais', y = 'estimados') +
      theme_bw()"
          )
        )
      )
    }
  }

  jpeg (
    filename = paste0 ('graphics/scatter_simulated_', area, '.jpg'),
    width = 3200,
    height = 3200,
    units = "px",
    pointsize = 12,
    quality = 200,
    bg = "white",
    res = 300,
    restoreConsole = TRUE
  )

  multiplot(plotlist=plot, cols = 3, layout = matrix(c(1:9), ncol=3, byrow=FALSE))

  dev.off()
}

# conditional mad graphic ----

source('R/fct_cond.mad.graphic.R', encoding = 'UTF-8')

pars <- c('a', 'b', 'c')

area <- 'MT'

mad.conditional <- read.table(
  paste0('results/mad_conditional_results_', area, '.csv'),
  sep = ';',
  dec = ',',
  header = TRUE
)

plot <- fct_cond.mad.graphic(area)

jpeg (
  filename = paste0 ('graphics/conditional_mad_', area, '.jpg'),
  width = 3200,
  height = 3200,
  units = "px",
  pointsize = 12,
  quality = 200,
  bg = "white",
  res = 300,
  restoreConsole = TRUE
)

cowplot::plot_grid(
  ggplot_gtable(plot[[1]][[1]]),
  ggplot_gtable(plot[[2]][[1]]),
  ggplot_gtable(plot[[3]][[1]]),
  ggplot_gtable(plot[[1]][[2]]),
  ggplot_gtable(plot[[2]][[2]]),
  ggplot_gtable(plot[[3]][[2]]),
  ggplot_gtable(plot[[1]][[3]]),
  ggplot_gtable(plot[[2]][[3]]),
  ggplot_gtable(plot[[3]][[3]])
)

dev.off()

# end



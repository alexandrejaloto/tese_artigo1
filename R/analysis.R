library (simCAT)
library (car)
library (ggpubr)
library (ggplot2)
library (Rmisc)
library (dplyr)
library (tidyr)
library (plyr)
# library (effectsize)
library (rstatix)

rm(list = ls())

# import official parameters
load ('rdata/pars_01.RData')

pars <- c('a', 'b', 'c')
replications <- 100
areas <- c('CH', 'CN', 'LC', 'MT')

# functions ----

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

# description ----
raw_results <- read.table('results/raw_results.csv',
                          sep = ';',
                          dec = ',',
                          header = TRUE
)

df_rmse <- data_summary(
  data=raw_results,
  varname = 'rmse',
  groupnames = c('area', 'data', 'sample', 'parameter')
) %>%
  subset (data == 'simulated')

rmse_min_max <- df_rmse %>%
  group_by(parameter) %>%
  summarise(min(rmse), max(rmse)) %>%
  data.frame()

rmse_min_max

df_rmse[which(df_rmse$rmse == rmse_min_max$min.rmse.[1]),]
df_rmse[which(df_rmse$rmse == rmse_min_max$min.rmse.[2]),]
df_rmse[which(df_rmse$rmse == rmse_min_max$min.rmse.[3]),]

df_rmse[which(df_rmse$rmse == rmse_min_max$max.rmse.[1]),]
df_rmse[which(df_rmse$rmse == rmse_min_max$max.rmse.[2]),]
df_rmse[which(df_rmse$rmse == rmse_min_max$max.rmse.[3]),]

df_cor <- data_summary(
  data=raw_results,
  varname = 'correlation',
  groupnames = c('area', 'data', 'sample', 'parameter')
) %>%
  subset (data == 'simulated')

cor_min_max <- df_cor %>%
  # group_by(parameter) %>%
  group_by(area, parameter) %>%
  summarise(min(correlation), max(correlation)) %>%
  data.frame()

cor_min_max

df_cor[which(df_cor$correlation == cor_min_max$min.correlation.[1]),]
df_cor[which(df_cor$correlation == cor_min_max$min.correlation.[2]),]
df_cor[which(df_cor$correlation == cor_min_max$min.correlation.[3]),]

df_cor[which(df_cor$correlation == cor_min_max$max.correlation.[1]),]
df_cor[which(df_cor$correlation == cor_min_max$max.correlation.[2]),]
df_cor[which(df_cor$correlation == cor_min_max$max.correlation.[3]),]

table <- cbind(
  df_cor %>%
    arrange(area, data, parameter, sample),
  df_rmse %>%
    arrange(area, data, parameter, sample) %>%
    select(rmse)
)
write.table(
  table,
  'results/table_sim.csv',
  dec = ',',
  sep = ';',
  row.names = FALSE
)
table
# graphics ----

raw_results <- read.table('results/raw_results.csv',
                          sep = ';',
                          dec = ',',
                          header = TRUE
)

pars <- c('a', 'b', 'c')
titles <- c('(discriminação)', '(dificuldade)', '(pseudochute)')
areas <- c('CH', 'CN', 'LC', 'MT')

# RMSE
df_rmse <- data_summary(raw_results,
                   varname = 'rmse',
                   groupnames = c('area', 'sample', 'parameter'))

plot <- list()

for (k in 1:4)
  plot[[k]] <- df_rmse %>%
  subset(area == areas[k]) %>%
  ggplot(aes(x = parameter, y = rmse, group = sample, color = sample)) +
  geom_line() +
  geom_pointrange(aes(ymin=rmse-sd, ymax=rmse+sd)) +
  labs(title=paste0('REQM em ', areas[k]), x="parâmetro", y = "REQM") +
  theme_classic()

plot

jpeg (filename = paste0 ('graphics/reqm_simulated.jpg'), width = 800,
      height = 800, units = "px", pointsize = 12, quality = 200,
      bg = "white", res = 100, restoreConsole = TRUE)

multiplot(plotlist=plot, cols = 2)

dev.off()

# conditional rmse graphic ----
conditional_results <- read.table('results/raw_conditional_results.csv',
                                  sep = ';',
                                  dec = ',',
                                  header = TRUE
)

df_rmse <- data_summary(conditional_results,
                   varname = 'rmse',
                   groupnames = c('area', 'data', 'sample', 'parameter', 'decile'))

pars <- c('a', 'b', 'c')
data.type <- c('real', 'simulated')
titles <- c('da discriminação', 'da dificuldade', 'do pseudochute')

plot <- list()

# area
for (k in 1:4)
{
  plot[[k]] <- list()
  # parameters
  for (j in 1:3)
  {
    plot[[k]][[j]] <- list()
    # data type
    for (i in 1:2)
      plot[[k]][[j]][[i]] <- df_rmse %>%
        # subset(area == 'CH') %>%
        subset(area == areas[k]) %>%
        # subset(data == 'simulated') %>%
        subset(data == data.type[i]) %>%
        # subset(parameter == 'a') %>%
        subset(parameter == pars[j]) %>%
        ggplot(aes(x = decile, y = rmse, group = sample, color = sample)) +
        geom_line() +
        geom_pointrange(aes(ymin=rmse-sd, ymax=rmse+sd)) +
        # labs(title="REQM condicional em CH (discriminação)", x="theta", y = "REQM") +
        labs(title=paste0('REQM condicional em ', areas[k], titles[j], ' banco ', data.type[i]), x="theta", y = "REQM") +
        theme_classic()
  }
}

plot

# just for simulated

plot <- list()

# area
for (k in 1:4)
{
  # k <- 1
  plot[[k]] <- list()
  # parameters
  for (j in 1:3)
  {
    # j <- 1
    plot[[k]][[j]] <- df_rmse %>%
      subset(area == areas[k]) %>%
      subset(data == 'simulated') %>%
      subset(parameter == pars[j]) %>%
      ggplot(aes(x = decile, y = rmse, group = sample, linetype = sample, shape = sample, color = sample)) +
      geom_line() +
      geom_point() +
      # geom_pointrange(aes(ymin=rmse-sd, ymax=rmse+sd), fatten = 1) +
      labs(title=paste0('REQM ', titles[j], ' - ', areas[k]), x="theta", y = "REQM") +
      # theme_classic()
      theme_bw()
    }
}

plot

for (i in 1:4)
{
  area <- areas[i]
  jpeg (
    filename = paste0 ('graphics/reqm_conditional_simulated_', area, '.jpg'),
    width = 1600,
    height = 1600,
    # width = 3200,
    # height = 3200,
    units = "px",
    pointsize = 12,
    quality = 200,
    bg = "white",
    res = 300,
    restoreConsole = TRUE
  )

  multiplot(plotlist=plot[[i]], cols = 1)

  dev.off()
}

# mad graphic ----

area <- 'MT'

plot <- fct_mad.graphic(area)

jpeg (
  filename = paste0 ('graphics/mad_item_', area, '.jpg'),
  width = 3200,
  height = 3200,
  units = "px",
  pointsize = 12,
  quality = 200,
  bg = "white",
  res = 300,
  restoreConsole = TRUE
)

# cowplot::plot_grid(ggplot_gtable(plot[[1]]), ggplot_gtable(plot[[2]]), ggplot_gtable(plot[[3]]), ncol = 1)
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

# conditional mad graphic ----

source('R/fct_cond.mad.graphic.R', encoding = 'UTF-8')

area <- 'MT'

mad.conditional <- read.table(
  paste0('results/mad_conditional_results_', area, '.csv'),
  sep = ';',
  dec = ',',
  header = TRUE
)

# titles <- c('da discriminação', 'da dificuldade', 'do pseudochute')

plot <- fct_cond.mad.graphic(area)

jpeg (
  filename = paste0 ('graphics/conditional_mad2_', area, '.jpg'),
  width = 3200,
  height = 3200,
  units = "px",
  pointsize = 12,
  quality = 200,
  bg = "white",
  res = 300,
  restoreConsole = TRUE
)

# cowplot::plot_grid(ggplot_gtable(plot[[1]]), ggplot_gtable(plot[[2]]), ggplot_gtable(plot[[3]]), ncol = 1)

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

# scatter plot ----
k <- 1
for (k in 1:4)
{
  area <- areas[k]

  # official parameters
  items_area <- pars_01[[area]] %>%
    drop_na()

  # load RData
  load (paste0('rdata/pars_unif_sim_2020_', area, '.RData'))
  load (paste0('rdata/pars_strat_sim_2020_', area, '.RData'))
  load (paste0('rdata/pars_rand_sim_2020_', area, '.RData'))

  replications <- 100

  # change name from 'g' to 'c'
  for (i in 1:replications)
    names(pars_unif_sim_2020[[i]])[4] <- 'c'

  for (i in 1:replications)
    names(pars_strat_sim_2020[[i]])[4] <- 'c'

  for (i in 1:replications)
    names(pars_rand_sim_2020[[i]])[4] <- 'c'

  # parameters in metric (0,1)
  for (i in 1:replications)
  {
    pars_unif_sim_2020[[i]]$a <- pars_unif_sim_2020[[i]]$a_transf*100
    pars_unif_sim_2020[[i]]$b <- (pars_unif_sim_2020[[i]]$b_transf-500)/100

    pars_strat_sim_2020[[i]]$a <- pars_strat_sim_2020[[i]]$a_transf*100
    pars_strat_sim_2020[[i]]$b <- (pars_strat_sim_2020[[i]]$b_transf-500)/100

    pars_rand_sim_2020[[i]]$a <- pars_rand_sim_2020[[i]]$a_transf*100
    pars_rand_sim_2020[[i]]$b <- (pars_rand_sim_2020[[i]]$b_transf-500)/100
  }

  plot <- list()

  samples <- c('rand', 'unif', 'strat')
  pars <- c('a', 'b', 'c')

  par.mean <- list()
  for(sample in samples)
  {
    # sample <- 'rand'
    par.mean[[sample]] <- list()
    for(par in pars)
      # par <- 'a'
      # select par from estimated pars object
      par.mean[[sample]][[par]] <- lapply(
        get(
          paste0(
            'pars_',
            sample,
            '_sim_2020'
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
  titles.sample <- c(' (aleatória)', ' (uniforme)', ' (deslocada)')

  graphic <- list()
  plot <- list()
  for(i in 1:3)
  {
    sample <- samples[i]
    sample2 <- paste0('"', sample, '"')
    # plot[[sample]] <- list()
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

# anova ----

raw_results <- read.table('results/raw_results.csv',
                          # 'results/summary_rmse_results_CH.csv',
                          sep = ';',
                          dec = ',',
                          header = TRUE
)
raw_results

raw_results$sample <- factor(raw_results$sample, levels = c('stratified', 'uniform', 'random'))

names(raw_results)

## anova assumptions ----
assumption <- list()
for (area_ in areas)
{
  assumption[[area_]] <- list()
  for (par_ in pars)
  {
    # homoscedasticity
    levene <- raw_results %>%
      filter (area == area_) %>%
      filter (parameter == par_) %>%
      leveneTest(rmse ~ sample, data = .)
    # normality
    model <- raw_results %>%
      filter (area == area_) %>%
      filter (parameter == par_) %>%
      lm(rmse ~ sample, data = .)

    shapiro <- shapiro_test(residuals(model))
    assumption[[area_]][[par_]] <- (levene$`Pr(>F)`[1] >= .05 & shapiro$p.value >= .05)
  }
}

assumption

anova_results <- data.frame()

for(area_ in areas)
  for (par_ in pars)
  {
    # area_ <- 'CN'
    # par_ <- 'b'

    correction <- ifelse(assumption[[area_]][[par_]], 'none', 'bonferroni')

    anova <- raw_results %>%
      filter (area == area_) %>%
      subset (parameter == par_) %>%
      oneway.test(mad ~ sample, data = ., var.equal = assumption[[area_]][[par_]])

    # raw_results %>%
    #   filter (area == area_) %>%
    #   subset (parameter == par_) %>%
    #   tukey_hsd(mad ~ sample)

    post_hoc <- raw_results %>%
      filter (area == area_) %>%
      filter (parameter == par_) %>%
      pairwise_t_test(formula = mad ~ sample, data = .,
                      comparisons = list(
                        c('uniform', 'random'), c('stratified', 'uniform'), c('stratified', 'random')),
                      # p.adjust.method = correction) %>%
                      p.adjust.method = 'bonferroni') %>%
      data.frame()

    rownames(post_hoc) <- paste0(post_hoc$group1, '-', post_hoc$group2)

    m <- raw_results %>%
      filter (area == area_) %>%
      filter (parameter == par_) %>%
      group_by(sample) %>%
      dplyr::summarise(mean(mad)) %>%
      data.frame()

    rownames(m) <- m$sample

    sd <- raw_results %>%
      filter (area == area_) %>%
      filter (parameter == par_) %>%
      group_by(sample) %>%
      dplyr::summarise(sd(mad)) %>%
      data.frame()

    rownames(sd) <- sd$sample

    set.seed(1000)
    post.effect <- raw_results %>%
      filter (area == area_) %>%
      subset (parameter == par_) %>%
      rstatix::cohens_d(formula = mad ~ sample, data = .,
                        ci = FALSE) %>%
      # ci= TRUE, ci.type = "bca", nboot = 1000) %>%
      data.frame()

    rownames(post.effect) <- paste0(post.effect$group1, '-', post.effect$group2)

    anova_results. <- data.frame(
      area = area_,
      par = par_,
      f.anova = anova$statistic,
      p.anova = anova$p.value,
      p.unif.strat = post_hoc['stratified-uniform',8],
      p.rand.strat = post_hoc['stratified-random',8],
      p.rand.unif = post_hoc['uniform-random',8],
      d.strat.unif = post.effect['stratified-uniform',4],
      d.strat.unif.low = post.effect['stratified-uniform',7],
      d.strat.unif.high = post.effect['stratified-uniform',8],
      d.strat.rand = post.effect['stratified-random',4],
      d.strat.rand.low = post.effect['stratified-random',7],
      d.strat.rand.high = post.effect['stratified-random',8],
      d.unif.rand = post.effect['uniform-random',4],
      d.unif.rand.low = post.effect['uniform-random',7],
      d.unif.rand.high = post.effect['uniform-random',8],
      m.unif = m['uniform', 2],
      sd.unif = sd['uniform', 2],
      m.strat = m['stratified', 2],
      sd.strat = sd['stratified', 2],
      m.rand = m['random', 2],
      sd.rand = sd['random', 2]
    )

    anova_results <- rbind(anova_results, anova_results.)
  }

anova_results

write.table(
  anova_results,
  'results/anova.csv',
  dec = ',',
  sep = ';',
  row.names = FALSE
)


# end

# sample density ----

# p <- list()
sample.type <- c('rand', 'unif', 'strat')
sample.title <- c('aleatória', 'retangular', 'deslocada')

fct_density.graphic (area = 'CH', ylim = c(0, .45))
fct_density.graphic (area = 'CN', ylim = c(0, .5))
fct_density.graphic (area = 'LC', ylim = c(0, .6))
fct_density.graphic (area = 'MT', ylim = c(0, .4))


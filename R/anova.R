library (dplyr)
library (car)
library (rstatix)

areas <- c ('CH', 'CN', 'LC', 'MT')
pars <- c('a', 'b', 'c')

raw_results <- read.table('results/raw_results.csv',
                          # 'results/summary_rmse_results_CH.csv',
                          sep = ';',
                          dec = ',',
                          header = TRUE
)
raw_results

raw_results$sample <- factor(raw_results$sample, levels = c('stratified', 'uniform', 'random'))

names(raw_results)

# anova assumptions ----

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

# analysis ----

anova_results <- data.frame()

for(area_ in areas)
  for (par_ in pars)
  {

    correction <- ifelse(assumption[[area_]][[par_]], 'none', 'bonferroni')

    anova <- raw_results %>%
      filter (area == area_) %>%
      subset (parameter == par_) %>%
      oneway.test(mad ~ sample, data = ., var.equal = assumption[[area_]][[par_]])

    post_hoc <- raw_results %>%
      filter (area == area_) %>%
      filter (parameter == par_) %>%
      pairwise_t_test(formula = mad ~ sample, data = .,
                      comparisons = list(
                        c('uniform', 'random'), c('stratified', 'uniform'), c('stratified', 'random')),
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
                        # ci = FALSE) %>%
      ci= TRUE, ci.type = "bca", nboot = 1000) %>%
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

# save ----

write.table(
  anova_results,
  'results/anova.csv',
  dec = ',',
  sep = ';',
  row.names = FALSE
)


# end


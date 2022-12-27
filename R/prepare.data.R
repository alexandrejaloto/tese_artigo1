# library (data.table)
library (dplyr)
# library (INEPsico)
library (mirt)
# library (anchors)
# library (mirtCAT)
# library (sampling)

source('R/fct_prepare.data.R')
source('R/testlets.codes.R')

data <- data.table::fread (
  'D:/Microdados/2020/MICRODADOS_ENEM_2020.csv',
  # nrow = 30000,
  select = c(
    paste0(
      c(
        'TP_PRESENCA_',
        'TX_RESPOSTAS_',
        'NU_NOTA_',
        'CO_PROVA_',
        'TX_GABARITO_'
      ),
      rep (c('CH', 'CN', 'LC', 'MT'), each = 5)),
    'NU_INSCRICAO',
    'TP_LINGUA')
)

# area <- 'LC'

areas <- c('CH', 'CN', 'LC', 'MT')

for (area in areas)
{
  print(area)

  prepare.data(
    data = data,
    codes = codes,
    area = area
  )
}

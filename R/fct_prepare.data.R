prepare.data <- function(data, codes, area)
{
  bank <- data %>%
    # select variables of interest
    dplyr::select (paste0 (c('TP_PRESENCA_', 'TX_RESPOSTAS_', 'NU_NOTA_', 'CO_PROVA_', 'TX_GABARITO_'), area), NU_INSCRICAO, TP_LINGUA) %>%
    # filter participants that were present and had at least one answer
    filter (get (paste0 ('TP_PRESENCA_', area)) == 1 & get (paste0 ('TX_RESPOSTAS_', area)) != '.............................................') %>%
    # filter participants that had at least one answer in LC test
    filter (get (paste0 ('TX_RESPOSTAS_', area)) != '99999.............................................' & get (paste0 ('TX_RESPOSTAS_', area)) != '.....99999........................................') %>%
    # select variables of interest
    dplyr::select (NU_INSCRICAO, paste0 ( c('CO_PROVA_', 'TX_RESPOSTAS_', 'TX_GABARITO_', 'NU_NOTA_'), area), TP_LINGUA) %>%
    # filter participants who answered P1 (first application) without adpatations
    filter ( get (paste0 ('CO_PROVA_', area)) %in% codes[[area]])

  # score must be numeric
  bank[,paste0('NU_NOTA_', area)] <- as.numeric(bank[,get(paste0('NU_NOTA_', area))])

  bank <- stringr::str_split (bank[, get (paste0('TX_RESPOSTAS_',area))], '', simplify = TRUE) %>%
    data.frame (bank, .) %>%
    anchors::replace.value (paste0('X', 1:10), 9, as.double(NA))

  # import item dictionary
  items <- read.table (
    'D:/Microdados/2020/ITENS_PROVA_2020.csv',
    sep = ";",
    header = T
  )

  # arrange items by test code and position
  items <- arrange(items, CO_PROVA, TP_LINGUA, CO_POSICAO)

  bank.new <- data.frame()

  # correct answers according to testlet
  for (k in 1:length(codes[[area]]))
  {
    # k <- 1
    testlet <- codes[[area]] [k]
    resp.testlet <- subset (bank, get (paste0 ('CO_PROVA_', area)) == testlet)

    # get the key of each testlet (variable TX_GABARITO)
    key <- resp.testlet[1, paste0 ('TX_GABARITO_', area)] %>%
      # the key of a canceled item must be 'X'
      stringr::str_replace('\\*', 'X') %>%
      strsplit (NULL) %>%
      data.frame()

    # item code
    items.cod <- subset (items, CO_PROVA == testlet)$CO_ITEM

    if (area == 'LC')
    {
      correction <- key2binary (resp.testlet [,paste ("X", 1:50, sep = "")], key)
    } else {
      correction <- key2binary (resp.testlet [,paste ("X", 1:45, sep = "")], key)
    }

    bank.new. <- apply (correction, 1, sum, na.rm = TRUE) %>%
      cbind (correction, .) %>%
      data.frame (resp.testlet[, c('NU_INSCRICAO', paste0( c('CO_PROVA_', 'TX_RESPOSTAS_'), area))],
                  ., resp.testlet[ , paste0 ('NU_NOTA_',area)])


    # check if items have code. if they don't, change to I1:I45
    if (length (items.cod) > 0)
    {
      names (bank.new.) <- c ('NU_INSCRICAO', paste0 ( c('CO_PROVA_', 'TX_RESPOSTAS_'), area),
                              items.cod, 'ESCORE_TCT', paste0 ('NU_NOTA_', area))

      # check if there is any canceled item. when calibrating, this item must be excluded
      # k == 1 is the first round and the order of the items is correct
      if (k == 1)
      {
        # code of canceled item
        cod.canceled <- subset (items, CO_PROVA == testlet)$CO_ITEM [which (key == 'X')]
        # column (position) of canceled item
        col.canceled <- which (key == 'X')
      }
    } else {
      # check if there is any canceled item. when calibrating, this item must be excluded
      # column (position) of canceled item
      col.canceled <- which (key == 'X')
      cod.canceled <- which (key == 'X')
      names (bank.new.) <- c ('NU_INSCRICAO', paste0 ( c('CO_PROVA_', 'TX_RESPOSTAS_'), area),
                              paste0('I', 1:nrow(key)), 'ESCORE_TCT', paste0 ('NU_NOTA_', area))
    }

    # combine bank.new. with bank.new
    bank.new <- data.table::rbindlist(list(bank.new, bank.new.), use.names = TRUE)

    # stop loop if items have no code
    if (length (items.cod) <= 0) break
  }
  names (bank.new) <- c ('NU_INSCRICAO', paste0 ( c('CO_PROVA_', 'TX_RESPOSTAS_'), area),
                         paste0('I', 1:nrow(key)), 'ESCORE_TCT', paste0 ('NU_NOTA_', area))

  if (length (col.canceled) > 0)
    bank.new <- dplyr::select(bank.new, -paste0('I', col.canceled))

  rm (list = c('bank.new.', 'resp.testlet'))
  rm ('bank')
  gc()

  # save data
  data.table::fwrite (bank.new, paste0 ('data/data_', area, '.csv'), sep = ';', dec = ',', row.names = FALSE, col.names = TRUE)
}


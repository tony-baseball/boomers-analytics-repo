library(rvest)
library(dplyr)
library(glue)
library(slackr)
library(stringr)

# https://baseball.pointstreak.com/transactions.html?leagueid=193&seasonid=33873
leagueid <- 193
seasonid <- 33873

url <- glue('https://baseball.pointstreak.com/transactions.html?leagueid={leagueid}&seasonid={seasonid}')

webpage <- read_html(url)

transactions_table <- webpage %>% html_table(fill = T) %>% .[[1]] %>% as.data.frame()

transactions_tb <- transactions_table %>%
  dplyr::rename(Transaction = 3) %>%
  mutate(
    Date = as.Date(Date, "%m/%d/%y"),
    Transaction = ifelse(
      grepl("Jr.,", Transaction),
      sub("^(\\w+ \\w+\\.?),\\s*(\\w+)", "\\2 \\1", Transaction),
      sub("(\\w+),\\s*(\\w+)", "\\2 \\1", Transaction)
    )
  )



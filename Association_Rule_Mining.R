library(readxl)
library(tidyverse)
library(data.table)
library(arules)
library(arulesViz)


#read in dataset
getwd()
setwd("C:/Users/faco9/Downloads")
retail<-read_xlsx("Online Retail.xlsx")
retail

#data manipulation to reduce characters to lower case and remove white-spaces
colnames(retail) <- tolower(colnames(retail))
retail

df <- retail %>% 
  select(invoiceno, description)%>%
  mutate(description = str_trim(description, side = "both"))%>%
  mutate(invoiceno = factor(invoiceno), description = str_replace_all(description, "[']", replacement = ""))%>%
  mutate(description = tolower(str_replace_all(description, pattern = "[ ]", replacement = "_")))

head(df)

#Extracting transaction details from dataset
prep_data<- function(x) {
  y <- data.frame()
  for (i in 1:n_distinct(df$invoiceno)) 
    {
    x <- df %>% 
      filter(invoiceno == levels(invoiceno)[i]) %>% 
      t() %>% 
      as.data.frame() %>% 
      slice(2) %>% 
      mutate(invoiceno = levels(df$invoiceno)[i]) %>% 
      select(invoiceno, everything())
    colnames(x) <- c("invoiceno", paste0("item_", 1:(ncol(x) - 1)))
    print(i)
    y <- list(y, x) %>% 
      rbindlist(fill = T)
  }
  return(y)
}

df_prep <- prep_data()
df_prep

#converting data into market basket format
transaction <- as(df_prep, Class = "transactions")
LIST(head(transaction, 6))

#save transaction data
write.csv(df_prep %>% select(-invoiceno), "transaction retail.csv", row.names = F)
#write_csv(df_prep %>% select(-invoiceno), "c:/Users/faco9/Downloads/transaction retail.csv")

trans_retail <- read.transactions("transaction retail.csv", sep = ",",  header = T)
LIST(head(trans_retail, 6))

rules <- apriori(trans_retail, parameter = list(supp = 0.01, conf = 0.7))
rules

rules_conf <- sort(rules, by = "confidence", decreasing = TRUE)
inspect(head(rules_conf))

rules2 <- apriori(trans_retail, parameter = list(supp = 0.02, conf = 0.7))
rules2

rules2_conf <- sort(rules, by = "confidence", decreasing = TRUE)
inspect(head(rules2_conf))

rules_lift <- sort(rules, by = "lift", decreasing = TRUE)
inspect(head(rules_lift))

subrules <- head(rules, n = 25, by = "lift")
inspect(head(subrules))
inspect(tail(subrules))

plot(subrules, method = "graph", measure = "lift", shading = "confidence")
plot(subrules, method = "graph", measure = "lift", shading = "confidence", engine = "htmlwidget")

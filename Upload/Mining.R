library(rvest)
library(dplyr)

datareview = data.frame()

for (page_result in seq(1:18)) {
  link = paste0("https://www.walmart.com/reviews/product/389129803?page=",page_result)
  page = read_html(link)
  review = page %>% html_nodes("p") %>% html_text()
  review <- gsub("\\n","",review)
  review <- gsub("\\s+"," ",review)
  review <- review[1:20]
  datareview = rbind(datareview,data.frame(review))
  write.csv(datareview,file = "Datareview.csv")
  }


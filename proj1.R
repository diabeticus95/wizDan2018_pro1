library(rjson)
library(tidyverse)
library(tidytext)

#nazwy wszystkich folderów
dirs <- read.csv(file = "D:\\mgr\\wizualizacja_danych\\proj1\\messages\\inbox\\dirs.txt", sep = "/", header = FALSE)
dirs <- dirs$V1 
dirs <- dirs[dirs != 'dirs.txt']

rozmowy <- sapply(dirs, function(i) 
  fromJSON(file = paste0("D:\\mgr\\wizualizacja_danych\\proj1\\messages\\inbox\\", i, "\\", "message.json")))

#z kazdego interesuje mnie messages/content
messg <- lapply(rozmowy, function(i)
            lapply(i$messages, function(j) j$content))
times <- lapply(rozmowy, function(i)
            lapply(i$messages, function(j) j$timestamp))
sender <- lapply(rozmowy, function(i)
          lapply(i$messages, function(j) j$sender_name))
#Encoding trzeba naprawić już teraz, potem nie zadziała

messg <- lapply(1:length(messg), function(i) {
  lapply(1:length(messg[[i]]), function(j)   {
    if (!is.null(messg[[i]][[j]])){
      messg[[i]][[j]] <- iconv(messg[[i]][[j]], to = "latin1", from = "UTF-8")
      Encoding(messg[[i]][[j]]) <- "UTF-8"
      messg[[i]][[j]]
    }
    else{
      'null'
    }
  }
  )
}
)

#1. najczesciej uzywane slowa
stopw <- fromJSON(file = "D:\\mgr\\dyplom\\R_analysis\\stopwords.txt")
unl.times <- unlist(times, recursive = T) 
unl.sender <- unlist(sender, recursive = F) #sendery też są nullami niektóre
unl.mssg <- unlist(messg, recursive = T)

unl.mssg <- unl.mssg[unl.sender %in% 'Patryk Kowalski']
unl.times <- unl.times[unl.sender %in% 'Patryk Kowalski']
unl.times <- as.Date(as.POSIXct(unl.times/1000, origin="1970-01-01"))


df <- tibble(text = unl.mssg, times = unl.times)
df.words <- df %>% 
  unnest_tokens(word, text) %>%
  filter(!word %in% stopw)

top <- df.words %>%
  count(word, sort = TRUE) 

head(top,20)

#2. zbinować to i nakreślić w funkcji timestamp
#nie chce mi sie zastanawiac jak przepisac timestamp na unnest_tokens, więc tu lecimy calymi zdaniami
library(scales)
library(ggplot2)
g <- ggplot(df, aes(x = times)) +
  scale_x_date(labels = date_format("%Y"), breaks = "1 year") +
  geom_histogram(binwidth = 31) +
  theme_bw()
#wyciągam histogram żeby znaleźć maks i min
gb <- ggplot_build(g)
bin.df <- tibble(date = as.Date(gb$data[[1]]$x, origin = "1970-01-01"), count = gb$data[[1]]$count)
bin.df %>%
  filter(count == max(count))

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
#co rekordowo daje na dzień 700 wiadomosci
#przy średnich 200 wiadomościach na dzień
mean(bin.df$count)/31

#binowanie po dniu, ciekawe czy mam jakąś serię 0 wiadomosci
g2 <- ggplot(df, aes(x = times)) +
  scale_x_date(labels = date_format("%Y"), breaks = "1 year") +
  geom_histogram(binwidth = 1) 
gb2 <- ggplot_build(g2)
bin.df2 <- tibble(date = as.Date(gb2$data[[1]]$x, origin = "1970-01-01"), count = gb2$data[[1]]$count)
empty.streaks <- bin.df2 %>% 
  filter(count == 0) %>%
  mutate(days.diff = 0) %>%
  select(-count)
#zrobic roznice dni
empty.streaks$days.diff[2:nrow(empty.streaks)] = diff(empty.streaks$date)
findLongestStreak <- function(empty.streaks){
  series.list <- list()
  bool <- 0
  series.start <- 0
  for (row in 1:nrow(empty.streaks)){
    if((empty.streaks[row,]$days.diff == 1) && (bool == 0)){
      bool <- 1
      series.start <- empty.streaks[row,]$date
    }
    else if ((empty.streaks[row,]$days.diff != 1) && (bool == 1)){
      bool <- 0
      series.list <- append(series.list, list(list(series.start, as.numeric(empty.streaks[row-1,]$date - series.start))))
    }
  }
  return(series.list)
}

result <- findLongestStreak(empty.streaks)

df.diffs <- tibble(date = sapply(result, function(i) i[[1]]), streak = sapply(result, function(i) i[[2]]))
df.diffs$date <- as.Date(df.diffs$date, origin = "1970-01-01")
#df.diffs zawiera wszystkie dni, gdzie nie pisalem na messenger przynajmniej jeden dzien z rzedu (wartosc 0 wtedy)
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
messg <- lapply(messg, function(conversation) {
  lapply(conversation, function(message) {
    message <- iconv(message, to = "latin1", from = "UTF-8")
    Encoding(message) <- "UTF-8"
    message
  })
})

######1. najczesciej uzywane slowa#####
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

#ręcznie wybrałem top 20 sensownych słów -> subiektywne określenie stopwordów, wywalenie emot, wulgaryzmów.
top_sel <- read.table(header = F, file = "D:\\mgr\\wizualizacja_danych\\proj1\\top_words.txt")
top_sel <- top[top_sel$V1,]

#wordcloud

library("SnowballC")
library("wordcloud")
library("RColorBrewer")

wordcloud(words = top_sel$word, freq = top_sel$n,  min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

##### wulgaryzmy #####
##### import #####
wulg.pl <- c('chuj','chuja', 'chujek', 'chuju', 'chujem', 'chujnia',
              'chujowy', 'chujowa', 'chujowe', 'cipa', 'cipę', 'cipe', 'cipą',
              'cipie', 'dojebać','dojebac', 'dojebie', 'dojebał', 'dojebal',
              'dojebała', 'dojebala', 'dojebałem', 'dojebalem', 'dojebałam',
              'dojebalam', 'dojebię', 'dojebie', 'dopieprzać', 'dopieprzac',
              'dopierdalać', 'dopierdalac', 'dopierdala', 'dopierdalał',
              'dopierdalal', 'dopierdalała', 'dopierdalala', 'dopierdoli',
              'dopierdolił', 'dopierdolil', 'dopierdolę', 'dopierdole', 'dopierdoli',
              'dopierdalający', 'dopierdalajacy', 'dopierdolić', 'dopierdolic',
              'dupa', 'dupie', 'dupą', 'dupcia', 'dupeczka', 'dupy', 'dupe', 'huj',
              'hujek', 'hujnia', 'huja', 'huje', 'hujem', 'huju', 'jebać', 'jebac',
              'jebał', 'jebal', 'jebie', 'jebią', 'jebia', 'jebak', 'jebaka', 'jebal',
              'jebał', 'jebany', 'jebane', 'jebanka', 'jebanko', 'jebankiem',
              'jebanymi', 'jebana', 'jebanym', 'jebanej', 'jebaną', 'jebana',
              'jebani', 'jebanych', 'jebanymi', 'jebcie', 'jebiący', 'jebiacy',
              'jebiąca', 'jebiaca', 'jebiącego', 'jebiacego', 'jebiącej', 'jebiacej',
              'jebia', 'jebią', 'jebie', 'jebię', 'jebliwy', 'jebnąć', 'jebnac',
              'jebnąc', 'jebnać', 'jebnął', 'jebnal', 'jebną', 'jebna', 'jebnęła',
              'jebnela', 'jebnie', 'jebnij', 'jebut', 'koorwa', 'kórwa', 'kurestwo',
              'kurew', 'kurewski', 'kurewska', 'kurewskiej', 'kurewską', 'kurewska',
              'kurewsko', 'kurewstwo', 'kurwa', 'kurwaa', 'kurwami', 'kurwą', 'kurwe',
              'kurwę', 'kurwie', 'kurwiska', 'kurwo', 'kurwy', 'kurwach', 'kurwami',
              'kurewski', 'kurwiarz', 'kurwiący', 'kurwica', 'kurwić', 'kurwic',
              'kurwidołek', 'kurwik', 'kurwiki', 'kurwiszcze', 'kurwiszon',
              'kurwiszona', 'kurwiszonem', 'kurwiszony', 'kutas', 'kutasa', 'kutasie',
              'kutasem', 'kutasy', 'kutasów', 'kutasow', 'kutasach', 'kutasami',
              'matkojebca', 'matkojebcy', 'matkojebcą', 'matkojebca', 'matkojebcami',
              'matkojebcach', 'nabarłożyć', 'najebać', 'najebac', 'najebał',
              'najebal', 'najebała', 'najebala', 'najebane', 'najebany', 'najebaną',
              'najebana', 'najebie', 'najebią', 'najebia', 'naopierdalać',
              'naopierdalac', 'naopierdalał', 'naopierdalal', 'naopierdalała',
              'naopierdalala', 'naopierdalała', 'napierdalać', 'napierdalac',
              'napierdalający', 'napierdalajacy', 'napierdolić', 'napierdolic',
              'nawpierdalać', 'nawpierdalac', 'nawpierdalał', 'nawpierdalal',
              'nawpierdalała', 'nawpierdalala', 'obsrywać', 'obsrywac', 'obsrywający',
              'obsrywajacy', 'odpieprzać', 'odpieprzac', 'odpieprzy', 'odpieprzył',
              'odpieprzyl', 'odpieprzyła', 'odpieprzyla', 'odpierdalać',
              'odpierdalac', 'odpierdol', 'odpierdolił', 'odpierdolil',
              'odpierdoliła', 'odpierdolila', 'odpierdoli', 'odpierdalający',
              'odpierdalajacy', 'odpierdalająca', 'odpierdalajaca', 'odpierdolić',
              'odpierdolic', 'odpierdoli', 'odpierdolił', 'opieprzający',
              'opierdalać', 'opierdalac', 'opierdala', 'opierdalający',
              'opierdalajacy', 'opierdol', 'opierdolić', 'opierdolic', 'opierdoli',
              'opierdolą', 'opierdola', 'piczka', 'pieprznięty', 'pieprzniety',
              'pieprzony', 'pierdel', 'pierdlu', 'pierdolą', 'pierdola', 'pierdolący',
              'pierdolacy', 'pierdoląca', 'pierdolaca', 'pierdol', 'pierdole',
              'pierdolenie', 'pierdoleniem', 'pierdoleniu', 'pierdolę', 'pierdolec',
              'pierdola', 'pierdolą', 'pierdolić', 'pierdolicie', 'pierdolic',
              'pierdolił', 'pierdolil', 'pierdoliła', 'pierdolila', 'pierdoli',
              'pierdolnięty', 'pierdolniety', 'pierdolisz', 'pierdolnąć',
              'pierdolnac', 'pierdolnął', 'pierdolnal', 'pierdolnęła', 'pierdolnela',
              'pierdolnie', 'pierdolnięty', 'pierdolnij', 'pierdolnik', 'pierdolona',
              'pierdolone', 'pierdolony', 'pierdołki', 'pierdzący', 'pierdzieć',
              'pierdziec', 'pizda', 'pizdą', 'pizde', 'pizdę', 'piździe', 'pizdzie',
              'pizdnąć', 'pizdnac', 'pizdu', 'podpierdalać', 'podpierdalac',
              'podpierdala', 'podpierdalający', 'podpierdalajacy', 'podpierdolić',
              'podpierdolic', 'podpierdoli', 'pojeb', 'pojeba', 'pojebami',
              'pojebani', 'pojebanego', 'pojebanemu', 'pojebani', 'pojebany',
              'pojebanych', 'pojebanym', 'pojebanymi', 'pojebem', 'pojebać',
              'pojebac', 'pojebalo', 'popierdala', 'popierdalac', 'popierdalać',
              'popierdolić', 'popierdolic', 'popierdoli', 'popierdolonego',
              'popierdolonemu', 'popierdolonym', 'popierdolone', 'popierdoleni',
              'popierdolony', 'porozpierdalać', 'porozpierdala', 'porozpierdalac',
              'poruchac', 'poruchać', 'przejebać', 'przejebane', 'przejebac',
              'przyjebali', 'przepierdalać', 'przepierdalac', 'przepierdala',
              'przepierdalający', 'przepierdalajacy', 'przepierdalająca',
              'przepierdalajaca', 'przepierdolić', 'przepierdolic', 'przyjebać',
              'przyjebac', 'przyjebie', 'przyjebała', 'przyjebala', 'przyjebał',
              'przyjebal', 'przypieprzać', 'przypieprzac', 'przypieprzający',
              'przypieprzajacy', 'przypieprzająca', 'przypieprzajaca',
              'przypierdalać', 'przypierdalac', 'przypierdala', 'przypierdoli',
              'przypierdalający', 'przypierdalajacy', 'przypierdolić',
              'przypierdolic', 'qrwa', 'rozjebać', 'rozjebac', 'rozjebie',
              'rozjebała', 'rozjebią', 'rozpierdalać', 'rozpierdalac', 'rozpierdala',
              'rozpierdolić', 'rozpierdolic', 'rozpierdole', 'rozpierdoli',
              'rozpierducha', 'skurwić', 'skurwiel', 'skurwiela', 'skurwielem',
              'skurwielu', 'skurwysyn', 'skurwysynów', 'skurwysynow', 'skurwysyna',
              'skurwysynem', 'skurwysynu', 'skurwysyny', 'skurwysyński',
              'skurwysynski', 'skurwysyństwo', 'skurwysynstwo', 'spieprzać',
              'spieprzac', 'spieprza', 'spieprzaj', 'spieprzajcie', 'spieprzają',
              'spieprzaja', 'spieprzający', 'spieprzajacy', 'spieprzająca',
              'spieprzajaca', 'spierdalać', 'spierdalac', 'spierdala', 'spierdalał',
              'spierdalała', 'spierdalal', 'spierdalalcie', 'spierdalala',
              'spierdalający', 'spierdalajacy', 'spierdolić', 'spierdolic',
              'spierdoli', 'spierdoliła', 'spierdoliło', 'spierdolą', 'spierdola',
              'srać', 'srac', 'srający', 'srajacy', 'srając', 'srajac', 'sraj',
              'sukinsyn', 'sukinsyny', 'sukinsynom', 'sukinsynowi', 'sukinsynów',
              'sukinsynow', 'śmierdziel', 'udupić', 'ujebać', 'ujebac', 'ujebał',
              'ujebal', 'ujebana', 'ujebany', 'ujebie', 'ujebała', 'ujebala',
              'upierdalać', 'upierdalac', 'upierdala', 'upierdoli', 'upierdolić',
              'upierdolic', 'upierdoli', 'upierdolą', 'upierdola', 'upierdoleni',
              'wjebać', 'wjebac', 'wjebie', 'wjebią', 'wjebia', 'wjebiemy',
              'wjebiecie', 'wkurwiać', 'wkurwiac', 'wkurwi', 'wkurwia', 'wkurwiał',
              'wkurwial', 'wkurwiający', 'wkurwiajacy', 'wkurwiająca', 'wkurwiajaca',
              'wkurwić', 'wkurwic', 'wkurwi', 'wkurwiacie', 'wkurwiają', 'wkurwiali',
              'wkurwią', 'wkurwia', 'wkurwimy', 'wkurwicie', 'wkurwiacie', 'wkurwić',
              'wkurwic', 'wkurwia', 'wpierdalać', 'wpierdalac', 'wpierdalający',
              'wpierdalajacy', 'wpierdol', 'wpierdolić', 'wpierdolic', 'wpizdu',
              'wyjebać', 'wyjebac', 'wyjebali', 'wyjebał', 'wyjebac', 'wyjebała',
              'wyjebały', 'wyjebie', 'wyjebią', 'wyjebia', 'wyjebiesz', 'wyjebie',
              'wyjebiecie', 'wyjebiemy', 'wypieprzać', 'wypieprzac', 'wypieprza',
              'wypieprzał', 'wypieprzal', 'wypieprzała', 'wypieprzala', 'wypieprzy',
              'wypieprzyła', 'wypieprzyla', 'wypieprzył', 'wypieprzyl', 'wypierdal',
              'wypierdalać', 'wypierdalac', 'wypierdala', 'wypierdalaj',
              'wypierdalał', 'wypierdalal', 'wypierdalała', 'wypierdalala',
              'wypierdalać', 'wypierdolić', 'wypierdolic', 'wypierdoli',
              'wypierdolimy', 'wypierdolicie', 'wypierdolą', 'wypierdola',
              'wypierdolili', 'wypierdolił', 'wypierdolil', 'wypierdoliła',
              'wypierdolila', 'zajebać', 'zajebac', 'zajebie', 'zajebią', 'zajebia',
              'zajebiał', 'zajebial', 'zajebała', 'zajebiala', 'zajebali', 'zajebana',
              'zajebani', 'zajebane', 'zajebany', 'zajebanych', 'zajebanym',
              'zajebanymi', 'zajebiste', 'zajebisty', 'zajebistych', 'zajebista',
              'zajebistym', 'zajebistymi', 'zajebiście', 'zajebiscie', 'zapieprzyć',
              'zapieprzyc', 'zapieprzy', 'zapieprzył', 'zapieprzyl', 'zapieprzyła',
              'zapieprzyla', 'zapieprzą', 'zapieprza', 'zapieprzy', 'zapieprzymy',
              'zapieprzycie', 'zapieprzysz', 'zapierdala', 'zapierdalać',
              'zapierdalac', 'zapierdalaja', 'zapierdalał', 'zapierdalaj',
              'zapierdalajcie', 'zapierdalała', 'zapierdalala', 'zapierdalali',
              'zapierdalający', 'zapierdalajacy', 'zapierdolić', 'zapierdolic',
              'zapierdoli', 'zapierdolił', 'zapierdolil', 'zapierdoliła',
              'zapierdolila', 'zapierdolą', 'zapierdola', 'zapierniczać',
              'zapierniczający', 'zasrać', 'zasranym', 'zasrywać', 'zasrywający',
              'zesrywać', 'zesrywający', 'zjebać', 'zjebac', 'zjebał', 'zjebal',
              'zjebała', 'zjebala', 'zjebana', 'zjebią', 'zjebali', 'zjeby')
###### ang #####
wulg.en <- read.csv('https://raw.githubusercontent.com/LDNOOBW/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en')
wulg <- c(as.character(wulg.en$X2g1c), wulg.pl)
  
ile.wulg <- sapply(df$text, function(i){
    wynik <- sum(str_detect(i, wulg.pl))
    return(ifelse(wynik > 0, 1, 0))
  })
ile.wulg/length(unl.messg)
#####2. zbinować to i nakreślić w funkcji timestamp#####
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

#####binowanie po dniu, ciekawe czy mam jakąś serię 0 wiadomosci#####
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
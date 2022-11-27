# Project Title: 
# Course: ITEC-SIS-724
# Name: Ashley Pinamonti
# Date: December 7, 2022

## Set working directory, install packages, and load libraries.  

setwd("~/Grad School/Coursework/5. Fall 2022/SIS 724 Text Mining/Final")

#install.packages("tm")
#install.packages("tidytext")
#install.packages("tidyverse")
#install.packages("textdata")
#install.packages("dplyr")
#install.packages("stringr")
#install.packages("tidyr")
#install.packages("ggplot2")
#install.packages("wordcloud")
#install.packages("reshape2")
#install.packages("quanteda")
#install.packages("readtext")
#install.packages("stringi")
#install.packages("stringr")
#install.packages("qdap")
#install.packages("rJava")
#install.packages("ggthemes")
#install.packages("rvest")
#install.packages("Rcrawler")
#install.packages("RSelenium")
#install.packages("xml2")
#install.packages("SnowballC")

library(tm)
library(tidytext)
library(tidyverse)
library(textdata)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(wordcloud)
library(reshape2)
library(quanteda)
library(readtext)
library(stringi)
library(stringr)
library(qdap)
library(rJava)
library(ggthemes)
library(scales)
library(rvest)
library(Rcrawler)
library(RSelenium)
library(xml2)
library(SnowballC)

## Read in news broadcast trancripts downloaded from Factiva.   

AFGpath_data <- "C:/Users/ashle/OneDrive/Documents/Grad School/Coursework/5. Fall 2022/SIS 724 Text Mining/Final/AFG"

AFG <- readtext(paste0(AFGpath_data, "*/*.pdf"), 
                        docvarsfrom = "filenames", 
                        docvarnames = c("Number", "Date", "Agency", "Population"),
                        sep = "_")

UKRpath_data <- "C:/Users/ashle/OneDrive/Documents/Grad School/Coursework/5. Fall 2022/SIS 724 Text Mining/Final/UKR"

UKR <- readtext(paste0(UKRpath_data, "*/*.pdf"), 
                docvarsfrom = "filenames", 
                docvarnames = c("Number", "Date", "Agency", "Population"),
                sep = "_")

## Tokenize data, remove stop words, and remove numbers. 

AFG_words <- AFG %>%
  unnest_tokens(word,text)

data(stop_words)

AFG_clean <- AFG_words %>%
  anti_join(stop_words) %>%
  filter(!grepl('[0-9]', word))

UKR_words <- UKR %>%
  unnest_tokens(word,text) 

UKR_clean <- UKR_words %>%
  anti_join(stop_words) %>%
  filter(!grepl('[0-9]', word))

## Determine most frequently occurring words in each dataset. 

AFG_clean %>%
  count(word, sort = TRUE) %>%
  print(n = 50)

# Description: df [10,441 × 3]
#word               n text     
#<chr>          <int> <chr>    
#1 people          1030 "\"\"..."
#2 afghanistan      638 "\"\"..."
#3 taliban          516 "\"\"..."
#4 u.s              494 "\"\"..."
#5 president        482 "\"\"..."
#6 rights           444 "\"\"..."
#7 video            428 "\"\"..."
#8 country          426 "\"\"..."
#9 biden            424 "\"\"..."
#10 clip             401 "\"\"..."
#11 afghan           395 "\"\"..."
#12 reserved         383 "\"\"..."
#13 time             352 "\"\"..."
#14 page             333 "\"\"..."
#15 factiva          326 "\"\"..."
#16 united           315 "\"\"..."
#17 ingraham         307 "\"\"..."
#18 americans        279 "\"\"..."
#19 government       278 "\"\"..."
#20 refugees         271 "\"\"..."
#21 begin            256 "\"\"..."
#22 news             255 "\"\"..."
#23 american         252 "\"\"..."
#24 lot              247 "\"\"..."
#25 military         246 "\"\"..."
#26 airport          235 "\"\"..."
#27 house            233 "\"\"..."
#28 kabul            228 "\"\"..."
#29 day              219 "\"\"..."
#30 afghans          215 "\"\"..."
#31 administration   212 "\"\"..."
#32 coming           205 "\"\"..."
#33 cnn              202 "\"\"..."
#34 covid            201 "\"\"..."
#35 security         200 "\"\"..."
#36 vaccine          193 "\"\"..."
#37 trump            191 "\"\"..."
#38 copyright        188 "\"\"..."
#39 tonight          175 "\"\"..."
#40 ve               170 "\"\"..."
#41 world            170 "\"\"..."
#42 white            167 "\"\"..."
#43 days             165 "\"\"..."
#44 left             162 "\"\"..."
#45 unidentified     161 "\"\"..."
#46 break            147 "\"\"..."
#47 situation        144 "\"\"..."
#48 america          143 "\"\"..."
#49 war              142 "\"\"..."
#50 troops           141 "\"\"..."
# … with 10,391 more rows

UKR_clean %>%
  count(word, sort = TRUE) %>%
  print(n = 50)

# Description: df [10,213 × 3]
#word              n text     
#<chr>         <int> <chr>    
#1 ukraine        1105 "\"\"..."
#2 people          851 "\"\"..."
#3 russia          691 "\"\"..."
#4 russian         676 "\"\"..."
#5 president       660 "\"\"..."
#6 ukrainian       602 "\"\"..."
#7 war             546 "\"\"..."
#8 putin           446 "\"\"..."
#9 video           444 "\"\"..."
#10 u.s             399 "\"\"..."
#11 rights          395 "\"\"..."
#12 reserved        374 "\"\"..."
#13 clip            358 "\"\"..."
#14 news            351 "\"\"..."
#15 refugees        351 "\"\"..."
#16 biden           335 "\"\"..."
#17 page            319 "\"\"..."
#18 factiva         317 "\"\"..."
#19 country         313 "\"\"..."
#20 city            300 "\"\"..."
#21 united          300 "\"\"..."
#22 time            281 "\"\"..."
#23 poland          277 "\"\"..."
#24 ukrainians      275 "\"\"..."
#25 world           270 "\"\"..."
#26 begin           261 "\"\"..."
#27 kyiv            240 "\"\"..."
#28 military        234 "\"\"..."
#29 border          233 "\"\"..."
#30 russians        232 "\"\"..."
#31 nato            223 "\"\"..."
#32 copyright       212 "\"\"..."
#33 fox             210 "\"\"..."
#34 baier           205 "\"\"..."
#35 velshi          202 "\"\"..."
#36 tonight         201 "\"\"..."
#37 house           189 "\"\"..."
#38 lot             189 "\"\"..."
#39 correspondent   184 "\"\"..."
#40 forces          182 "\"\"..."
#41 cnn             174 "\"\"..."
#42 coming          172 "\"\"..."
#43 media           172 "\"\"..."
#44 government      167 "\"\"..."
#45 invasion        165 "\"\"..."
#46 day             164 "\"\"..."
#47 china           163 "\"\"..."
#48 weapons         156 "\"\"..."
#49 zelenskyy       154 "\"\"..."
#50 vladimir        153 "\"\"..."
# … with 10,163 more rows

## Remove custom stop words, convert words to stems, and determine most frequently occurring words in each dataset. 

custom_stop_words <- tibble (word = c("afghan", "sort", "ii", "weeks", "camera", "videotape", "john", "dr", "llc", "evening", "month", "months", "roberts", "cavuto", "secretary", "host", "msnbc", "week", "time", "commercial", "break", "ukrainian", "putin", "told", "august", "february", "days", "poland", "russia", "russian", "russians", "administration", "afghans", "afghanistan", "ukraine", "ukrainians", "white", "house", "ukrain", "people", "lviv", "percent", "week", "ago", "laura", "america", "lot", "bret", "zelenskyy", "mariupol", "countries", "cities", "vause", "todd", "americans", "joe", "hayes", "don", "carlson", "american", "baier", "velshi", "kyiv", "kabul", "ve",  "u.s", "u.s.", "president", "video", "country", "city", "biden", "clip", "page", "copyright", "factiva", "correspondent", "cnn", "day", "fox", "reserved", "rights", "united", "begin", "news", "american", "ingraham", "vladimir", "zelensky", "tonight", "trump"))

AFG_clean_custom <- AFG_words %>%
  anti_join(stop_words) %>%
  anti_join(custom_stop_words) %>%
  filter(!grepl('[0-9]', word)) %>%
  mutate(word = wordStem(word))

UKR_clean_custom <- UKR_words %>%
  anti_join(stop_words) %>%
  anti_join(custom_stop_words) %>%
  filter(!grepl('[0-9]', word)) %>%
  mutate(word = wordStem(word))

AFG_clean_custom %>%
  count(word, sort = TRUE) %>%
  print(n = 50)

AFG_clean_custom %>%
  count(word, sort = TRUE) %>%
  filter(n>200) %>%
  mutate(word=reorder(word,n)) %>%
  ggplot(aes(word,n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

UKR_clean_custom %>%
  count(word, sort = TRUE) %>%
  print(n = 50)

UKR_clean_custom %>%
  count(word, sort = TRUE) %>%
  filter(n>200) %>%
  mutate(word=reorder(word,n)) %>%
  ggplot(aes(word,n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

## Determine words most associated with Ukrain- and Afghan-. (see lab 4)

AFG_corpus <- Corpus(DirSource('AFG'), readerControl=list(reader=readPlain))
tm_map(AFG_corpus, PlainTextDocument)
AFG_corpus <- tm_map(AFG_corpus, stripWhitespace)
AFG_corpus <- tm_map(AFG_corpus, tolower)
AFG_corpus <- tm_map(AFG_corpus, removeWords, stopwords("english"))
AFG_corpus = tm_map(AFG_corpus, removeWords, c("sort", "ii", "weeks", "camera", "videotape", "john", "dr", "llc", "evening", "month", "months", "roberts", "cavuto", "secretary", "host", "msnbc", "week", "time", "commercial", "break", "putin", "told", "august", "february", "days", "poland", "russia", "russian", "russians", "administration", "white", "house", "people", "lviv", "percent", "week", "ago", "laura", "america", "lot", "bret", "zelenskyy", "mariupol", "countries", "cities", "vause", "todd", "americans", "joe", "hayes", "don", "carlson", "american", "baier", "velshi", "kyiv", "kabul", "ve",  "u.s", "u.s.", "president", "video", "country", "city", "biden", "clip", "page", "copyright", "factiva", "correspondent", "cnn", "day", "fox", "reserved", "rights", "united", "begin", "news", "american", "ingraham", "vladimir", "zelensky", "tonight", "trump", stopwords("english")))
tm_map(AFG_corpus, stemDocument)

AFG_dtm <- DocumentTermMatrix(AFG_corpus)
inspect(AFG_dtm)

#<<DocumentTermMatrix (documents: 30, terms: 1239)>>
#Non-/sparse entries: 2534/34636
#Sparsity           : 93%
#Maximal term length: 525
#Weighting          : term frequency (tf)
#Sample             :
#  Terms
#Docs                      %%eof %<e2><e3><cf><d3> %pdf-1.4 <</e <</linearized
#12_08182021_FOX_AFG.pdf     1                 1        1    1             1
#15_08172021_FOX_AFG.pdf     1                 1        1    1             1
#16_08232021_FOX_AFG.pdf     1                 1        1    1             1
#17_08192021_FOX_AFG.pdf     1                 1        1    1             1
#19_10252021_FOX_AFG.pdf     1                 1        1    1             1
#21_09262021_CNN_AFG.pdf     1                 1        1    1             1
#22_08192021_CNN_AFG.pdf     1                 1        1    1             1
#23_09262021_CNN_AFG.pdf     1                 1        1    1             1
#26_09182021_CNN_AFG.pdf     1                 1        1    1             1
#27_09232021_CNN_AFG.pdf     1                 1        1    1             1

#Terms
#Docs                      00000 0000000016 1/l endobj obj
#12_08182021_FOX_AFG.pdf    29          1   1     12  13
#15_08172021_FOX_AFG.pdf    29          1   1     11  12
#16_08232021_FOX_AFG.pdf    29          1   1     10  11
#17_08192021_FOX_AFG.pdf    29          1   1     11  12
#19_10252021_FOX_AFG.pdf    29          1   1     10  11
#21_09262021_CNN_AFG.pdf    29          1   1     11  12
#22_08192021_CNN_AFG.pdf    29          1   1     11  12
#23_09262021_CNN_AFG.pdf    29          1   1     11  12
#26_09182021_CNN_AFG.pdf    32          1   1     11  12
#27_09232021_CNN_AFG.pdf    29          1   1     10  11

findFreqTerms(AFG_dtm, 30)
#[1] "%%eof"             "%<e2><e3><cf><d3>" "%pdf-1.4"         
#[4] "00000"             "0000000016"        "1/l"              
#[7] "<</e"              "<</linearized"     "<</size"          
#[10] "endobj"            "obj"               "r/info"           
#[13] "startxref"         "trailer"           "xref"  

UKR_corpus <- Corpus(DirSource('UKR'), readerControl=list(reader=readPlain))
UKR_corpus <- tm_map(UKR_corpus, as.PlainTextDocument)
UKR_corpus <- tm_map(UKR_corpus, stripWhitespace)
UKR_corpus <- tm_map(UKR_corpus, tolower)
UKR_corpus <- tm_map(UKR_corpus, removeWords, stopwords("english"))
AFG_corpus = tm_map(AFG_corpus, removeWords, c("sort", "ii", "weeks", "camera", "videotape", "john", "dr", "llc", "evening", "month", "months", "roberts", "cavuto", "secretary", "host", "msnbc", "week", "time", "commercial", "break", "putin", "told", "august", "february", "days", "poland", "russia", "russian", "russians", "administration", "white", "house", "people", "lviv", "percent", "week", "ago", "laura", "america", "lot", "bret", "zelenskyy", "mariupol", "countries", "cities", "vause", "todd", "americans", "joe", "hayes", "don", "carlson", "american", "baier", "velshi", "kyiv", "kabul", "ve",  "u.s", "u.s.", "president", "video", "country", "city", "biden", "clip", "page", "copyright", "factiva", "correspondent", "cnn", "day", "fox", "reserved", "rights", "united", "begin", "news", "american", "ingraham", "vladimir", "zelensky", "tonight", "trump", stopwords("english")))
tm_map(UKR_corpus, stemDocument)
UKR_dtm <- DocumentTermMatrix(UKR_corpus)
UKR_dtm <- DocumentTermMatrix(UKR_corpus)
findFreqTerms(UKR_dtm, 500)








###############IGNORE###################
## Determine most frequently occurring different words in each dataset. 
# Round 1: Add common words from top 50 to stop words list. 

common_stop_words <- tibble (word = c("government", "refugees", "refugee", "military", "coming", "security", "world", "left", "unidentified", "situation", "war", "troops", "media", "question", "border", "law", "talking", "home", "weeks", "million", "support", "forces", "live", "stay", "content", "crisis"))

AFG_clean_custom_common <- AFG_words %>%
  mutate(stem = wordStem(word)) %>%
  anti_join(stop_words) %>%
  anti_join(custom_stop_words) %>%
  anti_join(common_stop_words) %>%
  filter(!grepl('[0-9]', word)) 

UKR_clean_custom_common <- UKR_words %>%
  mutate(stem = wordStem(word)) %>%
  anti_join(stop_words) %>%
  anti_join(custom_stop_words) %>%
  anti_join(common_stop_words) %>%
  filter(!grepl('[0-9]', word))

AFG_clean_custom_common %>%
  count(word, sort = TRUE) %>%
  print(n = 50)

UKR_clean_custom_common %>%
  count(word, sort = TRUE) %>%
  print(n = 50)

AFG_clean_custom_common %>%
  count(word, sort = TRUE) %>%
  filter(n>100) %>%
  mutate(word=reorder(word,n)) %>%
  ggplot(aes(word,n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

UKR_clean_custom_common %>%
  count(word, sort = TRUE) %>%
  filter(n>100) %>%
  mutate(word=reorder(word,n)) %>%
  ggplot(aes(word,n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# Round 2: Add common words from top 50 to stop words list. 

common_stop_words2 <- tibble (word = c("leave", "sort", "ground", "national", "family", "power", "defense", "foreign", "taking", "call", "voice", "happening", "change", "officials"))

AFG_clean_custom_common2 <- AFG_words %>%
  mutate(stem = wordStem(word)) %>%
  anti_join(stop_words) %>%
  anti_join(custom_stop_words) %>%
  anti_join(common_stop_words) %>%
  anti_join(common_stop_words2) %>%
  filter(!grepl('[0-9]', word)) 

UKR_clean_custom_common2 <- UKR_words %>%
  mutate(stem = wordStem(word)) %>%
  anti_join(stop_words) %>%
  anti_join(custom_stop_words) %>%
  anti_join(common_stop_words) %>%
  anti_join(common_stop_words2) %>%
  filter(!grepl('[0-9]', word))

AFG_clean_custom_common2 %>%
  count(word, sort = TRUE) %>%
  print(n = 50)

UKR_clean_custom_common2 %>%
  count(word, sort = TRUE) %>%
  print(n = 50)

AFG_clean_custom_common2 %>%
  count(word, sort = TRUE) %>%
  filter(n>100) %>%
  mutate(word=reorder(word,n)) %>%
  ggplot(aes(word,n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

UKR_clean_custom_common2 %>%
  count(word, sort = TRUE) %>%
  filter(n>100) %>%
  mutate(word=reorder(word,n)) %>%
  ggplot(aes(word,n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# Round 3: Add common words from top 50 to stop words list. 

common_stop_words3 <- tibble (word = c("thousands", "allies", "international", "bring", "moment", "hear", "talk", "refugee", "services", "deal", "europe"))

AFG_clean_custom_common3 <- AFG_words %>%
  mutate(stem = wordStem(word)) %>%
  anti_join(stop_words) %>%
  anti_join(custom_stop_words) %>%
  anti_join(common_stop_words) %>%
  anti_join(common_stop_words2) %>%
  anti_join(common_stop_words3) %>%
  filter(!grepl('[0-9]', word)) 

UKR_clean_custom_common3 <- UKR_words %>%
  mutate(stem = wordStem(word)) %>%
  anti_join(stop_words) %>%
  anti_join(custom_stop_words) %>%
  anti_join(common_stop_words) %>%
  anti_join(common_stop_words2) %>%
  anti_join(common_stop_words3) %>%
  filter(!grepl('[0-9]', word))

AFG_clean_custom_common3 %>%
  count(word, sort = TRUE) %>%
  print(n = 50)

UKR_clean_custom_common3 %>%
  count(word, sort = TRUE) %>%
  print(n = 50)

AFG_clean_custom_common3 %>%
  count(word, sort = TRUE) %>%
  filter(n>100) %>%
  mutate(word=reorder(word,n)) %>%
  ggplot(aes(word,n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

UKR_clean_custom_common3 %>%
  count(word, sort = TRUE) %>%
  filter(n>100) %>%
  mutate(word=reorder(word,n)) %>%
  ggplot(aes(word,n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()



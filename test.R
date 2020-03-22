library("rjson")
library("tm")
library("wordcloud")

baseUrl <- "https://dadosabertos.camara.leg.br/api/v2"

deputadoId <- "74847"

dataInicio <- "1990-01-01"

dataFim <- "1991-12-31"

quantidadeItens <- "100"


discursosUrl <- paste(baseUrl, "/deputados/", deputadoId, "/discursos?dataInicio=", dataInicio, "&dataFim=", dataFim, "&ordenarPor=dataHoraInicio&itens=", quantidadeItens, "&ordem=ASC", sep = "", collapse = "")

print(discursosUrl)

discursos <- fromJSON(file = discursosUrl)

allWords <- c()

for (discurso in discursos$dados) {
  # search <- regexpr("MILITAR", discurso$keywords, fixed = TRUE)
  # found <- FALSE
  # if (search > 0) {
  #   found <- TRUE
  # }
  # print(paste(discurso$dataHoraInicio, found))
  cleanKeywords <- gsub("[\r\n']", "", discurso$keywords)
  cleanKeywords <- gsub("[.]", ",", cleanKeywords)
  words <- strsplit(cleanKeywords, ",\ ?")
  allWords <- append(allWords, unlist(words));
}

freq <- termFreq(allWords);

m <- as.matrix(freq)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

head(d, 10)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=150, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))




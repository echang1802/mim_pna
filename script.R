
library(tidytext)
library(dplyr)
library(igraph)

data<- read.csv("dataset.csv")$text

words<- strsplit(as.character(data)," ")

drop_stop_words<- function(phrase){
  phrase<- tolower(phrase)
  phrase<- phrase[!(phrase %in% stop_words$word)]
} 

words<- sapply(words, drop_stop_words)

sentiments_words<- get_sentiments("nrc")

get_sentiments<- function(phrase){
  sent<- sentiments_words$sentiment[sentiments_words$word %in% phrase]
  if(length(sent) == 0){
    return(character())
  }
  return(unique(sent))
}

phrase_sentiments<- sapply(words, get_sentiments)

# limpiamos de aquellas frases sin emociones
phrases<- list()
i<- 1
for(phrase in phrase_sentiments){
  if(length(phrase) > 0){
    phrases[[i]]<- phrase
    i<- i + 1
  }
}

# Creamos los nodos

nodes<- data.frame(
  "id" = integer(),
  "color" = character(),
  "trust" = logical(),
  "fear" =  logical(),
  "negative" = logical(),
  "sadness" = logical(),
  "anger" = logical(),
  "surprise" = logical(),
  "positive" = logical(),
  "disgust" = logical(),
  "joy" = logical(),
  "anticipation" = logical()
)
for(id in 1:length(phrases)){
  nodes<- rbind(nodes,data.frame(
    "id" = id,
    "color" = ifelse(
      "trust" %in% phrases[[id]], "cyan", ifelse(
        "fear" %in% phrases[[id]], "darkgrey", ifelse(
          "negative" %in% phrases[[id]],"red", ifelse(
            "sadness" %in% phrases[[id]],"grey", ifelse(
              "anger" %in% phrases[[id]],"darkred", ifelse(
                "surprise" %in% phrases[[id]],"yellow", ifelse(
                  "positive" %in% phrases[[id]],"blue", ifelse(
                    "disgust" %in% phrases[[id]],"darkgreen", ifelse(
                      "joy" %in% phrases[[id]],"green", ifelse(
                        "anticipation" %in% phrases[[id]], "orange", ""
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    ),
    "trust" = "trust" %in% phrases[[id]],
    "fear" = "fear" %in% phrases[[id]],
    "negative" = "negative" %in% phrases[[id]],
    "sadness" = "sadness" %in% phrases[[id]],
    "anger" = "anger" %in% phrases[[id]],
    "surprise" = "surprise" %in% phrases[[id]],
    "positive" = "positive" %in% phrases[[id]],
    "disgust" = "disgust" %in% phrases[[id]],
    "joy" = "joy" %in% phrases[[id]],
    "anticipation" = "anticipation" %in% phrases[[id]]
  ))
}

# Creamos los vertices

links<- data.frame(
  "from" = integer(),
  "to" = integer(),
  "weight" = integer()
)
for(id in 1:(length(phrases) - 1)){
  for(subId in (id + 1):length(phrases)){
    weigth<- (nodes$trust[id] && nodes$trust[subId]) +
      (nodes$fear[id] && nodes$fear[subId]) +
      (nodes$negative[id] && nodes$negative[subId]) +
      (nodes$sadness[id] && nodes$sadness[subId]) +
      (nodes$anger[id] && nodes$anger[subId]) +
      (nodes$surprise[id] && nodes$surprise[subId]) +
      (nodes$positive[id] && nodes$positive[subId]) +
      (nodes$disgust[id] && nodes$disgust[subId]) +
      (nodes$joy[id] && nodes$joy[subId]) +
      (nodes$anticipation[id] && nodes$anticipation[subId])
    if(weigth > 0){
      links<- rbind(links, data.frame(
        "from" = id,
        "to" = subId,
        "weigth" = weigth
      ))
    }
  }
}

# Creeamos grafo

grafo <- graph_from_data_frame(d=links, vertices=nodes, directed=FALSE) 

plot(grafo, edge.arrow.size=.4, edge.color="grey",
     vertex.color=V(grafo)$color, vertex.frame.color=V(grafo)$color,
     vertex.label.color="black") 

mean_distance(grafo)

wc <- walktrap.community(grafo)

plot(grafo, edge.arrow.size=.4, edge.color="grey",
     vertex.frame.color="#ffffff", vertex.label.color=V(grafo)$color,
     mark.groups = wc) 


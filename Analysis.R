library(RMeCab)
library(ggplot2)
library(igraph)
library(ggraph)
library(reshape2)

# Read tweet data ----
# list for data storage
twtfile <- grep("csv", list.files("UBData"), value = TRUE)
twtdata <- vector("list", length = length(twtfile))
names(twtdata) <- twtfile

for (i in twtfile) {
  # read data
  twtdata[[i]] <- read.csv(paste0("UBData/", i))
  
  # new column: clean text 
  twtdata[[i]]$text_clr <- twtdata[[i]]$text
  twtdata[[i]]$text_clr <- gsub("@\\w+", "", twtdata[[i]]$text_clr)
  twtdata[[i]]$text_clr <- gsub("https?://.+", "", twtdata[[i]]$text_clr)
  twtdata[[i]]$text_clr <- gsub("\\d+\\w*\\d*", "", twtdata[[i]]$text_clr)
  twtdata[[i]]$text_clr <- gsub("#\\w+", "", twtdata[[i]]$text_clr)
  twtdata[[i]]$text_clr <- gsub("[[:punct:]]", " ", twtdata[[i]]$text_clr)
  twtdata[[i]]$text_clr <- gsub("[0-9]", "", twtdata[[i]]$text_clr)
}

# Temporal change ----
# need: time slice of big event 
twtdata_info <- data.frame(
  filename = twtfile, 
  twtnum = sapply(twtdata, dim)[1, ]
)
ggplot(twtdata_info) + geom_col(aes(filename, twtnum))

# Data for word frequeny and collocation ----
# stopwords
stopwords <- c(
  "する","それ","なる","ない","そこ","これ","ある", "さん", "なん", "の", "ん",
  "いる", "思う", "そう", "れる", "くる", "考える", "言う", "ー",
  "できる", "てる", "でる", "一", "いい", "何", "いう", "できる", "られる",
  "n", "RT", letters, LETTERS,
  "+", "<", ">", "><"
)

# list for word frequency storage
freq <- vector("list", length = length(twtfile))
names(freq) <- twtfile
freq_short <- vector("list", length = length(twtfile))
names(freq_short) <- twtfile

# list for ngram data storage 
ngram <- vector("list", length = length(twtfile))
names(ngram) <- twtfile
ngram_short <- vector("list", length = length(twtfile))
names(ngram_short) <- twtfile

for (i in twtfile) {
  # report process 
  print(i)
  
  # generate temp *.txt file
  write(twtdata[[i]]$text_clr, "UBtwt.txt")
  
  # word frequency data
  freq[[i]] <- docDF("UBTwt.txt", type = 1, dic = "ubdic.dic")
  # elimate unuseful characters
  freq[[i]] <- freq[[i]][freq[[i]]$POS1 %in% c("名詞","動詞","形容詞") &
                 !(freq[[i]]$POS2 %in% c("非自立","接尾", "数")) &
                 !(freq[[i]]$TERM %in% stopwords),]
  # merge duplicated words 
  freq[[i]] <- aggregate(UBTwt.txt ~ TERM, data = freq[[i]], sum)
  # order by frequency 
  freq[[i]] <- freq[[i]][order(freq[[i]]$UBTwt.txt, decreasing = TRUE), ]
  # short version
  freq_short[[i]] <- head(freq[[i]], 30)
  
  # ngram data for collocation network
  ngram[[i]] <- docDF("UBTwt.txt", type = 1, pos = c("名詞", "形容詞", "動詞"),
                      nDF = 1, N =2, dic = "ubdic.dic")
  ngram[[i]] <- ngram[[i]][order(ngram[[i]]$UBTwt.txt, decreasing = TRUE), ]
  ngram[[i]] <- head(ngram[[i]], 30)
  
  # short version
  ngram_short[[i]] <- head(ngram[[i]], 30)
}

# word frequency graph
for (i in twtfile) {
  theme_set(theme_gray(base_size=12, base_family="HiraKakuProN-W3"))
  print(ggplot(freq_short[[i]], aes(x = reorder(TERM, UBTwt.txt), y = UBTwt.txt)) +
          geom_bar(stat = "identity") +
          coord_flip() + 
          labs(title = i))
}

# collocation graph
for (i in twtfile) {
  ngram_plotdata <- graph_from_data_frame(ngram[[i]])
  plot(ngram_plotdata, vertex.size = 10, 
       vertex.label.family = "HiraKakuProN-W3")
}

# word frequency change
func_mergefreq <- function(freq_ls) {
  freq_ls_std <- lapply(freq_ls, function(x) {
    data.frame(x["TERM"], x["UBTwt.txt"]/max(x["UBTwt.txt"])) 
  })
  
  # merge word frequency data.frame in the list
  freq_df <- Reduce(function(df1, df2) {
    merge(df1, df2, by = "TERM", all.x = TRUE)
  }, freq_ls_std)
  
  # order by sum of relative frequency 
  freq_df$sum <- rowSums(freq_df[2: ncol(freq_df)], na.rm = TRUE)
  freq_df <- freq_df[order(freq_df$sum, decreasing = TRUE), ]
  freq_df$sum <- NULL
  names(freq_df) <- c("TERM", twtfile)
  
  # output
  freq_df
}

freq_month <- func_mergefreq(freq_ls = freq)
freq_month_short <- head(freq_month, 16)

# make graph
freq_month_short_long <- melt(freq_month_short, id = "TERM")
dim(freq_month_short_long)
ggplot(freq_month_short_long, aes(variable, value)) + 
  geom_col(aes(fill = TERM, color = TERM)) + 
  facet_wrap(.~TERM) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), 
        axis.title = element_blank())

# Change of certain topic ----
topicwords <- c("鳥", "哺乳類", "爬虫類", "両生類", "魚", 
                "昆虫", "貝", "甲殻類", "植物", 
                "在来種", "外来種", 
                "被害")

topic_change_list <- vector("list", length = length(topicwords))
names(topic_change_list) <- topicwords

for (i in topicwords) {
  topic_change_list[[i]] <- 
    sapply(twtdata, function(x) length(grep(i, x[["text"]])))
}

topic_change <- matrix(Reduce(c, topic_change_list), 
                       nrow = length(topicwords), byrow = TRUE)
topic_change <- as.data.frame(topic_change)
names(topic_change) <- twtfile
topic_change$topic <- topicwords
topic_change <- topic_change[c("topic", twtfile)]
topic_change$topic <- factor(topic_change$topic, levels = topicwords)

ggplot(melt(topic_change, id = "topic")) + 
  geom_col(aes(variable, value)) + 
  facet_wrap(.~topic, scales = "free_y")

# Locations ----
loc_tab <- table(twtdata$jst202101.csv$author.location)
loc_df <- data.frame(location = names(loc_tab), num = as.numeric(loc_tab))
loc_df <- loc_df[order(loc_df$num, decreasing = TRUE), ]
loc_df_short <- head(loc_df, 30)
loc_df_short <- loc_df_short[
  loc_df_short$location != "" & 
    loc_df_short$location != "日本" & 
    loc_df_short$location != "Japan" & 
    loc_df_short$location != "めっちゃ面白いマダイの骨を組み立てる動画↓", 
]
ggplot(loc_df_short) + 
  geom_bar(aes(reorder(location, num), num), stat = "identity") + 
  coord_flip()




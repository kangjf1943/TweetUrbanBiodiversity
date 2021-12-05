library(readr)

# 输入用户自定义词典的词语条目
userdictionary <- 
  c("生物多様性", 
    "気候変動", 
    "外来種", 
    "環境保護", 
    "環境保全")
# 完善词典结构
userdictionary <- data.frame(
  userdictionary, 
  "-1","-1","1000","名詞","固有名詞","人名","名","*","*", 
  userdictionary, 
  "モトヒロ","モトヒロ"
)
# 输出为*.csv文件
write_csv(userdictionary, "kang.csv", col_names = FALSE)

# run following code in terminal to generate the user-defined dictionary
# /usr/local/Cellar/mecab/0.996/libexec/mecab/mecab-dict-index -d /usr/local/
# lib/mecab/dic/ipadic -u kangnewdic.dic -f utf8 -t utf8 /Users/VickyWang/R/
# TweetUrbanBiodiversity/kang.csv


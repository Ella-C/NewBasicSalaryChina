library(rvest)
library(showtext)
showtext_auto()
library(tidyverse)

path <- 'http://www.mohrss.gov.cn/SYrlzyhshbzb/laodongguanxi_/fwyd/202204/t20220408_442833.html'


df <- read_html(path,encoding = 'utf-8') %>% html_element('div[class="TRS_Editor"]') %>% html_table()

df[df==''] <- '0'

names(df) <- as.character(df[2,])

df1 <- df[-c(1,2),]

names(df1) <- c("地区","第一档(月薪)","第二档(月薪)","第三档(月薪)","第四档(月薪)",
                "第一档（时薪）","第二档（时薪）","第三档（时薪）","第四档（时薪）")

df1<- df1 %>% mutate_at(2:9,as.numeric)

df1 %>% glimpse

write_csv(df1,file='全国各地最新最低工资标准（2022-04-08）.csv')


for (i in 1:dim(df1)[1]){
    x <- df1 %>% filter(地区==df1[['地区']][i]) %>% t
    m <- rownames(x)[-1]
    x[-1,] %>% as.data.frame() %>% rename('salary'=".") %>%
        mutate(salary=as.numeric(salary)) %>% 
        ggplot(aes(m,salary))+geom_col() + coord_flip()+
        geom_text(aes(label=salary))+labs(title=paste(df1[['地区']][i],'最低工资标准',sep=''),tag='2022-04-08') 
    p <- png(paste(df1[['地区']][i],'最低工资标准.png',sep=''))
    dev.off()
}




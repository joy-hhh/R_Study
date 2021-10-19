if(!require(quantmod)){install.packages("quantmod");library(quantmod)}
library(ggplot2)

KOSPI <- getSymbols("^KS11",
                   from = "2016-01-01",
                   to = "2020-12-31",
                   # to = Sys.time(),
                   auto.assign = FALSE)

KOSDAQ <- getSymbols("^KQ11",
                   from = "2016-01-01",
                   to = "2020-12-31",
                   # to = Sys.time(),
                   auto.assign = FALSE)

head(KOSPI)
tail(KOSPI)

SEC <- getSymbols("005930.KS",
                  from = "2015-01-01",
                  to = "2021-01-01",
                  auto.assign = FALSE)


kq <- getSymbols("091990,KQ",
                 from = "2015-01-01",
                 to = "2021-01-01",
                 auto.assign = FALSE)



chartSeries(KOSPI)
str(KOSPI)

sample = data.frame(date = time(KOSPI),
                    KOSPI,
                    growth = ifelse(Cl(KOSPI) > Op(KOSPI), "up", "down"))


colnames(KOSPI)
colnames(sample) <- c("date", "Open", "High", "Low", "Close", "Volume", "Adjusted", "growth")

summary(sample)

ggplot(sample, aes(x = date)) +
  geom_line(aes(y = Low))

### KOSPI 시각화
   
ggplot(sample[sample$date >= "2020-01-01",], aes(x = date)) +
  geom_linerange(aes(ymin = Low, ymax = High)) +
  geom_rect(aes(xmin = date -0.3,
                xmax = date +0.3,
                ymin = pmin(Open, Close),
                ymax = pmax(Open, Close),
                fill = growth)) +
  guides(fill = "none") +
  scale_fill_manual(values = c("down" = "blue", "up" = "red"))+
  labs(
    title = "KOSPI",
    subtitle = "2020-01-01 ~ 2021-10-15"
  ) +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(hjust = 1),
        axis.title = element_blank(),
        axis.line.x.bottom = element_line(color = "grey"),
        axis.ticks = element_line(color = "grey"),
        axis.line.y.left = element_line(colour = "grey"),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"))



library(tqk)
code <- code_get()
code
sscode <- code[grep("^삼성전자$", code$name),3]
sscode
samsung <- tqk_get(sscode, from="2021-01-01")
samsung

kscode <- "^KS11"
KS11 <- tqk_get(kscode, from="2021-01-01")



## 파이프 (%>%) 사용 with dplyr
library(dplyr)
code_get() %>% 
  filter(grepl("^삼성전자$", name)) %>% 
  select(code) %>% 
  tqk_get(from = "2016-01-01") -> ss
ss <- ss %>% filter(date <= "2020-12-31")
ss$Stock <- ss$adjusted/lag(ss$adjusted) - 1
names(stm) <-c("Stock")

dates <- index(KOSPI)
KOSPI <- as_tibble(KOSPI)
KOSPI <- KOSPI %>% mutate(date = dates)
KOSPI <- KOSPI %>% mutate(ks = KS11.Adjusted /lag(KS11.Adjusted) - 1)

dates <- index(KOSDAQ)
KOSDAQ <- as_tibble(KOSDAQ)
KOSDAQ <- KOSDAQ %>% mutate(date = dates)
KOSDAQ <- KOSDAQ %>% mutate(ks = KS11.Adjusted /lag(KS11.Adjusted) - 1)



test <- KOSPI %>% select(date, KS11.Adjusted)
test_df <- left_join(ss, test)
test_df %>% filter(is.na(test_df$KS11.Adjusted))

start_date <- '2016-01-01'
end_date <- '2020-12-31'

library(reticulate)
fdr <- import("FinanceDataReader")
KOSPI <-　fdr$DataReader('ks11', start_date, end_date)                  
KOSPI <- KOSPI %>% mutate(rate = (Close /lag(Close) - 1))
samsung <-　fdr$DataReader('005930', start_date, end_date)                  
samsung <- samsung %>% mutate(rate = (Close /lag(Close) - 1))
Coeff <- lm(samsung$rate ~ KOSPI$rate)
Coeff[[1]][[2]]



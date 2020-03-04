## Date from https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/archived_data/archived_time_series/time_series_2019-ncov-Confirmed.csv

test <- read.csv("jh021920.csv", stringsAsFactors = FALSE)
library(lubridate)

header <- readLines("jh021920.csv", n = 1)

nms <- unlist(strsplit(header, split = ","))
dates <- mdy_hm(nms[-c(1:4)])

colnames(test)[-c(1:4)] <- as.character(dates)
library(dplyr)
library(tidyr)

out <- test %>% tidyr::pivot_longer(cols = -c(1:4), names_to = "full_date") %>%
    dplyr::mutate(full_date = lubridate::ymd_hms(full_date)) %>%
    dplyr::mutate(ymd = lubridate::date(full_date)) %>%
    dplyr::group_by(Province.State, Country.Region, ymd) %>%
    dplyr::summarize(count = max(c(0, value), na.rm = TRUE))


library(ggplot2)
ggplot(data = out %>% filter(Country.Region == "Mainland China"),
       aes(x = ymd, y = count, group = Province.State,
           col = Province.State)) + geom_point() + geom_line() +
    labs(x = "Date", y = "Cumulative Case Counts") +
    facet_wrap(~Province.State, scales = "free")

## via google
pops <- c("Anhui" = 62,
          "Beijing" = 22,
          "Chongqing" = 30,
          "Fujian" = 38.6,
          "Gansu" = 25.6,
          "Guangdong" = 113.5,
          "Guangxi" = 48.38,
          "Guizhou" = 34.8,
          "Hainan" = 9.3,
          "Hebei" = 74.7,
          "Heilongjiang" = 38.3,
          "Henan" = 94,
          "Hubei" = 58.5,
          "Hunan" = 63.4,
          "Inner Mongolia" = 24.7,
          "Jiangsu" = 80.4,
          "Jiangxi" = 45.2,
          "Jilin" = 27.5,
          "Liaoning" = 43.9,
          "Ningxia" = 6.3,
          "Qinghai" = 5.6,
          "Shaanxi" = 37.3,
          "Shandong" = 90,
          "Shanghai" = 23,
          "Shanxi" = 36.5,
          "Sichuan" = 81.1,
          "Tianjin" = 15.6,
          "Tibet" = 3.2,
          "Xinjiang" = 21.8,
          "Yunnan" = 46,
          "Zhejiang" = 57.4
          ) * 10^3 # changed because hard

pop_df <- as.data.frame(pops)
pop_df$Province.State <- as.character(rownames(pop_df))


china_ncov <- out %>% filter(Country.Region == "Mainland China")

china_ncov_df <- dplyr::left_join(china_ncov, pop_df, by = "Province.State")


## tomorrow  write function to use with nesting
china_ncov_df <- china_ncov_df %>%
    dplyr::mutate(lag = dplyr::lag(count, n = 1, default = 0)) %>%
    mutate(new_cases = count - lag,
               S = pops - count,
           I = new_cases, R = 0)
set.seed(2019)
gamma <- 1/3 ## 3 days infectious


get_IR <- function(df, gamma = 1/3){
    pop <- df$pops[1]
    for(ii in 2:nrow(df)){
        if(df$I[ii-1] > 0){
            df$R[ii] <- df$R[ii-1] +
                rbinom(n = 1, size = df$I[ii-1], prob = gamma)
        }
        df$I[ii] <- pop - df$S[ii] - df$R[ii]
    }
    return(df)
}

test <- china_ncov_df %>% filter(Province.State == "Hubei") %>%
    mutate(pops = pops)

out <- get_IR(test, gamma = 1/7)
gamma <- 1/7

set.seed(2020)
out <- china_ncov_df %>% group_by(Province.State) %>%
    tidyr::nest() %>%
    dplyr::mutate(update = purrr:::map(.data$data,
                                       get_IR,
                                       gamma = gamma)) %>%
    dplyr::select(-.data$data) %>%
    tidyr::unnest(cols = c(.data$update)) %>%
    select(-lag)


china_ncov19 <- out
max_rec <- china_ncov19 %>% group_by(Province.State) %>%
    summarize(maxR = max(R, na.rm = TRUE))

ggplot(data = china_ncov19 %>%
           filter(Province.State %in% c("Henan", "Hunan", "Hubei",
                                        "Guangdong", "Zhejiang")),
       aes(x = ymd, y = I, col = Province.State)) +
    geom_point() + geom_line() + facet_wrap(~Province.State, scale = "free") +
    theme_bw() +
    labs(x = "Date", y = "Count", title = "Imputed number of Infectious 2019-nCov cases in China")


china_ncov19_sub <- china_ncov19 %>%
           filter(Province.State %in% c("Henan","Hunan", "Hubei",
                                        "Guangdong", "Zhejiang"))

devtools::load_all()

ggplot(data = china_ncov19_sub,
       aes(x = S , y = I , z = R , col = Province.State,
           group = Province.State)) + coord_tern() + geom_point() +
    geom_line() + 
    facet_wrap(~Province.State) +
    theme_zoom_L(x = .1) + theme_bw()

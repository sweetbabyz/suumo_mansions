
#── ライブラリ読み込み ──────────────────────────────────────────────
# CRANミラーを設定
options(repos = c(CRAN = "https://cloud.r-project.org"))

# パッケージがインストールされていない場合のみインストール
if (!require("showtext", quietly = TRUE)) install.packages("showtext")
if (!require("janitor", quietly = TRUE)) install.packages("janitor")
if (!require("kableExtra", quietly = TRUE)) install.packages("kableExtra")
if (!require("viridis", quietly = TRUE)) install.packages("viridis")
library(showtext)
library(readr)
library(tidyverse)
library(lubridate)
library(data.table)
library(showtext)
library(sysfonts)
library(ggplot2)
library(broom)
library(modelr)
library(scales)
library(janitor)
library(knitr)
library(kableExtra)
library(viridis)
library(gridExtra)

font_add_google(name = "Noto Sans JP", family = "NotoJP")
showtext_auto()

#── データ読み込み ────────────────────────────────────────────────
df <- read_csv("suumo_mansions_final.csv")
               
df <- df %>%
  mutate(`バルコニー (m2)` = case_when(
    `バルコニー (m2)` == "なし" ~ 0, TRUE ~ parse_number(`バルコニー (m2)`)
    )
    )

desired_levels <- c("3DK","3LDK","3LDK+S（納戸）","4DK","4LDK","4LDK+S（納戸）","5LDK")

#── データ整形 ───────────────────────────────────────────────────
reference_date <- as.Date("2025-07-01")

df_clean <- df %>%
  # 1) 間取りから数値部屋数を抽出してフィルタ（3LDK以上）
  mutate(room_num = as.integer(str_extract(間取り, "\\d+"))) %>%
  
  # 2) 販売価格(万円) → 円
  mutate(price_yen = `販売価格 (万円)` * 10000) %>%
  
  # 3) 築年月 → 年月日型
  mutate(
    build_year  = as.integer(str_extract(築年月, "\\d{4}")),
    build_month = as.integer(str_extract(築年月, "(?<=年)\\d+")),
    build_date  = make_date(build_year, build_month, 1)
  ) %>%
  
  # 4) 築年数(年)
  mutate(age_years = as.numeric(interval(build_date, reference_date) / years(1))) %>%
  
  # 5) 徒歩分数・電車分数（すでに数値型の場合は不要）
  mutate(
    walk_min  = `最寄駅までの徒歩分数`,
    train_min = `東京駅までの分数`
  )
  


df2 <- df %>%
  filter(!is.na(間取り) & 間取り != "") %>%      # 欠損・空文字を除外
  mutate(
    間取り2 = if_else(
      間取り %in% desired_levels,
      間取り,          # desired_levels にあればそのまま
      "その他"         # それ以外は “その他”
    ),
    # 因子化して表示順を指定
    間取り2 = factor(間取り2, levels = c(desired_levels, "その他"))
  )

df_clean2 <- df_clean %>%
  mutate(total_commute = walk_min + train_min)

df_clean2 <- df_clean2 %>%
  mutate(
    `バルコニー (m2)` = ifelse(
      `バルコニー (m2)` == "なし", 
      0, 
      as.numeric(`バルコニー (m2)`)
    )
  )

df_clean2 <- df_clean2[!is.na(df_clean2$walk_min), ]

df_clean2 <- df_clean2[!is.na(df_clean2$train_min), ]

df_clean2 <- df_clean2 %>%
  filter(age_years > 0)

df_clean2 <- df_clean2 %>%
  rename(
    area =`専有面積 (m2)`,
    balcony = `バルコニー (m2)`
  )

# 3) カウント
madori_counts <- df2 %>%
  count(間取り2, name = "件数")

# 4) プロット
ggplot(madori_counts, aes(x = 間取り2, y = 件数)) +
  geom_col(fill = "tomato") +
  labs(
    title = "23区内の3LDK以上 中古マンション 間取り別物件数",
    x     = "間取り",
    y     = "物件数"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




#── 全景把握のためのヒストグラム作成 ────────────────────────────────────────────

# 価格分布
median_price <- median(df_clean2$price_yen, na.rm = TRUE)
price_25 <- quantile(df_clean2$price_yen, 0.25, na.rm = TRUE)
price_75 <- quantile(df_clean2$price_yen, 0.75, na.rm = TRUE)
price_95 <- quantile(df_clean2$price_yen, 0.95, na.rm = TRUE)  # NEW
ggplot(df_clean2, aes(x = price_yen)) +
  geom_histogram(binwidth = 10000000, fill = "steelblue", alpha = 0.7) +
  geom_vline(xintercept = median_price, color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = price_25, color = "darkgreen", linetype = "dotted", size = 1) +
  geom_vline(xintercept = price_75, color = "darkgreen", linetype = "dotted", size = 1) +
  scale_x_continuous(labels = comma) +
  labs(
    title = "２３区内の3LDK以上 中古マンション価格分布",
    x = "販売価格 (円)",
    y = "物件数"
  ) +
  coord_cartesian(xlim = c(0, price_95))  +
  #annotate("text", x = median_price, y = Inf, label = "中央値", vjust = -0.5, hjust = -0.1, color = "red") +
  #annotate("text", x = price_95, y = Inf, label = "95パーセンタイル", vjust = -0.5, hjust = -0.1, color = "darkgreen") +
  theme_minimal()

# 専有面積分布
median_area <- median(df_clean2$area, na.rm = TRUE)
area_95 <- quantile(df_clean2$area, 0.95, na.rm = TRUE)
area_25 <- quantile(df_clean2$area, 0.25, na.rm = TRUE)
area_75 <- quantile(df_clean2$area, 0.75, na.rm = TRUE)
ggplot(df_clean2, aes(x = area)) +
  geom_histogram(binwidth = 5, fill = "tomato", alpha = 0.7) +
  geom_vline(xintercept = median_area, color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = area_25, color = "darkgreen", linetype = "dotted", size = 1) +
  geom_vline(xintercept = area_75, color = "darkgreen", linetype = "dotted", size = 1) +
  scale_x_continuous(labels = comma) +
  labs(title = "２３区内の3LDK以上 中古マンション専有面積の分布",
       x = "専有面積 (m²)", y = "物件数") +
  coord_cartesian(xlim = c(min(df_clean2$area), area_97))  +
  theme_minimal()

# 築年数分布
median_age <- median(df_clean2$age_years, na.rm = TRUE)
age_25 <- quantile(df_clean2$age_years, 0.25, na.rm = TRUE)
age_75 <- quantile(df_clean2$age_years, 0.75, na.rm = TRUE)
age_95 <- quantile(df_clean2$age_years, 0.95, na.rm = TRUE)
ggplot(df_clean2, aes(x = age_years)) +
  geom_histogram(binwidth = 2, fill = "seagreen", alpha = 0.7) +
  geom_vline(xintercept = median_age, color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = age_25, color = "darkgreen", linetype = "dotted", size = 1) +
  geom_vline(xintercept = age_75, color = "darkgreen", linetype = "dotted", size = 1) +
  scale_x_continuous(labels = comma) +
  labs(title = "２３区内の3LDK以上 中古マンション築年数の分布",
       x = "築年数 (年)", y = "物件数") +
  theme_minimal()

# 駅徒歩時間分布
median_walk <- median(df_clean2$walk_min, na.rm = TRUE)
walk_25 <- quantile(df_clean2$walk_min, 0.25, na.rm = TRUE)
walk_75 <- quantile(df_clean2$walk_min, 0.75, na.rm = TRUE)
walk_95 <- quantile(df_clean2$walk_min, 0.95, na.rm = TRUE)
ggplot(df_clean2, aes(x = walk_min)) +
  geom_histogram(binwidth = 1, fill = "orchid", alpha = 0.7) +
  geom_vline(xintercept = median_walk, color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = walk_25, color = "darkgreen", linetype = "dotted", size = 1) +
  geom_vline(xintercept = walk_75, color = "darkgreen", linetype = "dotted", size = 1) +
  scale_x_continuous(labels = comma) +
  labs(title = "最寄駅までの徒歩時間分布",
       x = "徒歩時間 (分)", y = "物件数") +
  theme_minimal()

# 電車所要時間分布
median_train <- median(df_clean2$train_min, na.rm = TRUE)
train_25 <- quantile(df_clean2$train_min, 0.25, na.rm = TRUE)
train_75 <- quantile(df_clean2$train_min, 0.75, na.rm = TRUE)
train_95 <- quantile(df_clean2$train_min, 0.95, na.rm = TRUE)
ggplot(df_clean2, aes(x = train_min)) +
  geom_histogram(binwidth = 2, fill = "gold", alpha = 0.7) +
  geom_vline(xintercept = median_train, color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = train_25, color = "darkgreen", linetype = "dotted", size = 1) +
  geom_vline(xintercept = train_75, color = "darkgreen", linetype = "dotted", size = 1) +
  scale_x_continuous(labels = comma) +
  labs(title = "東京駅までの電車所要時間分布",
       x = "電車所要時間 (分)", y = "物件数") +
  theme_minimal()

# バルコニー面積分布
median_balcony <- median(df_clean2$balcony, na.rm = TRUE)
balcony_95 <- quantile(df_clean2$balcony, 0.95, na.rm = TRUE)
balcony_25 <- quantile(df_clean2$balcony, 0.25, na.rm = TRUE)
balcony_75 <- quantile(df_clean2$balcony, 0.75, na.rm = TRUE)
ggplot(df_clean2, aes(x = balcony)) +
  geom_histogram(binwidth = 1, fill = "black", alpha = 0.7) +
  geom_vline(xintercept = median_balcony, color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = balcony_25, color = "darkgreen", linetype = "dotted", size = 1) +
  geom_vline(xintercept = balcony_75, color = "darkgreen", linetype = "dotted", size = 1) +
  scale_x_continuous(labels = comma) +
  labs(title = "２３区内の3LDK以上 中古マンションバルコニー面積の分布",
       x = "バルコニー面積 (m²)", y = "物件数") +
  coord_cartesian(xlim = c(min(df_clean2$balcony), balcony_97))  +
  theme_minimal()

# 通勤所要時間分布
median_commute <- median(df_clean2$total_commute, na.rm = TRUE)
commute_95 <- quantile(df_clean2$total_commute, 0.95, na.rm = TRUE)
ggplot(df_clean2, aes(x = total_commute)) +
  geom_histogram(binwidth = 2, fill = "darkblue", alpha = 0.7) +
  geom_vline(xintercept = median_commute, color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = commute_95, color = "darkgreen", linetype = "dotted", size = 1) +
  scale_x_continuous(labels = comma) +
  labs(title = "東京駅までの通勤所要時間分布",
       x = "通勤所要時間 (分)", y = "物件数") +
  annotate("text", x = median_commute, y = Inf, label = "中央値", vjust = -0.5, hjust = -0.1, color = "red") +
  annotate("text", x = commute_95, y = Inf, label = "95パーセンタイル", vjust = -0.5, hjust = -0.1, color = "darkgreen") +
  theme_minimal()

#── 散布図作成 ────────────────────────────────────────────

# 東京駅までの分数　vs 販売価格
ggplot(df_clean2, aes(x = train_min, y = price_yen)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  labs(
    title = "最寄駅から東京駅までの所要時間 vs 販売価格",
    x     = "最寄駅から東京駅までの所要時間（分）",
    y     = "販売価格（万円）"
  ) +
  theme_minimal()

# 通勤時間　vs 販売価格
ggplot(df_clean2, aes(x = total_commute, y = price_yen)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  labs(
    title = "東京駅までの合計通勤時間 vs 販売価格",
    x     = "東京駅までの合計所要時間（分）",
    y     = "販売価格（万円）"
  ) +
  theme_minimal()

# 最寄駅までの徒歩分数　vs 販売価格
ggplot(df_clean2, aes(x = walk_min, y = price_yen)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  labs(
    title = "最寄駅までの徒歩分数 vs 販売価格",
    x     = "最寄駅までの徒歩分数（分）",
    y     = "販売価格（万円）"
  ) +
  theme_minimal()

# 専有面積　vs 販売価格
ggplot(df_clean2, aes(x = `専有面積 (m2)`, y = price_yen)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  labs(
    title = "専有面積 vs 販売価格",
    x     = "専有面積 (m²)",
    y     = "販売価格（万円）"
  ) +
  theme_minimal()

# 築年数　vs 販売価格
ggplot(df_clean2, aes(x = age_years, y = price_yen)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  labs(
    title = "築年数 vs 販売価格",
    x     = "築年数",
    y     = "販売価格（万円）"
  ) +
  theme_minimal()

# 
ggplot(df_clean2, aes(x = age_years, y = room_num)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  labs(
    title = "バルコニーがなぜ不適か",
    x     = "",
    y     = ""
  ) +
  theme_minimal()

# 通勤時間が30分未満かつ１億円未満の物件で面積と築年数の散布図 - 忙しい価格重視
df_filter_commute_price <- df_clean2 %>%
  filter(total_commute < 30, price_yen < 200000000)
ggplot(df_filter_commute_price, aes(x = age_years, y = `専有面積 (m2)`)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_vline(xintercept = median(df_filter_commute_price$age_years, na.rm = TRUE), linetype="dashed", color="blue") +
  geom_hline(yintercept = median(df_filter_commute_price$`専有面積 (m2)`, na.rm = TRUE), linetype="dashed", color="red") +
  labs(
    title = "通勤時間が30分未満かつ１億円未満の物件(1671件)の面積と築年数",
    x     = "築年数",
    y     = "専有面積 (m²)"
  ) +
  theme_minimal()

# 通勤時間が30分未満かつ築年数が10年未満の物件で面積と価格の散布図 - 忙しい綺麗好き
df_filter_commute_age <- df_clean2 %>%
  filter(total_commute < 30, age_years < 10)
ggplot(df_filter_commute_age, aes(x = `専有面積 (m2)`, y = price_yen)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_vline(xintercept = median(df_filter_commute_age$`専有面積 (m2)`, na.rm = TRUE), linetype="dashed", color="blue") +
  geom_hline(yintercept = median(df_filter_commute_age$price_yen, na.rm = TRUE), linetype="dashed", color="red") +
  labs(
    title = "通勤時間が30分未満かつ築年数が10年未満の物件(555件)の面積と価格",
    x     = "専有面積 (m²)",
    y     = "販売価格（万円）"
  ) +
  theme_minimal()

# 通勤時間が30分未満かつ部屋数が4以上の物件で面積と価格 - 忙しい子持ち
df_filter_commute_room <- df_clean2 %>%
  filter(total_commute < 30, room_num >= 4)
ggplot(df_filter_commute_room, aes(x = `専有面積 (m2)`, y = price_yen)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_vline(xintercept = median(df_filter_commute_room$`専有面積 (m2)`, na.rm = TRUE), linetype="dashed", color="blue") +
  geom_hline(yintercept = median(df_filter_commute_room$price_yen, na.rm = TRUE), linetype="dashed", color="red") +
  labs(
    title = "通勤時間が30分未満かつ部屋数が4以上の物件(136件)の面積と価格",
    x     = "専有面積 (m²)",
    y     = "販売価格（万円）"
  ) +
  theme_minimal()

# 築年数が10年未満かつ部屋数が4以上かつ専有面積が100m²以上の物件で最寄駅までの徒歩分数と価格の散布図 - 子持ちインドア
df_filter_age_room_area <- df_clean2 %>%
  filter(age_years < 10, room_num >= 4, `専有面積 (m2)` > 100)
ggplot(df_filter_age_room_area, aes(x = walk_min, y = price_yen)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_vline(xintercept = median(df_filter_age_room_area$walk_min, na.rm = TRUE), linetype="dashed", color="blue") +
  geom_hline(yintercept = median(df_filter_age_room_area$price_yen, na.rm = TRUE), linetype="dashed", color="red") +
  labs(
    title = "築年数が10年未満、部屋数が4以上、専有面積が100m²以上の物件(19件)の利便性と価格",
    x     = "最寄駅までの徒歩分数",
    y     = "販売価格（万円）"
  ) +
  theme_minimal()

# 通勤時間が30分未満かつ価格が3000万円以内の物件で面積と築年数 - 若い働き盛り
df_filter_commute_price1 <- df_clean2 %>%
  filter(total_commute < 30, price_yen <= 30000000)
ggplot(df_filter_commute_price1, aes(x = `専有面積 (m2)`, y = age_years)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_vline(xintercept = median(df_filter_commute_price1$`専有面積 (m2)`, na.rm = TRUE), linetype="dashed", color="blue") +
  geom_hline(yintercept = median(df_filter_commute_price1$age_years, na.rm = TRUE), linetype="dashed", color="red") +
  labs(
    title = " 通勤時間が30分未満かつ価格が3000万円以内の物件(8件)で面積と築年数",
    x     = "専有面積 (m²)",
    y     = "築年数"
  ) +
  theme_minimal()

# 通勤時間が30分未満かつ価格が7000万円以内の物件で面積と築年数 - 若い働き盛り
df_filter_commute_price2 <- df_clean2 %>%
  filter(total_commute < 30, price_yen <= 70000000)
ggplot(df_filter_commute_price, aes(x = `専有面積 (m2)`, y = age_years)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_vline(xintercept = median(df_filter_commute_price$`専有面積 (m2)`, na.rm = TRUE), linetype="dashed", color="blue") +
  geom_hline(yintercept = median(df_filter_commute_price$age_years, na.rm = TRUE), linetype="dashed", color="red") +
  labs(
    title = " 通勤時間が30分未満かつ価格が7000万円以内の物件(8件)で面積と築年数",
    x     = "専有面積 (m²)",
    y     = "築年数"
  ) +
  theme_minimal()

#── クロス集計表 ────────────────────────────────────────────

# 築年数
price_breaks <- seq(
  floor(min(df_clean2$price_yen, na.rm = TRUE) / 5) * 5,
  ceiling(price_95 / 5) * 5,
  by = 10000000
)
age_breaks <- seq(
  floor(min(df_clean2$age_years, na.rm = TRUE) / 5) * 5,
  ceiling(max(df_clean2$age_years, na.rm = TRUE) / 5) * 5,
  by = 5
)
df_cross_age <- df_clean2 %>%
  mutate(
    price_bin = cut(price_yen,
                    breaks = price_breaks,
                    include.lowest = TRUE),
    age_bin   = cut(age_years,
                    breaks = age_breaks,
                    include.lowest = TRUE)
  )
count_df_age <- df_cross_age %>%
  count(age_bin, price_bin)
count_df_age <- count_df_age %>%
  complete(age_bin, price_bin, fill = list(n = 0))
count_df_age <- count_df_age %>%
  filter(!is.na(price_bin))
ggplot(count_df_age, aes(x = age_bin, y = price_bin, fill = n)) +
  geom_tile(color = "black") +
  geom_text(aes(label = n), color = "black", size = 5) +
  scale_fill_gradient(low = "lightyellow", high = "red", name = "件数") +
  labs(
    title = "築年ビン × 価格ビン のクロス集計表",
    x     = "築年数ビン",
    y     = "価格ビン"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid  = element_blank()
  )

# 専有面積
price_breaks <- seq(
  floor(min(df_clean2$price_yen, na.rm = TRUE) / 5) * 5,
  ceiling(price_95 / 5) * 5,
  by = 10000000
)
area_breaks <- seq(
  floor(min(df_clean2$area, na.rm = TRUE) / 5) * 5,
  ceiling(area_95 / 5) * 5,
  by = 5
)
df_cross_area <- df_clean2 %>%
  mutate(
    price_bin = cut(price_yen,
                    breaks = price_breaks,
                    include.lowest = TRUE),
    area_bin   = cut(area,
                    breaks = area_breaks,
                    include.lowest = TRUE)
  )
count_df_area <- df_cross_area %>%
  count(area_bin, price_bin)
count_df_area <- count_df_area %>%
  complete(area_bin, price_bin, fill = list(n = 0))
count_df_area <- count_df_area %>%
  filter(!is.na(price_bin), !is.na(area_bin))
ggplot(count_df_area, aes(x = area_bin, y = price_bin, fill = n)) +
  geom_tile(color = "black") +
  geom_text(aes(label = n), color = "black", size = 5) +
  scale_fill_gradient(low = "lightyellow", high = "red",name = "件数") +
  labs(
    title = "専有面積ビン × 価格ビン のクロス集計表",
    x     = "専有面積(m²)ビン",
    y     = "価格ビン"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid  = element_blank()
  )

# バルコニー面積
price_breaks <- seq(
  floor(min(df_clean2$price_yen, na.rm = TRUE) / 5) * 5,
  ceiling(price_95 / 5) * 5,
  by = 10000000
)
balcony_breaks <- seq(
  floor(min(df_clean2$balcony, na.rm = TRUE) / 5) * 5,
  ceiling(balcony_95 / 5) * 5,
  by = 2
)
df_cross_balcony <- df_clean2 %>%
  mutate(
    price_bin = cut(price_yen,
                    breaks = price_breaks,
                    include.lowest = TRUE),
    balcony_bin   = cut(balcony,
                     breaks = balcony_breaks,
                     include.lowest = TRUE)
  )
count_df_balcony <- df_cross_balcony %>%
  count(balcony_bin, price_bin)
count_df_balcony <- count_df_balcony %>%
  complete(balcony_bin, price_bin, fill = list(n = 0))
count_df_balcony <- count_df_balcony %>%
  filter(!is.na(price_bin), !is.na(balcony_bin))
ggplot(count_df_balcony, aes(x = balcony_bin, y = price_bin, fill = n)) +
  geom_tile(color = "white") +
  geom_text(aes(label = n), color = "red", size = 5) +
  scale_fill_viridis_c(name = "件数", option = "D") +
  labs(
    title = "バルコニー面積ビン × 価格ビン のクロス集計表",
    x     = "バルコニー面積(m²)ビン",
    y     = "価格ビン"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid  = element_blank()
  )

# 部屋数
df_room <- df_clean2%>%
  mutate(room_bin = cut(
    room_num,
    breaks = c(2, 3, 4, Inf),  # Note: lower bound must be below your min
    labels = c("3", "4", "5+"),
    right = TRUE,
    include.lowest = TRUE
  ))
price_breaks <- seq(
  floor(min(df_clean2$price_yen, na.rm = TRUE) / 5) * 5,
  ceiling(price_95 / 5) * 5,
  by = 10000000
)
df_cross_room <- df_room %>%
  mutate(
    price_bin = cut(price_yen,
                    breaks = price_breaks,
                    include.lowest = TRUE)
  )
count_df_room <- df_cross_room %>%
  count(room_bin, price_bin)
count_df_room <- count_df_room %>%
  complete(room_bin, price_bin, fill = list(n = 0))
count_df_room <- count_df_room %>%
  filter(!is.na(price_bin))
ggplot(count_df_room, aes(x = room_bin, y = price_bin, fill = n)) +
  geom_tile(color = "black") +
  geom_text(aes(label = n), color = "black", size = 5) +
  scale_fill_gradient(low = "lightyellow", high = "red", name = "件数") +
  labs(
    title = "部屋数ビン × 価格ビン のクロス集計表",
    x     = "部屋数ビン",
    y     = "価格ビン"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1),
    panel.grid  = element_blank()
  )

# 最寄駅までの徒歩
price_breaks <- seq(
  floor(min(df_clean2$price_yen, na.rm = TRUE) / 5) * 5,
  ceiling(price_95 / 5) * 5,
  by = 10000000
)
walk_breaks <- seq(
  floor(min(df_clean2$walk_min, na.rm = TRUE) / 5) * 5,
  ceiling(walk_95 / 5) * 5,
  by = 2
)
df_cross_walk <- df_clean2 %>%
  mutate(
    price_bin = cut(price_yen,
                    breaks = price_breaks,
                    include.lowest = TRUE),
    walk_bin   = cut(walk_min,
                    breaks = walk_breaks,
                    include.lowest = TRUE)
  )
count_df_walk <- df_cross_walk %>%
  count(walk_bin, price_bin)
count_df_walk <- count_df_walk %>%
  complete(walk_bin, price_bin, fill = list(n = 0))
count_df_walk <- count_df_walk %>%
  filter(!is.na(price_bin), !is.na(walk_bin))
ggplot(count_df_walk, aes(x = walk_bin, y = price_bin, fill = n)) +
  geom_tile(color = "black") +
  geom_text(aes(label = n), color = "black", size = 5) +
  scale_fill_gradient(low = "lightyellow", high = "red", name = "件数") +
  labs(
    title = "最寄駅までの徒歩分数ビン × 価格ビン のクロス集計表",
    x     = "最寄駅までの徒歩分数ビン",
    y     = "価格ビン"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid  = element_blank()
  )

# 最寄駅から東京駅までの所要時間
price_breaks <- seq(
  floor(min(df_clean2$price_yen, na.rm = TRUE) / 5) * 5,
  ceiling(price_95 / 5) * 5,
  by = 10000000
)
train_breaks <- seq(
  floor(min(df_clean2$train_min, na.rm = TRUE) / 5) * 5,
  57,
  by = 3
)
df_cross_train <- df_clean2 %>%
  mutate(
    price_bin = cut(price_yen,
                    breaks = price_breaks,
                    include.lowest = TRUE),
    train_bin   = cut(train_min,
                     breaks = train_breaks,
                     include.lowest = TRUE)
  )
count_df_train <- df_cross_train %>%
  count(train_bin, price_bin)
count_df_train <- count_df_train %>%
  complete(train_bin, price_bin, fill = list(n = 0))
count_df_train <- count_df_train %>%
  filter(!is.na(price_bin))
ggplot(count_df_train, aes(x = train_bin, y = price_bin, fill = n)) +
  geom_tile(color = "black") +
  geom_text(aes(label = n), color = "black", size = 5) +
  scale_fill_gradient(low = "lightyellow", high = "red", name = "件数") +
  labs(
    title = "最寄駅から東京駅までの所要分数ビン × 価格ビン のクロス集計表",
    x     = "最寄駅から東京駅までの所要分数ビン",
    y     = "価格ビン"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid  = element_blank()
  )

df_cross_ba <- df_clean2 %>%
  mutate(
    balcony_bin = cut(balcony,
                  breaks = balcony_breaks,
                  include.lowest = TRUE),
    area_bin = cut(area,
                   breaks = area_breaks,
                   include.lowest = TRUE)
  )
count_df_ba <- df_cross_ba %>%
  count(balcony_bin, area_bin)
count_df_ba <- count_df_ba %>%
  complete(balcony_bin, area_bin, fill = list(n = 0))
count_df_ba <- count_df_ba %>%
  filter(!is.na(balcony_bin), !is.na(area_bin))
ggplot(count_df_ba, aes(x = balcony_bin, y = area_bin, fill = n)) +
  geom_tile(color = "black") +
  geom_text(aes(label = n), color = "black", size = 5) +
  scale_fill_gradient(low = "lightyellow", high = "red", name = "件数") +
  labs(
    title = "バルコニー面積ビン × 専有面積ビン のクロス集計表",
    x     = "バルコニー面積(m²)ビン",
    y     = "専有面積(m²)ビン"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid  = element_blank()
  )

#── 回帰分析 ────────────────────────────────────────────
balcony_75 <- quantile(df_clean2$balcony, 0.75, na.rm = TRUE)

df_balcony <- df_clean2 %>%
  filter(balcony < balcony_75)

model <- lm(log(price_yen) ~ log(age_years) + log(walk_min) + log(train_min) + area, data = df_clean2)
tidy(model)
summary(model)

fixed_vals <- df_clean2 %>%
  summarize(
    walk_min = median(walk_min, na.rm = TRUE),
    train_min = median(train_min, na.rm = TRUE),
    age_years = median(age_years, na.rm = TRUE),
    area      = median(area, na.rm = TRUE)
  )

# 各変数の範囲を作成
age_seq   <- seq(min(df_clean2$age_years,   na.rm = TRUE),
                 max(df_clean2$age_years,   na.rm = TRUE),
                 length.out = 100)
walk_seq  <- seq(min(df_clean2$walk_min,     na.rm = TRUE),
                 max(df_clean2$walk_min,     na.rm = TRUE),
                 length.out = 100)
train_seq <- seq(min(df_clean2$train_min,    na.rm = TRUE),
                 max(df_clean2$train_min,    na.rm = TRUE),
                 length.out = 100)
area_seq <- seq(
  min(df_clean2$area, na.rm=TRUE),
  area_95,
  length.out = 100
)

# 新データフレームを作成し、予測・exp()で戻す
new_age <- tibble(
  age_years = age_seq,
  walk_min  = fixed_vals$walk_min,
  train_min = fixed_vals$train_min,
  area      = fixed_vals$area
) %>%
  mutate(
    pred_log   = predict(model, newdata = .),
    pred_price = exp(pred_log)
  )
new_walk <- tibble(
  age_years = fixed_vals$age_years,
  walk_min  = walk_seq,
  train_min = fixed_vals$train_min,
  area      = fixed_vals$area
) %>%
  mutate(
    pred_log   = predict(model, newdata = .),
    pred_price = exp(pred_log)
  )
new_train <- tibble(
  age_years = fixed_vals$age_years,
  walk_min  = fixed_vals$walk_min,
  train_min = train_seq,
  area      = fixed_vals$area
) %>%
  mutate(
    pred_log   = predict(model, newdata = .),
    pred_price = exp(pred_log)
  )
new_area <- tibble(
  age_years = fixed_vals$age_years,
  walk_min  = fixed_vals$walk_min,
  train_min = fixed_vals$train_min,
  area      = area_seq
) %>%
  mutate(
    pred_log   = predict(model, newdata = .),
    pred_price = exp(pred_log)
  )

# プロットを作成
p1 <- ggplot(new_age, aes(x = age_years, y = pred_price)) +
  geom_line(size = 1) +
  labs(title = "築年数 vs 予想価格",
       x     = "築年数（年）",
       y     = "予想価格（円）") +
  theme_minimal()

p2 <- ggplot(new_walk, aes(x = walk_min, y = pred_price)) +
  geom_line(size = 1) +
  labs(title = "徒歩分数 vs 予想価格",
       x     = "最寄駅までの徒歩分数（分）",
       y     = "予想価格（円）") +
  theme_minimal()

p3 <- ggplot(new_train, aes(x = train_min, y = pred_price)) +
  geom_line(size = 1) +
  labs(title = "電車所要時間 vs 予想価格",
       x     = "東京駅までの電車所要時間（分）",
       y     = "予想価格（円）") +
  theme_minimal()

p4 <- ggplot(new_area, aes(x = area, y = pred_price)) +
  geom_line(size = 1) +
  labs(title = "専有面積 vs 予想価格",
       x     = "専有面積（m²）",
       y     = "予想価格（円）") +
  theme_minimal()

# 4つ並べて表示
grid.arrange(p1, p2, p3, p4, ncol = 4)

#── 最安提示 ────────────────────────────────────────────

df_filter_busy <- df_clean2 %>%
  filter(train_min < 15, walk_min <= 5)

df_filter_kids <- df_clean2 %>%
  filter(room_num > 3, age_years < 15)

df_filter_hybrid <- df_clean2 %>%
  filter(balcony >= 20, area >= 80)

#── 割安提示 ────────────────────────────────────────────

df_final <- df_clean2 %>%
  mutate(
    predicted_log_price = predict(model),
    predicted_price = exp(predicted_log_price),
    price_ratio = price_yen / predicted_price # 1以上 → 理論価格より実価格が安い
  )

df_busy <- df_clean2 %>%
  filter(train_min < 15, walk_min <= 5)

df_kids <- df_clean2 %>%
  filter(room_num > 3, age_years < 15)

df_hybrid <- df_clean2 %>%
  filter(room_num >= 4, area >= 100)

df_busycomp <- df_final %>%
  filter(total_commute < 25, walk_min <= 7)

df_kidscomp <- df_final %>%
  filter(room_num > 3, age_years < 20)

df_hybridcomp <- df_final %>%
  filter(room_num >= 4, area >= 85)

df_busycheap <- df_busy %>%
  filter(price_yen < mean(df_busy$price_yen))

df_kidscheap <- df_kids %>%
  filter(price_yen < mean(df_kids$price_yen))

df_hybridcheap <- df_hybrid %>%
  filter(price_yen < mean(df_hybrid$price_yen))

# 価格分布 busy
median_busyprice <- median(df_busy$price_yen, na.rm = TRUE)
busyprice_97 <- quantile(df_busy$price_yen, 0.97, na.rm = TRUE)
ggplot(df_busy, aes(x = price_yen)) +
  geom_histogram(binwidth = 20000000, fill = "steelblue", alpha = 0.7) +
  geom_vline(xintercept = median_busyprice, color = "red", linetype = "dashed", size = 1) +
  scale_x_continuous(labels = comma) +
  labs(
    title = "電車所要時間が15分未満かつ最寄駅までの徒歩分数が５分以内の物件の価格分布",
    x = "販売価格 (円)",
    y = "物件数"
  ) +
  coord_cartesian(xlim = c(min(df_busy$price_yen), busyprice_97))  +
  theme_minimal()

# 価格分布 kids
median_kidprice <- median(df_kids$price_yen, na.rm = TRUE)
kidprice_99 <- quantile(df_kids$price_yen, 0.99, na.rm = TRUE)
ggplot(df_kids, aes(x = price_yen)) +
  geom_histogram(binwidth = 20000000, fill = "steelblue", alpha = 0.7) +
  geom_vline(xintercept = median_kidprice, color = "red", linetype = "dashed", size = 1) +
  scale_x_continuous(labels = comma) +
  labs(
    title = "間取りが４DK以上かつ築年数が15年未満の物件の価格分布",
    x = "販売価格 (円)",
    y = "物件数"
  ) +
  coord_cartesian(xlim = c(min(df_kids$price_yen), kidprice_99))+
  theme_minimal()

# 価格分布 hybrid
median_hybridprice <- median(df_hybrid$price_yen, na.rm = TRUE)
hybridprice_95 <- quantile(df_hybrid$price_yen, 0.95, na.rm = TRUE)
ggplot(df_hybrid, aes(x = price_yen)) +
  geom_histogram(binwidth = 20000000, fill = "steelblue", alpha = 0.7) +
  geom_vline(xintercept = median_hybridprice, color = "red", linetype = "dashed", size = 1) +
  scale_x_continuous(labels = comma) +
  labs(
    title = "間取りが４DK以上かつ専有面積が100m²以上の物件の価格分布",
    x = "販売価格 (円)",
    y = "物件数"
  ) +
  coord_cartesian(xlim = c(min(df_hybrid$price_yen), hybridprice_95))+
  theme_minimal()


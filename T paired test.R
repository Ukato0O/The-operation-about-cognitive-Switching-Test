library(effectsize)
library(broom)
library(dplyr)
library(openxlsx)

data = read.csv(file.choose(), header = T, sep = ",")
High = data |> dplyr::filter(教育水準分類 == "H") 
Low = data |> dplyr::filter(教育水準分類 == "L")

descriptive = tibble(Condition = c("前測反應時間差", "後測反應時間差"), 
                Mean = c(mean(data$前_RT_delta), mean(data$後_RT_delta)),
                SD = c(sd(data$前_RT_delta), sd(data$後_RT_delta)),
                N = c(sum(!is.na(data$前_RT_delta)), sum(!is.na(data$後_RT_delta))))
write.xlsx(ttable, "C:/Users/User/Desktop/高齡專題/數據/Switching/ttable.xlsx")

apa.t = t.test(data$前_RT_delta, data$後_RT_delta, paired = TRUE)
d = cohens_d(data$前_RT_delta, data$後_RT_delta, paired = TRUE)
ttable = tidy(apa.t)
result = tibble("Paired T-Test" = "前後測切換成本", 
                t = round(ttable$statistic, 3), 
                df = ttable$parameter,
                p = round(ttable$p.value, 3),
                "Cohen's d" = round(d$Cohens_d, 3))
write.xlsx(result, "C:/Users/User/Desktop/高齡專題/數據/Switching/ttable.xlsx")

data$教育水準分類 = as.factor(data$教育水準分類) 
t = t.test(data$Rtcost ~ data$教育水準分類, paired = F)
d2 = cohens_d(data$Rtcost ~ data$教育水準分類, paired = F)
ttable2 = tidy(t)
result2 = tibble("Independent T-Test" = "教育水準間前後測切換成本", 
                t = round(ttable2$statistic, 3), 
                df = round(ttable2$parameter, 3),
                p = round(ttable2$p.value, 3),
                "Cohen's d" = round(d2$Cohens_d, 3))
write.xlsx(result2, "C:/Users/User/Desktop/高齡專題/數據/Switching/ttable.xlsx")

edu = tibble( order = c("前測切換成本", "後測切換成本", "前測切換成本", "後測切換成本"),
              學歷 = c("國小", "國小", "國中以上", "國中以上"),
              mean = c(mean(Low$前_RT_delta), mean(Low$後_RT_delta), mean(High$前_RT_delta), mean(High$後_RT_delta)),
              se = c(sd(Low$前_RT_delta) /sqrt(length(Low$前_RT_delta)), sd(Low$後_RT_delta) /sqrt(length(Low$後_RT_delta)), sd(High$前_RT_delta) /sqrt(length(High$前_RT_delta)), sd(High$後_RT_delta) /sqrt(length(High$後_RT_delta)))
)
edu$order_num <- ifelse(edu$order == "前測切換成本", 1, 2)
##
# Generate an APA compatible interaction plot
##
library(tidyverse)
library(ggeffects)
library(jtools)
library(forcats)
install.packages(afex)
library(afex)
library(ggpubr)

dv_label = "切換成本"
legend_pos = "bottomright"

p <- ggplot(edu, aes(x = order, y = mean, shape = 學歷, group = 學歷)) +
    geom_point(size = 2) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.1, position = position_dodge(width = 0.03)) +
    geom_line(aes(linetype = 學歷)) +
    labs(x = '', y = dv_label) +
    ylim(-110, 170) +
    theme_apa(legend.pos = all_of(legend_pos)) +
    scale_colour_grey() +
    theme(legend.background = element_rect(fill = "white", colour = "black"),
          axis.text = element_text(size=12, colour = "black")) +
    labs(title = "教育水準分類與前後切換成本之關係") +
    theme(plot.title = element_text(hjust = 0.5))
# Output the plot
p
##
edu2 = read.csv(file.choose(), header = T)
dv_label = "切換成本(ms)"
legend_pos = "bottomright"
edu2$attendance <- factor(edu2$attendance)

p2 <- ggplot(edu2, aes(x = order, y = mean, shape = attendance, group = attendance)) +
    geom_point(size = 2) +
    geom_line(aes(linetype = attendance)) +
    labs(x = '', y = dv_label) +
    ylim(-200,210) +
    theme_apa(legend.pos = all_of(legend_pos)) +
    scale_colour_grey() +
    theme(legend.background = element_rect(fill = "white", colour = "black"),
          axis.text = element_text(size=12, colour = "black")) +
    labs(title = "出席次數與前後切換成本之關係") +
    theme(plot.title = element_text(hjust = 0.5))
# Output the plot
p2
##
pic = ggplot(data, aes(x = 教育水準, y = Rtcost)) +
    geom_point(size = 2) +
    geom_smooth(method = "lm", se = F, size = 0.5) +
    stat_cor(label.x = 10, label.y = 0) +
    theme_apa(legend.pos = all_of(legend_pos)) +
    scale_colour_grey() +
    theme(legend.background = element_rect(fill = "white", colour = "black"),
          axis.text = element_text(size=12, colour = "black")) +
    labs(title = "教育水準與切換成本改變之相關", y = "切換成本差異(ms)") +
    theme(plot.title = element_text(hjust = 0.5)) 

pic

pic2 = ggplot(data, aes(x = 出席次數, y = Rtcost)) +
    geom_point(size = 2) +
    geom_smooth(method = "lm", se = F, size = 0.5) +
    stat_cor(label.x = 6, label.y = -80) +
    theme_apa(legend.pos = all_of(legend_pos)) +
    scale_colour_grey() +
    theme(legend.background = element_rect(fill = "white", colour = "black"),
          axis.text = element_text(size=12, colour = "black")) +
    labs(title = "出席次數與切換成本改變之相關", y = "切換成本差異(ms)") +
    theme(plot.title = element_text(hjust = 0.5)) 

pic2

RTmonomix <- t.test(
    data$前_RT_delta,
    data$後_RT_delta,
    paired = TRUE
)

tidy(RTmonomix)

data = read.csv(file.choose(), header = T, sep = ",")
t.test(data$pre_cost, data$post_cost, paired = TRUE)
plot(data$出席次數, data$後_RT_delta-data$前_RT_delta)

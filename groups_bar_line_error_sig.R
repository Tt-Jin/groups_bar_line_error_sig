library(tidyverse)
library(reshape2)
library(rstatix)
library(ggpubr)
library(patchwork)


##read data
data_bar  = readxl::read_xlsx("data.xlsx", sheet = 1)
data_line = readxl::read_xlsx("data.xlsx", sheet = 2)

## prepare data
df_bar <- melt(data_bar, id = c("group", "treat")) %>%
  mutate(group = factor(group, levels = c("Fat", "Lean")),
         treat = factor(treat, levels = c("Control", "Acox1-LKO")))

df_line <- melt(data_line, id = c("weeks", "treat")) %>%
  mutate(treat = factor(treat, levels = c("Control", "Acox1-LKO")))

##组内差异显著性
df_bar_test <- df_bar %>% 
  group_by(group) %>% 
  t_test(value ~ treat,
         ref.group = "Control") %>% 
  mutate(p = round(p, 4),
         p.signif = case_when(p < 0.05 ~ as.character(p),
                              .default = "NS")) %>% 
  add_xy_position(x = "group", scales = "free", fun = "max") %>%
  mutate(y.position = y.position + 2)

df_line_test <- df_line %>% 
  group_by(weeks) %>% 
  t_test(value ~ treat,
         ref.group = "Control") %>% 
  mutate(p = round(p, 4),
         p.signif = case_when(p < 0.05 ~ as.character(p),
                              .default = "")) %>%
  add_xy_position(x = "weeks", scales = "free", fun = "mean") %>%
  mutate(y.position = y.position + 4)

## bar plot
p_bar <- ggplot(data = df_bar,
       aes(x = group, y= value, color = treat)) +
  geom_bar(stat = "summary", fun="mean", fill = "white",
           position = position_dodge2(padding = 0.25), width = 0.8,
           size = 1) +
  stat_summary(geom = "errorbar",fun.data = "mean_se",
               width=0.25, position = position_dodge(width = 0.8),
               size = 1) +
  geom_jitter(aes(fill = treat, shape = treat),
              position = position_jitterdodge(jitter.width = 0.15),
              size=2, stroke=1.5, color = "transparent") +
  stat_pvalue_manual(df_bar_test, label = "p.signif",
                     label.size = 4, tip.length = 0, vjust = -0.2)+
  scale_color_manual(values = c("black", "red"), guide = "none") +
  scale_fill_manual(values = c("black", "red")) +
  scale_shape_manual(values = c(21,22)) +
  scale_y_continuous(limits = c(0, 30),
                     expand = c(0, 0),
                     breaks = seq(0, 30, 10)) +
  labs(x = NULL, y = "Body composition (g)") +
  theme_classic(base_size = 20) +
  theme(plot.margin = margin(30,1,1,1),
        axis.title.x = element_blank(),
        axis.title.y = element_text(color = 'black'),
        axis.text = element_text(color = 'black'),
        legend.title = element_blank(),
        legend.text = element_text(color = 'black'),
        legend.position = c(0.05, 1.2), 
        legend.justification = c(0,1))
p_bar


p_line <- ggplot(data = df_line,
                aes(x = weeks, y = value)) +
  stat_summary(aes(color = treat),
               geom = "errorbar",fun.data = "mean_se", width=0.25,
               size = 1) +
  geom_line(aes(group = treat),
            stat = "summary", fun="mean", cex=1.3) +
  geom_point(aes(fill = treat, shape = treat),
             stat = "summary", fun="mean", size=4, color = "transparent") +
  scale_color_manual(values = c("black", "black")) +
  scale_fill_manual(values = c("black", "red")) +
  scale_shape_manual(values = c(21,22)) +
  geom_text(data = df_line_test,
            aes(x = weeks, y = y.position, label = p.signif),
            angle=90, size=4) +
  scale_y_continuous(limits = c(10, 50),
                     expand = c(0,0),
                     breaks = seq(10, 50, 5)) +
  scale_x_continuous(limits = c(0, 12),
                     breaks = seq(0, 12, 2)) +
  labs(x = "Weeks (HFD)", y = "Body weight (g)") +
  theme_classic(base_size = 20) +
  theme(plot.margin = margin(30,1,1,1),
        axis.title = element_text(color = 'black'),
        axis.text = element_text(color = 'black'),
        legend.title = element_blank(),
        legend.text = element_text(color = 'black'),
        legend.position = c(0.05, 1.2), 
        legend.justification = c(0,1))

p_line 


p_line + p_bar +
  plot_layout(widths = c(2, 1), heights = c()) +
  plot_annotation(tag_levels = 'a')

ggsave("groups_bar_line.png", width = 8, height = 5, dpi = 600)
ggsave("groups_bar_line.pdf", width = 8, height = 5)

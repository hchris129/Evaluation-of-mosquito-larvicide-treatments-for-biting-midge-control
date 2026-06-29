# 4/13/25 This script is created to visualize data and generate figures for the larval
# treatment paper titled "Evaluation of mosquito larvicide treatments for biting midge control"
# Created with the help of 
# OpenAI. (2026). ChatGPT (GPT-5) [Large language model]. https://openai.com/chatgpt
# Last edited 6/29/26


# ---- 0) Install/load packages ----
# Uncomment to install if needed:
# install.packages(c("tidyverse", "readxl", "janitor", "ggpubr"))

#install.packages("ggplot2", lib = Sys.getenv("R_LIBS_USER"))
#install.packages("tidyverse", lib = Sys.getenv("R_LIBS_USER"))

suppressPackageStartupMessages({
  library(tidyverse)  # dplyr, ggplot2, readr
  library(ggplot2)
})

library(ggplot2)
library(tidyverse)

# ---- 1) User settings (EDIT THESE) ----
# Read csv
data <- read.csv("2016J25_Larval treatment data.csv")

# Reorganize treatments
# Example renaming
data$chemical <- factor(data$chemical,
                        levels = c("Agnique_0.1", "Agnique_0.2", "meth2x", "pyri2x", "diflubenzuron", "novaluron", "temephos", "bti", "control"),   # new order
                        labels = c("Agnique® lower", "Agnique® upper", "Methoprene", "Pyriproxyfen", "Diflubenzuron", "Novaluron", "Temephos", "BTI", "Control"))  # new names


############SIGNIFICANCE###########################
library(dplyr)

# Get chemical levels in order
chem_levels <- levels(data$chemical)

# Compute max for each chemical
sig_df <- data %>%
  group_by(chemical) %>%
  summarise(max_y = max(adults_emerge, na.rm = TRUE)) %>%
  ungroup()

# Add significance labels: 
# Default: no label
sig_df$label <- ""

# Assign significance levels by position
sig_df$label[c(1, 2, 5, 6, 7)] <- "***"  # p < 0.0001
sig_df$label[c(4)] <- "*"              # p < 0.05

# Position slightly above max for each chemical
sig_df$y_pos <- sig_df$max_y + 3  # adjust +3 if needed
############PLOT#########################
# Graph

ggplot(data, aes(x = chemical, y = adults_emerge, color = trial)) +
  geom_boxplot(outlier.shape = NA) + # hides the boxplot’s own outlier dots
  geom_jitter(width = 0, size =2) +
  scale_color_continuous(
    breaks = sort(unique(data$trial)),
    labels = sort(unique(data$trial))
  ) +
  # geom_text(data = sig_df, aes(x = chemical, y = y_pos, label = label),
  #           inherit.aes = FALSE, color = "red", size = 7, vjust = 0, fontface="bold") +
  theme_minimal(base_size = 12, base_family = "Times") +  # 12pt Times New Roman
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),           # diagonal x-axis labels
    axis.text.y = element_text(angle = 0),                       # y-axis horizontal
    legend.title = element_text(size = 12, family = "Times"),    # legend title
    legend.text = element_text(size = 10, family = "Times")      # legend numbers smaller & Times New Roman
  ) +
  coord_cartesian(ylim = c(0, 100))   # y-axis goes from 0 to 100


library(ggplot2)


############Graph LS Means#############
library(dplyr)

df <- data.frame(
  trt = c("Agnique_0.1","Agnique_0.2","bti","control",
          "diflubenzuron","meth2x","novaluron","pyri2x","temephos"),
  emmean = c(2.5926,1.8345,3.8624,4.0006,1.3932,3.7236,1.3946,3.3935,-1.2095),
  lower.CL = c(2.3369,1.5219,3.6471,3.7875,1.0291,3.5059,1.0306,3.1685,-2.373),
  upper.CL = c(2.8482,2.1471,4.0777,4.2138,1.7574,3.9414,1.7585,3.6186,-0.046)
)

library(dplyr)

df <- df %>%
  mutate(category = case_when(
    grepl("Agnique", trt) ~ "MMF",
    trt == "bti" ~ "Biorational",
    trt == "temephos" ~ "Organophosphate",
    trt %in% c("meth2x","pyri2x") ~ "JGHA",
    trt %in% c("diflubenzuron","novaluron") ~ "Chitin inhibitor",
    trt == "control" ~ "Control"
  ))

cat_df <- df %>%
  group_by(category) %>%
  summarise(mean = mean(emmean))

cat_df$letters <- c(
  "b",   # Biorational
  "b",   # Control
  "a",   # Chitin inhibitor
  "c",   # JGHA
  "ab",  # MMF (overlaps)
  "d"    # Organophosphate
)

cols <- c(
  "MMF" = "#1b9e77",
  "JGHA" = "#7570b3",
  "Chitin inhibitor" = "#d95f02",
  "Organophosphate" = "#e7298a",
  "Biorational" = "#66a61e",
  "Control" = "gray50"
)

df$letters <- c(
  "c",  # Agnique_0.1
  "b",  # Agnique_0.2
  "d",  # bti
  "d",  # control
  "a",  # diflubenzuron
  "d",  # meth2x
  "a",  # novaluron
  "d",  # pyri2x
  "e"   # temephos
)

df$trt <- factor(df$trt,
                 levels = c("Agnique_0.1","Agnique_0.2","meth2x","pyri2x",
                            "diflubenzuron","novaluron","temephos","bti","control"),
                 labels = c("Agnique® lower","Agnique® upper","Methoprene","Pyriproxyfen",
                            "Diflubenzuron","Novaluron","Temephos","BTI","Control")
)


library(ggplot2)

ggplot(df, aes(x = trt, y = emmean, fill = category)) +
  
  geom_col(width = 0.7, color = "black") +
  
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                width = 0.2, linewidth = 0.6) +
  
  geom_text(aes(label = letters, y = upper.CL + 0.2),
            size = 5) +
  
  coord_flip() +
  
  scale_fill_manual(values = cols) +
  
  labs(
    x = "Larvicide Treatment",
    y = "Least Squares Mean",
    fill = "Larvicide Categorization",
    title = "Least Squares Means of Larvicide Effectiveness by Treatment"
  ) +
  
  theme_minimal(base_size = 14) +
  
  theme(
    legend.position = "right",
    panel.grid.major.y = element_blank()
  )

library(ggplot2)
library(dplyr)

###########Graph Colors@############
# ----------------------------
# 1. DATA
# ----------------------------
df <- data.frame(
  trt = c("control","Agnique_0.1","Agnique_0.2","meth2x",
          "pyri2x","diflubenzuron","novaluron","temephos","bti"),
  mean = c(54.6333, 13.364, 6.2618, 41.4142,
           29.7703, 4.0279, 4.0333, 0.2983, 47.5782),
  lower = c(44.1444, 10.3495, 4.5808, 33.3112,
            23.7711, 2.7986, 2.8028, 0.0932, 38.3625),
  upper = c(67.6143, 17.2566, 8.5598, 51.4881,
            37.2836, 5.7973, 5.804, 0.955, 59.0076)
)

# ----------------------------
# 2. ORDER (controls x-axis order)
# ----------------------------
df$trt <- factor(df$trt,
                 levels = c("control","Agnique_0.1","Agnique_0.2","meth2x",
                            "pyri2x","diflubenzuron","novaluron","temephos","bti")
)

# ----------------------------
# 3. CATEGORY (for fill colors)
# ----------------------------
df$category <- c(
  "Control",
  "MMF","MMF",
  "JGHA",
  "JGHA",
  "Chitin inhibitor",
  "Chitin inhibitor",
  "Organophosphate",
  "Biorational"
)

df$category <- factor(df$category,
                      levels = c("Control","MMF","JGHA","Chitin inhibitor","Organophosphate","Biorational")
)

# ----------------------------
# 4. SIGNIFICANCE LABELS (vs control example)
# ----------------------------
df$signif <- c("Control","***","***","ns","**","***","***","***","ns")

# ----------------------------
# 5. RENAME FOR PLOT (clean labels)
# ----------------------------
df$trt_plot <- recode(df$trt,
                      "control" = "Control",
                      "Agnique_0.1" = "Agnique® (low)",
                      "Agnique_0.2" = "Agnique® (high)",
                      "meth2x" = "Methoprene",
                      "pyri2x" = "Pyriproxyfen",
                      "diflubenzuron" = "Diflubenzuron",
                      "novaluron" = "Novaluron",
                      "temephos" = "Temephos",
                      "bti" = "BTI"
)

# lock plotting order
df$trt_plot <- factor(df$trt_plot,
                      levels = c("Control","Agnique® (low)","Agnique® (high)",
                                 "Methoprene","Pyriproxyfen",
                                 "Diflubenzuron","Novaluron",
                                 "Temephos","BTI")
)

# ----------------------------
# 6. PLOT
# ----------------------------
ggplot(df, aes(x = trt_plot, y = mean, fill = category)) +
  geom_col(color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  geom_text(aes(label = signif, y = upper + 3), size = 5) +
  
  scale_fill_manual(values = c(
    "Control" = "black",
    "MMF" = "#1b9e77",
    "JGHA" = "#7570b3",
    "Chitin inhibitor" = "#d95f02",
    "Organophosphate" = "#e7298a",
    "Biorational" = "#66a61e"
  )) +
  
  labs(title = "Treatment LS Means Significance", 
       x = "Treatment",
       y = "LS Mean (response scale)",
       fill = "Mode of Action") +
  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank()
  )



install.packages("patchwork")
library(patchwork)

# Fix the order of labels
treat_order <- c(
  "Control",
  "Agnique® lower",
  "Agnique® upper",
  "Methoprene",
  "Pyriproxyfen",
  "Diflubenzuron",
  "Novaluron",
  "Temephos",
  "BTI"
)

# Fix plot A
data$chemical <- recode(data$chemical,
                        "control" = "Control",
                        "Agnique_0.1" = "Agnique® lower",
                        "Agnique_0.2" = "Agnique® upper",
                        "meth2x" = "Methoprene",
                        "pyri2x" = "Pyriproxyfen",
                        "diflubenzuron" = "Diflubenzuron",
                        "novaluron" = "Novaluron",
                        "temephos" = "Temephos",
                        "bti" = "BTI"
)

data$chemical <- factor(data$chemical, levels = treat_order)

# Fix plot B
df$trt_plot <- recode(df$trt,
                      "control" = "Control",
                      "Agnique_0.1" = "Agnique® lower",
                      "Agnique_0.2" = "Agnique® upper",
                      "meth2x" = "Methoprene",
                      "pyri2x" = "Pyriproxyfen",
                      "diflubenzuron" = "Diflubenzuron",
                      "novaluron" = "Novaluron",
                      "temephos" = "Temephos",
                      "bti" = "BTI"
)

df$trt_plot <- factor(df$trt_plot, levels = treat_order)


# Limit y-axis
y_lim <- c(0, 80)

p1 <- ggplot(data, aes(x = chemical, y = adults_emerge, color = trial)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0, size = 2) +
  scale_color_continuous(
    breaks = seq(min(data$trial, na.rm = TRUE),
                 max(data$trial, na.rm = TRUE),
                 by = 1),
    labels = seq(min(data$trial, na.rm = TRUE),
                 max(data$trial, na.rm = TRUE),
                 by = 1)
  ) +
  scale_y_continuous(limits = y_lim) +
  theme_minimal(base_size = 12, base_family = "Times") +
  labs(
    x = NULL,
    y = "No. of Emerged Adults",
    color = "Trial"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# This removes the x-axis for Panel A
p1 <- p1 + theme(
  axis.title.x = element_blank(),
  axis.text.x = element_blank(),
  axis.ticks.x = element_blank()
)

p2 <- ggplot(df, aes(x = trt_plot, y = mean, fill = category)) +
  geom_col(color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  geom_text(aes(label = signif, y = upper + 3), size = 5) +
  scale_fill_manual(values = c(
    "Control" = "black",
    "MMF" = "#1b9e77",
    "JGHA" = "#7570b3",
    "Chitin inhibitor" = "#d95f02",
    "Organophosphate" = "#e7298a",
    "Biorational" = "#66a61e"
  )) +
  labs(x = "Treatment",
       y = "LS Mean (response scale)",
       fill = "Larvicide Treatment 
       Categorization") +
  scale_y_continuous(limits = y_lim) +
  theme_minimal(base_size = 12, base_family = "Times") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



library(patchwork)

p1 <- p1 + labs(x = NULL)
p2 <- p2 + labs(x = NULL)

(p1 / p2) +
  plot_annotation(
    tag_levels = "A",
    caption = "Larvicide Treatment"
  ) &
  theme(
    plot.caption = element_text(
      family = "Times",
      size = 12,
      hjust = 0.5
    )
  )

library(grid)

final_plot <- (p1 / p2) + plot_annotation(tag_levels = "A")

final_plot

grid::grid.text(
  "Larvicide Treatment",
  x = 0.5, y = 0.02,
  gp = grid::gpar(fontsize = 12, fontfamily = "Times")
)

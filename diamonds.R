## ----setup, include=FALSE, message=FALSE, warning=FALSE-------------------------------------------------------------

# Setup
knitr::opts_chunk$set(
  echo = FALSE,
  message = FALSE,
  warning = FALSE,
  results = "hide",
  fig.align = "center"
)



## ----libraries and set-up-------------------------------------------------------------------------------------------

# Libraries
# library(tidyr)
library(tidyverse)
library(janitor)
library(psych)
library(psychTools)
library(papaja)
library(RColorBrewer)
library(kableExtra)
library(ggpubr)
library(VennDiagram)
library(scales)


# Creation of R package citation file
knitr::write_bib(c(.packages(), "rmarkdown"), "../common/packages.bib")

# Set colour scheme
mycol <- brewer.pal(n = 8, name = "Set2")


## ----data-----------------------------------------------------------------------------------------------------------

# Import data and move 'price' variable to front of data frame
d <- diamonds



## ----desc-----------------------------------------------------------------------------------------------------------

# Describe data
describe(d)


## ----smaller data---------------------------------------------------------------------------------------------------

# original, and adding numerical values for cut/color/clarity (for psych::mediate())

d_small <- d |> 
    select(carat, cut, color, clarity, price) |>  
    mutate("Cut" = as.numeric(cut),
           "Color" = as.numeric(color),
           "Clarity" = as.numeric(clarity))

# cut data

d_cut <- d_small |> 
    tabyl(cut) |> 
    adorn_pct_formatting()

# color data

d_col <- d_small |> 
    tabyl(color) |> 
    adorn_pct_formatting()

# clarity data

d_cla <- d_small |> 
    tabyl(clarity) |> 
    adorn_pct_formatting()




## ----tables, results = 'asis'---------------------------------------------------------------------------------------

# Table for number of diamonds in each categorical variable

kbl(list(d_cut, d_col, d_cla),
  booktabs = TRUE,
  valign = "t",
  col.names = c("", "N", "Percent"),
  format.args = list(big.mark = ","),
  caption = "<b>Table 1.</b><br><i>Number of Occurances and Average Price for each Categorical Variable</i>"
) |>
  kable_classic() |>
  kable_styling() |>
  add_header_above(header = c("$Cut$", "$Color$", "$Clarity$"), align = "l") |>
  footnote(
      footnote_as_chunk = T,
      c("_Color_- D (Absolutely colorless or icy-white); E & F (Colorless); G - J (Near colorless).", "_Clarity_- I1 - I3 (Included); SI2 & SI1 (Slightly included); VS2 & VS1 (Very slightly included); VVS2 & VVS1 (Very very slightly included); IF (Internally flawless); [FL (Flawless), not listed here].", "See _Brilliant Earth_ for further details of variable identification (ID) categories: [_Cut_](https://www.brilliantearth.com/diamond-cuts/); [_Color_](https://www.brilliantearth.com/diamond-color/); [_Clarity_](https://www.brilliantearth.com/diamond-clarity/)"),
      general_title = "__Note. __ ")


## ----categorical figures--------------------------------------------------------------------------------------------

# figures to show number of items in each category

cut_plot <- d_cut |>
  ggplot(aes(cut, n, fill = mycol[1:5])) +
  geom_col(show.legend = F) +
  labs(title = "Cut", 
       x = "", 
       y = "", 
       caption = "") +
  theme(title = element_text(size = 24),
        axis.text = element_text(size = 20)) +
  scale_x_discrete(limits = rev(levels(d_cut$cut))) +
  coord_flip() 

col_plot <- d_col |>
  ggplot(aes(color, n, fill = mycol[1:7])) +
  geom_col(show.legend = F) +
  labs(title = "Color", 
       x = "", 
       y = "", 
       caption = "") +
  theme(title = element_text(size = 24),
        axis.text = element_text(size = 20)) +
  scale_x_discrete(limits = rev(levels(d_col$color))) +
  coord_flip()

cla_plot <- d_cla |>
  ggplot(aes(clarity, n, fill = mycol[1:8])) +
    geom_col(show.legend = FALSE) +
  labs(title = "Clarity", 
       x = "", 
       y = "", caption = "") +
  theme(title = element_text(size = 24),
        axis.text = element_text(size = 20)) +
  scale_x_discrete(limits = rev(levels(d_cla$clarity))) +
  coord_flip()




## ----show_cat_figs, fig.show='hold', out.width="33.3%", fig.cap="__Figure 1.__ _Number of Occurances for each Categorical Variable_"----

# plot categorical figures

op <- par(mfrow = c(1, 3))

cut_plot
col_plot
cla_plot

par(op)






## ----color and correlation------------------------------------------------------------------------------------------

# preferred correlation table colours

gr <- colorRampPalette(c("#FC8D62", "white", "#66C2A5"))

# correlation

dcor <- lowerCor(d_small[,c(1, 6:8, 5)])



## ----corr plot, fig.cap="__Figure 2.__ _Correlation Plot of Diamonds Data with the Main Variables_"-----------------

# lower correlation plot

corPlot(dcor, upper = FALSE, stars = TRUE, main = "", gr = gr, cex = .95,
        labels = c("carat", "cut", "color", "clarity", "price"))




## ----correlation venns, fig.show='hold', out.width="25%",fig.cap="__Figure 3.__ _Percentage of Variance in Price (shown in green) and each Variable_"----

op <- par(mfrow = c(1, 4))

# carat venn

car_venn <- draw.pairwise.venn(
  area1 = 92.5, # Draw pairwise venn diagram
  area2 = 92.5,
  cross.area = 85, fill = c(mycol[1], mycol[2]), 
  alpha = 0.5, category = c("", "Carat"), lty = "blank", 
  cat.cex = 3, cat.pos = 15, cat.dist = .03, cex =3
)
grid.newpage()

# color venn

col_venn <- draw.pairwise.venn(
  area1 = 51.5, # Draw pairwise venn diagram
  area2 = 51.5,  cross.area = 3, fill = c(mycol[1], mycol[2]), 
  alpha = 0.5, category = c("", "Color"), lty = "blank", 
  cat.cex = 3, cat.pos = 0, cat.dist = .05, cex =3, 
  ext.line.lty = "dashed", ext.length = .8
)

grid.newpage()

# clarity venn

cla_venn <- draw.pairwise.venn(
  area1 = 51, # Draw pairwise venn diagram
  area2 = 51,
  cross.area = 2, fill = c(mycol[1], mycol[2]), 
  alpha = 0.5, category = c("", "Clarity"), lty = "blank", 
  cat.cex = 3, cat.pos = 0, cat.dist = .05, cex =3, 
  ext.line.lty = "dashed", ext.length = .8
)

grid.newpage()

# cut

cut_venn <- draw.pairwise.venn(
  area1 = 50.15, # Draw pairwise venn diagram
  area2 = 50.15,
  cross.area = 0.3, fill = c(mycol[1], mycol[2]), 
  alpha = 0.5, category = c("", "Cut"), lty = "blank", 
  cat.cex = 3, cat.pos = 0, cat.dist = .05, cex =3, 
  ext.line.lty = "dashed", ext.length = .8
)

par(op)



## ----Linear Regression----------------------------------------------------------------------------------------------

# for table info

dia_lr <- lm(price ~ carat + Cut + Color + Clarity, data = d_small)

dia_lr_info <- apa_print(dia_lr)




## -------------------------------------------------------------------------------------------------------------------



## ----lr table, results='asis'---------------------------------------------------------------------------------------

# construct LR table

kbl(dia_lr_info$table,
    booktabs = TRUE,
    caption = "<b>Table 2.</b><br><i>Linear Regression Showing the Effect Size Between Variables and Price.</i>", 
    col.names = c("$Predictor$", "$b$", "$95\\% \\space CI$", "$t$", "$\\mathit{df}$", "$p$")
) |> 
  kable_classic() |>
    kable_styling(full_width = FALSE)



## ----med calc, cache=TRUE, fig.cap="__Figure 4.__ _Mediation Plot Between Carat and Price Mediated by Cut, Color and Clarity_"----

# mediation model

dmed1 <- mediate(price ~ carat + (Cut) + (Color) + (Clarity),
  data = d_small, plot = TRUE, main = "", n.obs = 53940
)



## ----mediation summary----------------------------------------------------------------------------------------------

# Numerical detail of mediation
summary(dmed1)


## -------------------------------------------------------------------------------------------------------------------



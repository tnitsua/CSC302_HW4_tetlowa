#Q5
#Please inspect the following code which can be also found in TimeSeries_Trends.R and try to run how it generates
#three time series in a single plot. Then, perform the steps in the following bullet points:
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)

load("/home/rootuser/Downloads/Slides12Rscripts_Data") #loaded file through file manager (ubuntu os)

head(preprint_growth)

preprint_growth %>% filter(archive == "bioRxiv") %>%
  filter(count > 0) -> biorxiv_growth

preprints<-preprint_growth %>% filter(archive %in%
  c("bioRxiv", "arXiv q-bio", "PeerJ Preprints")) %>%filter(count > 0) %>%
  mutate(archive = factor(archive, levels = c("bioRxiv", "arXiv q-bio", "PeerJ Preprints")))

preprints_final <- filter(preprints, date == ymd("2017-01-01"))

ggplot(preprints) +
  aes(date, count, color = archive, fill = archive) +
  geom_line(size = 1) +
  scale_y_continuous(
    limits = c(0, 600), expand = c(0, 0),
    name = "preprints / month",
    sec.axis = dup_axis( #this part is for the second y axis
      breaks = preprints_final$count, #and we use the counts to position our labels
      labels = c("arXivq-bio", "PeerJPreprints", "bioRxiv"),
      name = NULL)
  ) +
  scale_x_date(name = "year",
               limits = c(min(biorxiv_growth$date), ymd("2017-01-01"))) +
  scale_color_manual(values = c("#0072b2", "#D55E00", "#009e73"),
                     name = NULL) +
  theme(legend.position = "none")


#(a) By using drop_na( ) and filter( ) on preprint_growth data frame, get the rows which have count greater
#than 0 and year later than 2004, and output it to another data frame called preprint_full.

drop_na(preprint_growth)
preprint_growth %>% filter(date >= "2004-01-01") -> preprint_full

#(b) Use the filter function again to select the rows that have ”bioRxiv”, ”F1000Research” in it only by looking at the
#example in the code above.

preprint_full %>% filter(archive == "bioRxiv") %>% filter(archive == "F1000Research")

#(c) Draw line graphs for these two time series, ”bioRxiv” and ”F1000Research”, by coloring them with ”#7c6bea” and
#”#fe8d6d”.

ggplot(preprint_full) +
  aes(date, count, color = archive, fill = archive) +
  geom_line(size = 1) +
  scale_x_date(name = "year",
               limits = c(min(preprint_full$date), ymd("2004-01-01"))) +
  scale_color_manual(values = c("#7c6beg", "#fe8d6d"), name = NULL) + 
  theme(legend.position = "none")

#(d) Put the legend to the right of the figure.

ggplot(preprint_full) +
  aes(date, count, color = archive, fill = archive) +
  geom_line(size = 1) +
  scale_x_date(name = "year",
               limits = c(min(preprint_full$date), ymd("2004-01-01"))) +
  scale_color_manual(values = c("#7c6beg", "#fe8d6d"), name = NULL) + 
  theme(legend.position = "right")

#(e) For the x-axis, start the values from Feb 2014.

ggplot(preprint_full) +
  aes(date, count, color = archive, fill = archive) +
  geom_line(size = 1) +
  scale_x_date(name = "year",
               limits = c(min(preprint_full$date), ymd("2014-01-01"))) +
  scale_color_manual(values = c("#7c6beg", "#fe8d6d"), name = NULL) + 
  theme(legend.position = "right")

#(f) Add a title ”Preprint Counts” to the figure.
ggplot(preprint_full) +
  aes(date, count, color = archive, fill = archive) +
  geom_line(size = 1) +
  labs(title = "Preprint Counts") +
  scale_x_date(name = "year",
               limits = c(min(preprint_full$date), ymd("2004-01-01"))) +
  scale_color_manual(values = c("#7c6beg", "#fe8d6d"), name = NULL) + 
  theme(legend.position = "right")
library(jsonlite)
library(tidyverse)
library(lubridate)
library(ggrepel)
library(viridis)
library(gganimate)
library(cowplot)
library(ggthemes)

me <- jsonlite::fromJSON("MyActivity.json")

# converting date-time in string to date-time format along with time-zone conversion

me$time_ist <- with_tz(parse_datetime(me$time),"Asia/Calcutta") 

# remove incomplete years and irrelevant years too - Kept 2019 to see just January if required

me <- filter(me, year(time_ist) %in% c(2017,2018,2019))


# Sample

tibble::tibble(head(me))


# Overall Daily usage trend

me %>%
filter(!str_detect(header,"com.")) %>%
filter(as.Date(time_ist) >= as.Date("2017-01-01")) %>% 
group_by(Date = as.Date(time_ist)) %>%
count(n = n()) %>%
ggplot(aes(Date,n, group = 1, color = "red")) +
geom_line(aes(alpha = 0.8),show.legend = FALSE) +
stat_smooth() +
# Courtesy: https://stackoverflow.com/a/42929948
scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") + 
labs(
    title = "Daily-wise usage",
    subtitle = "2+ years (including some 2019)",
    x = "Months",
    y = "# of Interactions"
  ) +

  theme(axis.text.x=element_text(angle=60, hjust=1))+
  theme(legend.position="none") +
  ggthemes::theme_hc(style  = "darkunica") 

# Month-wise usage trend

me %>% 
  filter(!str_detect(header,"com.")) %>% 
  group_by(Date = as.Date(time_ist)) %>% 
  count(n = n()) %>% 
  mutate(year = as.factor(year(Date)),
         month = month(Date)) %>% 
  filter(year %in% c(2017,2018)) %>% 
  group_by(year,month) %>% 
  summarize(n = sum(n)) %>% 
  ggplot(aes(fct_relevel(month.abb[month], month.abb),n, group = year, color = year)) + 
  geom_line() +
  labs(
    title = "Month-wise usage",
    subtitle = "For two years",
    x = "Months",
    y = "# of Interactions"
  ) +
  ggthemes::theme_hc(style = "darkunica")

# Day-wise usage trend


me %>% 
  filter(!str_detect(header,"com.")) %>% 
  group_by(Date = as.Date(time_ist)) %>% 
  count(n = n()) %>% 
  mutate(year = as.factor(year(Date)),
         weekday = weekdays(Date, abbr = TRUE)) %>% 
  filter(year %in% c(2017,2018)) %>% 
  group_by(year,weekday) %>% 
  summarize(n = sum(n)) %>% 
  ggplot(aes(fct_relevel(weekday, c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")),
             n, group = year, color = year)) + 
  geom_line() +
  labs(
    title = "Day-wise usage",
    subtitle = "For two years",
    x = "Weekday",
    y = "# of Interactions"
  ) +
  ggthemes::theme_hc(style = "darkunica")


# Day-wise usage trend



me %>% 
  filter(!str_detect(header,"com.")) %>% 
  group_by(Date = as.Date(time_ist)) %>% 
  count(n = n()) %>% 
  mutate(year = as.factor(year(Date)),
         weekday = weekdays(Date, abbr = TRUE)) %>% 
  mutate(what_day = ifelse(weekday %in% c("Sat","Sun"),"Weekend","Weekday")) %>% 
  filter(year %in% c(2017,2018)) %>% 
  group_by(year,what_day) %>% 
  summarize(n = sum(n)) %>% 
  ggplot(aes(fct_relevel(what_day, c("Weekday","Weekend")),
             n, group = year, color = year)) + 
  geom_line() +
  labs(
    title = "Weekday vs Weekend usage",
    subtitle = "For two years",
    x = "Weekday / Weekend",
    y = "# of Interactions"
  ) +
  ggthemes::theme_excel_new()


# Messaging Usage

p <- me %>% 
  filter(str_detect(tolower(header), regex("signal|message|whatsapp"))) %>% 
  mutate(ym = as.Date(paste0(format(as.Date(time_ist),"%Y-%m"),"-01"))) %>% 
  group_by(ym) %>% 
  count() %>% 
  #https://community.rstudio.com/t/tweenr-gganimate-with-line-plot/4027/10
  ggplot(aes(ym,n, group = 1)) + geom_line(color = "green") +
  geom_point() +
  ggthemes::theme_hc(style = "darkunica") +
  theme(axis.text.x = element_text(colour = "white",
                                   angle = 60),
        axis.text.y = element_text(colour = "white")) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") + 
  labs(
    title = "Messaging usage",
    x = "Year-Month"
  ) +
  transition_reveal(ym) + 
  ease_aes('cubic-in-out')


animate(p, nframes = 20, renderer = gifski_renderer("msging.gif"), width = 800, height = 800)



# YouTube Usage

yt <- me %>% 
  filter(header %in% "YouTube") %>% 
  mutate(ym = as.Date(paste0(format(as.Date(time_ist),"%Y-%m"),"-01"))) %>% 
  group_by(ym) %>% 
  count() %>% 
  #https://community.rstudio.com/t/tweenr-gganimate-with-line-plot/4027/10
  ggplot(aes(ym,n, group = 1)) + geom_line(color = "red") +
  geom_point() +
  ggthemes::theme_hc(style = "darkunica") +
  theme(axis.text.x = element_text(colour = "white",
                                   angle = 60),
        axis.text.y = element_text(colour = "white")) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") + 
  labs(
    title = "YouTube usage",
    x = "Year-Month"
  ) +
  transition_reveal(ym) + 
  ease_aes('quintic-in-out')


#anim_save("yt.gif", yt , width = 600, height = 600)

animate(yt, nframes = 10, renderer = gifski_renderer("yt2.gif"), width = 800, height = 800)

# Top apps



me_count <- me %>% 
  group_by(year = year(time_ist),header) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  top_n(20,n) #%>% #View() 
#mutate(header = fct_reorder(header,n)) %>% 



me_count %>%  
  filter(year %in% "2017") %>% 

  ggplot(aes(fct_reorder(header,n),n, label = n)) +  
   
  geom_bar(aes(fill = n),stat = "identity") +
  #scale_y_log10() +
  
  coord_flip() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1,size = 8))  +
  scale_fill_viridis() +
  theme_minimal() +
  theme(legend.position="none") +
  labs(
    title = "Most used 20 Apps",
    subtitle = "2017",
    x = "App name"
  ) -> y1


me_count %>%  
  filter(year %in% "2018") %>% 
  
  ggplot(aes(fct_reorder(header,n),n, label = n)) +  
  
  geom_bar(aes(fill = n),stat = "identity") +
  
  
  #scale_y_log10() +
  coord_flip() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1,size = 8))  +
  scale_fill_viridis() +
    theme_minimal() +
  theme(legend.position="none") +
  labs(

    subtitle = "2018",
    x = "App name"
  ) -> y2

me_count %>%  
  filter(year %in% "2019") %>% 
  ggplot(aes(fct_reorder(header,n),n, label = n)) +  
  
  geom_bar(aes(fill = n),stat = "identity") +
  #scale_y_log10() +
  
  
  coord_flip() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1,size = 8))  +
  scale_fill_viridis() +
  
  theme_minimal() +
  theme(legend.position="none") +
  
  labs(
    subtitle = "2019",
    x = "App name"
  ) -> y3

cowplot::plot_grid(y1,y2,y3, ncol = 3, scale = 0.7, vjust = 0, label_size = 8)


#usethis::use_mit_license("Github @amrrs")
#usethis::use_readme_md()

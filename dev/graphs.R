


ggplot(covid, aes_string(x="Date", y="total_cases")) + 
  geom_col()+
  geom_line(aes(x=Date, 
                y=rollmean(total_cases, 4, na.pad=TRUE, align="right"),
                color = "4 week rolling avg."
  ) 
  )+
  scale_color_manual(name = NULL, values = c("4 week rolling avg." = "black"))




ggplot(school, aes_string(x="Date", y="new_cases")) + 
  geom_col(aes(fill=school))+
  geom_line(aes(x=Date, 
                y=rollmean(new_cases, 4, na.pad=TRUE, align="right"),
                color = "rolling avg."
  ) 
  )+
  scale_color_manual(name = NULL, values = c("rolling avg." = "black"))


ggplot(school, aes_string(x="Date", y="new_cases")) + 
  geom_col(aes(fill=school))

ggplot(school, aes_string(x="Date", y="total_cases", fill="school")) + 
  geom_area()

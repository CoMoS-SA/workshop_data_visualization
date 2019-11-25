library(tidyverse)

##### DATA MAMIPULATION

data("mtcars")                                      #load sample dataset
mean(exp(mtcars$cyl))                               #exponential and mean with conventional workflow
mtcars$cyl %>% exp() %>% mean                       #with pipeline

mtcars %>% filter(cyl==4)                           #single filtering
mtcars %>% filter(cyl==4, hp>90)                    #multiple filtering

mtcars %>% select(mpg, cyl)                         #select mpg and cyl
mtcars %>% select(-mpg, -cyl)                       #select everything but mpg and cyl


mtcars %>% mutate( 
    hp_100 = hp*100,                                #create a new vatiable hp_100
    drat = drat/100                                 #replace drat variable
)

mtcars %>% mutate(                                  #create new variable conditional on other variable values
    gear_5=case_when(
        gear>=5 ~ 1,
        gear<5  ~ 0
    )
)

mtcars %>% summarise(                               #summarise 
    m_cyl=mean(cyl)
)

mtcars %>% group_by(gear, am) %>%                   #summarise by group. NOTE: group_by doesn't alter data in anyway, until summarise is called
    summarise(
        m_cyl=mean(cyl),
        s_hp=sum(hp)
    )


data(iris)                                          #get sample data

iris %>% gather(variable, value, -Species)          #transform in long format

iris %>% gather(variable, value, -Species) %>%      #get long format and then summarise
    group_by(variable) %>% 
    summarise(
        m_var= mean(value)
    )

iris %>% gather(variable, value, -Species) %>%      #multiple grouping
    group_by(variable, Species) %>% 
    summarise(
        m_var= mean(value)
    )

##### GGPLOT

#simple graph
head(economics) #economics is a ggplot built-in dataset
economics %>%  # pass data
    ggplot(aes(x=date, y=unemploy)) + #specify aes
    geom_line() -> g #specify geom
g

#work with data in the same pipe: e.g. filter
economics %>%  
    filter(date>"1990-01-01") %>% #filter data
    ggplot(aes(x=date, y=unemploy)) + 
    geom_line() + #inerith aes from ggplot unless specified
    ylab("Unemployment") +
    xlab("Time") +
    theme(
        panel.grid.minor = element_blank()
    )
    
# # # # # # # # # # # get recession data # # # # # # # # # #

install.packages("tis")
library(tis)
recessions <- data.frame(
    start=as.Date(as.character(nberDates()[,1]),format="%Y%m%d"),
    end=as.Date(as.character(nberDates()[,2]),format="%Y%m%d")
        ) %>% filter(start>"1967-07-01")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

#add layers
economics %>% 
ggplot(aes(x = date, y = unemploy/pop)) + 
    geom_line() +
    geom_smooth(method = "loess", se=F, size=0.4, linetype=2) + #add kernel smoothed tendency line. se turns off standard error
    geom_rect(data = recessions, #new dataset
              aes(xmin = start, xmax = end, ymin = -Inf, ymax = +Inf),
              inherit.aes = FALSE, fill = "red", alpha = 0.2) #alpha for transparency. inherit.aes is nencessary as aes are inerithed from ggplot (not necessary if you define aes inside geom)

#faceting
economics %>% 
    gather(variable, value, -date) %>%
    ggplot(aes(x=date, y=value)) +
    geom_line() +
    facet_grid(variable~., scales = "free_y") # row~column, free_y allow y axis ranges of each facet to vary independently

#grouping
iris %>% 
    ggplot() +
    geom_boxplot(aes(y=Sepal.Length, fill=Species)) + #put grouping attriute inside aes and provide grouping variable
    theme(legend.position = "bottom")

iris %>% gather(variable, value, -Species) %>%        #combine grouping and faceting
    ggplot() +
    geom_boxplot(aes(y=value, fill=Species)) +
    facet_wrap(variable~., scales = "free_y", nrow=2)  +
    theme(legend.position = "bottom")


    

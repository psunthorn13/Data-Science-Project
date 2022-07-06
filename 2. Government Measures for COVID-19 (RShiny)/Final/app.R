##############################################0. Import the libraries############################################

# Instruction
# Please install the libraries if there are no libraries in your local environment 
# Please wait a few second after running the app
# Recommend to open in the web browser when opening the app

# Used for import shiny themes
library("shinythemes")

# Used for R Shiny
library("shiny")

# Used for reading excel files
library("readxl")

# Used for data preparation
library("dplyr")

# Used for creating interactive data visualization
library("ggplot2")
library("plotly")
library("ggiraph")
library("bslib")
library("thematic")
library("scales")


# Used for creating interactive map
library("leaflet")
library("rworldmap")

# Used for importing image files
library("png")


############################################## 1. Data Pre-processing ############################################

##### 1.1 Data Source 1 Measure detail######
# Read Excel data
country_measure <- read_excel("data/acaps_covid19_government_measures_dataset_0.xlsx"
                              , sheet = "Dataset")

# Select only relevant columns
ds1measure <- country_measure %>%
  select(-"REGION",-'ADMIN_LEVEL_NAME',-'PCODE',
         -'SOURCE',-'SOURCE_TYPE',-'LINK',-'ENTRY_DATE',-'Alternative source')




# Load csv file that contains original country names and new country names
# of country that has inconsistent name across all data sources
ds1_change <- read.csv('data/ds1_country_change.csv',)

# Change the name
names(ds1_change)[1] <- 'Country_Name'

# Map country name
ch1 <- ds1_change[match(ds1measure$COUNTRY,
                        ds1_change$Country_Name), 2, drop=F]

# Change the country name in the data source
for (i in 1: nrow(ch1)){
  if (!is.na(ch1[i,1])){
    ds1measure[i,3] <- ch1[i,1]
  }
}



##### 1.2 Data Source 2 Policy flag######
# Read CSV data
policy <- read.csv('data/covid_policy.csv')


# Choose data in country level and select only relevant data
ds2policytrack <-    policy%>%
  filter( Jurisdiction == 'NAT_TOTAL') %>%
  select(colnames(policy)[c(1,6)]|colnames(policy)[7:36]
         |colnames(policy)[40:49])


# Load csv file that contains original country names and new country names
# of country that has inconsistent name across all data sources
ds2_change <- read.csv('data/ds2_country_change.csv',)

# Change the name
names(ds2_change)[1] <- 'Country_Name'

# Map country name
ch1 <- ds2_change[match(ds2policytrack$CountryName,
                        ds2_change$Country_Name), 2, drop=F]

# Change the country name in the data source
for (i in 1: nrow(ch1)){
  if (!is.na(ch1[i,1])){
    ds2policytrack[i,1] <- ch1[i,1]
  }
}



##### 1.3 Data Source 3 cases######

# Read CSV data
cases <- read.csv('data/COVID-19 Activity.csv')

# Aggregate number of cases
ds3cases<- aggregate(cases[,c(1,7,12,13)],cases[,c(10,4)],FUN =   sum)
names(ds3cases)[1] <- 'COUNTRY'


# Load csv file that contains original country names and new country names
# of country that has inconsistent name across all data sources
ds3_change <- read.csv('data/ds3_country_change.csv',)

# Change the name
names(ds3_change)[1] <- 'Country_Name'

# Map country name
ch1 <- ds3_change[match(ds3cases$COUNTRY,
                        ds3_change$Country_Name), 2, drop=F]

# Change the country name in the data source
for (i in 1: nrow(ch1)){
  if (!is.na(ch1[i,1])){
    ds3cases[i,1] <- ch1[i,1]
  }
}



##### 1.4 Data Source 4 Total Population######

# Read excel file
pop <-read_excel("data/WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.xlsx", sheet = 1)


# Clean the format
pop <-pop[-(1:11),]
colnames(pop) <- pop[1,]
pop<-pop[-1,]

# Select only relevant fields
pop <- pop %>%
  select('Region, subregion, country or area *',
         'Country code','Type','2020')%>%
  filter(Type == 'Country/Area')

# Name the field
names(pop) <- c('Country','Country code','Type','Total_Population')

# Manipulate data
ds4pop <- pop %>%
  select('Country','Total_Population') %>%
  mutate(Total_Population = as.numeric(Total_Population)*1000)%>%
  arrange(Country)

# Load csv file that contains original country names and new country names
# of country that has inconsistent name across all data sources
ds4_change <- read.csv('data/ds4_country_change.csv',)

# Change the name
names(ds4_change)[1] <- 'Country_Name'

# Map country name
ch1 <- ds4_change[match(ds4pop$Country,
                        ds4_change$Country_Name), 2, drop=F]

# Change the country name in the data source
for (i in 1: nrow(ch1)){
  if (!is.na(ch1[i,1])){
    ds4pop[i,1] <- ch1[i,1]
  }
}

# Change the country name in the data source (Manually change)
ds4pop[53,1] <- "Cote d'Ivoire"
ds4pop[56,1] <- 'Curacao'
ds4pop[172,1] <- 'Reunion'
ds4pop[176,1] <- 'Saint Barthelemy'



##### 1.5 Data Source 5 Continent######
# Read the file
continent <- read.csv('data/countryContinent.csv',)



# Select only interesting columns
ds5continent <- continent %>%
  select('country','continent','code_3','sub_region')

# Load csv file that contains original country names and new country names
# of country that has inconsistent name across all data sources
ds5_change <- read.csv('data/ds5_country_change.csv',)

# Change the name
names(ds5_change)[1] <- 'Country_Name'

# Map country name
ch1 <- ds5_change[match(ds5continent$country,
                        ds5_change$Country_Name), 2, drop=F]

# Change the country name in the data source
for (i in 1: nrow(ch1)){
  if (!is.na(ch1[i,1])){
    ds5continent[i,1] <- ch1[i,1]
  }
}

# Change the country name in the data source (Manually change)
ds5continent[55,1] <- "Cote d'Ivoire"
ds5continent[58,1] <- 'Curacao'
ds5continent[181,1] <- 'Reunion'
ds5continent[185,1] <- 'Saint Barthelemy'



##### 1.6 Data Source 6 final aggregation group by country ######

# Aggregate number of measures group by country
measure_group <- ds1measure %>%
  group_by(COUNTRY)%>%
  summarize(number_of_measures =n_distinct(ID)) 

# Aggregate index group by country
index_group<-
  ds2policytrack %>% 
  group_by(CountryName) %>%
  summarize(StringencyIndex= mean(StringencyIndex,na.rm = TRUE),
            EconomicSupportIndex = mean(EconomicSupportIndex,na.rm = TRUE),
            GovernmentResponseIndex= mean(GovernmentResponseIndex,na.rm = TRUE),
            ContainmentHealthIndex =mean(ContainmentHealthIndex,na.rm = TRUE) ) %>%
  left_join(y=measure_group,by = c("CountryName" = "COUNTRY")) %>%
  rename(Country = CountryName)

# Join Multiple data sources
final.ds <-ds3cases%>%
  group_by(COUNTRY)%>%
  summarize(max(PEOPLE_POSITIVE_CASES_COUNT),max(PEOPLE_DEATH_COUNT))%>%
  rename(Number_of_cases ='max(PEOPLE_POSITIVE_CASES_COUNT)',
         Number_of_dealth = 'max(PEOPLE_DEATH_COUNT)',
         Country = 'COUNTRY') %>%
  left_join(ds4pop,by = c("Country" = "Country")) %>%
  left_join(y=ds5continent,by = c("Country" = "country")) %>%
  mutate(mortality_rate = Number_of_dealth/Number_of_cases,
         infection_rate = Number_of_cases/Total_Population)%>%
  left_join(index_group,by = c("Country" = "Country"))

  

# Create  New column that flags performance group to each country
final.ds<- final.ds %>%
  mutate(group = case_when(infection_rate <= median(infection_rate,na.rm = TRUE)
                           & mortality_rate <= median(mortality_rate,na.rm = TRUE) ~ 'Best Performer',
                           
                           infection_rate > median(infection_rate,na.rm = TRUE)
                           & mortality_rate <= median(mortality_rate,na.rm = TRUE) ~ 'High Spreader',
                           
                           infection_rate <= median(infection_rate,na.rm = TRUE)
                           & mortality_rate > median(mortality_rate,na.rm = TRUE) ~ 'Mortality Higher',                           

                           infection_rate > median(infection_rate,na.rm = TRUE)
                           & mortality_rate > median(mortality_rate,na.rm = TRUE) ~ 'Worst Controller'                        
                     )) 


##### 1.7 Create list of possible input values used in interface ########
area_level <- c("Continent","Sub-Region","Country")   
contient_uniq <- na.omit(unique(final.ds$continent))
subreg_uniq   <- na.omit(unique(final.ds$sub_region))
country_uniq   <- na.omit(unique(final.ds$Country))
group_uniq   <- na.omit(unique(final.ds$group))

# Assign Color pallete for 2nd dashboard
my_colors <- RColorBrewer::brewer.pal(8, "Dark2")[c(1,2,3,8,4)]
names(my_colors) <- c('Best Performer','High Spreader','Mortality Higher','Others','Worst Controller')
mypalette <- colorFactor( palette=my_colors, domain=c('Best Performer','High Spreader','Mortality Higher','Others','Worst Controller'), na.color="transparent")

###################################### 2. User Interface ##################################################

# Interface
ui <-fluidPage(
  
  # Add Theme
  theme = shinytheme("darkly"),
  
  # 2.Navigation Page
  navbarPage(
    "Global Government Measures for COVID-19", 
    
    ####2.1 Home tab ####
    tabPanel("HOME", fluidPage(
      br(),
      h1("Global Government Measures for COVID-19"),
      br(),
      h2("Introduction"),
      HTML('Since the end of 2019, the novel unprecedented pandemic called coronavirus (COVID-19) has been globally causing social and economic disruptions. 
      All countries around the world have been impacted by this invisible enemies in some levels depending on social and economic structures. Government around the world implemented
      several COVID-19 policies and countries, which successfully handled the situation, had a better chance to recover their economies. 
      <b style="color: #f39c12;">This interactive dashboard will provide interesting insight about worldwide COVID-19 measures and how best practice countries handled the situations</b>'),
      
      br(),
      br(),
      
      h2("Motivation"),
      HTML("In today's modern world, data-driven decisions in business and policy making, are becoming new waves.
        Data is more accessible and technologies are more advance compared to the last global pandamic. As a data analyst/scientist, this is a good
        opportunity to <b style='color: #f39c12;'> leverage our knowledge for public interests by finding benificial insight</b>."),
      br(),
      br(),
      h2("Scope of Data"),
      p ("The analysis used several data sources with different available time frames"),
      p ("- Jan 2020 - Dec 2020 for measure data "),
      p ("- Jan 2020 - Mar 2021 for daily policy flags and COVID-19 cases"),
      p ("- Aug 2019 for estimated population in 2020"),
      br(),
      br(),
      h2("Questions"),
      HTML("To address the main objective, the analysis is divided into <b style='color: #f39c12;'> 3 sub-questions</b>., which are:"),
      p("1. How did each area across the globe launch public measures to respond to COVID-19?"),
      p("2. Which areas/groups of countries are considered as good practices for handling COVID-19?"),
      p("3. Which strategies differentiate well-performed countries to others?"),
      br(),
      br(),
      h2("Findings"),
      HTML("Findings for sub analysis questions can be presented into three parts, namely <b style='color: #f39c12;'> COVID-19 Measures for Q1, COVID-19 Situation for Q2 and Best Practices for Q3 </b>.
        Dashboard of each topic can be selected at the tabs above and examples of dashboard displays are shown below"),
      br(),
      br(),
      br(),
      br(),
      fluidRow(
        column(4,HTML('<center><img src="/covid_measure.jpg"></center>')),
        column(4,HTML('<center><img src="/covid_situation.jpg"></center>')),
        column(4,HTML('<center><img src="/best_practice.jpg"></center>'))
      ),

      br(),
      br(),
      h2("About Me"),
      br(),
      br(),
      fluidRow(
        
        column(5,img(src="/IMG_2554.jpg", height = "70%", width = "70%", align = "left")),
        column(7, h3("Name"),
               HTML("<b> Pichaphop Sunthornjittanon</b>"),
               h3("Degree"),
               HTML("<b> Master of Data Science at Monash University</b>"),
               h3("Visualisation Ceritficate"),
               HTML("<b>Tableau Specialist</b>"),
               h3("LinkedIn"),
               HTML('<a href="https://linkedin.com/in/pichaphop">Click here!</a>'),
               h3("About"),
               
               HTML("Firstly, I love analysing data and recommending data-driven strategies to real-world business problems. 
               I always believe in the power of data to improve efficiencies not only in the business world but also in the country's development. 
               As data plays an essential role increasingly both in private and public sectors, <b style='color: #f39c12;'> my carrier goal is to become a part of this digital 
               transformation using data to enhance human-being and productivity.</b>"),
               br(),
               br(),  
               HTML("For my professional carrier, I was an <b style='color: #f39c12;'> experienced data analyst </b> in the leading data consultant and artificial intelligent(AI) innovation company in Thailand. 
               My specialisation is <b style='color: #f39c12;'> turning big data into business insights with hands-on experiences </b>  in R, Python, SQL, Tableau, and Machine learning.
               With the integration of those skills, I was given an opportunity to be the main consultant suggesting data-driven strategies to the Thai's largest retail company 
               for the nationwide marketing campaign and the loyalty program. Moreover, in data visualisation, I successfully designed actionable dashboards 
               to the well-known international fast-moving consumer good (FMCG) company, which requires an understanding of FMCG business and data-related skills."),
               br(),
               br(),
               HTML("For my educational background, I studied in a bachelor of economics at Thammasart University in Thailand. Currently, 
                   I am pursuing a <b style='color: #f39c12;'> Master of Data Science at Monash University in Australia. </b> "))
        
      ),
      br(),
      br(),
      h2("References"),
      p("- Phillips, T. (2021, March 15). Methodology for calculating indices. Https://Github.Com/OxCGRT/Covid-Policy-Tracker/Blob/Master/Documentation/Index_methodology.Md. 
        https://github.com/OxCGRT/covid-policy-tracker/tree/master/documentation"),
      p("- COVID-19 Government Measures Dataset. (2020, December 10). ACAPS. https://www.acaps.org/covid-19-government-measures-dataset"),
      p("- COVID-19 Activity - dataset by covid-19-data-resource-hub. (2021, June 1). Data World. https://data.world/covid-19-data-resource-hub/covid-19-case-counts"),
      p("- U.N. (2019, August 28). Population data. Https://Population.Un.Org/Wpp/Download/Standard/Population/. https://population.un.org/wpp/Download/Standard/Population/"),
      p("- Country to Continent. (2018, March 4). Kaggle. https://www.kaggle.com/statchaitya/country-to-continent")

      
      
    )),
    
    ####2.2 COVID-19 MEASURES tab ####
    tabPanel("COVID-19 MEASURES", 
             fluidPage(
               titlePanel("COVID-19 MEASURES"),
               ("Q1: How did each area across the globe launch public measures to respond to COVID-19?"),
               hr(),
               sidebarLayout(
                 
                 ####2.2.1 COVID-19 MEASURES: Sidebar ####
                 sidebarPanel(
                   h2("User Guide"),
                   HTML("- Users can hover around the plots to see more information"),
                   p("- Users can zoom in/out and play around with several functions on top of the plots (it will show when users hover at the plot)"),
                   p("- To drill down or do further exploration, users can change level of detail or filtering in the sidebar below"),
                   br(),
                   h2("Drill-Down Options"),
                   p("Feel free to explore more insight by changing level of detail or specifying any interesting areas that you want to know more."),
                   radioButtons("area_level", "Area Level", area_level),
                   selectInput("continent", "Continent", contient_uniq,multiple = TRUE),
                   selectInput("subregion", "Sub-Region", subreg_uniq,multiple = TRUE),
                   selectInput("country", "Country", country_uniq,multiple = TRUE),
                   numericInput("top", "Top Highest Number of Measures", value = 5, min = 0, max = 100),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   width = 3

                   
                 ),
                 
                 ####2.2.2 COVID-19 MEASURES: main panel ####
                 mainPanel(

                  fluidRow(
                    h2("Number of Measures and Policy Choices Across the Globe"),
                    HTML("The boxplot below displays the distribution of each country's number of measures broken down by areas. The graph shows that 
                    countries in <b style='color: #f39c12;'> Oceania and Europe tended to have higher number of COVID-19 policies </b> shown in the boxplot below.
                    
                    Also, stacked bar on the right illustrates policy contribution, which tells us that 
                      <b style='color: #f39c12;'> Americas, Europe and Oceania had high contribution or focus more in health system policies, while Africa and Asia had higher proportion in Containment & Closure Policies. </b>
                      "),
                    br(),

                    column(5,plotlyOutput("box")),
                    column(7,plotlyOutput("stack"))
                    ),
                  h2("Level of Stringency Trend"),
                  HTML("The line graph below shows stringency index or stringent level of measures. One insight that can be found is that <b style='color: #f39c12;'> Europe, who was in the 3rd place for stringency index, 
                  implemented less strict policy at the beginning, but it became the most stringent in Feb 2021.</b>
                      "),
                  br(),
                  plotlyOutput("line.str")
                 )
               )
               
             )),
             
    ####2.3 COVID-19 SITUATION tab ####         
    tabPanel("COVID-19 SITUATION", titlePanel("COVID-19 SITUATION"),
             ("Q2: Which areas/groups of countries are considered as good practices for handling COVID-19?"),
             hr(),
             fluidPage(
               sidebarLayout(
                 sidebarPanel(
                   
                   ####2.3.1 COVID-19 SITUATION: Sidebar #### 
                   h2("User Guide"),
                   p("- Users can hover around the plots to see more information"),
                   p("- Users can zoom in/out and play around with several functions on top of the plots (it will show when users hover at the plot)"),
                   p("- To drill down or do further exploration, users can change level of detail or filtering in the sidebar below"),
                   br(),
                   h2("Drill-Down Options"),
                   h4("Filter options for groups"),
                   p("Users can set their own criteria to exclude some countries from best performer group"),
                   selectInput("group", "Group", group_uniq,multiple = TRUE),
                   numericInput("Minpop", "Min Pop(Best.)", 
                              value = 1000000,),
                   numericInput("Maxcase", "Max Cases(Best.)", 
                              value = 100000,),
                   br(),
                   h4("Filter options for areas"),  
                   selectInput("continent2", "Continent", contient_uniq,multiple = TRUE),
                   selectInput("subregion2", "Sub-Region", subreg_uniq,multiple = TRUE),
                   selectInput("country2", "Country", country_uniq,multiple = TRUE),                   
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   
                   width = 3
                  ),
                 
                 mainPanel(
                   
                   ####2.3.1 COVID-19 SITUATION: Main panel #### 
                   h2("COVID-19 Matrices for Handling Performance"),
                   HTML("To answer the question, we use two measurements, namely <b style='color: #f39c12;'> infection rate (%)
                   and mortality rate (%) </b> to cluster countries into 4 main groups. These two indicators illustrate how 
                   countries control the outbreak and how they prevent contagious persons from death. Using ratio provides 
                   a fair competition among different population countries and groups are divided into <b style='color: #f39c12;'> Best performer, High spreader, 
                   Mortality Higher and Worst Controller </b> using median of each measurement as a threshold. By doing this, 
                   the results can be visualized through <b style='color: #f39c12;'>  bubble chart and choropleth map.</b>  "),
                   br(),
                   br(),
                   HTML(" When we initially visualized the results, we found that there were some countries, which had extremely high and low population affecting the final clustering results. 
                   Therefore, we set another two criterions to exclude some countries from the best performer group (which will study further in the next section) and named this group to <b style='color: #f39c12;'>'others'.</b>  
                   The additional criterions for best performers were set by the  <b style='color: #f39c12;'>  maximum number of total cases (default value <100K)</b> and <b style='color: #f39c12;'> minimum number of population (default value > 1M)</b>, 
                   which users can adjust those numbers from the sidebar  
                     "),
                   br(),
                   br(),
                   HTML("In conclusion, there were <b style='color: #f39c12;'>  best performer countries all over the world appearing in every continent</b>, which we considered these as best practices for handling COVID19. 
                      From what can be observed from the map, <b style='color: #f39c12;'> many countries are from African and Asia continent</b>. In the next section, users can drill down how best practice countries differetate to others 
                      (The criterions for excluding best performers in this page will be also apply in the next page)"),
                   br(),
                   h3("Bubble Chart for COVID-19 Matrices"),
                   br(),
        
                   plotlyOutput("scat.per"),
                   br(),
                   h3("Map for Groups"),
                   br(),
                   leafletOutput("map.per")
                   
                   )
                 )
               
               ,tabPanel("Best Performer")
             
          
             
             )),
    
    ####2.4 BEST PRACTICES tab #### 
    tabPanel("BEST PRACTICES", titlePanel("BEST PRACTICES"),
             ("Q3: Which strategies that differentiate well-perform countries to others?"),
             hr(),
             
             fluidPage(
               
               ####2.4.1 BEST PRACTICES  side bar #### 
               sidebarLayout(
                 sidebarPanel(
                   h2("User Guide"),
                   p("- Users can hover around the plots to see more information"),
                   p("- Users can zoom in/out and play around with several functions on top of the plots (it will show when users hover at the plot)"),
                   p("- To drill down or do further exploration, users can change level of detail or filtering in the sidebar below"),
                   br(),
                   h2("Drill-Down Options"),
                   
                   radioButtons("sortby",  label = "Sort By", 
                                choices = c('Differences in Percentage','Best Performer','Others'),
                                selected ='Differences in Percentage'),
                   
                   radioButtons("policylevel",  label = "Policy Level", 
                                choices = c('Sub-Category','Category'),
                                selected ='Sub-Category'),
                   numericInput("top2", "Top", value = 10, min = 1, max = 100),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   width = 3),
                 
                 ####2.4.2 BEST PRACTICES  main panel #### 
                 mainPanel(fluidRow(
                   h2("Differences in Policy Choices "),
                   HTML(" After we classified countries to several groups, we want to study about best practices or best performers who can well-managed COVID-19 situation. 
                     First aspect that we want to look at, is how was policy decision in best performer different from others. The plots below display measure contribution(%) of best performer
                     on the left, that of others on the right and differences in percentage in the middle. In the default display, the graphs are sorted by percentage differences. In other word,
                     we sorted this way in order to see which policies that best performers differentiated from others. <b style='color: #f39c12;'> the highest differences in measure contribution from best performer to others were  
                        Visa restriction,(2.5 % higher), Curfews(1.65% higher) and Economic Measures (1.2 %). In other words, best performers focused more on these policies. </b>"),
                   br(),
                   h3("Policy Contribution (%)"),
                   p("of Best Performers(Right) and Others(Left) with differences(Middle)"),
                   column(12,plotlyOutput("best")),
                   br(),
                   h2("Speed of Response"),
                   HTML("Another aspect is speed of response. This is presented through two plots below. The plot on the left is an animation bubble chart that users can click play or adjust slider to
                     see how stringency index and infection rate changed over time. One interesting insight from here is that <b style='color: #f39c12;'> some best performer such as Singapore and Taiwan implemented strict measures
                     in the early stage compared to others.</b> When time changed countries with high infection rate seemed to have high stringency index, while countries with low infection rate had more
                     variability in stringency (depend on countries)."),
                   
                   HTML("The line chart on the right displayed average stringency index of best performer and others. Interestingly, <b style='color: #f39c12;'> In Jan-Feb 2020, best performer had more stringent policies. After that,
                     best performer tended to have lower stringency index than others. In conclusion, considering respond quickly and strictly in the early outbreak stage potentially leads to controllable situation </b>"),
                   
                   br(),
                   h3("Animation Bubble Chart and Stringency Trend"),
                   p("You can clicks play to look at the change overtime in animation plot (Left) and investigate stringency trend (Right)"),
                   column(6,plotlyOutput("aniplot")),
                   column(6,plotlyOutput("strtrend"))
                   
                 )
                           )
                 )
               )

             
             )
         )
  

)

################### 3. Server ############################# 

server <- function(input, output,session) {
  thematic:: thematic_shiny()
  


############## 3.1 Boxplot - Number of Measures by Areas ###################
  output$box   <- renderPlotly({

    ### Create interactivity with filter
    
    # Filter by continent
    if(!is.null(input$continent)){
      final.ds<- final.ds %>% filter(continent %in% input$continent )}
    
    # Filter by sub region
    if(!is.null(input$subregion)){
      final.ds<- final.ds %>% filter(sub_region %in% input$subregion )}    

    # Filter by country
    if(!is.null(input$country)){
      final.ds<- final.ds %>% filter(Country %in% input$country )} 
    
    ### Create box plot
    
    # In continent level (we allow user to select area level)
    if (input$area_level == "Continent"){
          
          # Allow user to select number of row to display
          top.mea.conti <-final.ds %>%
            group_by(continent) %>%
            summarize(med = median(number_of_measures,na.rm = TRUE))%>%
            arrange(desc(med)) %>%
            na.omit() %>%
            slice(1:input$top)
          
          # Create box plot
          meas.box <-
            ggplot(subset(final.ds[!is.na(final.ds$number_of_measures), ],continent %in% top.mea.conti$continent),
                   aes(x=reorder(continent,number_of_measures,FUN =function(x){median(x,na.rm=TRUE)}),
                       y=number_of_measures,
                       fill=continent,
                       text = paste("Country :",Country,"<br>",
                                    "Number of Measures :",number_of_measures ))) +
            geom_boxplot(outlier.shape=NA)+
            theme(legend.position = "none") +
            geom_point(aes(color = continent))+
            coord_flip()+
            labs(x = "Continent", y= "Number of Measures",title = 'Number of Measures By Area')
          
          # Create plot for plotly with customised tooltips
          fig<- ggplotly(meas.box,tooltip = "text") 
          
          
          
          # makes box plot outliers invisible and hover info off
          for (i in 1:length(fig$x$data)){
            if (fig$x$data[[i]]$type=="box"){
              fig$x$data[[i]]$marker$opacity = 0  
              fig$x$data[[i]]$hoverinfo = "none"
            }
          }
          
          
          # Show box plot
          fig 
          
          # In Sub Region level  (we allow user to select area level)
          } else if (input$area_level == "Sub-Region"){
           
                 # Allow user to select number of row to display 
                top.mea.sub <-final.ds %>%
                  group_by(sub_region) %>%
                  summarize(med = median(number_of_measures,na.rm = TRUE))%>%
                  arrange(desc(med)) %>%
                  slice(1:input$top)
              
                # Create box plot
                meas.box <-
                  ggplot(subset(final.ds[!is.na(final.ds$number_of_measures), ],sub_region %in% top.mea.sub$sub_region),
                         aes(x=reorder(sub_region,number_of_measures,FUN =function(x){median(x,na.rm=TRUE)})
                                      , y=number_of_measures,
                             text = paste("Country :",Country,"<br>",
                                          "Number of Measures :",number_of_measures ))) +
                  geom_boxplot(aes(fill=sub_region),outlier.shape = NA)+
                  geom_point(aes(color = sub_region))+
                  coord_flip()+
                  labs(x = "Sub-Regionn", y= "Number of Measures",title = 'Number of Measures By Area')+
                  theme(legend.position = "none")
                
                # Show box plot
                fig<- ggplotly(meas.box,tooltip = "text") 
                fig
                
                
                # makes box plot outliers invisible and hover info off
                for (i in 1:length(fig$x$data)){
                  if (fig$x$data[[i]]$type=="box"){
                    fig$x$data[[i]]$marker$opacity = 0  
                    fig$x$data[[i]]$hoverinfo = "none"
                  }
                }
                
                fig<- ggplotly(fig,tooltip = "text") 
                
          # In Country level (we allow user to select area level)
          }else if (input$area_level == "Country"){
            
            # Allow user to select number of row to display
            top.mea.con <-final.ds %>%
              group_by(Country) %>%
              summarize(med = median(number_of_measures,na.rm = TRUE))%>%
              arrange(desc(med)) %>%
              slice(1:input$top)
            
            # Create box plot
            meas.box <-
              ggplot(subset(final.ds[!is.na(final.ds$number_of_measures), ],Country %in% top.mea.con$Country),
                     aes(x=reorder(Country,number_of_measures,FUN =function(x){median(x,na.rm=TRUE)})
                         , y=number_of_measures,fill=Country,
                         text = paste("Country :",Country,"<br>",
                                      "Number of Measures :",number_of_measures ))) +
              geom_bar(stat = "identity")+
              coord_flip()+
              labs(x = "Country", y= "Number of Measures",title = 'Number of Measures By Area')+
              theme(legend.position = "none")
            
            # Show box plot
            ggplotly(meas.box,tooltip = NA)
          }
  })
  
  output$stack <-  renderPlotly({
    
    ###### 3.2 Stacked Bar - Measure Category Contribution ############
    
    # Aggregate data for 100% stacked bar chart
    stack_bar_agg <- ds1measure %>%
      group_by(COUNTRY,CATEGORY)%>%
      summarize(number_of_measures =n_distinct(ID))%>%
      left_join(y=ds5continent,by = c("COUNTRY" = "country")) %>%
      mutate( cat =
                case_when( CATEGORY %in% c("Lockdown",
                                           "Movement restrictions","Social distancing") ~ 'Containment & Closure Policies',
                           CATEGORY == "Public health measures" ~ "Health System Policies ",
                           CATEGORY == "Governance and socio-economic measures" ~ "Economic Policies"
                )
      ) %>%
      na.omit(cat)
    
    # Create interactivity with filter
    
    # Filter stack_bar_agg by continent
    if(!is.null(input$continent)){
      stack_bar_agg<- stack_bar_agg %>% filter(continent %in% input$continent )}
    
    # Filter stack_bar_agg by sub region
    if(!is.null(input$subregion)){
      stack_bar_agg<- stack_bar_agg %>% filter(sub_region %in% input$subregion )}    
    
    # Filter stack_bar_agg by country
    if(!is.null(input$country)){
      stack_bar_agg<- stack_bar_agg %>% filter(COUNTRY %in% input$country )} 
    
    # Filter final.ds by continent
    if(!is.null(input$continent)){
      final.ds<- final.ds %>% filter(continent %in% input$continent )}
    
    # Filter final.ds by sub region
    if(!is.null(input$subregion)){
      final.ds<- final.ds %>% filter(sub_region %in% input$subregion )}    
    
    # Filter final.ds by country
    if(!is.null(input$country)){
      final.ds<- final.ds %>% filter(Country %in% input$country )} 
    
    # In continent level (we allow user to select area level)
    if (input$area_level == "Continent"){
            
            # Select number of measure group by continent,cat
            stack_bar_agg <- stack_bar_agg %>%
                                group_by(continent,cat) %>%
                                summarize(number_of_measures = sum(number_of_measures))
            
            # rank the data that is similar to the box plot manner
            rank_med <- final.ds %>%
              group_by(continent) %>%
              summarize(med = (median(number_of_measures,na.rm = TRUE))) %>%
              arrange(med)
            
            # Create level to continent in order to follow box plot order
            stack_bar_agg$continent <- factor(stack_bar_agg$continent,
                                              levels = rank_med$continent)
            
            # create variable used for stacked bar label (text position,label)
            test_labels <- stack_bar_agg %>%
              arrange(continent, desc(cat)) %>%
              group_by(continent) %>%
              mutate(ylabel_pos = cumsum(number_of_measures)/sum(number_of_measures),
                     ylabel = number_of_measures/sum(number_of_measures))%>%
              group_by(cat, add = TRUE) %>%
              mutate(ylabel = sum(ylabel)) %>%
              slice(n())
            
            # Allow user to select number of row to display
            top.mea.con <-final.ds %>%
              group_by(continent) %>%
              summarize(med = median(number_of_measures,na.rm = TRUE))%>%
              arrange(desc(med))%>%
              slice(1:input$top)
            
            
                               
            # create stacked bar 
            meas.stack<-ggplot(subset(stack_bar_agg,continent %in% top.mea.con$continent),
                               aes(fill=cat, y=number_of_measures, 
                                                  x=continent)) + 

              geom_bar(position="fill", stat="identity")+
              scale_y_continuous(labels = percent) +
              geom_text(data = subset(test_labels,continent %in% top.mea.con$continent) , 
                        aes(y = ylabel_pos, label=paste(round(ylabel*100,0),"%")),size=3)+
              
              coord_flip()+
              scale_fill_manual(values=c("#1b9e77", "#d95f02", "#7570b3")) +
              labs(x = "Continent", y= "% Number of Measures by Category",title = "Measure Category Contribution (%)")+ 
              theme(legend.title = element_blank())
            
            ggplotly(meas.stack,tooltip = "")

            
      # In Sub-region level (we allow user to select area level)         
            }else if (input$area_level == "Sub-Region"){
      
      # Select number of measure group by sub_region,cat
      stack_bar_agg <- stack_bar_agg %>%
        group_by(sub_region,cat) %>%
        summarize(number_of_measures = sum(number_of_measures,na.rm = TRUE))
      
      # rank the data that is similar to the box plot manner
      rank_med <- final.ds %>%
        group_by(sub_region) %>%
        summarize(med = (median(number_of_measures,na.rm = TRUE))) %>%
        arrange(med)
      
      # Create level to sub region in order to follow box plot order
      stack_bar_agg$sub_region <- factor(stack_bar_agg$sub_region,
                                        levels = rank_med$sub_region)
      
      stack_bar_agg <- stack_bar_agg%>%
                        na.omit(sub_region)
      
      # create variable used for stacked bar label (text position,label)
      test_labels <- stack_bar_agg %>%
        arrange(sub_region, desc(cat)) %>%
        group_by(sub_region) %>%
        mutate(ylabel_pos = cumsum(number_of_measures)/sum(number_of_measures),
               ylabel = number_of_measures/sum(number_of_measures))%>%
        group_by(cat, add = TRUE) %>%
        mutate(ylabel = sum(ylabel)) %>%
        slice(n())
      
      # Allow user to select number of row to display
      top.mea.sub <-final.ds %>%
        group_by(sub_region) %>%
        summarize(med = median(number_of_measures,na.rm = TRUE))%>%
        arrange(desc(med))%>%
        slice(1:input$top)
      
      
      # create stacked bar 
      meas.stack<-ggplot(subset(stack_bar_agg,sub_region %in% top.mea.sub$sub_region),
                         aes(fill=cat, y=number_of_measures, 
                                            x=sub_region)) + 

        geom_bar(position="fill", stat="identity")+
        scale_y_continuous(labels = percent) +
        
        geom_text(data = subset(test_labels,sub_region %in% top.mea.sub$sub_region), 
                  aes(y = ylabel_pos, label=paste(round(ylabel*100,0),"%")),size=3)+

        coord_flip()+
        
        scale_fill_manual(values=c("#1b9e77", "#d95f02", "#7570b3")) +
        labs(x = "Sub-Region", y= "% Number of Measures by Category",title = "Measure Category Contribution (%)")+ 
        theme(legend.title = element_blank())
      
      ggplotly(meas.stack,tooltip = "")
            
            # In country level (we allow user to select area level) 
            }else if (input$area_level == "Country"){
              
              # Select number of measure group by country,cat
              stack_bar_agg <- stack_bar_agg %>%
                group_by(COUNTRY,cat) %>%
                summarize(number_of_measures = sum(number_of_measures))
              
              # rank the data that is similar to the box plot manner
              rank_med <- final.ds %>%
                group_by(Country) %>%
                summarize(med = (median(number_of_measures,na.rm = TRUE))) %>%
                arrange(med)
              
              # Create level to country in order to follow box plot order
              stack_bar_agg$COUNTRY <- factor(stack_bar_agg$COUNTRY,
                                                 levels = rank_med$Country)
              
              stack_bar_agg <- stack_bar_agg%>%
                na.omit(sub_region)
              
              # create variable used for stacked bar label
              test_labels <- stack_bar_agg %>%
                arrange(COUNTRY, desc(cat)) %>%
                group_by(COUNTRY) %>%
                mutate(ylabel_pos = cumsum(number_of_measures)/sum(number_of_measures),
                       ylabel = number_of_measures/sum(number_of_measures))%>%
                group_by(cat, add = TRUE) %>%
                mutate(ylabel = sum(ylabel)) %>%
                slice(n())
              
              # Allow user to select number of row to display
              top.mea.sub <-final.ds %>%
                group_by(Country) %>%
                summarize(med = median(number_of_measures,na.rm = TRUE))%>%
                arrange(desc(med))%>%
                slice(1:input$top)

              
              # create stacked bar 
              meas.stack<-ggplot(subset(stack_bar_agg,COUNTRY %in% top.mea.sub$Country),
                                 aes(fill=cat, y=number_of_measures, 
                                     x=COUNTRY, tooltip = cat)) + 

                geom_bar(position="fill", stat="identity")+
                scale_y_continuous(labels = percent) +
                geom_text(data = subset(test_labels,COUNTRY %in% top.mea.sub$Country), 
                          aes(y = ylabel_pos, label=paste(round(ylabel*100,0),"%")),size=3)+
                
                coord_flip()+
                
                scale_fill_manual(values=c("#1b9e77", "#d95f02", "#7570b3")) +
                labs(x = "Country", y= "% Number of Measures by Category",title = "Measure Category Contribution (%)")+ 
                theme(legend.title = element_blank())
              
              ggplotly(meas.stack,tooltip = "")
            }        
    }
  
  )  
  ########### 3.3 Line graph - Stringency Index Trend #####################
  output$line.str <- 
    renderPlotly({
      
      # Manipulate Stringency Index Data
      policytrack <-
        ds2policytrack %>% 
        mutate(Date =  as.Date(as.character(Date), format ="%Y%m%d")) %>%
        left_join(ds5continent,by =  c(CountryName="country")) %>%
        rename(Country = CountryName) %>%
        select(Date,Country,continent,sub_region,StringencyIndex)
        
      
      ### Create interactivity with filter
      
      # Filter by continent
      if(!is.null(input$continent)){
        policytrack<- policytrack %>% filter(continent %in% input$continent )}

      # Filter by sub region
      if(!is.null(input$subregion)){
        policytrack<- policytrack %>% filter(sub_region %in% input$subregion )}

      # Filter by country
      if(!is.null(input$country)){
        policytrack<- policytrack %>% filter(Country %in% input$country )}
      
      # Filter final.ds by continent
      if(!is.null(input$continent)){
        final.ds<- final.ds %>% filter(continent %in% input$continent )}
      
      # Filter final.ds by sub region
      if(!is.null(input$subregion)){
        final.ds<- final.ds %>% filter(sub_region %in% input$subregion )}    
      
      # Filter final.ds by country
      if(!is.null(input$country)){
        final.ds<- final.ds %>% filter(Country %in% input$country )} 
      
      # In continent level (we allow user to select area level) 
      if (input$area_level == "Continent"){
      
      # Aggregate data group by data, continent 
      policytrack <-
        policytrack %>%
        group_by(Date,continent) %>%
        na.omit(continent) %>%
        summarize(Stringency_index = round(mean(StringencyIndex,na.rm = TRUE),2))
      

      # rank the data that is similar to the box plot manner
      rank_med <- final.ds %>%
        group_by(continent) %>%
        summarize(med = (median(number_of_measures))) %>%
        arrange(med)
      

      # Allow user to select number of row to display
      top.mea.con <-final.ds %>%
        group_by(continent) %>%
        summarize(med = median(number_of_measures))%>%
        arrange(desc(med))%>%
        slice(1:input$top)
      
      # Create Line Chart
      line.chart <-ggplot(subset(policytrack,continent %in% top.mea.con$continent),
                          aes(x=Date,y=Stringency_index,color =continent)) +
        geom_line()+
        labs(title = "Stringency Index Trend")
      
      # Use the chart in Plotly(library) mode
      fig <- ggplotly(line.chart)
      
      # Create Interactive date Filter
      fig <- fig %>% layout(
        xaxis = list(
          title = "Date Range Selection for Stringency Index",
          rangeslider = list(type = "date")),
        
        yaxis = list(title = "Stringency Index"))
      
      fig
      
      # In Sub Region level (we allow user to select area level) 
      }else if (input$area_level == "Sub-Region"){
        
        # Aggregrate data group by date, sub region 
        policytrack <-
          policytrack %>%
          group_by(Date,sub_region) %>%
          na.omit(sub_region) %>%
          summarize(Stringency_index = mean(StringencyIndex,na.rm = TRUE))
        
        
        # rank the data that is similar to the box plot manner
        rank_med <- final.ds %>%
          group_by(sub_region) %>%
          summarize(med = (median(number_of_measures))) %>%
          arrange(med)
        
        
        # Allow user to select number of row to display
        top.mea.sub <-final.ds %>%
          group_by(sub_region) %>%
          summarize(med = median(number_of_measures))%>%
          arrange(desc(med))%>%
          slice(1:input$top)

        # Create Line Chart
        line.chart <-ggplot(subset(policytrack,sub_region %in% top.mea.sub$sub_region),
                            aes(x=Date,y=Stringency_index,color =sub_region)) +
          geom_line()+
          labs(title = "Stringency Index Trend")
        
        # Use the chart in Plotly(library) mode
        fig <- ggplotly(line.chart)
        
        # Create Interactive date Filter
        fig <- fig %>% layout(
          xaxis = list(
            title = "Date Range Selection for Stringency Index",
            rangeslider = list(type = "date")),
          
          yaxis = list(title = "Stringency"))
        
        fig
        
      # In country level (we allow user to select area level)  
      }else if (input$area_level == "Country"){
        
        # Aggregate data group by date, country 
        policytrack <-
          policytrack %>%
          group_by(Date,Country) %>%
          na.omit(Country) %>%
          summarize(Stringency_index = mean(StringencyIndex,na.rm = TRUE))
        

        
        # rank the data that is similar to the box plot manner
        rank_med <- final.ds %>%
          group_by(Country) %>%
          summarize(med = (median(number_of_measures))) %>%
          arrange(med)
        
        
        # Allow user to select number of row to display
        top.mea.con <-final.ds %>%
          group_by(Country) %>%
          summarize(med = median(number_of_measures))%>%
          arrange(desc(med))%>%
          slice(1:input$top)
        
        # Create Line Chart
        line.chart <-ggplot(subset(policytrack,Country %in% top.mea.con$Country),
                            aes(x=Date,y=Stringency_index,color =Country)) +
          geom_line()+
          labs(title = "Stringency Index Trend")
        
        # Use the chart in Plotly(library) mode
        fig <- ggplotly(line.chart)
        
        # Create Interactive date Filter
        fig <- fig %>% layout(
          xaxis = list(
            title = "Date Range Selection for Stringency Index",
            rangeslider = list(type = "date")),
          
          yaxis = list(title = "Stringency"))
        
        fig
        
      }
    })  
  ########### 3.4 Bubble Chart - Bubble Chart for COVID-19 Matrices #####################
  output$scat.per <- 
    
      renderPlotly({
        


        # Create group column (Allow user to interactively set criteria for
        # filtering best performer group)
        
        final.ds<- final.ds %>%
          mutate(group = case_when(infection_rate <= median(infection_rate,na.rm = TRUE)
                                   & mortality_rate <= median(mortality_rate,na.rm = TRUE) 
                                   & Number_of_cases <=input$Maxcase & Total_Population >= input$Minpop
                                   ~ 'Best Performer',
                                   
                                   infection_rate > median(infection_rate,na.rm = TRUE)
                                   & mortality_rate <= median(mortality_rate,na.rm = TRUE) ~ 'High Spreader',
                                   
                                   infection_rate <= median(infection_rate,na.rm = TRUE)
                                   & mortality_rate > median(mortality_rate,na.rm = TRUE) ~ 'Mortality Higher',                           
                                   
                                   infection_rate > median(infection_rate,na.rm = TRUE)
                                   & mortality_rate > median(mortality_rate,na.rm = TRUE) ~ 'Worst Controller',
                                   TRUE ~ 'Others'
                                   )) %>%
          mutate(group = as.factor(group))
        
        # Create interactivity with filter
        
        # Filter by continent
        if(!is.null(input$continent2)){
          final.ds<- final.ds %>% filter(continent %in% input$continent2 )}
        
        # Filter by sub region
        if(!is.null(input$subregion2)){
          final.ds<- final.ds %>% filter(sub_region %in% input$subregion2 )}    
        
        # Filter by country
        if(!is.null(input$country2)){
          final.ds<- final.ds %>% filter(Country %in% input$country2 )} 
          
        

        
        # Create interactivity with filter (group)
        if(!is.null(input$group)){
          final.ds<- final.ds %>% filter(group %in% input$group )}
        

        
        # Create scatter plot 

        scatterPlot <-  ggplot(data = final.ds, 
                                    aes(x = infection_rate, y = mortality_rate,color =group,
                                        size = Number_of_cases,
                                        text = paste("Country : ", Country,"<br>",
                                                     "Group :",group,"<br>",
                                                     "Infection Rate :",round(infection_rate,4)*100,'%',"<br>", 
                                                     "Mortality Rate :",round(mortality_rate,4)*100,'%',"<br>",
                                                     "Number of Cases :",Number_of_cases,"<br>",
                                                     "Population :",Total_Population,"<br>")
                                        
                                        ))+
                              geom_point(alpha=0.5) +
                              labs(x = "Infection Rate (%)",
                                   y = "Mortality Rate (%)",
                                   title = "") +
                              scale_y_continuous(labels = percent) +
                              scale_x_continuous(labels = percent) +
                              scale_colour_manual(name = "group",values= my_colors)+
                              guides(size = FALSE) +
                              geom_hline(yintercept=median(final.ds$mortality_rate),
                                         linetype="dashed", color = "#2C528C", size=0.5) +
                              geom_vline(xintercept=median(final.ds$infection_rate,na.rm = TRUE), 
                                         linetype="dashed", color = "#2C528C", size=0.5) 
                                                  
        ggplotly(scatterPlot,tooltip = "text") %>% 
          layout(legend = list(title=list(text='<b> Group </b>'),x = 0.8, y = 1))
        
        })
  
  ###### 3.5 Choropleth map - Map for groups ############
  output$map.per <- 
    renderLeaflet({


      # Create group column (Allow user to interactively set criteria for
      # filtering best performer group)
      
      final.ds<- final.ds %>%
        mutate(group = case_when(infection_rate <= median(infection_rate,na.rm = TRUE)
                                 & mortality_rate <= median(mortality_rate,na.rm = TRUE) 
                                 & Number_of_cases <=input$Maxcase & Total_Population >= input$Minpop
                                 ~ 'Best Performer',
                                 
                                 infection_rate > median(infection_rate,na.rm = TRUE)
                                 & mortality_rate <= median(mortality_rate,na.rm = TRUE) ~ 'High Spreader',
                                 
                                 infection_rate <= median(infection_rate,na.rm = TRUE)
                                 & mortality_rate > median(mortality_rate,na.rm = TRUE) ~ 'Mortality Higher',                           
                                 
                                 infection_rate > median(infection_rate,na.rm = TRUE)
                                 & mortality_rate > median(mortality_rate,na.rm = TRUE) ~ 'Worst Controller',
                                 TRUE ~ 'Others'
        )) %>%
        mutate(group = as.factor(group))
      
      
      # Create interactivity with filter
      
      # Filter by continent
      if(!is.null(input$continent2)){
        final.ds<- final.ds %>% filter(continent %in% input$continent2 )}
      
      # Filter by sub region
      if(!is.null(input$subregion2)){
        final.ds<- final.ds %>% filter(sub_region %in% input$subregion2 )}    
      
      # Filter by country
      if(!is.null(input$country2)){
        final.ds<- final.ds %>% filter(Country %in% input$country2 )} 
      
      

      
      # Create interactivity with filter (group)
      if(!is.null(input$group)){
        final.ds<- final.ds %>% filter(group %in% input$group )}
      
      
      # Reference
      # https://www.r-graph-gallery.com/183-choropleth-map-with-leaflet.html
      # https://cran.r-project.org/web/packages/rworldmap/vignettes/rworldmap.pdf
                
       # Join data to get spatial data (prepare before inputting in leaflet map)
       malMap <- joinCountryData2Map(final.ds, joinCode = "ISO3",
                                              nameJoinColumn = "code_3")
       
       
       
       # Create html label
       labs <- lapply(seq(nrow(malMap@data)), function(i) {
         paste0( 'Group :', malMap@data[i, "group"], '<br>', 
                 "Country: ",malMap@data[i, "Country"], '<br>',
                 "Infection Rate: ",round(malMap@data[i, "infection_rate"],4)*100,'%', '<br>',
                 "Mortality Rate: ",round(malMap@data[i, "mortality_rate"],4)*100,'%'
                 
                 ) 
       })
       
       # Create Map
       m <- leaflet(malMap) %>% 
               addTiles()  %>%
               addProviderTiles(providers$CartoDB.DarkMatter) %>%
               setView( lat=10, lng=0 , zoom=0.5) %>%
               addPolygons(color = ~mypalette(malMap@data$group)
                              , stroke=FALSE, fillOpacity = 0.8,
                           label= lapply(labs, htmltools::HTML))

                m

                })
  
  output$strtrend <-
    renderPlotly({
      
      ######3.6 Line Chart - Stringency Trend (BEST PRACTICE)  ############
      # Manipulate Stringency Index Data
      
      # Cluster countries to groups with excluding criteria for best performers
      final.ds<-final.ds  %>%
        mutate(group = case_when(infection_rate <= median(infection_rate,na.rm = TRUE)
                                 & mortality_rate <= median(mortality_rate,na.rm = TRUE)
                                 & Number_of_cases <=input$Maxcase & Total_Population >= input$Minpop 
                                 ~ 'Best Performer'
                                 ,
                                 
                                 infection_rate > median(infection_rate,na.rm = TRUE)
                                 & mortality_rate <= median(mortality_rate,na.rm = TRUE) ~ 'High Spreader',
                                 
                                 infection_rate <= median(infection_rate,na.rm = TRUE)
                                 & mortality_rate > median(mortality_rate,na.rm = TRUE) ~ 'Mortality Higher',                           
                                 
                                 infection_rate > median(infection_rate,na.rm = TRUE)
                                 & mortality_rate > median(mortality_rate,na.rm = TRUE) ~ 'Worst Controller',
                                 TRUE ~ 'Others'
        )) %>%
        mutate(best_group =  case_when(group == "Best Performer" ~ "Best Performer",
                                       TRUE ~ "Others" ))
      
      # Prepare data for stringency trend
      policytrack <-
        ds2policytrack %>%
        mutate(Date =  as.Date(as.character(Date), format ="%Y%m%d")) %>%
        left_join(final.ds,by =  c(CountryName = "Country")) %>%
        rename(Country = CountryName) %>%
        select(Date,Country,best_group,StringencyIndex.x) %>%
        na.omit() %>%
        group_by(Date,best_group) %>%
        summarize(Stringency_index = round(mean(StringencyIndex.x,na.rm = TRUE),2))
      
      # Create Line Chart
      line.chart <-ggplot(policytrack,aes(x=Date,y=Stringency_index,color =best_group)) +
        geom_line() +
        scale_color_manual(values = c('#00bc8c','#f39c12'))
      
      # Use the chart in Plot library mode
      fig <- ggplotly(line.chart)
      
      # Create Interactive Filter
      fig <- fig %>% layout(
        xaxis = list(rangeslider = list(type = "date")),
        yaxis = list(title = "Stringency Index"),
        legend = list(orientation = 'h'),
        font = list(family = "sans serif",color = 'white',size =14))
      
      fig
      
      
    })
    
  output$aniplot <-
    renderPlotly({
      
      ######3.7 Animation Bubble Chart - Stringency VS Infection Rate ############
      # Manipulate Stringency Index Data
      
      # Cluster countries to groups with excluding criteria for best performers
      final.ds<-final.ds  %>%
        mutate(group = case_when(infection_rate <= median(infection_rate,na.rm = TRUE)
                                 & mortality_rate <= median(mortality_rate,na.rm = TRUE)
                                 & Number_of_cases <=input$Maxcase & Total_Population >= input$Minpop 
                                 ~ 'Best Performer'
                                 ,
                                 
                                 infection_rate > median(infection_rate,na.rm = TRUE)
                                 & mortality_rate <= median(mortality_rate,na.rm = TRUE) ~ 'High Spreader',
                                 
                                 infection_rate <= median(infection_rate,na.rm = TRUE)
                                 & mortality_rate > median(mortality_rate,na.rm = TRUE) ~ 'Mortality Higher',                           
                                 
                                 infection_rate > median(infection_rate,na.rm = TRUE)
                                 & mortality_rate > median(mortality_rate,na.rm = TRUE) ~ 'Worst Controller',
                                 TRUE ~ 'Others'
        )) %>%
        mutate(best_group =  case_when(group == "Best Performer" ~ "Best Performer",
                                       TRUE ~ "Others" ))
      
      # Data preparation for daily stringency index joining with data source that has already clustered the countries 
      dailypolicy <-
        ds2policytrack %>%
        mutate(Date =  as.Date(as.character(Date), format ="%Y%m%d")) %>%
        left_join(final.ds,by =  c(CountryName = "Country")) %>%
        rename(Country = CountryName) %>%
        select(Date,Country,best_group,StringencyIndex.x)
      
      # Data preparation for daily COVID-19 cases index joining with data source that has already clustered the countries 
      dailycases <- ds3cases %>%
        mutate(Date =  as.Date(as.character(REPORT_DATE), format ="%Y-%m-%d")) %>%
        left_join(ds4pop,by= c("COUNTRY" = "Country")) %>%
        mutate(infection_rate = PEOPLE_POSITIVE_CASES_COUNT/Total_Population)%>%
        select(Date,COUNTRY,infection_rate,Total_Population)%>%
        rename(Country = COUNTRY)
      
      # Join both data sources above and aggregate data to monthly level
      dailycases
      join.daily <- dailypolicy %>%
        left_join(dailycases,by= c("Country" = "Country","Date"="Date")) %>%
        na.omit() %>%
        mutate(Month = format(Date, format = "%Y-%m")) %>%
        group_by(Month,Country,best_group) %>%
        summarise(infection_rate = max(infection_rate),StringencyIndex =mean(StringencyIndex.x),Total_Population =mean(Total_Population))
      
      
      # Plot the animation bubble chart
      fig <- join.daily %>%
        plot_ly(
          x = ~infection_rate, 
          y = ~StringencyIndex, 
          # size = ~Total_Population, 
          color = ~best_group, 
          frame = ~Month, 
          text = ~Country, 
          # hoverinfo = "text",
          type = 'scatter',
          mode = 'markers',
          colors = c('#00bc8c','#f39c12'),
          hovertemplate = paste(
            "<b>%{text}</b><br><br>",
            "%{yaxis.title.text}: %{y:.2f}<br>",
            "%{xaxis.title.text}: %{x:.0%f}<br>")
          # marker = list(sizemode = 'diameter')
        )
      
      # Adjust the layout
      fig  %>%
        layout(paper_bgcolor = "#222222",
               plot_bgcolor = "#303030",
               font = list(family = "sans serif",color = 'white',size =14),
               legend = list(orientation = 'h',y=0.93),
               xaxis = list(title="Infection Rate",tickformat = "%",color = '#888'),
               yaxis = list(title="Stringency Index"))%>%
        animation_slider(
          currentvalue = list(prefix = "", font = list(color="#888",size =14))
        )
      
      
    })
  
  
  ######3.7 Multiple bar charts - Policy Contribution (%) of Best Performers and Others with Differences############
  output$best <-
    renderPlotly({
      
      # Cluster countries to groups with excluding criteria for best performers
      final.ds<-final.ds  %>%
        mutate(group = case_when(infection_rate <= median(infection_rate,na.rm = TRUE)
                                 & mortality_rate <= median(mortality_rate,na.rm = TRUE)
                                 & Number_of_cases <=input$Maxcase & Total_Population >= input$Minpop 
                                 ~ 'Best Performer'
                                 ,
                                 
                                 infection_rate > median(infection_rate,na.rm = TRUE)
                                 & mortality_rate <= median(mortality_rate,na.rm = TRUE) ~ 'High Spreader',
                                 
                                 infection_rate <= median(infection_rate,na.rm = TRUE)
                                 & mortality_rate > median(mortality_rate,na.rm = TRUE) ~ 'Mortality Higher',                           
                                 
                                 infection_rate > median(infection_rate,na.rm = TRUE)
                                 & mortality_rate > median(mortality_rate,na.rm = TRUE) ~ 'Worst Controller',
                                 TRUE ~ 'Others'
        )) %>%
        mutate(best_group =  case_when(group == "Best Performer" ~ "Best Performer",
                                       TRUE ~ "Others" ))
      
      
      
      # Create data set that has number of policy by best performer group status and policy category
      measurecat <-
        ds1measure %>%
        left_join(final.ds,by= c("COUNTRY" = "Country"))%>%
        mutate( cat =
                  case_when( CATEGORY %in% c("Lockdown",
                                             "Movement restrictions","Social distancing") ~ 'Containment & Closure Policies',
                             CATEGORY == "Public health measures" ~ "Health System Policies ",
                             CATEGORY == "Governance and socio-economic measures" ~ "Economic Policies")) %>%
        na.omit() %>%
        select(best_group,CATEGORY,MEASURE) %>%
        group_by(MEASURE,best_group,CATEGORY) %>%
        summarise(number_of_policies = n()) 
      
      
      # Create data set for best performer measures and find measure contribution
      best_measure<-
        measurecat %>%
        filter(best_group == 'Best Performer' ) 
      
      best_measure<- 
        best_measure %>%
        mutate(policy_percentage = number_of_policies/sum(best_measure$number_of_policies))
      
      # Create data set for others and find measure contribution
      other_measure<-
        measurecat %>%
        filter(best_group == 'Others' ) 
        
        
      other_measure<- 
        other_measure %>%
        mutate(policy_percentage = number_of_policies/sum(other_measure$number_of_policies)) %>%
        filter(MEASURE %in% best_measure$MEASURE)
      
      
      # Create data set for percentage different between best performer and others 
      diff_measure <-
        other_measure %>%
        full_join(best_measure,by= c("MEASURE" = "MEASURE"))%>%
        mutate(percentage_different = policy_percentage.x - policy_percentage.y) %>%
        select(MEASURE,CATEGORY.x,percentage_different)%>%
        arrange(desc(percentage_different)) %>%
        rename(CATEGORY = CATEGORY.x)
      

      # If policy is selected as Sub-Category
      if(input$policylevel == "Sub-Category"){

      
              # Assign same order to percentage different
              if(input$sortby == 'Differences in Percentage'){
              best_measure$MEASURE <- factor(best_measure$MEASURE,
                                             levels = diff_measure$MEASURE)
              
              other_measure$MEASURE <- factor(other_measure$MEASURE,
                                              levels = diff_measure$MEASURE)  
              
              diff_measure$MEASURE<- factor(diff_measure$MEASURE,
                                            levels = diff_measure$MEASURE)  
              
              # Assign same order to best performer highest policy contribution
              
              }else if(input$sortby == 'Best Performer'){
                
                best_measure<- best_measure %>% arrange((policy_percentage))
                
                best_measure$MEASURE <- factor(best_measure$MEASURE,
                                               levels = best_measure$MEASURE)
                
                other_measure$MEASURE <- factor(other_measure$MEASURE,
                                                levels = best_measure$MEASURE)  
                
                diff_measure$MEASURE<- factor(diff_measure$MEASURE,
                                              levels = best_measure$MEASURE) 
                
              # Assign same order to others highest policy contribution
              }else if(input$sortby == 'Others'){
                
                other_measure<- other_measure %>% arrange((policy_percentage))
                
                best_measure$MEASURE <- factor(best_measure$MEASURE,
                                               levels = other_measure$MEASURE)
                
                other_measure$MEASURE <- factor(other_measure$MEASURE,
                                                levels = other_measure$MEASURE)  
                
                diff_measure$MEASURE<- factor(diff_measure$MEASURE,
                                              levels = other_measure$MEASURE) 
                
                
              }
        
              # Sort the data by selected condition with selecting only top rows
              best_measure<- best_measure %>%
                arrange(MEASURE) %>%
                tail(input$top2)
              
              # Sort the data by selected condition with selecting only top rows
              other_measure<- other_measure %>%
                arrange(MEASURE)%>%
                tail(input$top2)
              
              # Sort the data by selected condition with selecting only top rows
              diff_measure <- diff_measure %>%
                arrange(MEASURE) %>%
                tail(input$top2)
              
              # Create bar chart
              fig1 <- plot_ly(best_measure, 
                              y = ~MEASURE,
                              x = ~policy_percentage, 
                              type = 'bar', orientation='h',
                              marker = list(color = '#00bc8c'), 
                              name = 'Best Performer',
                              hovertemplate = paste(
                                "%{y}<br>",
                                "Policy Contribution: %{x:.2%f}<br>"))
            
              fig1 <- fig1 %>% layout(xaxis = list(tickformat = "%.2f"))
              
              # Create bar chart
              fig2 <- plot_ly(other_measure,
                              y = ~MEASURE,
                              x = ~policy_percentage, 
                              type = 'bar',
                              orientation='h',
                              marker = list(color = '#f39c12')
                              , name = 'Others',
                              hovertemplate = paste(
                                "%{y}<br>",
                                "Percentage Differences: %{x:.2%f}<br>"))
              
              fig2 <- fig2 %>% layout(yaxis = list(showgrid = FALSE, 
                                                   showline = TRUE, 
                                                   showticklabels = FALSE),
                                      xaxis = list(tickformat = "%"))
              
              # Create bar chart
              fig3 <- plot_ly(diff_measure, y = ~MEASURE, x = ~percentage_different, 
                              type = 'bar', orientation='h',
                              marker = list(color=ifelse(diff_measure$percentage_different>0,"#e83e8c","#375a7f"))
                              , name = 'Differences in Percentage',
                              hovertemplate = paste(
                                "%{y}<br>",
                                "Policy Contribution: %{x:.2%f}<br>"))
        
              fig3 <- fig3 %>% layout(yaxis = list(showgrid = FALSE, showline = TRUE, showticklabels = FALSE),
                                      xaxis = list(tickformat = "%"))
              
              # Create multiple bar charts
              fig <- subplot(fig1, fig3,fig2) 
              
              # adjust the layout
              fig  %>%
                layout(paper_bgcolor = "#222222",
                       plot_bgcolor = "#303030",
                       font = list(family = "sans serif",color = 'white'),
                       legend = list(orientation = 'h')
                       )
              # If users select category level then
              } else if(input$policylevel == "Category"){
        
        # Aggregate data to category level for best performer  
        best_measure<- 
          best_measure %>%
          group_by(CATEGORY) %>%
          summarise(policy_percentage = sum(policy_percentage))
        
        # Aggregate data to category level for others  
        other_measure<-
          other_measure %>%
          group_by(CATEGORY) %>%
          summarise(policy_percentage = sum(policy_percentage))
        
        # Aggregate data to category level for percentage differences 
        diff_measure<-
          diff_measure %>%
          group_by(CATEGORY) %>%
          summarise(percentage_different = sum(percentage_different))
        
        
        
        # Assign same order to percentage different
        if(input$sortby == 'Differences in Percentage'){
          
          diff_measure <- diff_measure %>% arrange(desc(percentage_different))
          best_measure$CATEGORY <- factor(best_measure$CATEGORY,
                                          levels = diff_measure$CATEGORY)
          
          other_measure$CATEGORY <- factor(other_measure$CATEGORY,
                                           levels = diff_measure$CATEGORY)  
          
          diff_measure$CATEGORY<- factor(diff_measure$CATEGORY,
                                         levels = diff_measure$CATEGORY)  
        
        # Assign same order to best performer highest policy contribution
        }else if(input$sortby == 'Best Performer'){
          
          best_measure<- best_measure %>% arrange((policy_percentage))
          
          best_measure$CATEGORY <- factor(best_measure$CATEGORY,
                                          levels = best_measure$CATEGORY)
          
          other_measure$CATEGORY <- factor(other_measure$CATEGORY,
                                           levels = best_measure$CATEGORY)  
          
          diff_measure$CATEGORY<- factor(diff_measure$CATEGORY,
                                         levels = best_measure$CATEGORY) 
          
        # Assign same order to other highest policy contribution
        }else if(input$sortby == 'Others'){
          
          other_measure<- other_measure %>% arrange((policy_percentage))
          
          best_measure$CATEGORY <- factor(best_measure$CATEGORY,
                                          levels = other_measure$CATEGORY)
          
          other_measure$CATEGORY <- factor(other_measure$CATEGORY,
                                           levels = other_measure$CATEGORY)  
          
          diff_measure$CATEGORY<- factor(diff_measure$CATEGORY,
                                         levels = other_measure$CATEGORY) 
          
          
        }
        
        # Sort the data by selected condition with selecting only top rows
        best_measure<- best_measure %>%
          arrange(CATEGORY) %>%
          tail(input$top2)
        
        # Sort the data by selected condition with selecting only top rows
        other_measure<- other_measure %>%
          arrange(CATEGORY)%>%
          tail(input$top2)
        
        # Sort the data by selected condition with selecting only top rows
        diff_measure <- diff_measure %>%
          arrange(CATEGORY) %>%
          tail(input$top2)
        
        # Create bar chart
        fig1 <- plot_ly(best_measure, 
                        y = ~CATEGORY,
                        x = ~policy_percentage, 
                        
                        type = 'bar', 
                        orientation='h',
                        marker = list(color = '#00bc8c'), 
                        name = 'Best Performer',
                        hovertemplate = paste(
                          "%{y}<br>",
                          "Policy Contribution: %{x:.2%f}<br>")
                        )
        
        
        
        # Create bar chart
        fig2 <- plot_ly(other_measure,
                        y = ~CATEGORY, 
                        x = ~policy_percentage, 
                        type = 'bar', 
                        orientation='h',
                        marker = list(color = '#f39c12'),
                        name = 'Others',
                        hovertemplate = paste(
                          "%{y}<br>",
                          "Policy Contribution: %{x:.2%f}<br>"))
        
        fig2 <- fig2 %>% layout(yaxis = list(showgrid = FALSE, 
                                             showline = TRUE,
                                             showticklabels = FALSE))
        
        # Create bar chart
        fig3 <- plot_ly(diff_measure,
                        y = ~CATEGORY, 
                        x = ~percentage_different, 
                        type = 'bar', 
                        orientation='h',
                        marker = list(color = '#375a7f'), 
                        name = 'Differences in Percentage',
                        hovertemplate = paste(
                          "%{y}<br>",
                          "Policy differences: %{x:.2%f}<br>"))
        
        fig3 <- fig3 %>% layout(yaxis = list(showgrid = FALSE, showline = TRUE, showticklabels = FALSE))
        
        
        # Create multiple bar charts
        fig <- subplot(fig1, fig3,fig2) 
        
        # Adjust the layout
        fig  %>%
          layout(paper_bgcolor = "#222222",
                 plot_bgcolor = "#303030",
                 font = list(family = "sans serif",color = 'white'),
                 legend = list(orientation = 'h')
          )
        
      }
      
      })
  

}



shinyApp(ui = ui, server = server)



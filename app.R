library(shiny)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(extrafont)
library(scales)
library(stringr)
library(lintr)
options(scipen = 999)

# groups the database by major category, calculating new columns containing
# information about the number of employed and unemployed to be used in the
# data
df <- read.csv("major.csv", stringsAsFactors = FALSE) %>%
  group_by(Major_category) %>%
  summarize(total = sum(Total),
            employed = sum(Employed),
            employedFull = sum(Employed_full_time_year_round),
            unemployed = sum(Unemployed),
            avgMedian = round(mean(as.double(Median)), 0)
            )
df <- mutate(df, unemploymentRate = round(as.double(unemployed) /
                                            as.double(total), 4) * 100) %>%
  mutate(category_abbr = word(Major_category, 1))
df[nrow(df) + 1, ] <- c("All Categories", round(mean(df$total), 0), sum(df$employed),
                        sum(df$employedFull), sum(df$unemployed),
                        round(mean(df$avgMedian), 0),
                        unemploymentRate = round(sum(df$unemployed) / sum(df$total), 4) * 100,
                        "All")
#df <- df[-c("Computers & Mathematics"), ]
#df[nrow(df) + 1, ] <- c("Computers and Mathematics", 1297470, 1089887, 928724, 61790, 51870, 4.76)

# creates the home page displayed on the application that includes
# a brief introduction to our project
home_page <- tabPanel(
  "Home",
  titlePanel(h1("Hierarchy In College Majors", align = "center")),
  fluidPage(
    h4("Introduction"),
    p("Many people believe that majors can be categorized in one of two ways:
      the ones that make money and the ones that don't. Because of this
      categorization, a hierarchy has been made of the major system, where
      the ones that lead to the highest paying jobs are on top and everything
      else is beneath them. The problem that we want to analyze is this
      currently existing hierarchy and whether or not your major determines
      your income / employment status upon graduating. Through data, we hope
      to investigate whether all the emphasis behind the importance of picking
      particular majors to ensure higher incomes is valid."),
    p("This issue affects many people, including college students, professors,
      universities, and companies that hire employees directly out of college.
      Often times these hirings use college major as an initial screening
      process, but interviews also influence these decisions. Because of this,
      more and more students are congregating to majors that 'make money' such
      as STEM majors, and away from majors in the arts. Oftentimes, this
      decreases the funding that particular departments receive, which can
      affect the quality of their program."),
    h4("Our Data Analysis Plan"),
    p("We analyzed the problem by visualizing how different categories of
      majors correlate to after-graduation employment opportunities, and how
      the income levels vary among these categories. To do this, we used a bar
      graph that compares the incomes of each major category that you choose
      with each other. We also used bar charts for each individual major
      category that compares employment and unemployment rates after graduation
      within that category."),
    p("Read more about our project below:"),
    tags$a(href = "https://github.com/WeiDuan816/project-college-major/wiki", 
           "Link")
    )
)

# creates a page where the data visualizations of this project will go, one
# being a bar graph that compares employment rates across majors and another
# being a series of pie charts that comapre the number of employed and
# unemployed in each major.
visualization_page <- tabPanel(
  "Visualizations",
  titlePanel(h1("Visualizations", align = "center")),
  sidebarLayout(
    sidebarPanel(
      selectInput("data_category", label = "Select Category to Compare Majors",
                  c("Employment Rates", "Income")),
         checkboxGroupInput("major_category", label = "Choose Majors to
                            Visualize",
                  c("Agriculture & Natural Resources", "Arts", "Business",
                    "Biology & Life Science", "Communications & Journalism",
                    "Computers and Mathematics", "Education", "Engineering",
                    "Health", "Humanities & Liberal Arts",
                    "Industrial Arts & Consumer Services",
                    "Interdisciplinary", "Law & Public Policy",
                    "Physical Sciences", "Psychology & Social Work",
                    "Social Science")
      )
    ),
    mainPanel(
      plotOutput("major_plot"),
      p("These specific data visualizations methods are effective to represent
      the data major surrounding majors because they are continous (income),
      and rates (employment). However, the rates can be converted into
      continuous variables by multiplying the rate by the total number of
      people in a major."),
      p("*The median for the 'All Categories' aggregate was found by using the average of 
        all the medians."),
      p("**The total people (fill color) in the 'All Categories' major is the average 
        number of people in each major (total people / number of majors) so that the 
        fill color better compares the specific majors to the overall average.")
    )
  )
)

# Includes information about the dataset that we chose
data_page <- tabPanel(
  "Data",
  titlePanel(h1("Data", align = "center")),
  fluidPage(
    h4("College Majors Dataset"),
    p("This dataset provides information about recently graduated college
    students,their majors, and their employment status. It has 79,668,7996
      observations and 11 attributes."),
    h4("Graduate Student Majors Dataset"),
    p("This dataset provides information about both recently graduated college
    students and those attending graduate school. It provides information
    about their majors and their employment status. It has 59,233,874
      observations and 22 attributes."),
    h4("Who created it?"),
    p("The data were collected and organized by United States' Census Bureau"),
    h4("Why was it created?"),
    p("A college degree is never a guarantee of economic success.
      But researchers believe that through careful choice of major,
      students can take at least some steps toward boosting their odds.
      Research has also shown that many schools do little push students to
      make informed choices about what to study."),
    h4("Where did you access it?"),
    p("The data can be downloaded from the website - census.gov,
      and we accessed through github."),
    h4("What represents an “observation”?"),
    p("Each observation represents a specific major, which contains
    information including number of people, employment,
      income earnings, etc."),
    h4("What “variables” does each observation have?"),
    p("Specific majors, major catgories, number of people employed and
    unemployed,and median, 25th percentile, 75th percentile income earnings.
    In addition, graduate students dataset also has comparable data values
    for people who do not have a graduate degree."),
    h4("How big is the data set?
       If it is a sub-set of a larger data set, how was the sub-set created?"),
    p("The college major dataset contains 79,668,7996 observations and 11
    attributes,and the graduate students dataset contains 59,233,874
    observations and 22 attributes."),
    h4("If the data set represents people,
       who has been included and who has been excluded?"),
    p("Undergraduate and graduate students were included.")
  )
)

# The page that contains the conclusion of our project based on our analysis
# of the data
conclusion_page <- tabPanel(
  "Conclusion",
  titlePanel(h1("Conclusion", align = "center")),
  fluidPage(
    h4("Is there an income disparity between different majors or major
       subjects?"),
    p("Yes, as shown in our second data visualization, income disparity exists
    between majors. Just as representation to how big this disparity is, 
    the major with the largest average income, Engineering, makes
    $34,759 more on average than the major with the smallest average income,
    Interdisciplinary Studies. Our visualization also shows that, generally,
    the stereotypes of certain majors (especially those in STEM or Business)
    making more money than Humanities and Arts majors does actually hold true.
    Humanities and Arts majors have amongst the lowest ranking average incomes,
    which are $43,000 and $43,525 respectively. This is a huge difference from
    majors such as Business or Physical Sciences which made $60,615 and
    $62,400 on average respectively. Based on the data, these majors with
    higher average incomes seem to also be where the most students are,
    with Business being the most popular major. This suggests that students
    have a tendency to choose majors that promise financial stability.")
,
    h4("Is there an employment disparity between different majors or major
       subjects?"),
    p("Yes, and it halfway goes along with the per-conceived notions of which majors 
      are reliable in the job market. The jobs with high unemployement rates (arts, 
      communications and journalism, interdisciplinary, social science, and psychology) 
      are typically thought of as lower hierarchy majors. These all have unemployment 
      rates at least 0.76% higher than the average. This is at least a 18% higher chance 
      of being unemployed. Interdisciplinary, as the highest unemployment, is 2.4% 
      above the average, which is a 57% increase. However, the jobs with low 
      unemployment rates (agriculture, education, health, physical sciences) aren't 
      typically thought of as higher hierarchy majors. Each are at least 0.49% below 
      the average, which correlates to 11.6% overall decrease. The lowest unemployment 
      rate is Health, which is 1.68% below the average, or 40% less. In fact, some 
      majors that are typically thought of as safe (business, biology and life sciences, 
      law, etc.) are all above the average. While total number of students in each major 
      can explain this to an extent, employment is often based on the market supply and 
      demand for people of certain skillsets, so it may be that some majors aren't 
      as safe for employment as they seem."),
    h4("Based on all the data, is there a heirarchy in college majors?"),
    p("Yes, based on the analysis we performed on this data we hace concluded there is
      a heirarchy that applies to college majors. While it is obvious that in terms of income 
      there is a heirarchy, we have also found that there is heirarchy in terms of employment
      rate and these programs with large income and employment then obtain the consideration of
      better to the people concidering them. Popularity correlates with money and employment showing
      the general trend of heirarchy in college majors. For example there is a high income and large
      amount of students in business while in humanities and liberal arts there is the opposite effect."),
    h4("Limitations"),
    p("There are other potential confounding variables. For example, it has
    been well documented that there is a wage gap between men and women, so
    a major that is predominately male would most likely have a greater
    average income than the same exact major if it were majority female.
    Additionally, race and national origin has a similar effect, as some
    races are more likely to get paid more many or hired then other races.
    The hope is that using a large dataset would help eliminate these effects,
    but if a major has a different race composition than another major, it
    could have an effect on the results."),
    h4("Areas for More Research or Analysis"),
    p("Evaluating and analyzing different majors while taking into account the
    various confounding factors. Additionally, looking at potential market
    fluctuations that influence the supply and demand of specific jobs
    or majors. These fluctuations could come from many different trends
    including technology, improved education, global warming, and other
    social, scientific, and political trends.")
  )
)

# creates a page that contains teh about me information for the group members

aboutme_page <-tabPanel(
  "About Us", 
  titlePanel(h1(align = "center")),
  fluidPage(
    h4("Matthew Leviten"),
    p("Sophmore, Business Major"),
    p("I want to see how our team can use R and our other resources to take 
      a dataset and create a complete analysis and report. It will be exciting 
      to see a project that we make from start to finish. My personal roles 
      involve ideation, communication/planning, and helping write code to 
      analyze the data. In terms of the actual coding, I helped a lot with the 
      data visualizations, espcially concerning how to make the graphs and how to 
      clean and alter the data so it could be easily represented. I also 
      worked on the input widgets and the styling of the visualizations page. 
      Finally, I helped our team come to conclusions that we could all agree 
      was represented by the data."),
    h4("Wei's Goals"),
    p("Senior, Economics Major"),
    p("My personal goal is to learn how we can use R as well as other tools
      to produce an analysis, given the necessary data; how we can make the
      analysis visual and easy-understanding. My role is to help the team 
      to achieve our goal by writing codes, giving feedback and making
      improvements. For this project, I helped the team with building the data
      page and adding information about our dataset. I also helped to debug and
      fix some of the ploting problems. I mainly contributed to organizing and
      updating most of our works and progress to the GitHub wiki page."),
    h4("Katie's Goals"),
    p("Sophmore, Informatics"),
    p("My personal goal was is to learn how to take data that is hard to understand and
      make it easy to understand. I wanted to learn this because it is so applicable to 
      anything that requires taking raw data and summarizing it. My role was making the about
      me page and presenting all of our various personal descritions. I helped add this page to
       the user interface. Throughout the project I helped with ideas and code development. I 
      also helped give feedback and develop code."),
    h4("Julianne's Goals"),
    p("Sophomore, Informatics Major"),
    p("My personal goal was to be able to analyze data and create data 
      visualizations that are both clear and concise. My role was to contribute 
      to the group assignment through writing code and also helping with the 
      appearance of our final project. In terms of what I did with the actual
      code, I worked on the home page for the site, making sure that
      people that visit the site get a clear understanding of what we're
      trying to answer with the dataset we chose. I also participated in
      group discussions on how we should plan out certain aspects of
      our site and collaborated with other team members in making
      decisions about the Visualizations page.")
  )                    
)

# creates the navigation bar at the top of the page with all the pages that
# were created above
ui <- navbarPage(
  "Hierarchy in College Majors",
  home_page,
  visualization_page,
  data_page,
  conclusion_page,
  aboutme_page
)

# displays the plot shown in our visualizations page that takes in input
# from the user on which majors they'd like to analyze and displays it on
# the bar graph
server <- function(input, output) {
  output$major_plot <- renderPlot({
    categories <- input$major_category
    categories[length(categories) + 1] <- "All Categories"
    plotdf <- df[match(categories, df$Major_category), ]
    if(input$data_category == "Employment Rates"){
    ggplot() + geom_bar(aes(y = unemploymentRate, x = category_abbr,
                        fill = as.double(total)),
                        data = plotdf, stat = "identity") +
      geom_text(data = plotdf, aes(x = category_abbr, y = unemploymentRate,
                label = paste0(unemploymentRate, "%")), size = 5,
                color = "white",
                vjust = 1.5) +
      labs(x = "Major Category", y = "Unemployment Rate",
           fill = "Total Students in Major") +
      ggtitle("Comparison of Majors by Unemployment Rate") +
        theme(legend.position="bottom") +
        theme(plot.title = element_text(hjust = 0.5, size = 24)) +
        theme(legend.key.size = unit(1, "cm"))
    }else{
      ggplot() + geom_bar(aes(y = avgMedian, x = category_abbr,
                              fill = as.double(total)),
                          data = plotdf, stat = "identity") +
        geom_text(data = plotdf, aes(x = category_abbr, y = avgMedian,
                                     label = paste0("$", avgMedian)), size = 5,
                  color = "white",
                  vjust = 1.5) +
        labs(x = "Major Category", y = "Average Median Income",
             fill = "Total Students in Major") +
        ggtitle("Comparison of Majors by Income") + theme(legend.position="bottom") +
        theme(plot.title = element_text(hjust = 0.5, size = 24)) +
        theme(legend.key.size = unit(1, "cm"))
    }
  })
}

shinyApp(ui, server)

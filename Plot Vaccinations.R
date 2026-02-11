## ----------------------------------------------------------------------------
## Produced by Yale's Public Health Data Science and Data Equity (DSDE) team
##
##     Workshop: Getting Started with Git and GitHub Part 1
##      Authors: Shelby Golden, M.S.
## Last Updated: 2026-02-11
## 
##       R version: 4.5.2
## RStudio version: 2026.01.0+392
##    renv version: 1.1.7


## ----------------------------------------------------------------------------
## SET UP THE ENVIRONMENT
## renv() will install all of the packages and their correct version used here
renv::init()          # Initialize the project
renv::restore()       # Download packages and their version saved in the lockfile.

suppressPackageStartupMessages({
  library("readr")         # For reading in the data
  library("dplyr")         # For data manipulation
  library("stringr")       # For string manipulation
  library("ggplot2")       # For creating static visualizations
  library("lubridate")     # Facilitates working with dates and times
  library("scales")        # Override default ggplot2 axes and legend settings
})

# Function to select "Not In"
'%!in%' <- function(x,y)!('%in%'(x,y))




## ----------------------------------------------------------------------------
## LOAD IN THE DATA

## This data is from the COVID-19 Data Repository by the Center for Systems 
## Science and Engineering (CSSE) at Johns Hopkins University (JHU), GitHub 
## GovEX. Additional details can be found in the project repositories main 
## directory's README file.

covid19_vaccinations_url <- "https://raw.githubusercontent.com/ysph-dsde/Book-of-Workshops/refs/heads/main/Workshops/Git-and-GitHub/Data/Vaccinations%20Aggregated%20by%20Week.csv"
covid19_vaccinations     <- read_csv(file = covid19_vaccinations_url, show_col_types = FALSE) |>
  as.data.frame() |> _[, -1]


covid19_vaccinations_cumulative_url <- "https://raw.githubusercontent.com/ysph-dsde/Book-of-Workshops/refs/heads/main/Workshops/Git-and-GitHub/Data/Vaccinations%20Aggregated%20by%20Week_Annual%20Cumulative.csv"
covid19_vaccinations_cumulative     <- read_csv(file = covid19_vaccinations_cumulative_url, show_col_types = FALSE) |>
  as.data.frame() |> _[, -1]

# Inspect the data.
glimpse(covid19_vaccinations)
glimpse(covid19_vaccinations_cumulative)








## ----------------------------------------------------------------------------
## GENERATE AND SAVE OUR PLOT

## --------------------
## DATASET COMPONENTS

# Vectors with the unique entries for "Province_State", excluding the "United
# States" total counts.
unique_states      = sort(c(datasets::state.name, "District of Columbia"))
unique_territories = c("American Samoa", "Guam", "Northern Mariana Islands", 
                       "Puerto Rico", "Virgin Islands, U.S.")

unique_regions = covid19_vaccinations$Province_State |>
  (\(x) { x[x %!in% c("United States", unique_states, unique_territories)] }) () |>
  unique() |> _[1:4]

unique_divisions = covid19_vaccinations$Province_State |>
  (\(x) { x[x %!in% c("United States", unique_states, unique_territories)] }) () |>
  unique() |> _[-c(1:4)]


# The earliest and latest date represented. All weeks between these dates
# are included, and each region has the same span of dates.
dates = covid19_vaccinations$Week |> unique()
c(min(dates), max(dates))

dates_cumulative = covid19_vaccinations_cumulative$Week |> unique()


# Vectors with the different column-value names by the types of counts they
# represent. Recall the following y-axis variable options:
#                "_yf": cumulative counts smoothed to be monotonically increasing.
#          "_yf_Daily": back-calculated daily counts.
#  "_yf_Daily_Percent": daily counts normalized by census data for percentage
#                       of the population that is vaccinated.

cumulative_counts  = colnames(covid19_vaccinations)[str_detect(colnames(covid19_vaccinations), "yf\\b")]
daily_counts       = colnames(covid19_vaccinations)[str_detect(colnames(covid19_vaccinations), "Daily\\b")]
percentages_counts = colnames(covid19_vaccinations)[str_detect(colnames(covid19_vaccinations), "Percent")]




## --------------------
## PLOTS

# Plot 1: Line Plot
line_plot <- covid19_vaccinations %>%
  # Filter the data to plot only a selection of "Province_State" entries.
  filter(Province_State %in% unique_regions) %>%
  # Generate the plot with time as the x-axis and vaccinations as the y-axis.
  ggplot(data = ., aes(x = Week, y = Doses_yf_Daily)) +
      # Generate a line plot and separate data by unique "Province_State".
      geom_line(aes(color = Province_State)) +
      # Format the y-axis to show values as a percent.
      scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
      # Format the x-axis to show dates as Jan 2020 from 01/01/2020, spaced
      # every four months.
      scale_x_date(date_breaks = "4 month", date_labels =  "%b %Y") +
      # To adjust the legend title both the color and fill need to by
      # changed to the same string.
      scale_fill_discrete(name  = "U.S. Regions") +
      scale_color_discrete(name = "U.S. Regions") +
      # Title, x-axis, and y-axis labels. NOTE: "\n" forms a new line.
      labs(title = "Doses Administered by U.S. Regions",
           x = "Month", y = "Number of Doses Administered") +
      # Graph displays as minimal with a legend.
      theme_minimal() #+ theme(legend.position = "none")


line_plot




# Plot 2: Cumulative bar plot

# Get the reference horizontal line for overall U.S. vaccinations.
overall <- covid19_vaccinations_cumulative[covid19_vaccinations_cumulative$Province_State == "United States" & 
                                      covid19_vaccinations_cumulative$Week == dates_cumulative[4], 
                                    "People_At_Least_One_Dose_yf_Percent"]


bar_plot <- covid19_vaccinations_cumulative %>%
  # Filter the data to plot only the states and one date.
  filter(Province_State %in% unique_states & Week %in% dates_cumulative[4]) %>%
  # Readjust the percentage so they plot better.
  ggplot(data = ., aes(x = reorder(Province_State, People_At_Least_One_Dose_yf_Percent), 
                       y = People_At_Least_One_Dose_yf_Percent/100)) +
      # stat = "identity" tells the algorithm to not aggregate values, but
      # plot them as provided. alpha = 0.25 fills the bars with 25% opacity.
      geom_bar(stat = "identity", position = "dodge", alpha = 0.3, fill = "#A353FF") +
      scale_y_continuous(labels = scales::percent) +
      # Include a line and annotation that shows the overall US rate.
      geom_hline(yintercept = overall/100, linetype = "dashed", color = "#00356B") +
      annotate("text", x = 3, y = overall/100, label = "U.S. Rate", 
               vjust = -0.5, colour = "#00356B") +
      labs(title = "Percentage of Population With At Least One Dose by March 2023",
           x = "", y = "Percentage of the Population") +
      theme_minimal() + 
      # Adjust the x-axis tics so they are easier to read.
      theme(axis.text.x = element_text(angle = 65,  hjust = 1))


bar_plot




## ----------------------------------------------------------------------------
## SAVE THE FILES

ggsave("plot_line.jpeg", line_plot, width = 20, height = 12, units = "cm")
ggsave("plot_bar.jpeg", bar_plot, width = 20, height = 12, units = "cm")






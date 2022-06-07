library(shiny)
library(shinyjs)
library(bslib)
library(tidyverse)
library(patchwork)
library(DT)

options(shiny.maxRequestSize = 1024 * 1024 * 1024)
options(scipen = 999)

# Datasets
# ========

load("datasets.RData")

datasets <- list(
  "1.1 Effect of disease X on height" = disease_x,
  "1.2 Blood vessel formation" = blood_vessel_formation_1,
  "2.1 Biological process duraction" = biological_process_durations,
  "2.2 Blood vessel formation" = blood_vessel_formation_2,
  "2.3 NIBP gene expression in breast cancer patients" = nibp_expression,
  "2.4 Vitamin D levels and fibrosis" = fibrosis_vitamin_d,
  "2.5 Birth weight of twins and Sudden Infant Death Syndrome" = sids_twin_birth_weight,
  "3.1 Growth of Zea seedlings from self- and cross-fertilization" = plant_growth_fertilization,
  "3.2 Florence Nightingale's hygiene regime" = hygiene_regime,
  "3.3 Effect of bran on diet of patients with diverticulosis" = bran_diverticulosis,
  "3.4 Effect on repetitive behaviour of autism drug" = autism_drug,
  "3.5 Effect of HIV drug on CD4 cell counts" = hiv_drug_cd4_counts,
  "3.6 Drink driving and reaction times" = drink_driving_reaction_times,
  "3.7 Pollution in poplar trees" = tree_pollution,
  "3.8 Sex effect on salaries of professors" = professorial_salaries,
  "Star Wars" = starwars,
  "Effectiveness of insect sprays" = select(InsectSprays, spray, count),
  "Plant growth for two different treatment conditions and a control" = select(PlantGrowth, group, weight),
  "Sibling chick weights reared in confinement or on open range" = chick_weights
)

# useful functions
# ================

# summary statistics
summary_statistics <- function(values) {
  as_tibble(list(
    `Number of observations` = length(values),
    # `Missing values` = sum(is.na(values)),
    `Valid/non-missing observations` = sum(is.finite(values)),
    Mean = mean(values, na.rm = TRUE),
    `Standard deviation` = sd(values, na.rm = TRUE),
    `95% confidence interval - lower` = mean(values, na.rm = TRUE)
      - 1.96 * sd(values, na.rm = TRUE) / sqrt(length(values)),
    `95% confidence interval - upper` = mean(values, na.rm = TRUE)
      + 1.96 * sd(values, na.rm = TRUE) / sqrt(length(values)),
    Minimum = min(values, na.rm = TRUE),
    `25th percentile (1st quartile)` = quantile(values, 0.25, na.rm = TRUE),
    Median = median(values, na.rm = TRUE),
    `75th percentile (3rd quartile)` = quantile(values, 0.75, na.rm = TRUE),
    Maximum = max(values, na.rm = TRUE),
    `Interquartile range (IQR)` = IQR(values, na.rm = TRUE)
  ))
}

# transform the given set of values
transform <- function(values, type) {
  switch(
    type,
    none = values,
    natural_log = log(values),
    square_root = sqrt(values),
    cube_root = sign(values) * abs(values) ^ (1 / 3)
  )
}

# box plot
create_boxplot <- function(values,
                           groups = "",
                           name = NULL,
                           xintercept = NULL,
                           limits = NULL,
                           show_points = TRUE,
                           show_density = FALSE) {
  if (is.null(values)) {
    return(ggplot() + theme_minimal())
  }

  data <- tibble(value = values, group = groups) %>%
    filter(is.finite(value))

  if (nrow(data) == 0) {
    return(ggplot() + theme_minimal())
  }

  boxplot <- ggplot(data, aes(x = value, y = group, fill = group))

  outlier_shape <- NULL
  if (show_points) {
    outlier_shape <- NA
  }

  if (show_density) {
    boxplot <- boxplot +
      geom_violin() +
      geom_boxplot(fill = "white", width = 0.2, outlier.shape = outlier_shape)
  } else {
    boxplot <- boxplot +
      geom_boxplot(outlier.shape = outlier_shape)
  }

  if (show_points) {
    # set the seed so that the points do not move when other plotting options
    # are changed
    set.seed(12345)
    boxplot <- boxplot +
      geom_jitter(
        colour = "black",
        width = 0,
        height = ifelse(show_density, 0.1, 0.15)
      )
  }

  limits <- range(c(data$value), limits)

  if (!is.null(xintercept) && is.finite(xintercept)) {
    boxplot <- boxplot +
      geom_vline(
        xintercept = xintercept,
        colour = "#FFB000",
        size = 1.25,
        linetype = "solid"
      )
    limits <- range(c(limits, xintercept))
  }

  boxplot <- boxplot +
    scale_x_continuous(limits = limits) +
    scale_y_discrete(drop = FALSE) +
    scale_fill_manual(values = c("#648FFF", "#DC267F"), drop = FALSE) +
    labs(x = name) +
    theme_minimal() +
    theme(
      axis.title.x = element_text(size = 14),
      axis.text.x = element_text(size = 12),
      axis.title.y = element_blank(),
      axis.text.y = element_text(size = 14),
      axis.ticks.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      legend.position = "none"
    )

  boxplot
}

# histogram
create_histogram <- function(values,
                             groups = "",
                             name = NULL,
                             xintercept = NULL,
                             number_of_bins = NULL,
                             show_normal_distribution = FALSE) {
  if (is.null(values)) {
    return(ggplot() + theme_minimal())
  }

  data <- tibble(value = values, group = groups) %>%
    filter(is.finite(value))

  if (nrow(data) == 0) {
    return(ggplot() + theme_minimal())
  }

  histogram <- ggplot(data, aes(x = value, fill = group, group = group))

  if (is.null(number_of_bins)) {
    number_of_bins <- nclass.Sturges(data$value)
  }

  breaks <- pretty(
    range(data$value, na.rm = TRUE),
    number_of_bins,
    min.n = 1
  )

  limits <- range(c(data$value, breaks))

  histogram <- histogram +
    geom_histogram(breaks = breaks, colour = "black")

  if (show_normal_distribution) {
    normal_density <- data %>%
      group_by(group) %>%
      summarize(mean = mean(value), sd = sd(value), n = n()) %>%
      expand_grid(quantile = seq(limits[1], limits[2], length.out = 101)) %>%
      mutate(density = dnorm(quantile, mean, sd)) %>%
      mutate(count = density * n * mean(diff(breaks)))

    histogram <- histogram +
      geom_line(data = normal_density, aes(x = quantile, y = count))
  }

  if (!is.null(xintercept) && is.finite(xintercept)) {
    histogram <- histogram +
      geom_vline(
        xintercept = xintercept,
        colour = "#FFB000",
        size = 1.25,
        linetype = "solid"
      )
  }

  histogram <- histogram +
    scale_x_continuous(limits = limits) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    scale_fill_manual(values = c("#648FFF", "#DC267F"), drop = FALSE) +
    labs(x = name, y = NULL) +
    facet_wrap(vars(group), drop = FALSE) +
    theme_minimal() +
    theme(
      strip.text = element_text(size = 14),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      axis.ticks.x = element_line(),
      axis.ticks.length = unit(2, "mm"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      legend.position = "none"
    )

  histogram
}

# Q-Q plot
create_qq_plot <- function(values, groups = "") {
  if (is.null(values)) {
    return(ggplot() + theme_minimal())
  }

  data <- tibble(value = values, group = groups) %>%
    filter(is.finite(value))

  if (nrow(data) == 0) {
    return(ggplot() + theme_minimal())
  }

  ggplot(data, aes(sample = value, colour = group)) +
    stat_qq() +
    stat_qq_line() +
    labs(x = "theoretical quantiles", y = "sample quantiles") +
    scale_colour_manual(values = c("#648FFF", "#DC267F"), drop = FALSE) +
    facet_wrap(vars(group), drop = FALSE) +
    theme_minimal() +
    theme(
      strip.text = element_text(size = 14),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.position = "none"
    )
}

# t-distribution plot for the given t-test result
create_t_distribution_plot <- function(t_test_result) {

  t <- t_test_result$statistic
  df <- t_test_result$parameter["df"]

  limit <- max(abs(t), qt(0.975, df)) * 1.1
  limit <- max(limit, 3)
  limit <- ceiling(limit)

  plot <- ggplot(tibble(t = c(-limit, limit)), aes(t))

  plot <- switch(t_test_result$alternative,
    "two.sided" = plot +
      stat_function(
        fun = dt, args = list(df = df),
        xlim = c(-limit, qt(0.025, df)),
        geom = "area", fill = "#FFB000"
      ) +
      stat_function(
        fun = dt, args = list(df = df),
        xlim = c(qt(0.975, df), limit),
        geom = "area", fill = "#FFB000"
      ),
    "less" = plot +
      stat_function(
        fun = dt, args = list(df = df),
        xlim = c(-limit, qt(0.05, df)),
        geom = "area", fill = "#FFB000"
      ),
    "greater" = plot +
      stat_function(
        fun = dt, args = list(df = df),
        xlim = c(qt(0.95, df), limit),
        geom = "area", fill = "#FFB000"
      )
  )

  plot <- plot + stat_function(
    fun = dt, args = list(df = df),
    colour = "grey50", size = 1,
  )

  plot <- plot +
    geom_vline(
      xintercept = t_test_result$statistic,
      colour = "#648FFF", size = 1
    )

  plot <- plot +
    labs(
      x = "t",
      title = str_c(
        "Student's t-distribution with ",
        round(df, 2),
        " degrees of freedom"
      )
    ) +
    scale_x_continuous(limits = c(-limit, limit), expand = expansion(0, 0)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    theme_minimal() +
    theme(
      title = element_text(size = 11),
      axis.title.x = element_text(size = 16, face = "italic"),
      axis.text.x = element_text(size = 12),
      axis.line.x = element_line(),
      axis.ticks.x = element_line(),
      axis.ticks.length = unit(2, "mm"),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )

  plot
}

# check validity of a single value
check_value <- function(value, name = "Value", context = NULL) {
  if (!is.null(context)) {
    context <- str_c(" ", context)
  }

  if (is.null(value)) {
    stop(name, " is unset", context)
  }

  if (is.nan(value)) {
    stop(name, " is invalid", context)
  }

  if (is.infinite(value)) {
    stop(name, " is not finite", context)
  }

  if (is.na(value)) {
    stop(name, " is unset", context)
  }
}

# check validity of the given values
check_values <- function(values, minimum_number = 3, context = NULL) {
  if (!is.null(context)) {
    context <- str_c(" ", context)
  }

  if (is_empty(values)) {
    stop("No data", context)
  }

  if (any(is.nan(values))) {
    stop("Data contain invalid values", context)
  }

  if (any(is.infinite(values))) {
    stop("Data contain non-finite values.", context)
  }

  if (length(values) < minimum_number) {
    stop("Too few observations", context)
  }

  # filter out missing values
  values <- values[!is.na(values)]

  if (is_empty(values)) {
    stop("No observations after removing missing values", context)
  }

  if (length(values) < minimum_number) {
    stop("Too few observations after removing missing values", context)
  }
}

# one sample t-test
one_sample_t_test <- function(values,
                              hypothesized_mean,
                              alternative = "two.sided",
                              variable = NULL) {
  check_values(values)
  check_value(hypothesized_mean, "Hypothesized mean")

  result <- t.test(
    values,
    mu = hypothesized_mean,
    alternative = alternative
  )

  # override the data name
  if (!is.null(variable)) {
    result$data.name <- variable
  }

  result
}

# one sample Wilcoxon signed rank test
one_sample_wilcoxon_test <- function(values,
                                     hypothesized_median,
                                     alternative = "two.sided",
                                     variable = NULL) {
  check_values(values)
  check_value(hypothesized_median, "Hypothesized median")

  result <- wilcox.test(
    values,
    mu = hypothesized_median,
    alternative = alternative
  )

  # override the data name
  if (!is.null(variable)) {
    result$data.name <- variable
  }

  result
}

# two sample t-test
two_sample_t_test <- function(values1,
                              values2,
                              alternative = "two.sided",
                              equal_variance = FALSE,
                              paired = FALSE,
                              variable1 = NULL,
                              variable2 = NULL) {
  check_values(values1, "(group 1)")
  check_values(values1, "(group 2)")

  result <- t.test(
    values1, values2,
    alternative = alternative,
    var.equal = equal_variance,
    paired = paired
  )

  # override the data name
  if (!is.null(variable1) && !is.null(variable2)) {
    # override the data name
    result$data.name <- str_c(variable1, " and ", variable2)
  }

  result
}

# two sample Wilcoxon rank sum test
two_sample_wilcoxon_test <- function(values1,
                                     values2,
                                     alternative = "two.sided",
                                     paired = FALSE,
                                     variable1 = NULL,
                                     variable2 = NULL) {
  check_values(values1, "(group 1)")
  check_values(values1, "(group 2)")

  result <- wilcox.test(
    values1, values2,
    alternative = alternative,
    paired = paired
  )

  # override the data name
  if (!is.null(variable1) && !is.null(variable2)) {
    # override the data name
    result$data.name <- str_c(variable1, " and ", variable2)
  }

  result
}

# Shiny user interface
# ====================

ui <- fluidPage(
  useShinyjs(),
  theme = bs_theme(
    bootswatch = "cerulean",
    primary = "#b3006b",
    font_scale = 0.9
  ),
  tags$style(HTML("
    h5 {
      color: #484848;
    }
    .checkbox {
      margin-top: 10px;
      margin-bottom: -15px;
    }
  ")),
  div(style = "margin-top: 90px;"),
  navbarPage(
    title = div(
      a(
        href = "https://www.cruk.cam.ac.uk",
        img(
          style = "width: 200px; margin-right: 10px;",
          src = "cruk_ci_transparent_logo.png"
        )
      ),
      strong(em("Statistical tests"))
    ),
    windowTitle = "Statistical tests",
    position = "fixed-top",
    tabPanel(
      title = "Data input",
      wellPanel(
        fluidRow(
          column(
            width = 5,
            fileInput(
              "data_file",
              "Upload CSV or TSV file",
              accept = c(".csv", ".tsv", ".txt")
            )
          ),
          column(
            width = 6,
            offset = 1,
            selectizeInput(
              "sample_data",
              label = "Sample data sets",
              choices = names(datasets)
            )
          )
        )
      ),
      br(),
      DT::dataTableOutput("data_table", width = "65%")
    ),
    tabPanel(
      title = "One sample test",
      wellPanel(
        fluidRow(
          column(
            width = 3,
            selectInput(
              "one_sample_variable",
              label = "Variable",
              choices = character()
            )
          ),
          column(
            width = 3,
            # would prefer to use a numericInput but the value defaults to zero
            # rather than NA when updating, e.g. when changing the variable or
            # dataset
            textInput(
              "one_sample_hypothesized_mean",
              label = "Hypothesized mean",
              value = ""
            )
          ),
          column(
            width = 6,
            radioButtons(
              "one_sample_transformation",
              label = "Transformation",
              choices = c(
                "None" = "none",
                "Natural log" = "natural_log",
                "Square root" = "square_root",
                "Cube root" = "cube_root"
              ),
              selected = "none",
              inline = TRUE
            )
          )
        ),
        em(textOutput("one_sample_info"))
      ),
      br(),
      tabsetPanel(
        id = "one_sample_tabs",
        selected = "Plots",
        tabPanel(
          "Summary statistics",
          br(),
          DT::dataTableOutput("one_sample_summary_statistics", width = "65%")
        ),
        tabPanel(
          "Plots",
          br(),
          sidebarLayout(
            sidebarPanel(
              checkboxInput(
                "one_sample_show_hypothesized_mean",
                label = "Show hypothesized mean",
                value = TRUE
              ),
              h5("Box plot", style = "margin-top: 30px;"),
              checkboxInput(
                "one_sample_show_points",
                label = "Show points on box plot",
                value = FALSE
              ),
              helpText("
Outlier points for those observations that are further than 1.5 IQR from the
edges of the box, i.e. the first and third quantiles, are always displayed.
              "),
              checkboxInput(
                "one_sample_violin",
                label = "Overlay density on box plot",
                value = FALSE
              ),
              h5("Histogram", style = "margin-top: 30px;"),
              checkboxInput(
                "one_sample_show_normal_distribution",
                label = "Overlay normal distribution",
                value = FALSE
              ),
              helpText("
The normal distribution shown is based on the computed mean and standard
deviation of the data.
              "),
              checkboxInput(
                "one_sample_choose_number_of_bins",
                label = "Choose number of bins",
                value = FALSE
              ),
              conditionalPanel(
                condition = "!input.one_sample_choose_number_of_bins",
              ),
              conditionalPanel(
                condition = "input.one_sample_choose_number_of_bins",
                sliderInput(
                  "one_sample_number_of_bins",
                  label = "Number of bins",
                  min = 5,
                  max = 50,
                  value = 20,
                  ticks = FALSE
                ),
                helpText("
The actual number of bins may differ for aesthetic reasons.
                ")
              )
            ),
            mainPanel(
              plotOutput("one_sample_plots", width = "80%", height = "500px"),
            )
          )
        ),
        tabPanel(
          "Assumptions",
          br(),
          sidebarLayout(
            sidebarPanel(
              helpText("
This page provides graphical and statistical tools that can help with assessing
the assumptions of a parametric test, e.g. t-test.
              "),
              p(),
              "The following assumptions are made in a parametric, one sample",
              "t-test:",
              p(),
              tags$ul(
                tags$li(
                  "the data are independent - values are not related to one",
                  "another"
                ),
                tags$li(
                  "the data are on a continuous scale"
                ),
                tags$li(
                  "the data are a random sample from a population that is",
                  "normally distributed"
                )
              ),
              helpText("
Preliminary statistical tests of assumptions such as normality or equal variance
between groups are controversial and often criticised within the statistics
community.
              "),
              helpText("
Applying a transformation, e.g. log, square root or cube root, can help to
make skewed data conform more closely to normality.
              ")
            ),
            mainPanel(
              tabsetPanel(
                tabPanel(
                  "Q-Q plot",
                  helpText("
The Q-Q (quantile-quantile) plot compares the data with a normal distribution by
plotting their quantiles against each other.
                  "),
                  plotOutput(
                    "one_sample_qq_plot",
                    height = "400px",
                    width = "66%"
                  ),
                  helpText("
The theoretical quantiles are for a standard normal distribution with mean 0 and
standard deviation 1.
                  "),
                  helpText("
The points will lie approximately along the diagonal line (fitted through the
points for the first and third quartiles) if the data are normally distributed.
                  "),
                  helpText("
Significant deviations from the line may suggest the use of a non-parametric
test.
                  ")
                ),
                tabPanel(
                  "Shapiro-Wilk test",
                  h5("Shapiro-Wilk test of normality"),
                  helpText("
The Shapiro-Wilk test tests the null hypothesis that the data come from a
normally distributed population.
                  "),
                  div(style = "margin-top: 10px;"),
                  verbatimTextOutput("one_sample_shapiro_wilk_test"),
                  helpText("
The null hypothesis can be rejected if the p-value is less than 0.05, suggesting
that the data come from a population that are not normally distributed.
                  "),
                  helpText("
If the null hypothesis can't be rejected, this means there is insufficient
evidence that the data are not normal. This is not the same as accepting that
the data come from a normal distribution, i.e. it does not prove that the null
hypothesis is true.
                  "),
                  helpText("
Caution is advised when using a preliminary test for normality to decide whether
a parametric or non-parametric test should subsequently be used, particularly
when the sample size is small. It is often better to make your own assessment by
looking at box plots, density plots, histograms and Q-Q plots.
                  ")
                )
              )
            )
          )
        ),
        tabPanel(
          "Statistical tests",
          br(),
          sidebarLayout(
            sidebarPanel(
              radioButtons(
                "one_sample_test_type",
                label = "Test",
                choices = c("Parametric", "Non-parametric")
              ),
              helpText("
A parametric, one sample t-test assumes that the data values are independent,
continuous and a random sample from a population that is normally distributed.
              "),
              br(),
              radioButtons(
                "one_sample_alternative",
                "Alternative hypothesis",
                choices = c(
                  "Two-sided" = "two.sided",
                  "Greater" = "greater",
                  "Less" = "less"
                )
              )
            ),
            mainPanel(
              conditionalPanel(
                condition = "input.one_sample_test_type == 'Parametric'",
                h5("One sample t-test"),
                verbatimTextOutput("one_sample_t_test"),
                plotOutput("one_sample_t_plot", height = "300px", width = "80%")
              ),
              conditionalPanel(
                condition = "input.one_sample_test_type == 'Non-parametric'",
                h5("Wilcoxon signed rank test"),
                helpText("
Tests whether the data come from a symmetric population centred around a
specified median value.
                "),
                div(style = "margin-top: 10px;"),
                verbatimTextOutput("one_sample_wilcoxon_test")
              )
            )
          )
        )
      )
    ),
    tabPanel(
      title = "Two sample test",
      wellPanel(
        div(
          style = "margin-top: -10px; margin-bottom: 20px;",
          checkboxInput(
            "two_sample_paired",
            label = "Paired observations"
          )
        ),
        div(style = "margin-top: 20px;"),
        conditionalPanel(
          condition = "input.two_sample_paired",
          fluidRow(
            column(
              width = 2,
              selectInput(
                "two_sample_variable1",
                label = "Variable 1",
                choices = character()
              )
            ),
            column(
              width = 2,
              selectInput(
                "two_sample_variable2",
                label = "Variable 2",
                choices = character()
              )
            ),
            column(
              width = 8,
              radioButtons(
                "two_sample_paired_transformation",
                label = "Transformation",
                choices = c(
                  "None" = "none",
                  "Natural log" = "natural_log",
                  "Square root" = "square_root",
                  "Cube root" = "cube_root"
                ),
                selected = "none",
                inline = TRUE
              )
            )
          )
        ),
        conditionalPanel(
          condition = "!input.two_sample_paired",
          fluidRow(
            column(
              width = 2,
              selectInput(
                "two_sample_categorical_variable",
                label = "Categorical variable",
                choices = character()
              )
            ),
            column(
              width = 2,
              selectInput(
                "two_sample_group1",
                label = "Group 1",
                choices = character()
              )
            ),
            column(
              width = 2,
              selectInput(
                "two_sample_group2",
                label = "Group 2",
                choices = character()
              )
            ),
            column(
              width = 2,
              selectInput(
                "two_sample_variable",
                label = "Variable",
                choices = character()
              )
            ),
            column(
              width = 4,
              radioButtons(
                "two_sample_transformation",
                label = "Transformation",
                choices = c(
                  "None" = "none",
                  "Natural log" = "natural_log",
                  "Square root" = "square_root",
                  "Cube root" = "cube_root"
                ),
                selected = "none",
                inline = TRUE
              )
            )
          )
        ),
        em(textOutput("two_sample_info"))
      ),
      br(),
      tabsetPanel(
        id = "two_sample_tabs",
        selected = "Plots",
        tabPanel(
          "Summary statistics",
          br(),
          DT::dataTableOutput("two_sample_summary_statistics", width = "80%")
        ),
        tabPanel(
          "Plots",
          br(),
          sidebarLayout(
            sidebarPanel(
              h5("Box plot"),
              checkboxInput(
                "two_sample_show_points",
                label = "Show points on box plots",
                value = FALSE
              ),
              helpText("
Outlier points for those observations that are further than 1.5 IQR from the
edges of the box, i.e. the first and third quantiles, are always displayed.
              "),
              checkboxInput(
                "two_sample_violin",
                label = "Overlay density on box plots",
                value = FALSE
              ),
              h5("Histogram", style = "margin-top: 30px;"),
              checkboxInput(
                "two_sample_show_normal_distribution",
                label = "Overlay normal distribution",
                value = FALSE
              ),
              helpText("
The normal distribution shown is based on the computed mean and standard
deviation of each group.
              "),
              checkboxInput(
                "two_sample_choose_number_of_bins",
                label = "Choose number of bins",
                value = FALSE
              ),
              conditionalPanel(
                condition = "!input.two_sample_choose_number_of_bins",
              ),
              conditionalPanel(
                condition = "input.two_sample_choose_number_of_bins",
                sliderInput(
                  "two_sample_number_of_bins",
                  label = "Number of bins",
                  min = 5,
                  max = 50,
                  value = 20,
                  ticks = FALSE
                ),
                helpText("
The actual number of bins may differ for aesthetic reasons.
                ")
              )
            ),
            mainPanel(
              plotOutput("two_sample_plots", width = "95%", height = "500px"),
            )
          )
        ),
        tabPanel(
          "Assumptions",
          br(),
          sidebarLayout(
            sidebarPanel(
              helpText("
This page provides graphical and statistical tools that can help with assessing
the assumptions of a parametric test, e.g. t-test.
              "),
              p(),
              conditionalPanel(
                condition = "!input.two_sample_paired",
                "The following assumptions are made in a parametric, two",
                "sample t-test:",
                p(),
                tags$ul(
                  tags$li(
                    "the data are independent - observations in one sample (or",
                    "group) are independent of observations in the other sample"
                  ),
                  tags$li(
                    "the data are on a continuous scale"
                  ),
                  tags$li(
                    "the data are a random sample from a population for each",
                    "group that is normally distributed"
                  )
                )
              ),
              conditionalPanel(
                condition = "input.two_sample_paired",
                "The following assumptions are made in a parametric, paired",
                "t-test of the differences between paired measurements:",
                p(),
                tags$ul(
                  tags$li(
                    "the data are independent - measurements for one",
                    "observation do not affect measurements for any other",
                    "subject"
                  ),
                  tags$li(
                    "each of the paired measurements must be obtained from",
                    "the same subject"
                  ),
                  tags$li(
                    "the measured differences are normally distributed"
                  )
                )
              ),
              helpText("
Preliminary statistical tests of assumptions such as normality or equal variance
between groups are controversial and often criticised within the statistics
community.
              "),
              helpText("
Applying a transformation, e.g. log, square root or cube root, can help to
make skewed data conform more closely to normality.
              ")
            ),
            mainPanel(
              conditionalPanel(
                condition = "!input.two_sample_paired",
                tabsetPanel(
                  tabPanel(
                    "Q-Q plot",
                    helpText("
The Q-Q (quantile-quantile) plot compares the data with a normal distribution by
plotting their quantiles against each other.
                    "),
                    plotOutput("two_sample_qq_plot", height = "400px"),
                    helpText("
The theoretical quantiles are for a standard normal distribution with mean 0 and
standard deviation 1.
                    "),
                    helpText("
The points will lie approximately along the diagonal line (fitted through the
points for the first and third quartiles) if the data are normally distributed.
                    "),
                    helpText("
Significant deviations from the line may suggest the use of a non-parametric
test.
                    ")
                  ),
                  tabPanel(
                    "Shapiro-Wilk test",
                    h5("Shapiro-Wilk test of normality"),
                    helpText("
The Shapiro-Wilk test tests the null hypothesis that the data come from a
normally distributed population.
The test is run for each of the two groups separately.
                    "),
                    div(style = "margin-top: 10px;"),
                    fluidRow(
                      column(
                        width = 6,
                        verbatimTextOutput("two_sample_shapiro_wilk_test1")
                      ),
                      column(
                        width = 6,
                        verbatimTextOutput("two_sample_shapiro_wilk_test2")
                      )
                    ),
                    helpText("
The null hypothesis can be rejected if the p-value is less than 0.05, suggesting
that the data come from a population that are not normally distributed.
                    "),
                    helpText("
If the null hypothesis can't be rejected, this means there is insufficient
evidence that the data are not normal. This is not the same as accepting that
the data come from a normal distribution, i.e. it does not prove that the null
hypothesis is true.
                    "),
                    helpText("
Caution is advised when using a preliminary test for normality to decide whether
a parametric or non-parametric test should subsequently be used, particularly
when the sample size is small. It is often better to make your own assessment by
looking at box plots, density plots, histograms and Q-Q plots.
                    ")
                  ),
                  tabPanel(
                    "F-test",
                    h5("F-test to compare two variances"),
                    helpText("
The F-test of equality of variances is a test for the null hypothesis that two
normal populations have the same variance.
                    "),
                    helpText("
The Welch t-test, an adaptation of Student's t-test, may be more reliable when
the samples have unequal variances and/or sample sizes.
                    "),
                    div(style = "margin-top: 10px;"),
                    verbatimTextOutput("two_sample_variance_test"),
                    helpText("
The null hypothesis can be rejected if the p-value is less than 0.05, suggesting
that the data come from populations with different variance.
                    "),
                    helpText("
Treat the result of this test with caution; it is often better assess
differences in variance between the two groups by inspecting the box and density
plots.
                    ")
                  )
                )
              ),
              conditionalPanel(
                condition = "input.two_sample_paired",
                tabsetPanel(
                  tabPanel(
                    "Q-Q plot",
                    helpText("
The Q-Q (quantile-quantile) plot compares the data with a normal distribution by
plotting their quantiles against each other. In the paired two sample case,
quantiles for the differences between pairs of measurements are used.
                    "),
                    plotOutput(
                      "paired_qq_plot",
                      height = "400px",
                      width = "66%"
                    ),
                    helpText("
The theoretical quantiles are for a standard normal distribution with mean 0 and
standard deviation 1.
                    "),
                    helpText("
The points will lie approximately along the diagonal line (fitted through the
points for the first and third quartiles) if the data are normally distributed.
                    "),
                    helpText("
Significant deviations from the line may suggest the use of a non-parametric
test.
                    ")
                  ),
                  tabPanel(
                    "Shapiro-Wilk test",
                    h5("Shapiro-Wilk test of normality"),
                    helpText("
The Shapiro-Wilk test tests the null hypothesis that the data come from a
normally distributed population.
In the paired two sample case, the test is run on the differences between pairs
of measurements.
                    "),
                    div(style = "margin-top: 10px;"),
                    verbatimTextOutput("two_sample_paired_shapiro_wilk"),
                    helpText("
The null hypothesis can be rejected if the p-value is less than 0.05, suggesting
that the data come from a population that are not normally distributed.
                    "),
                    helpText("
If the null hypothesis can't be rejected, this means there is insufficient
evidence that the data are not normal. This is not the same as accepting that
the data come from a normal distribution, i.e. it does not prove that the null
hypothesis is true.
                    "),
                    helpText("
Caution is advised when using a preliminary test for normality to decide whether
a parametric or non-parametric test should subsequently be used, particularly
when the sample size is small. It is often better to make your own assessment by
looking at box plots, density plots, histograms and Q-Q plots.
                    ")
                  )
                )
              )
            )
          )
        ),
        tabPanel(
          "Statistical tests",
          br(),
          sidebarLayout(
            sidebarPanel(
              radioButtons(
                "two_sample_test_type",
                label = "Test",
                choices = c("Parametric", "Non-parametric")
              ),
              conditionalPanel(
                condition = "!input.two_sample_paired",
                helpText("
A parametric, two sample t-test assumes that the observations of one sample are
independent of the observations of the other sample and that the observations
are random samples from a normally-distributed population for each group.
                ")
              ),
              conditionalPanel(
                condition = "input.two_sample_paired",
                helpText("
A parametric, paired t-test assumes that there are paired measurements for each
subject, that these observations are independent of one another, and that the
measured differences are normally distributed.
                ")
              ),
              conditionalPanel(
                condition = "input.two_sample_test_type == 'Parametric' && !input.two_sample_paired",
                checkboxInput(
                  "two_sample_equal_variance",
                  "Equal variance in each sample"
                ),
                helpText("
The Welch t-test, an adaptation of Student's t-test, may be more reliable when
the samples have unequal variances and/or sample sizes.
                ")
              ),
              br(),
              radioButtons(
                "two_sample_alternative",
                "Alternative hypothesis",
                choices = c(
                  "Two-sided" = "two.sided",
                  "Greater" = "greater",
                  "Less" = "less"
                )
              )
            ),
            mainPanel(
              conditionalPanel(
                condition = "!input.two_sample_paired",
                conditionalPanel(
                  condition = "input.two_sample_test_type == 'Parametric'",
                  conditionalPanel(
                    condition = "input.two_sample_equal_variance",
                    h5("Two sample t-test")
                  ),
                  conditionalPanel(
                    condition = "!input.two_sample_equal_variance",
                    h5("Welch two sample t-test")
                  ),
                  verbatimTextOutput("two_sample_t_test"),
                  plotOutput(
                    "two_sample_t_plot",
                    height = "300px",
                    width = "80%"
                  )
                ),
                conditionalPanel(
                  condition = "input.two_sample_test_type == 'Non-parametric'",
                  h5("Wilcoxon rank sum test"),
                  helpText("Also known as the Mann-Whitney U test."),
                  div(style = "margin-top: 10px;"),
                  verbatimTextOutput("two_sample_wilcoxon_test")
                )
              ),
              conditionalPanel(
                condition = "input.two_sample_paired",
                conditionalPanel(
                  condition = "input.two_sample_test_type == 'Parametric'",
                  h5("Paired t-test"),
                  verbatimTextOutput("paired_t_test"),
                  plotOutput(
                    "paired_t_plot",
                    height = "300px",
                    width = "80%"
                  )
                ),
                conditionalPanel(
                  condition = "input.two_sample_test_type == 'Non-parametric'",
                  h5("Wilcoxon signed rank test"),
                  verbatimTextOutput("paired_wilcoxon_test")
                )
              )
            )
          )
        )
      )
    )
  ),
  p(),
  br(),
  fluidRow(
    column( 
      width = 7,
      a(
        href = "https://www.cam.ac.uk",
        img(
          style = "width: 150px; margin-left: 15px;",
          src = "university_of_cambridge_logo.png"
        )
      ),
      a(
        href = "https://www.cruk.cam.ac.uk",
        img(
          style = "width: 175px; margin-left: 10px;",
          src = "cruk_ci_logo.png"
        )
      )
    ),
    column(
      width = 5,
      div(
        style = "font-size: 90%; float: right;",
        a(
          href = "mailto:statistics@cruk.cam.ac.uk",
          "Contact us"
        )
        # a(
        #   href = "https://www.cruk.cam.ac.uk/terms-and-conditions",
        #   "Terms and Conditions",
        #   style = "margin-left: 10px;"
        # )
      ),
      br(),
      div(
        style = "font-size: 80%; float: right;",
        "Copyright",
        HTML("&copy;"),
        tags$script(type = "text/javascript", "var d = new Date(); document.write(d.getFullYear())"),
        "University of Cambridge",
      )
    )
  ),
  br()
)

# Shiny server side logic
# =======================

server <- function(input, output, session) {

  # clear selections
  reset_selections <- function() {

    updateSelectInput(
      session,
      "one_sample_variable",
      choices = character()
    )

    updateRadioButtons(
      session,
      "one_sample_transformation",
      selected = "none"
    )

    updateTextInput(
      session,
      "one_sample_hypothesized_mean",
      value = ""
    )

    updateTabsetPanel(
      session,
      "one_sample_tabs",
      selected = "Plots"
    )

    updateRadioButtons(
      session,
      "one_sample_test_type",
      selected = "Parametric"
    )

    updateRadioButtons(
      session,
      "one_sample_alternative",
      selected = "two.sided"
    )

    updateCheckboxInput(
      session,
      "two_sample_paired",
      value = FALSE
    )

    updateSelectInput(
      session,
      "two_sample_categorical_variable",
      choices = character()
    )

    updateSelectInput(
      session,
      "two_sample_group1",
      choices = character()
    )

    updateSelectInput(
      session,
      "two_sample_group2",
      choices = character()
    )

    updateSelectInput(
      session,
      "two_sample_variable",
      choices = character()
    )

    updateRadioButtons(
      session,
      "two_sample_transformation",
      selected = "none"
    )

    updateSelectInput(
      session,
      "two_sample_variable1",
      choices = character()
    )

    updateSelectInput(
      session,
      "two_sample_variable2",
      choices = character()
    )

    updateRadioButtons(
      session,
      "two_sample_paired_transformation",
      selected = "none"
    )

    updateTabsetPanel(
      session,
      "two_sample_tabs",
      selected = "Plots"
    )

    updateRadioButtons(
      session,
      "two_sample_test_type",
      selected = "Parametric"
    )

    updateCheckboxInput(
      session,
      "two_sample_equal_variance",
      value = FALSE
    )

    updateRadioButtons(
      session,
      "two_sample_alternative",
      selected = "two.sided"
    )

  }

  reactive_values <- reactiveValues(data = NULL)

  # data file upload
  observe({
    file <- input$data_file
    if (!is.null(file)) {
      reset_selections()
      if (str_detect(file$name, regex("\\.csv$", ignore_case = TRUE))) {
        reactive_values$data <- read_csv(file$datapath)
      } else {
        reactive_values$data <- read_tsv(file$datapath)
      }
    }
  })

  # select sample data set
  observe({
    sample_data <- input$sample_data
    reset_selections()
    reactive_values$data <- datasets[[sample_data]]
  })

  # data file upload
  data <- reactive({
    data <- reactive_values$data
    if (is.null(data)) {
      tibble()
    } else {
      data
    }
  })

  # numerical variables that can be used in statistical tests
  numerical_variables <- reactive({

    data <- data()
    if (is_empty(data)) {
      return(character())
    }

    # __observation__ and __difference__ are reserved names used for paired data
    numeric_data <- data %>%
      select(where(is.numeric)) %>%
      select(-any_of(c("__observation__", "__difference__")))

    if (is_empty(numeric_data)) {
      return(character())
    }

    numeric_data %>%
      pivot_longer(
        everything(),
        names_to = "variable",
        values_to = "value",
        values_drop_na = TRUE
      ) %>%
      mutate(variable = as_factor(variable)) %>%
      count(variable) %>%
      filter(n >= 3) %>%
      pull(variable)
  })

  # categorical variables that can be used for selecting groups
  # for two sample tests
  categorical_variables <- reactive({

    data <- data()
    if (is_empty(data)) {
      return(character())
    }

    possible_categorical_variables <- data %>%
      select(where(is.atomic)) %>%
      summarize(across(everything(), n_distinct, na.rm = TRUE)) %>%
      pivot_longer(everything(), names_to = "variable", values_to = "n") %>%
      filter(n <= 20) %>%
      pull(variable)

    if (is_empty(possible_categorical_variables)) {
      return(character())
    }
    data %>%
      select(all_of(possible_categorical_variables)) %>%
      mutate(across(everything(), as.character)) %>%
      pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
      mutate(variable = as_factor(variable)) %>%
      count(variable, value) %>%
      filter(n >= 3) %>%
      count(variable) %>%
      filter(n >= 2) %>%
      pull(variable)
  })

  # table displaying uploaded data
  output$data_table <- DT::renderDataTable({
      DT::datatable(
        data(),
        rownames = FALSE,
        selection = "single"
      )
    },
    server = TRUE
  )

  # one sample test
  # ---------------

  # update variable selection list for one sample test
  observe({
    updateSelectInput(
      session,
      "one_sample_variable",
      label = "Variable",
      choices = numerical_variables()
    )
  })

  # update hypothesized mean value when a new variable is selected
  observe({
    variable <- input$one_sample_variable
    updateTextInput(
      session,
      "one_sample_hypothesized_mean",
      value = ""
    )
  })

  # selected data for one sample test
  one_sample_data <- reactive({
    data <- data()
    variable <- input$one_sample_variable
    if (variable == "" || !variable %in% colnames(data)) {
      numeric()
    } else {
      data %>%
        pull(variable)
    }
  })

  one_sample_transformed_data <- reactive({
    transform(one_sample_data(), input$one_sample_transformation)
  })

  # hypothesized mean, log transformed if required
  one_sample_hypothesized_mean <- reactive({
    transform(
      as.numeric(input$one_sample_hypothesized_mean),
      input$one_sample_transformation
    )
  })

  # information message about the current data set and whether there are any
  # numerical columns/variables
  output$one_sample_info <- renderText({
    data <- data()

    if (is_empty(data)) {
      return("No data loaded")
    }

    numerical_variables <- numerical_variables()
    if (is_empty(numerical_variables)) {
      return("No numerical variables in current data set")
    }

    # values prior to any transformation
    values <- one_sample_data()

    # filter out missing values
    values <- values[!is.na(values)]

    transform <- input$one_sample_transformation

    if (transform == "natural_log" && any(values < 0)) {
      return(
        "Log transformation is not suitable as there are negative values."
      )
    }
    if (transform == "natural_log" && any(values == 0)) {
      return("Log transformation is not suitable as there are zero values.")
    }

    if (transform == "square_root" && any(values < 0)) {
      return(str_c(
        "Square root transformation is not suitable as there are negative ",
        "values."
      ))
    }

    hypothesized_mean <- input$one_sample_hypothesized_mean

    if (str_length(hypothesized_mean) == 0) {
      return("Hypothesized mean is not set")
    } else if (is.na(as.numeric(hypothesized_mean))) {
      return("Non-numeric value specified for hypothesized mean")
    } else if (transform == "natural_log" && hypothesized_mean < 0) {
      return(str_c(
        "Log transformation is not suitable if the hypothesized mean is ",
        "negative."
      ))
    } else if (transform == "natural_log" && hypothesized_mean == 0) {
      return(str_c(
        "Log transformation is not suitable if the hypothesized mean is ",
        "zero."
      ))
    } else if (transform == "square_root" && hypothesized_mean < 0) {
      return(str_c(
        "Square root transformation is not suitable if the hypothesized mean ",
        "is negative."
      ))
    }
  })

  # summary statistics table
  output$one_sample_summary_statistics <- DT::renderDataTable({
      values <- one_sample_transformed_data()
      if (is_empty(values)) {
        NULL
      } else {
        summary_statistics <- summary_statistics(values) %>%
          pivot_longer(
            everything(),
            names_to = "statistic",
            values_to = "value"
          ) %>%
          mutate(value = round(value, digits = 3))
        DT::datatable(
          summary_statistics,
          options = list(
            dom = "t",
            bSort = FALSE,
            pageLength = nrow(summary_statistics)
          ),
          rownames = FALSE,
          colnames = NULL
        )
      }
    },
    server = TRUE
  )

  # boxplot and histogram
  output$one_sample_plots <- renderPlot({
    values <- one_sample_transformed_data()
    variable <- input$one_sample_variable

    xintercept <- NULL
    if (input$one_sample_show_hypothesized_mean) {
      xintercept <- one_sample_hypothesized_mean()
    }

    number_of_bins <- NULL
    if (input$one_sample_choose_number_of_bins) {
      number_of_bins <- input$one_sample_number_of_bins
    }

    histogram <- create_histogram(
      values,
      name = variable,
      xintercept = xintercept,
      number_of_bins = number_of_bins,
      show_normal_distribution = input$one_sample_show_normal_distribution
    )

    limits <- layer_scales(histogram)$x$limits

    boxplot <- create_boxplot(
      values,
      xintercept = xintercept,
      limits = limits,
      show_points = input$one_sample_show_points,
      show_density = input$one_sample_violin
    )

    boxplot + plot_spacer() + histogram +
      plot_layout(heights = c(1, 0.25, 1))
  })

  # one sample Q-Q plot
  output$one_sample_qq_plot <- renderPlot({
    create_qq_plot(one_sample_transformed_data())
  })

  # one sample Shapiro-Wilk test
  output$one_sample_shapiro_wilk_test <- renderPrint({
    values <- one_sample_transformed_data()

    tryCatch(
      {
        check_values(values)

        result <- shapiro.test(values)

        # override the data name
        result$data.name <- input$one_sample_variable

        cat("shapiro.test(values)\n")
        result
      },
      error = function(e) {
        cat(e$message)
      }
    )
  })

  # one sample t-test
  output$one_sample_t_test <- renderPrint({
    values <- one_sample_transformed_data()
    hypothesized_mean <- one_sample_hypothesized_mean()
    alternative <- input$one_sample_alternative
    variable <- input$one_sample_variable

    tryCatch(
      {
        result <- one_sample_t_test(
          values,
          hypothesized_mean,
          alternative = alternative,
          variable = variable
        )

        cat(
          "t.test(values, mu = ", result$null.value, ", ",
          "alternative = \"", result$alternative, "\")\n",
          sep = ""
        )

        result
      },
      error = function(e) {
        cat(e$message)
      }
    )
  })

  # one sample Student's t distribution plot
  output$one_sample_t_plot <- renderPlot({
    values <- one_sample_transformed_data()
    hypothesized_mean <- one_sample_hypothesized_mean()
    alternative <- input$one_sample_alternative
    variable <- input$one_sample_variable

    tryCatch(
      {
        result <- one_sample_t_test(
          values,
          hypothesized_mean,
          alternative = alternative,
          variable = variable
        )

        create_t_distribution_plot(result)
      },
      error = function(e) {
      }
    )
  })

  # one sample Wilcoxon signed rank test
  output$one_sample_wilcoxon_test <- renderPrint({
    values <- one_sample_transformed_data()
    hypothesized_mean <- one_sample_hypothesized_mean()
    alternative <- input$one_sample_alternative
    variable <- input$one_sample_variable

    tryCatch(
      {
        result <- one_sample_wilcoxon_test(
          values,
          hypothesized_mean,
          alternative = alternative,
          variable = variable
        )

        cat(
          "wilcox.test(values, mu = ", result$null.value, ", ",
          "alternative = \"", result$alternative, "\")\n",
          sep = ""
        )

        result
      },
      error = function(e) {
        cat(e$message)
      }
    )
  })

  # two sample test
  # ---------------

  # update categorical variable selection list for two sample test
  observe({
    updateSelectInput(
      session,
      "two_sample_categorical_variable",
      label = "Categorical variable",
      choices = categorical_variables()
    )
  })

  # groups must have at least 3 values to be available for selection
  two_sample_groups <- reactive({
    data <- data()
    categorical_variable <- input$two_sample_categorical_variable
    if (categorical_variable == "" ||
        !categorical_variable %in% colnames(data)) {
      character()
    } else {
      data() %>%
        select(group = !!categorical_variable) %>%
        filter(!is.na(group)) %>%
        count(group) %>%
        filter(n >= 3) %>%
        pull(group)
    }
  })

  # update group lists for two sample test
  observe({
    groups <- two_sample_groups()
    updateSelectInput(
      session,
      "two_sample_group1",
      label = "Group 1",
      choices = two_sample_groups(),
      selected = groups[1]
    )
    updateSelectInput(
      session,
      "two_sample_group2",
      label = "Group 2",
      choices = two_sample_groups(),
      selected = groups[2]
    )
  })

  # update numerical variable selection lists for two sample test
  observe({
    numerical_variables <- numerical_variables()
    updateSelectInput(
      session,
      "two_sample_variable",
      label = "Variable",
      choices = numerical_variables
    )
    updateSelectInput(
      session,
      "two_sample_variable1",
      label = "Variable 1",
      choices = numerical_variables,
      selected = numerical_variables[1]
    )
    updateSelectInput(
      session,
      "two_sample_variable2",
      label = "Variable 2",
      choices = numerical_variables,
      selected = numerical_variables[2]
    )
  })

  # selected data for two sample test
  two_sample_data <- reactive({

    data <- data()

    if (input$two_sample_paired) {
      # paired observations in two numerical columns
      variable1 <- input$two_sample_variable1
      variable2 <- input$two_sample_variable2

      if (variable1 == "" || variable2 == "" ||
          variable1 == variable2 ||
          !variable1 %in% colnames(data) ||
          !variable2 %in% colnames(data)
      ) {
        tibble(
          `__observation__` = numeric(),
          `__difference__` = numeric(),
          group = character(),
          value = numeric()
        )
      } else {
        data %>%
          select(all_of(c(variable1, variable2))) %>%
          mutate(`__observation__` = row_number()) %>%
          mutate(`__difference__` = get(variable1) - get(variable2)) %>%
          pivot_longer(
            cols = all_of(c(variable1, variable2)),
            names_to = "group",
            values_to = "value"
          ) %>%
          mutate(group = factor(group, levels = c(variable1, variable2)))
      }
    } else {
      # data expected to be structured with one categorical variable/column
      # specifying the two or more groups and another numerical column
      categorical_variable <- input$two_sample_categorical_variable
      variable <- input$two_sample_variable
      group1 <- input$two_sample_group1
      group2 <- input$two_sample_group2

      if (categorical_variable == "" || variable == "" ||
        group1 == "" || group2 == "" ||
        group1 == group2 ||
        !categorical_variable %in% colnames(data) ||
        !variable %in% colnames(data)
      ) {
        tibble(
          group = character(),
          value = numeric()
        )
      } else {
        data %>%
          select(all_of(c(categorical_variable, variable))) %>%
          `colnames<-`(c("group", "value")) %>%
          filter(group %in% c(group1, group2)) %>%
          mutate(group = factor(group, levels = c(group1, group2)))
      }
    }
  })

  two_sample_transformed_data <- reactive({
    data <- two_sample_data()
    if (input$two_sample_paired) {
      mutate(data, `__difference__` =
        transform(`__difference__`, input$two_sample_paired_transformation)
      )
    } else {
      mutate(data, value = transform(value, input$two_sample_transformation))
    }
  })

  # differences in paired observations following transformation if specified
  two_sample_paired_differences <- reactive({
    two_sample_transformed_data() %>%
      distinct(`__observation__`, .keep_all = TRUE) %>%
      pull(`__difference__`)
  })

  # information message about the current data set and whether there are any
  # numerical columns/variables
  output$two_sample_info <- renderText({
    data <- data()

    if (is_empty(data)) {
      return("No data loaded")
    }

    numerical_variables <- numerical_variables()
    categorical_variables <- categorical_variables()

    paired <- input$two_sample_paired

    if (paired) {
      if (is_empty(numerical_variables)) {
        return("No numerical variables in current data set")
      }
      if (length(numerical_variables) < 2) {
        return("Only one numerical variable in current data set")
      }
    } else {
      if (is_empty(numerical_variables)) {
        if (is_empty(categorical_variables)) {
          return("No categorical or numerical variables in current data set")
        } else {
          return("No numerical variables in current data set")
        }
      } else {
        if (is_empty(categorical_variables)) {
          return("No categorical variables in current data set")
        }
      }
    }

    if (paired) {
      variable1 <- input$two_sample_variable1
      variable2 <- input$two_sample_variable2
      if (variable1 == variable2 && variable1 != "") {
        return("Select different variables for the paired observations")
      }
    } else {
      group1 <- input$two_sample_group1
      group2 <- input$two_sample_group2
      if (group1 == group2 && group1 != "") {
        return("Select different categories for the two sample groups")
      }
    }

    # values prior to any transformation
    data <- two_sample_data()

    if (nrow(data) > 0) {
      if (paired) {
        transform <- input$two_sample_paired_transformation
        values <- data %>%
          distinct(`__observation__`, .keep_all = TRUE) %>%
          pull(`__difference__`)
      } else {
        transform <- input$two_sample_transformation
        values <- pull(data, value)
      }

      # filter out missing values
      values <- values[!is.na(values)]

      if (transform == "natural_log" && any(values < 0)) {
        return(
          "Log transformation is not suitable as there are negative values."
        )
      }
      if (transform == "natural_log" && any(values == 0)) {
        return(
          "Log transformation is not suitable as there are zero values.",
        )
      }

      if (transform == "square_root" && any(values < 0)) {
        return(str_c(
          "Square root transformation is not suitable as there are negative ",
          "values."
        ))
      }
    }
  })

  # summary statistics table
  output$two_sample_summary_statistics <- DT::renderDataTable({
      data <- two_sample_transformed_data()

      if (nrow(data) == 0) {
        NULL
      } else {
        summary_statistics <- data %>%
          group_by(group) %>%
          summarize(summary_statistics(value)) %>%
          pivot_longer(
            -group,
            names_to = "statistic",
            values_to = "value"
          ) %>%
          mutate(value = round(value, digits = 3)) %>%
          pivot_wider(
            id_cols = "statistic",
            names_from = "group",
            values_from = "value"
          )

        if (input$two_sample_paired) {
          difference_statistics <- data %>%
            distinct(`__observation__`, `__difference__`) %>%
            pull(`__difference__`) %>%
            summary_statistics() %>%
            pivot_longer(
              everything(),
              names_to = "statistic",
              values_to = "differences"
            ) %>%
            mutate(differences = round(differences, digits = 3))

          summary_statistics <- left_join(
            summary_statistics,
            difference_statistics,
            by = "statistic"
          )
        }

        colnames(summary_statistics)[1] <- ""

        DT::datatable(
          summary_statistics,
          options = list(
            dom = "t",
            bSort = FALSE,
            pageLength = nrow(summary_statistics)
          ),
          rownames = FALSE
        )
      }
    },
    server = TRUE
  )

  # boxplots and histograms
  output$two_sample_plots <- renderPlot({
    data <- two_sample_transformed_data()

    groups <- data$group
    values <- data$value

    variable <- NULL
    if (!input$two_sample_paired) {
      variable <- input$two_sample_variable
    }

    boxplot <- create_boxplot(
      values,
      groups = groups,
      name = variable,
      show_points = input$two_sample_show_points,
      show_density = input$two_sample_violin
    )

    number_of_bins <- NULL
    if (input$two_sample_choose_number_of_bins) {
      number_of_bins <- input$two_sample_number_of_bins
    }

    histogram <- create_histogram(
      values,
      groups = groups,
      name = variable,
      number_of_bins = number_of_bins,
      show_normal_distribution = input$two_sample_show_normal_distribution
    )

    plots <- boxplot + plot_spacer() + histogram +
      plot_layout(heights = c(1, 0.25, 1))

    if (input$two_sample_paired) {
      differences <- two_sample_paired_differences()

      number_of_bins <- NULL
      if (input$two_sample_choose_number_of_bins) {
        number_of_bins <- input$two_sample_number_of_bins
      }

      difference_histogram <- create_histogram(
        differences,
        name = "difference",
        number_of_bins = number_of_bins,
        show_normal_distribution = input$two_sample_show_normal_distribution
      )

      limits <- layer_scales(difference_histogram)$x$limits

      difference_boxplot <- create_boxplot(
        differences,
        name = "difference",
        limits = limits,
        show_points = input$two_sample_show_points,
        show_density = input$two_sample_violin
      )

      plots <-
        boxplot + plot_spacer() + difference_boxplot +
        plot_spacer() + plot_spacer() + plot_spacer() +
        histogram + plot_spacer() + difference_histogram +
        plot_layout(widths = c(2, 0.25, 1), heights = c(1, 0.25, 1))
    }

    plots
  })

  # two sample Q-Q plot
  output$two_sample_qq_plot <- renderPlot({
    data <- two_sample_transformed_data()
    create_qq_plot(data$value, data$group)
  })

  # paired Q-Q plot
  output$paired_qq_plot <- renderPlot({
    create_qq_plot(two_sample_paired_differences())
  })

  # two sample Shapiro-Wilk test - first group
  output$two_sample_shapiro_wilk_test1 <- renderPrint({
    data <- two_sample_transformed_data()

    variable <- input$two_sample_group1

    tryCatch(
      {
        check_values(data$value)

        cat("# Group 1: ", variable, "\n", sep = "")

        values <- data %>%
          filter(group == variable) %>%
          pull(value)

        cat("shapiro.test(group1)\n")

        result <- shapiro.test(values)

        # override the data name
        result$data.name <- variable

        result
      },
      error = function(e) {
        cat(e$message)
      }
    )
  })

  # two sample Shapiro-Wilk test - second group
  output$two_sample_shapiro_wilk_test2 <- renderPrint({
    data <- two_sample_transformed_data()

    variable <- input$two_sample_group2

    tryCatch(
      {
        check_values(data$value)

        cat("# Group 2: ", variable, "\n", sep = "")

        values <- data %>%
          filter(group == variable) %>%
          pull(value)

        check_values(values)

        cat("shapiro.test(group2)\n")

        result <- shapiro.test(values)

        # override the data name
        result$data.name <- variable

        result
      },
      error = function(e) {
        cat(e$message)
      }
    )
  })

  # two sample Shapiro-Wilk test for paired observation differences
  output$two_sample_paired_shapiro_wilk <- renderPrint({
    values <- two_sample_paired_differences()

    variable <- input$one_sample_variable

    tryCatch(
      {
        check_values(values)

        cat("shapiro.test(differences)\n")

        result <- shapiro.test(values)

        # override the data name
        result$data.name <- variable

        result
      },
      error = function(e) {
        cat(e$message)
      }
    )
  })

  # F-test to compare variances of two samples
  output$two_sample_variance_test <- renderPrint({
    data <- two_sample_transformed_data()

    variable1 <- input$two_sample_group1
    variable2 <- input$two_sample_group2

    values1 <- data %>%
      filter(group == variable1) %>%
      pull(value)

    values2 <- data %>%
      filter(group == variable2) %>%
      pull(value)

    tryCatch(
      {
        check_values(values1, "(group 1)")
        check_values(values2, "(group 2)")

        cat("var.test(group1, group2)\n")

        var.test(values1, values2)
      },
      error = function(e) {
        cat(e$message)
      }
    )
  })

  # two sample t-test
  output$two_sample_t_test <- renderPrint({
    data <- two_sample_transformed_data()

    variable1 <- input$two_sample_group1
    variable2 <- input$two_sample_group2

    values1 <- data %>%
      filter(group == variable1) %>%
      pull(value)

    values2 <- data %>%
      filter(group == variable2) %>%
      pull(value)

    alternative <- input$two_sample_alternative
    equal_variance <- input$two_sample_equal_variance

    tryCatch(
      {
        result <- two_sample_t_test(
          values1,
          values2,
          alternative = alternative,
          equal_variance = equal_variance,
          paired = FALSE,
          variable1 = variable1,
          variable2 = variable2
        )

        cat(
          "t.test(group1, group2, ",
          "alternative = \"", result$alternative, "\", ",
          "var.equal = ", equal_variance, ")\n",
          sep = ""
        )

        result
      },
      error = function(e) {
        cat(e$message)
      }
    )
  })

  output$two_sample_t_plot <- renderPlot({
    data <- two_sample_transformed_data()

    variable1 <- input$two_sample_group1
    variable2 <- input$two_sample_group2

    values1 <- data %>%
      filter(group == variable1) %>%
      pull(value)

    values2 <- data %>%
      filter(group == variable2) %>%
      pull(value)

    alternative <- input$two_sample_alternative
    equal_variance <- input$two_sample_equal_variance

    tryCatch(
      {
        result <- two_sample_t_test(
          values1,
          values2,
          alternative = alternative,
          equal_variance = equal_variance,
          paired = FALSE,
          variable1 = variable1,
          variable2 = variable2
        )

        create_t_distribution_plot(result)
      },
      error = function(e) {
      }
    )
  })

  # two sample Wilcoxon rank sum test
  output$two_sample_wilcoxon_test <- renderPrint({
    data <- two_sample_transformed_data()

    variable1 <- input$two_sample_group1
    variable2 <- input$two_sample_group2

    values1 <- data %>%
      filter(group == variable1) %>%
      pull(value)

    values2 <- data %>%
      filter(group == variable2) %>%
      pull(value)

    tryCatch(
      {
        check_values(values1, "(group 1)")
        check_values(values1, "(group 2)")

        result <- wilcox.test(
          values1,
          values2,
          alternative = input$two_sample_alternative
        )

        # override the data name
        result$data.name <- str_c(variable1, " and ", variable2)

        cat(
          "wilcox.test(group1, group2, ",
          "alternative = \"", result$alternative, "\")\n",
          sep = ""
        )

        result
      },
      error = function(e) {
        cat(e$message)
      }
    )
  })

  # paired t-test
  output$paired_t_test <- renderPrint({
    transformation <- input$two_sample_paired_transformation
    alternative <- input$two_sample_alternative

    if (transformation == "none") {
      # use paired values
      data <- two_sample_data()

      variable1 <- input$two_sample_variable1
      variable2 <- input$two_sample_variable2

      values1 <- data %>%
        filter(group == variable1) %>%
        arrange(`__observation__`) %>%
        pull(value)

      values2 <- data %>%
        filter(group == variable2) %>%
        arrange(`__observation__`) %>%
        pull(value)

      tryCatch(
        {
          check_values(values1, "(group 1)")
          check_values(values1, "(group 2)")

          result <- two_sample_t_test(
            values1,
            values2,
            alternative = alternative,
            paired = TRUE,
            variable1 = variable1,
            variable2 = variable2
          )

          cat(
            "t.test(variable1, variable2, ",
            "alternative = \"", result$alternative, "\", ",
            "paired = TRUE)\n",
            sep = ""
          )

          result
        },
        error = function(e) {
          cat(e$message)
        }
      )
    } else {
      # use one-sample test for differences and zero mean since the differences
      # have been transformed not the paired values
      differences <- two_sample_paired_differences()

      tryCatch(
        {
          check_values(differences)

          result <- one_sample_t_test(
            differences,
            hypothesized_mean = 0,
            alternative = alternative,
            variable = "differences"
          )

          cat(
            "t.test(differences, mu = ", result$null.value, ", ",
            "alternative = \"", result$alternative, "\")\n",
            sep = ""
          )

          result
        },
        error = function(e) {
          cat(e$message)
        }
      )
    }
  })

  output$paired_t_plot <- renderPlot({
    transformation <- input$two_sample_paired_transformation
    alternative <- input$two_sample_alternative

    if (transformation == "none") {
      # use paired values
      data <- two_sample_data()

      variable1 <- input$two_sample_variable1
      variable2 <- input$two_sample_variable2

      values1 <- data %>%
        filter(group == variable1) %>%
        arrange(`__observation__`) %>%
        pull(value)

      values2 <- data %>%
        filter(group == variable2) %>%
        arrange(`__observation__`) %>%
        pull(value)

      tryCatch(
        {
          check_values(values1, "(group 1)")
          check_values(values1, "(group 2)")

          result <- two_sample_t_test(
            values1,
            values2,
            alternative = alternative,
            paired = TRUE,
            variable1 = variable1,
            variable2 = variable2
          )

          create_t_distribution_plot(result)
        },
        error = function(e) {
        }
      )
    } else {
      # use one-sample test for differences and zero mean since the differences
      # have been transformed not the paired values
      differences <- two_sample_paired_differences()

      tryCatch(
        {
          check_values(differences)

          result <- one_sample_t_test(
            differences,
            hypothesized_mean = 0,
            alternative = alternative,
            variable = "differences"
          )

          create_t_distribution_plot(result)
        },
        error = function(e) {
        }
      )
    }
  })

  # paired Wilcoxon signed rank test
  output$paired_wilcoxon_test <- renderPrint({
    transformation <- input$two_sample_paired_transformation
    alternative <- input$two_sample_alternative

    if (transformation == "none") {
      # use paired values
      data <- two_sample_data()

      variable1 <- input$two_sample_variable1
      variable2 <- input$two_sample_variable2

      values1 <- data %>%
        filter(group == variable1) %>%
        arrange(`__observation__`) %>%
        pull(value)

      values2 <- data %>%
        filter(group == variable2) %>%
        arrange(`__observation__`) %>%
        pull(value)

      tryCatch(
        {
          check_values(values1, "(group 1)")
          check_values(values1, "(group 2)")

          result <- two_sample_wilcoxon_test(
            values1,
            values2,
            alternative = alternative,
            paired = TRUE,
            variable1 = variable1,
            variable2 = variable2
          )

          cat(
            "wilcox.test(variable1, variable2, ",
            "alternative = \"", result$alternative, "\", ",
            "paired = TRUE)\n",
            sep = ""
          )

          result
        },
        error = function(e) {
          cat(e$message)
        }
      )
    } else {
      # use one-sample test for differences and zero mean since the differences
      # have been transformed not the paired values
      differences <- two_sample_paired_differences()

      tryCatch(
        {
          check_values(differences)

          result <- one_sample_wilcoxon_test(
            differences,
            0,
            alternative = alternative,
            variable = "differences"
          )

          cat(
            "wilcox.test(differences, mu = ", result$null.value, ", ",
            "alternative = \"", result$alternative, "\")\n",
            sep = ""
          )

          result
        },
        error = function(e) {
          cat(e$message)
        }
      )
    }
  })

}

shinyApp(ui, server)

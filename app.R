library(shiny)
library(shinyjs)
library(tidyverse)
library(patchwork)
library(DT)

options(shiny.maxRequestSize = 1024 * 1024 * 1024)
options(scipen = 999)

# Datasets
# ========

datasets <- list(
  "2.1 Effect of disease X on height" = read_csv(
    "datasets/DiseaseX.csv",
    col_types = "cd"
  ),
  "2.2 Blood vessel formation" = read_csv(
    "datasets/BloodVesselFormation1.csv",
    col_types = "cd"
  ),
  "3.1 Biological process duraction" = read_csv(
    "datasets/BiologicalProcessDurations.csv",
    col_types = "fd"
  ),
  "3.2 Blood vessel formation" = read_csv(
    "datasets/BloodVesselFormation2.csv",
    col_types = "cdd"
  ),
  "3.3 NIBP gene expression in breast cancer patients" = read_csv(
    "datasets/NIBPExpression.csv",
    col_types = "fd"
  ),
  "3.4 Vitamin D levels and fibrosis" = read_csv(
    "datasets/FibrosisVitaminD.csv",
    col_types = "fd"
  ),
  "3.5 Birth weight of twins and Sudden Infant Death Syndrome" = read_csv(
    "datasets/SIDSTwinBirthWeight.csv",
    col_types = "dd"
  ),
  "4.1 Growth of Zea seedlings from self- and cross-fertilization" = read_csv(
    "datasets/PlantGrowthFertilization.csv",
    col_types = "dd"
  ),
  "4.2 Florence Nightingale's hygiene regime" = read_csv(
    "datasets/FlorenceNightingaleHygieneRegime.csv",
    col_types = "fd"
  ),
  "4.3 Effect of bran on diet of patients with diverticulosis" = read_csv(
    "datasets/BranDiverticulosis.csv",
    col_types = "fd"
  ),
  "4.4 Effect on repetitive behaviour of autism drug" = read_csv(
    "datasets/AutismDrugRepetitiveBehaviour.csv",
    col_types = "dd"
  ),
  "4.5 Effect of HIV drug on CD4 cell counts" = read_csv(
    "datasets/HIVDrugCD4Counts.csv",
    col_types = "dd"
  ),
  "4.6 Drink driving and reaction times" = read_csv(
    "datasets/DrinkDrivingReactionTimes.csv",
    col_types = "dd"
  ),
  "4.7 Pollution in poplar trees" = read_csv(
    "datasets/TreePollution.csv",
    col_types = "dd"
  ),
  "4.8 Sex effect on salaries of professors" = read_csv(
    "datasets/ProfessorialSalaries.csv",
    col_types = "fd"
  ),
  "Star Wars" = starwars,
  "Effectiveness of insect sprays" = InsectSprays,
  "Plant growth for two different treatment conditions and a control" =
    PlantGrowth,
  "Sibling chick weights reared in confinement or on open range" = read_csv(
    "datasets/chick_weights.csv",
    col_types = "cdd"
  ),
  "Test1" = tibble(
    Before = c(12, 13, 10, 10, 16, 15, 14),
    After = c(10, 12, 8, 9, 15, 12, 13)
  ),
  "Test2" = tibble(
    Group = c(rep("WT", 25), rep("KO", 25)),
    Expression = -round(c(rnorm(25, 0.2, 0.01), rnorm(25, 0.21, 0.015)), digits = 3)
  ),
  "Test3" = tibble(
    A = -round(rnorm(25, 0.2, 0.01), digits = 3),
    B = -round(rnorm(25, 0.3, 0.015), digits = 3)
  ),
  "Test4" = tibble(
    Group = c(rep("WT", 25), rep("KO", 25)),
    Expression = round(c(rnorm(25, 0.1, 0.01), rnorm(25, -0.1, 0.015)), digits = 3)
  ),
  "Categories" = tibble(Group = c("WT", "WT", "WT", "KO", "KO", "KO")),
  "Letters" = tibble(letter = letters),
  "Empty" = tibble()
)

# useful functions
# ================

# summary statistics
summary_statistics <- function(x) {
  as_tibble(list(
    `Number of observations` = length(x),
    `Missing values` = sum(is.na(x)),
    Mean = mean(x, na.rm = TRUE),
    `Standard deviation` = sd(x, na.rm = TRUE),
    `Confidence interval (C.I.) lower` =
      mean(x, na.rm = TRUE) - 1.96 * sd(x, na.rm = TRUE) / sqrt(length(x)),
    `Confidence interval (C.I.) upper` =
      mean(x, na.rm = TRUE) + 1.96 * sd(x, na.rm = TRUE) / sqrt(length(x)),
    Minimum = min(x, na.rm = TRUE),
    `25th percentile (1st quartile)` = quantile(x, 0.25, na.rm = TRUE),
    Median = median(x, na.rm = TRUE),
    `75th percentile (3rd quartile)` = quantile(x, 0.75, na.rm = TRUE),
    Maximum = max(x, na.rm = TRUE),
    `Interquartile range (IQR)` = IQR(x, na.rm = TRUE)
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
  boxplot <- tibble(value = values, group = groups) %>%
    ggplot(aes(x = value, y = group, fill = group))

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

  if (!is.null(xintercept) && is.finite(xintercept)) {
    boxplot <- boxplot +
      geom_vline(
        xintercept = xintercept,
        colour = "#FFB000",
        size = 1.25,
        linetype = "solid"
      )
  }

  if (is.null(limits)) {
    limits <- range(c(values, xintercept), na.rm = TRUE)
  }

  boxplot <- boxplot +
    scale_x_continuous(limits = limits) +
    scale_y_discrete(drop = FALSE) +
    scale_fill_manual(values = c("#648FFF", "#DC267F"), drop = FALSE) +
    labs(x = name) +
    theme_minimal() +
    theme(
      axis.title.x = element_text(size = 14),
      axis.title.y = element_blank(),
      axis.text = element_text(size = 12),
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

  histogram <- tibble(value = values, group = groups) %>%
    ggplot(aes(x = value, fill = group, group = group))

  limits <- range(c(values, xintercept), na.rm = TRUE)
  breaks <- NULL

  if (is.null(number_of_bins)) {
    number_of_bins <- nclass.Sturges(values)
  }

  breaks <- pretty(
    range(values, na.rm = TRUE),
    number_of_bins,
    min.n = 1
  )


  limits <- range(c(breaks, limits), na.rm = TRUE)

  if (show_normal_distribution) {
    histogram <- histogram +
      geom_histogram(
        aes(y = after_stat(density)),
        bins = number_of_bins,
        breaks = breaks,
        colour = "black"
      ) +
      stat_function(
        fun = dnorm,
        colour = "grey40",
        linetype = "solid",
        size = 0.75,
        args = list(
          mean = mean(values),
          sd = sd(values)
        )
      )
  } else {
    histogram <- histogram +
      geom_histogram(
        bins = number_of_bins,
        breaks = breaks,
        colour = "black"
      )
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
    labs(x = name) +
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

# t-distribution plot for the given t-test result
create_t_distribution_plot <- function(t_test_result) {

  t <- t_test_result$statistic
  df <- t_test_result$parameter["df"]

  limit <- max(abs(t), qt(0.975, df)) * 1.25
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
      title = element_text(size = 12),
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

# Shiny user interface
# ====================

ui <- fluidPage(
  useShinyjs(),
  div(style = "margin-top: 65px;"),
  navbarPage(
    title = div(
      style = "color: #231F7F; font-size: 90%;",
      a(
        href = "https://www.cruk.cam.ac.uk",
        target = "_blank",
        img(
          style = "width: 180px; margin-right: 10px;",
          src = "CRUK_CI_logo.png"
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
            width = 4,
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
      DT::dataTableOutput("data_table", width = "50%")
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
              choices = list()
            )
          ),
          column(
            width = 2,
            numericInput(
              "one_sample_hypothesized_mean",
              label = "Hypothesized mean",
              value = NA
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
      tabsetPanel(
        id = "one_sample_tabs",
        selected = "Plots",
        tabPanel(
          "Summary statistics",
          br(),
          DT::dataTableOutput("one_sample_summary_statistics", width = "50%")
        ),
        tabPanel(
          "Plots",
          br(),
          sidebarLayout(
            sidebarPanel(
              checkboxInput(
                "one_sample_show_hypothesized_mean",
                label = "Show hypothesized mean",
                value = FALSE
              ),
              h5("Box plot", style = "margin-top: 25px"),
              checkboxInput(
                "one_sample_show_points",
                label = "Show points on box plot",
                value = FALSE
              ),
              helpText(
                "Outlier points for those observations that are further than",
                "1.5 IQR from the edges of the box, i.e. the first and third",
                "quantiles, are always displayed."
              ),
              checkboxInput(
                "one_sample_violin",
                label = "Overlay density on box plot",
                value = FALSE
              ),
              h5("Histogram", style = "margin-top: 25px"),
              checkboxInput(
                "one_sample_choose_number_of_bins",
                label = "Choose number of bins",
                value = FALSE
              ),
              conditionalPanel(
                condition = "!input.one_sample_choose_number_of_bins",
                helpText(
                  "Chooses an optimal number of bins based on the",
                  "data if not selected."
                )
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
                helpText(
                  "The actual number of bins may differ from the specified",
                  "number.",
                )
              ),
              checkboxInput(
                "one_sample_show_normal_distribution",
                label = "Overlay normal distribution",
                value = FALSE
              ),
              helpText(
                "Densities instead of counts are displayed if selected."
              ),
              helpText(
                "The normal distribution shown is based on the mean and",
                "standard deviation of the sample observations."
              )
            ),
            mainPanel(
              plotOutput("one_sample_plots", width = "95%", height = "500px"),
            )
          )
        ),
        tabPanel(
          "Assumption tests",
          br(),
          fluidRow(
            column(
              width = 8,
              helpText(
                "This tab provides preliminary tests that can help with",
                "assessing the assumptions of a parametric test, e.g.",
                "t-test. Preliminary tests of assumptions such as normality",
                "are controversial and often criticised within the statistics",
                "community."
              ),
              h4("Shapiro-Wilk test of normality"),
              verbatimTextOutput("one_sample_shapiro_wilk_test"),
              helpText(
                "Null hypothesis: the data come from a normally distributed",
                "population."
              ),
              helpText(
                "The null hypothesis can be rejected if the p-value is less",
                "than 0.05, suggesting that the data come from a population",
                "that does not follow a normal distribution."
              ),
              helpText(
                "If the null hypothesis can't be rejected, this means there is",
                "insufficient evidence that the data are not normal. This is",
                "not the same as accepting that the data come from a normal",
                "distribution, i.e. it does not prove that the null hypothesis",
                "is true."
              ),
              helpText(
                "Caution is advised when using a preliminary test for",
                "normality to decide whether a parametric or non-parametric",
                "test should subsequently be used, particularly when the",
                "sample size is small. It is often better to make your own",
                "assessment by looking at box plots, density plots and",
                "histograms."
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
              "The assumptions of the parametric one-sample t-test are that",
              "the data values are:",
              tags$ul(
                tags$li("independent (values are not related to one another)"),
                tags$li("continuous (not discrete)"),
                tags$li(
                  "a random sample from a population that is normally",
                  "distributed"
                )
              ),
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
                h4("One sample t-test"),
                fluidRow(
                  column(
                    width = 10,
                    verbatimTextOutput("one_sample_t_test")
                  )
                ),
                br(),
                fluidRow(
                  column(
                    width = 8,
                    offset = 1,
                    plotOutput("one_sample_t_plot", height = "300px")
                  )
                )
              ),
              conditionalPanel(
                condition = "input.one_sample_test_type == 'Non-parametric'",
                h4("Wilcoxon signed rank test"),
                fluidRow(
                  column(
                    width = 10,
                    verbatimTextOutput("one_sample_wilcoxon_test")
                  )
                )
              )
            )
          )
        )
      )
    ),
    tabPanel(
      title = "Two sample test",
      wellPanel(
        checkboxInput(
          "two_sample_paired",
          label = "Paired observations"
        ),
        conditionalPanel(
          condition = "input.two_sample_paired",
          fluidRow(
            column(
              width = 2,
              selectInput(
                "two_sample_variable1",
                label = "Variable 1",
                choices = list()
              )
            ),
            column(
              width = 2,
              selectInput(
                "two_sample_variable2",
                label = "Variable 2",
                choices = list()
              )
            ),
            column(
              width = 4,
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
                choices = list()
              )
            ),
            column(
              width = 2,
              selectInput(
                "two_sample_group1",
                label = "Group 1",
                choices = list()
              )
            ),
            column(
              width = 2,
              selectInput(
                "two_sample_group2",
                label = "Group 2",
                choices = list()
              )
            ),
            column(
              width = 2,
              selectInput(
                "two_sample_variable",
                label = "Variable",
                choices = list()
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
      tabsetPanel(
        id = "two_sample_tabs",
        selected = "Plots",
        tabPanel(
          "Summary statistics",
          br(),
          DT::dataTableOutput("two_sample_summary_statistics", width = "66%")
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
                value = TRUE
              ),
              helpText(
                "Outlier points for those observations that are further than",
                "1.5 IQR from the edges of the box, i.e. the first and third",
                "quantiles, are always displayed."
              ),
              checkboxInput(
                "two_sample_violin",
                label = "Overlay density on box plots",
                value = FALSE
              ),
              h5("Histogram"),
              checkboxInput(
                "two_sample_choose_number_of_bins",
                label = "Choose number of bins",
                value = FALSE
              ),
              conditionalPanel(
                condition = "!input.two_sample_choose_number_of_bins",
                helpText(
                  "Chooses an optimal number of bins based on the",
                  "data if not selected."
                )
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
                helpText(
                  "The actual number of bins may differ from the specified",
                  "number.",
                )
              ),
              checkboxInput(
                "two_sample_show_normal_distribution",
                label = "Overlay normal distribution",
                value = FALSE
              ),
              helpText(
                "Densities instead of counts are displayed if selected."
              ),
              helpText(
                "The normal distribution shown is based on the mean and",
                "standard deviation of all observations from both groups."
              )
            ),
            mainPanel(
              plotOutput("two_sample_plots", width = "95%", height = "500px"),
            )
          )
        ),
        tabPanel(
          "Assumption tests",
          br(),
          fluidRow(
            column(
              width = 8,
              helpText(
                "This tab provides preliminary tests that can help with",
                "assessing the assumptions of a parametric test, e.g.",
                "t-test. Use of preliminary tests of the assumptions such as",
                "normality or equal variance between groups are controversial",
                "and often criticised within the statistics community."
              ),
              h4("Shapiro-Wilk test of normality")
            )
          ),
          conditionalPanel(
            condition = "input.two_sample_paired",
            fluidRow(
              column(
                width = 8,
                helpText(
                  "The test is run on the differences between pairs of",
                  "observations."
                )
              )
            ),
            fluidRow(
              column(
                width = 8,
                verbatimTextOutput("two_sample_paired_shapiro_wilk")
              )
            )
          ),
          conditionalPanel(
            condition = "!input.two_sample_paired",
            helpText("Tests are run for each of the two groups."),
            fluidRow(
              column(
                width = 5,
                verbatimTextOutput("two_sample_shapiro_wilk_test1")
              ),
              column(
                width = 5,
                verbatimTextOutput("two_sample_shapiro_wilk_test2")
              )
            )
          ),
          fluidRow(
            column(
              width = 8,
              helpText(
                "Null hypothesis: the data come from a normally distributed",
                "population."
              ),
              helpText(
                "The null hypothesis can be rejected if the p-value is less",
                "than 0.05, suggesting that the data come from a population",
                "that does not follow a normal distribution."
              ),
              helpText(
                "If the null hypothesis can't be rejected, this means there is",
                "insufficient evidence that the data are not normal. This is",
                "not the same as accepting that the data come from a normal",
                "distribution, i.e. it does not prove that the null hypothesis",
                "is true."
              ),
              helpText(
                "Caution is advised when using a preliminary test for",
                "normality to decide whether a parametric or non-parametric",
                "test should subsequently be used, particularly when the",
                "sample size is small. It is often better to make your own",
                "assessment by looking at box plots, density plots and",
                "histograms."
              )
            )
          ),
          conditionalPanel(
            condition = "!input.two_sample_paired",
            fluidRow(
              column(
                width = 8,
                h4("F test to compare two variances"),
                helpText(
                  "The F-test of equality of variances is a test for the null",
                  "hypothesis that two normal populations have the same",
                  "variance."
                ),
                helpText(
                  "The Welch t-test, an adaptation of Student's t-test, may be",
                  "more reliable when the samples have unequal variances",
                  "and/or sample sizes."
                ),
                verbatimTextOutput("two_sample_variance_test"),
                helpText(
                  "The null hypothesis can be rejected if the p-value is less",
                  "than 0.05, suggesting that the data come from populations",
                  "with different variance."
                ),
                helpText(
                  "Treat the result of this test with caution; it is often",
                  "better assess differences in variance between the two",
                  "groups by inspecting the box and density plots."
                )
              )
            )
          )
        ),
        tabPanel(
          "Statistical tests"
        )
      )
    )
  ),
  div(
    style = "margin-top: 25px;",
    HTML("&copy;"),
    tags$script(type = "text/javascript", "var d = new Date(); document.write(d.getFullYear())"), # nolint
    "University of Cambridge",
    div(
      style = "float:right",
      a(
        href = "https://www.cruk.cam.ac.uk/terms-and-conditions",
        target = "_blank", "Terms and Conditions"
      )
    )
  ),
  br()
)

# Shiny server side logic
# =======================

server <- function(input, output, session) {

  # clear selections
  clear_selections <- function() {

    updateSelectInput(
      session,
      "one_sample_variable",
      choices = list()
    )

    updateRadioButtons(
      session,
      "one_sample_transformation",
      selected = "none"
    )

    updateNumericInput(
      session,
      "one_sample_hypothesized_mean",
      value = NA
    )

    updateTabsetPanel(
      session,
      "one_sample_tabs",
      selected = "Plots"
    )

    updateCheckboxInput(
      session,
      "one_sample_show_hypothesized_mean",
      value = FALSE
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
      choices = list()
    )

    updateSelectInput(
      session,
      "two_sample_group1",
      choices = list()
    )

    updateSelectInput(
      session,
      "two_sample_group2",
      choices = list()
    )

    updateSelectInput(
      session,
      "two_sample_variable",
      choices = list()
    )

    updateRadioButtons(
      session,
      "two_sample_transformation",
      selected = "none"
    )

    updateSelectInput(
      session,
      "two_sample_variable1",
      choices = list()
    )

    updateSelectInput(
      session,
      "two_sample_variable2",
      choices = list()
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
  }

  reactive_values <- reactiveValues(data = NULL)

  # data file upload
  observe({
    file <- input$data_file
    if (!is.null(file)) {
      clear_selections()
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
    clear_selections()
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

    numeric_data <- select(data, where(is.numeric))
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
      summarize(across(everything(), n_distinct, na.rm = TRUE)) %>%
      pivot_longer(everything(), names_to = "variable", values_to = "n") %>%
      filter(n <= 10) %>%
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

  # update variable selection list for one-sample test
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
    updateNumericInput(
      session,
      "one_sample_hypothesized_mean",
      label = "Hypothesized mean",
      value = NA
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
      input$one_sample_hypothesized_mean,
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

    values <- one_sample_data()

    # filter out missing values
    values <- values[is.finite(values)]

    transform <- input$one_sample_transformation

    if (transform == "natural_log" & any(values < 0)) {
      return(
        "Log transformation may not be suitable as there are negative values."
      )
    }
    if (transform == "natural_log" & any(values == 0)) {
      return("Log transformation may not be suitable as there are zero values.")
    }

    if (transform == "square_root" & any(values < 0)) {
      return(str_c(
        "Square root transformation may not be suitable as there are negative ",
        "values."
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

    # filter out missing values
    values <- values[is.finite(values)]

    if (is_empty(values)) {
      return(NULL)
    }

    variable <- input$one_sample_variable

    xintercept <- NULL
    if (input$one_sample_show_hypothesized_mean) {
      xintercept <- one_sample_hypothesized_mean()
      if (!is.finite(xintercept)) {
        xintercept <- NULL
      }
    }

    number_of_bins <- NULL
    if (input$one_sample_choose_number_of_bins) {
      number_of_bins <- input$one_sample_number_of_bins
      if (is.na(number_of_bins)) {
        number_of_bins <- NULL
      }
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

  # one sample Shapiro-Wilk test
  output$one_sample_shapiro_wilk_test <- renderPrint({
    values <- one_sample_transformed_data()

    # filter out missing values
    values <- values[is.finite(values)]

    if (is_empty(values)) {
      cat("No data values")
    } else if (length(values) < 3) {
      cat("Too few values")
    } else {
      cat("shapiro.test(values)\n")

      result <- shapiro.test(values)

      # override the data name
      result$data.name <- input$one_sample_variable

      result
    }
  })

  # one sample t-test
  one_sample_t_test <- reactive({
    values <- one_sample_transformed_data()
    hypothesized_mean <- one_sample_hypothesized_mean()

    # filter out missing values
    values <- values[is.finite(values)]

    if (length(values) < 2 | !is.finite(hypothesized_mean)) {
      return(NULL)
    }

    result <- t.test(
      values,
      mu = hypothesized_mean,
      alternative = input$one_sample_alternative
    )

    # override the data name
    result$data.name <- input$one_sample_variable

    result
  })

  output$one_sample_t_test <- renderPrint({
    values <- one_sample_transformed_data()
    hypothesized_mean <- one_sample_hypothesized_mean()

    # filter out missing values
    values <- values[is.finite(values)]

    if (is_empty(values)) {
      cat("No data values")
    } else if (length(values) < 3) {
      cat("Too few values")
    } else if (is.na(hypothesized_mean)) {
      cat("Hypothesized mean is not set")
    } else if (!is.finite(hypothesized_mean)) {
      cat("Hypothesized mean is not finite")
    } else {
      cat(
        "t.test(values, mu = ", hypothesized_mean,
        ", alternative = \"", input$one_sample_alternative, "\")\n",
        sep = ""
      )

      result <- one_sample_t_test()
      if (!is.null(result)) {
        result
      }
    }
  })

  output$one_sample_t_plot <- renderPlot({
      result <- one_sample_t_test()
      if (!is.null(result)) {
        create_t_distribution_plot(result)
      }
  })

  # one sample Wilcoxon signed rank test
  output$one_sample_wilcoxon_test <- renderPrint({
    values <- one_sample_transformed_data()
    hypothesized_mean <- one_sample_hypothesized_mean()
    alternative <- input$one_sample_alternative

    # filter out missing values
    values <- values[is.finite(values)]

    if (is_empty(values)) {
      cat("No data values")
    } else if (length(values) < 3) {
      cat("Too few values")
    } else if (is.na(hypothesized_mean)) {
      cat("Hypothesized mean is not set")
    } else if (!is.finite(hypothesized_mean)) {
      cat("Hypothesized mean is not finite")
    } else {
      cat(
        "wilcox.test(values, mu = ", hypothesized_mean,
        ", alternative = \"", input$one_sample_alternative, "\")\n",
        sep = ""
      )

      result <- wilcox.test(
        values,
        mu = hypothesized_mean,
        alternative = alternative
      )

      # override the data name
      result$data.name <- input$one_sample_variable

      result
    }
  })

  # two sample test
  # ---------------

  # update categorical variable selection list for two-sample test
  observe({
    updateSelectInput(
      session,
      "two_sample_categorical_variable",
      label = "Categorical variable",
      choices = categorical_variables()
    )
  })

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

  # update group lists for two-sample test
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

  # update numerical variable selection lists for two-sample test
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
        tibble()
      } else {
        data %>%
          select(all_of(c(variable1, variable2))) %>%
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
        tibble()
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

    if (!is_empty(data)) {
      data <- data %>%
        mutate(value = transform(
          value,
          input$two_sample_transformation
        ))
    }

    data
  })

  # selected data for two sample test - paired observations
  two_sample_paired_differences <- reactive({

    if (input$two_sample_paired) {
      data <- data()

      # paired observations in two numerical columns
      variable1 <- input$two_sample_variable1
      variable2 <- input$two_sample_variable2

      if (variable1 == "" || variable2 == "" ||
          variable1 == variable2 ||
          !variable1 %in% colnames(data) ||
          !variable2 %in% colnames(data)
      ) {
        numeric()
      } else {
        data %>%
          select(all_of(c(variable1, variable2))) %>%
          transmute(difference = get(variable2) - get(variable1)) %>%
          pull(difference)
      }
    } else {
        numeric()
    }
  })

  two_sample_transformed_diffs <- reactive({
    transform(
      two_sample_paired_differences(),
      input$two_sample_paired_transformation
    )
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
      } else if (length(numerical_variables) < 2) {
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

    data <- two_sample_data()

    if (!is_empty(data)) {

      if (paired) {
        transform <- input$two_sample_paired_transformation
        values <- two_sample_paired_differences()
      } else {
        transform <- input$two_sample_transformation
        values <- pull(data, value)
      }

      # filter out missing values
      values <- values[is.finite(values)]

      if (transform == "natural_log" & any(values < 0)) {
        return(
          "Log transformation may not be suitable as there are negative values."
        )
      }
      if (transform == "natural_log" & any(values == 0)) {
        return(str_c(
          "Log transformation may not be suitable as there are zero ",
          "values."
        ))
      }

      if (transform == "square_root" & any(values < 0)) {
        return(str_c(
          "Square root transformation may not be suitable as there are ",
          "negative values."
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
          difference_statistics <-
            summary_statistics(two_sample_transformed_diffs()) %>%
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
    if (is_empty(data)) {
      return(NULL)
    }

    data <- filter(data, is.finite(value))
    if (nrow(data) == 0) {
      return(NULL)
    }

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
      if (is.na(number_of_bins)) {
        number_of_bins <- NULL
      }
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

      differences <- two_sample_transformed_diffs()

      # filter out missing values
      differences <- differences[is.finite(differences)]

      difference_boxplot <- plot_spacer()
      difference_histogram <- plot_spacer()

      if (!is_empty(differences)) {
        difference_boxplot <- create_boxplot(
          differences,
          name = "differences",
          show_points = input$two_sample_show_points,
          show_density = input$two_sample_violin
        )

        number_of_bins <- NULL
        if (input$two_sample_choose_number_of_bins) {
          number_of_bins <- input$two_sample_number_of_bins
          if (is.na(number_of_bins)) {
            number_of_bins <- NULL
          }
        }

        difference_histogram <- create_histogram(
          differences,
          name = "differences",
          number_of_bins = number_of_bins,
          show_normal_distribution = input$two_sample_show_normal_distribution
        )
      }

      plots <-
        boxplot + plot_spacer() + difference_boxplot +
        plot_spacer() + plot_spacer() + plot_spacer() +
        histogram + plot_spacer() + difference_histogram +
        plot_layout(widths = c(2, 0.25, 1), heights = c(1, 0.25, 1))
    }

    plots
  })

  # two sample Shapiro-Wilk test - first group
  output$two_sample_shapiro_wilk_test1 <- renderPrint({
    data <- two_sample_transformed_data()

    if (is_empty(data)) {
      cat(" ")
    } else {
      variable <- input$two_sample_group1

      values <- data %>%
        filter(group == variable) %>%
        filter(is.finite(value)) %>%
        pull(value)

      if (length(values) < 3) {
        cat("Too few values")
      } else {
        cat("shapiro.test(group1)\n")

        result <- shapiro.test(values)

        # override the data name
        result$data.name <- variable

        result
      }
    }
  })

  # two sample Shapiro-Wilk test - second group
  output$two_sample_shapiro_wilk_test2 <- renderPrint({
    data <- two_sample_transformed_data()

    if (is_empty(data)) {
      cat(" ")
    } else {
      variable <- input$two_sample_group2

      values <- data %>%
        filter(group == variable) %>%
        filter(is.finite(value)) %>%
        pull(value)

      if (length(values) < 3) {
        cat("Too few values")
      } else {
        cat("shapiro.test(group2)\n")

        result <- shapiro.test(values)

        # override the data name
        result$data.name <- variable

        result
      }
    }
  })

  # two sample Shapiro-Wilk test for paired observation differences
  output$two_sample_paired_shapiro_wilk <- renderPrint({
    values <- two_sample_transformed_diffs()

    # filter out missing values
    values <- values[is.finite(values)]

    if (is_empty(values)) {
      cat("No data values")
    } else if (length(values) < 3) {
      cat("Too few values")
    } else {
      cat("shapiro.test(differences)\n")

      result <- shapiro.test(values)

      # override the data name
      result$data.name <- "differences"

      result
    }
  })

  # F-test to compare variances of two samples
  output$two_sample_variance_test <- renderPrint({
    data <- two_sample_transformed_data()

    if (is_empty(data)) {
      cat(" ")
    } else {
      variable1 <- input$two_sample_group1
      variable2 <- input$two_sample_group2

      values1 <- data %>%
        filter(group == variable1) %>%
        filter(is.finite(value)) %>%
        pull(value)

      values2 <- data %>%
        filter(group == variable2) %>%
        filter(is.finite(value)) %>%
        pull(value)

      if (length(values1) < 3) {
        if (length(values2) < 3) {
          cat("Two few values")
        } else {
          cat("Two few values in group 1")
        }
      } else if (length(values2) < 3) {
          cat("Two few values in group 2")
      } else {
        cat("var.test(group1, group2)\n")
        var.test(values1, values2)
      }
    }
  })
}

shinyApp(ui = ui, server = server)

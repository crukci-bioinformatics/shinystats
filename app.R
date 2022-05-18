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
  "Star Wars" = starwars,
  "Fisher's iris data" = iris,
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
    col_types = "dd"
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
    col_types = "cd"
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
  )
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

  if (!is.null(xintercept) && !is.na(xintercept)) {
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
    scale_fill_manual(values = c("#648FFF", "#DC267F")) +
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
    # message("Sturges number of bins: ", number_of_bins)
  }

  breaks <- pretty(
    range(values, na.rm = TRUE),
    number_of_bins,
    min.n = 1
  )

  # message("Actual number of bins: ", length(breaks) - 1)
  # message("Breaks: ", str_c(as.character(breaks), collapse = ", "))

  limits <- range(c(breaks, limits), na.rm = TRUE)
  # message("Limits: ", str_c(limits, collapse = ", "))

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

  if (!is.null(xintercept) && !is.na(xintercept)) {
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
    scale_fill_manual(values = c("#648FFF", "#DC267F")) +
    labs(x = name) +
    facet_wrap(vars(group)) +
    theme_minimal() +
    theme(
      strip.text = element_text(size = 14),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      legend.position = "none"
    )

  histogram
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
            selectInput(
              "sample_data",
              label = "Sample data sets",
              choices = names(datasets)
            )
          )
        )
      ),
      DT::dataTableOutput("uploaded_data_table", width = "97.5%")
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
            width = 3,
            radioButtons(
              "one_sample_transformation",
              label = "Transformation",
              choices = c("None", "log10", "log2", "ln"),
              selected = "None",
              inline = TRUE
            ),
          ),
          column(
            width = 2,
            numericInput(
              "one_sample_hypothesized_mean",
              label = "Hypothesized mean",
              value = NA
            )
          )
        ),
        em(textOutput("one_sample_info"))
      ),
      tabsetPanel(
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
                value = TRUE
              ),
              h5("Box plot"),
              checkboxInput(
                "one_sample_show_points",
                label = "Show points on box plot",
                value = TRUE
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
              h5("Histogram"),
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
          "Statistical analysis"
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
              width = 3,
              radioButtons(
                "two_sample_paired_transformation",
                label = "Transformation",
                choices = c("None", "log10", "log2", "ln"),
                selected = "None",
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
              width = 3,
              radioButtons(
                "two_sample_transformation",
                label = "Transformation",
                choices = c("None", "log10", "log2", "ln"),
                selected = "None",
                inline = TRUE
              )
            )
          )
        ),
        em(textOutput("two_sample_info"))
      ),
      tabsetPanel(
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
          "Statistical analysis"
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
      label = "Variable",
      choices = list()
    )

    updateRadioButtons(
      session,
      "one_sample_transformation",
      selected = "None"
    )

    updateNumericInput(
      session,
      "one_sample_hypothesized_mean",
      label = "Hypothesized mean",
      value = NA
    )

    updateCheckboxInput(
      session,
      "two_sample_paired",
      value = FALSE
    )

    updateSelectInput(
      session,
      "two_sample_categorical_variable",
      label = "Categorical variable",
      choices = list()
    )

    updateSelectInput(
      session,
      "two_sample_group1",
      label = "Group 1",
      choices = list()
    )

    updateSelectInput(
      session,
      "two_sample_group2",
      label = "Group 2",
      choices = list()
    )

    updateSelectInput(
      session,
      "two_sample_variable",
      label = "Variable",
      choices = list()
    )

    updateRadioButtons(
      session,
      "two_sample_transformation",
      selected = "None"
    )

    updateSelectInput(
      session,
      "two_sample_variable1",
      label = "Variable 1",
      choices = list()
    )

    updateSelectInput(
      session,
      "two_sample_variable2",
      label = "Variable 2",
      choices = list()
    )

    updateRadioButtons(
      session,
      "two_sample_paired_transformation",
      selected = "None"
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
  output$uploaded_data_table <- DT::renderDataTable({
      DT::datatable(data(), rownames = FALSE)
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
      # tibble(group = character(), value = double())
      tibble()
    } else {
      data %>%
        select(all_of(variable)) %>%
        rename(value = !!variable) %>%
        mutate(group = "A", .before = "value")
    }
  })

  # information message about the current data set and whether there are any
  # numerical columns/variables
  output$one_sample_info <- renderText({
    data <- data()
    if (is_empty(data)) {
      "No data loaded"
    } else {
      numerical_variables <- numerical_variables()
      if (is_empty(numerical_variables)) {
        "No numerical variables in current data set"
      } else {
        ""
      }
    }
  })

  # summary statistics table
  output$one_sample_summary_statistics <- DT::renderDataTable({
      data <- one_sample_data()
      if (is_empty(data)) {
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
    data <- one_sample_data()
    if (is_empty(data)) {
      return(NULL)
    }

    data <- filter(data, !is.na(value))
    if (nrow(data) == 0) {
      return(NULL)
    }

    values <- data$value

    variable <- input$one_sample_variable

    xintercept <- NULL
    if (input$one_sample_show_hypothesized_mean) {
      xintercept <- input$one_sample_hypothesized_mean
      if (is.na(xintercept)) {
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
          mutate(observation = row_number()) %>%
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
          rename(group = !!categorical_variable) %>%
          rename(value = !!variable) %>%
          filter(group %in% c(group1, group2)) %>%
          mutate(group = factor(group, levels = c(group1, group2)))
      }
    }
  })

  # selected data for two sample test - paired observations
  two_sample_paired_differences <- reactive({

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

  # information message about the current data set and whether there are any
  # numerical columns/variables
  output$two_sample_info <- renderText({
    data <- data()
    if (is_empty(data)) {
      "No data loaded"
    } else {
      numerical_variables <- numerical_variables()
      if (input$two_sample_paired) {
        if (is_empty(numerical_variables)) {
          "No numerical variables in current data set"
        } else if (length(numerical_variables) < 2) {
          "Only one numerical variable in current data set"
        } else {
          ""
        }
      } else {
        categorical_variables <- categorical_variables()
        if (is_empty(numerical_variables)) {
          if (is_empty(categorical_variables)) {
            "No categorical or numerical variables in current data set"
          } else {
            "No numerical variables in current data set"
          }
        } else {
          if (is_empty(categorical_variables)) {
            "No categorical variables in current data set"
          } else {
          ""
          }
        }
      }
    }
  })

  # summary statistics table
  output$two_sample_summary_statistics <- DT::renderDataTable({
      data <- two_sample_data()
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
            summary_statistics(two_sample_paired_differences()) %>%
            pivot_longer(
              everything(),
              names_to = "statistic",
              values_to = "difference"
            ) %>%
            mutate(difference = round(difference, digits = 3))

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
    data <- two_sample_data()
    if (is_empty(data)) {
      return(NULL)
    }

    data <- filter(data, !is.na(value))
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

      differences <- two_sample_paired_differences()
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

        plots <-
          boxplot + plot_spacer() + difference_boxplot +
          plot_spacer() + plot_spacer() + plot_spacer() +
          histogram + plot_spacer() + difference_histogram +
          plot_layout(widths = c(2, 0.25, 1), heights = c(1, 0.25, 1))
      }
    }

    plots
  })

}

# shinyApp(ui = ui, server = server)
runApp(shinyApp(ui = ui, server = server))

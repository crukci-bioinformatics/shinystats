library(shiny)
library(shinyjs)
library(tidyverse)
library(tidyr)
library(dplyr)
library(cowplot)
library(DT)

options(shiny.maxRequestSize = 1024 * 1024 * 1024)
options(scipen = 999)

# useful functions
# ================

# summary statistics
summary_statistics <- function(x) {
  as_tibble(list(
    `Number of observations` = length(x),
    `Missing values` = sum(is.na(x)),
    Mean = mean(x, na.rm = TRUE),
    `Standard deviation` = sd(x, na.rm = TRUE),
    `Confidence interval (C.I.) lower` = mean(x, na.rm = TRUE) - 1.96 * sd(x, na.rm = TRUE) / sqrt(length(x)),
    `Confidence interval (C.I.) upper` = mean(x, na.rm = TRUE) + 1.96 * sd(x, na.rm = TRUE) / sqrt(length(x)),
    Minimum = min(x, na.rm = TRUE),
    `25th percentile (1st quartile)` = quantile(x, 0.25, na.rm = TRUE),
    Median = median(x, na.rm = TRUE),
    `75th percentile (3rd quartile)` = quantile(x, 0.75, na.rm = TRUE),
    Maximum = max(x, na.rm = TRUE),
    `Interquartile range (IQR)` = IQR(x, na.rm = TRUE)
  ))
}

# box plot


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
              "Upload file",
              accept = c(".csv", ".tsv", ".txt")
            )
          ),
          column(
            width = 3,
            offset = 1,
            selectInput(
              "sample_data",
              label = "Sample data sets",
              choices = c("starwars", "mtcars", "iris")
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
            width = 2,
            offset = 1,
            numericInput(
              "one_sample_hypothesized_mean",
              label = "Hypothesized mean",
              value = NA
            )
          )
        ),
        textOutput("one_sample_info")
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
              helpText("Outlier points for those observations that are further than 1.5 IQR from the edges of the box, i.e. the first and third quantiles, are always displayed."),
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
                condition = "input.one_sample_choose_number_of_bins",
                sliderInput(
                  "one_sample_number_of_bins",
                  label = "Number of bins",
                  min = 5,
                  max = 50,
                  value = 20,
                  ticks = FALSE
                )
              ),
              checkboxInput(
                "one_sample_show_normal_distribution",
                label = "Overlay normal distribution",
                value = FALSE
              ),
              helpText("The normal distribution shown is based on the mean and standard deviation of the sample observations. Densities instead of counts are displayed if this option is selected.")
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
        fluidRow(
          column(
            width = 3,
            checkboxInput(
              "two_sample_paired",
              label = "Paired observations"
            )
          )
        ),
        fluidRow(
          conditionalPanel(
            condition = "input.two_sample_paired",
            column(
              width = 3,
              selectInput(
                "two_sample_variable1",
                label = "Variable 1",
                choices = list()
              )
            ),
            column(
              width = 3,
              selectInput(
                "two_sample_variable2",
                label = "Variable 2",
                choices = list()
              )
            )
          ),
          conditionalPanel(
            condition = "!input.two_sample_paired",
            column(
              width = 3,
              selectInput(
                "two_sample_categorical_variable",
                label = "Categorical variable",
                choices = list()
              )
            ),
            column(
              width = 3,
              selectInput(
                "two_sample_group1",
                label = "Group 1",
                choices = list()
              )
            ),
            column(
              width = 3,
              selectInput(
                "two_sample_group2",
                label = "Group 2",
                choices = list()
              )
            ),
            column(
              width = 3,
              selectInput(
                "two_sample_variable",
                label = "Variable",
                choices = list()
              )
            )
          )
        ),
        textOutput("two_sample_info")
      ),
      tabsetPanel(
        tabPanel(
          "Summary statistics",
          br(),
          DT::dataTableOutput("two_sample_summary_statistics", width = "66%")
        ),
        tabPanel(
          "Plots"
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

    updateNumericInput(
      session,
      "one_sample_hypothesized_mean",
      label = "Hypothesized mean",
      value = NA
    )

    updateCheckboxInput(
      session,
      "one_sample_choose_number_of_bins",
      label = "Choose number of bins",
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
  }

  reactive_values <- reactiveValues(data = NULL)

  # data file upload
  observe({
    file <- input$data_file
    message("New file selected: ", file)
    if (!is.null(file)) {
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
    message("Selected sample data: ", sample_data)
    reactive_values$data <- as_tibble(get(sample_data))
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
  output$one_sample_summary_statistics <- DT::renderDataTable(
    {
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

    variable <- input$one_sample_variable

    hypothesized_mean <- input$one_sample_hypothesized_mean

    limits <- range(c(data$value, hypothesized_mean), na.rm = TRUE)

    if (input$one_sample_choose_number_of_bins) {
      number_of_bins <- input$one_sample_number_of_bins
      breaks <- NULL
    } else {
      number_of_bins <- nclass.Sturges(data$value)
      breaks <- pretty(
        range(data$value, na.rm = TRUE),
        number_of_bins,
        min.n = 1
      )
      limits <- range(c(breaks, limits), na.rm = TRUE)
    }

    boxplot <- ggplot(data, aes(x = value, y = group))

    outlier_shape <- NULL
    if (input$one_sample_show_points) {
      outlier_shape <- NA
    }

    if (input$one_sample_violin) {
      boxplot <- boxplot +
        geom_violin(fill = "#FF00FF", alpha = 0.75) +
        geom_boxplot(fill = "white", width = 0.2, outlier.shape = outlier_shape)
    } else {
      boxplot <- boxplot +
        geom_boxplot(fill = "#FF00FF", alpha = 0.75, outlier.shape = outlier_shape)
    }

    if (input$one_sample_show_points) {
      boxplot <- boxplot +
        geom_jitter(
          colour = "black",
          width = 0,
          height = ifelse(input$one_sample_violin, 0.1, 0.15)
        )
    }

    if (input$one_sample_show_hypothesized_mean && !is.na(hypothesized_mean)) {
      boxplot <- boxplot +
        geom_vline(xintercept = hypothesized_mean, lty = 2, col = "red")
    }

    boxplot <- boxplot +
      scale_x_continuous(limits = limits) +
      scale_y_discrete(breaks = NULL) +
      labs(x = variable, y = "") +
      theme_minimal()

    histogram <- ggplot(data, aes(x = value))

    if (input$one_sample_show_normal_distribution) {
      histogram <- histogram +
        geom_histogram(
          aes(y = after_stat(density)),
          bins = number_of_bins,
          breaks = breaks,
          colour = "black",
          fill = "navyblue"
        ) +
        stat_function(
          fun = dnorm,
          colour = "red",
          args = list(
            mean = mean(data$value),
            sd = sd(data$value)
          )
        )
    } else {
      histogram <- histogram +
        geom_histogram(
          bins = number_of_bins,
          breaks = breaks,
          colour = "black",
          fill = "navyblue"
        )
    }

    if (input$one_sample_show_hypothesized_mean && !is.na(hypothesized_mean)) {
      histogram <- histogram +
        geom_vline(xintercept = hypothesized_mean, lty = 2, col = "red")
    }

    histogram <- histogram +
      scale_x_continuous(limits = limits) +
      labs(x = variable) +
      theme_minimal() +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
      )

    plot_grid(boxplot, histogram, ncol = 1, rel_heights = c(2, 3), align = "v")
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

  # selected data for one sample test
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
        # tibble(group = character(), value = double())
        tibble()
      } else {
        data %>%
          select(all_of(c(variable1, variable2))) %>%
          pivot_longer(everything(), names_to = "group", values_to = "value") %>%
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
        # tibble(group = character(), value = double())
        tibble()
      } else {
        data %>%
          select(all_of(c(categorical_variable, variable))) %>%
          rename(group = !!categorical_variable) %>%
          rename(value = !!variable) %>%
          filter(group %in% c(group1, group2))
      }
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
          "Only one numerical variable in current data set - need 2 for paired data"
        } else {
          ""
        }
      } else {
        categorical_variables <- categorical_variables()
        if (is_empty(categorical_variables)) {
          "No categorical variables in current data set"
        } else {
          if (is_empty(numerical_variables)) {
            "No numerical variables in current data set"
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
}

app <- shinyApp(ui = ui, server = server)

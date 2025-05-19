###############################################################################
# app.R  ─ County-Level Voting Dashboard
#
# A Shiny app with three tabs:
#   1. “Vote Share Linear Model”    – scatter plot of predicted vs. actual GOP
#                                      share for a chosen year + model summary.
#   2. “Demographic Influence …”    – how each demographic predictor’s
#                                      coefficient changes across elections.
#   3. “2016 → 2020 Trump Shifts”   – which demographics differ most between
#                                      counties that swung hard to Trump vs.
#                                      hard to Biden.
#
# The goal here is clarity.  Code is written in a beginner-friendly style:
#   • Short, readable pipes (one idea per line).
#   • Very few purrr-style list manipulations.
#   • Base R helpers (paste/paste0) where they work just as well.
###############################################################################

# ── Packages ────────────────────────────────────────────────────────────
library(shiny)
library(shinydashboard)
library(tidyverse)   # ggplot2, dplyr, readr, etc.
library(plotly)      # interactive plots
library(DT)          # data tables
library(scales)      # percent() for labels
library(broom)       # tidy() for model output
library(janitor)     # clean_names()

# ── Read pre-processed data ─────────────────────────────────────────────
dat        <- readRDS("dat_clean.rds")          # county-level data set
coefs      <- readRDS("coef_tidy.rds")          # tidy model coefficients
shift_sum  <- read_csv("trump_demographic_summary_by_shift.csv") %>% clean_names()
shift_tests <- read_csv("trump_demographic_ttests_by_shift.csv",
                        show_col_types = FALSE) %>% clean_names()

# Make sure names line up across tables
shift_sum   <- rename(shift_sum,  term = var)
shift_tests <- rename(shift_tests, term = var)

# Convert any whole-number percentages to 0–1 proportions
if (max(shift_tests$mean_pos, na.rm = TRUE) > 1) {
  shift_tests <- mutate(
    shift_tests,
    mean_pos = mean_pos / 100,
    mean_neg = mean_neg / 100
  )
}

# Build one master table for the “shift” tab
shift_df <- mutate(
  shift_tests,
  diff_pp = 100 * (mean_pos - mean_neg),      # difference in percentage-points
  sig     = p_value < 0.05
)

# Helper: HTML <li> bullets for the “Key takeaways” box
takeaway_bullets <- function(df, n = 5) {
  top <- df %>% arrange(desc(abs(diff_pp))) %>% head(n)
  paste0(
    "<li>",
    ifelse(top$diff_pp > 0,
           paste0(top$term, " higher in Trump-shift counties (+",
                  round(top$diff_pp, 2), " pp)"),
           paste0(top$term, " higher in Biden-shift counties (",
                  round(top$diff_pp, 2), " pp)")),
    "</li>",
    collapse = ""
  )
}

# ── User Interface ──────────────────────────────────────────────────────
ui <- dashboardPage(
  dashboardHeader(title = "County-Level GOP Vote Explorer"),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem("Vote Share Linear Model",
                         tabName = "scatter", icon = icon("bar-chart")),
                menuItem("Demographic Influence on Vote Share",
                         tabName = "effects",  icon = icon("line-chart")),
                menuItem("2016 → 2020 Trump Demographics",
                         tabName = "shifts",  icon = icon("exchange"))
    ),
    ## Show a year-picker only on the first tab
    conditionalPanel(
      condition = "input.tabs === 'scatter'",
      selectInput("year_sel", "Election year:",
                  choices = sort(unique(dat$year)),
                  selected = 2020)
    )
  ),
  
  # ---- Dashboard Body (three tabs) ------------------------------------
  dashboardBody(
    tabItems(
      # 1. Scatter tab ----------------------------------------------------
      tabItem(tabName = "scatter",
              fluidRow(box(width = 12,
                           plotlyOutput("scatterPlot", height = 560))),
              fluidRow(box(width = 12,
                           title = "Model summary",
                           htmlOutput("modelInfo")))
      ),
      
      # 2. Demographic-effect tab ----------------------------------------
      tabItem(tabName = "effects",
              fluidRow(box(width = 12,
                           plotlyOutput("coefPlot", height = 560))),
              fluidRow(box(width = 12,
                           DTOutput("coefTable")))
      ),
      
      # 3. Shift tab (2016 → 2020) --------------------------------------
      tabItem(tabName = "shifts",
              fluidRow(
                box(width = 6, plotlyOutput("shiftBar",   height = 480)),
                box(width = 6, htmlOutput("shiftTake1"))
              ),
              fluidRow(
                box(width = 6, plotlyOutput("shiftBubble", height = 480)),
                box(width = 6, htmlOutput("shiftTake2"))
              )
      )
    )
  )
)

# ── Server logic ───────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # --- 1. Helpers that depend on the selected year ---------------------
  df_year <- reactive({
    req(input$year_sel)
    
    dat %>%
      filter(year == input$year_sel) %>%
      mutate(winner = if_else(rep_share > 0.5, "Republican", "Democrat"))
  })
  
  fit_year <- reactive({
    this_df <- df_year()
    
    # Build a *very* simple model:
    lm(rep_share ~ log1p(total_pop) + .,
       data = select(this_df, rep_share, total_pop, starts_with("pct_")))
  })
  
  # --- 2. Scatter plot --------------------------------------------------
  output$scatterPlot <- renderPlotly({
    df  <- df_year()
    fit <- fit_year()
    
    r2  <- round(summary(fit)$r.squared, 3)
    df$pred_share <- fitted(fit)
    
    p <- ggplot(df,
                aes(x = pred_share, y = rep_share, colour = winner,
                    text = paste0(county_name,
                                  "<br>Actual: ", percent(rep_share, 0.1),
                                  "<br>Predicted: ", percent(pred_share, 0.1)))) +
      geom_point(alpha = 0.6, size = 2) +
      ## —— fixed: force one group so Plotly keeps the line ——
      geom_smooth(aes(group = 1),         # <-- add this
                  method = "lm",
                  se     = FALSE,
                  colour = "black",
                  linewidth = 0.8,
                  show.legend = FALSE) +
      scale_colour_manual(values = c(Republican = "red", Democrat = "blue")) +
      labs(x = "Model-predicted Republican share",
           y = "Actual share",
           colour = "County winner") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text") %>%
      layout(title  = list(
        text = paste0("County GOP Share – ", input$year_sel,
                      " (R² = ", r2, ")"),
        x = 0.05, y = 0.97),
        legend = list(title = list(text = "Winner")))
  })
  
  # --- 3. Model summary box --------------------------------------------
  output$modelInfo <- renderUI({
    tidy_fit <- tidy(fit_year()) %>%
      filter(term != "(Intercept)") %>%
      arrange(desc(abs(estimate))) %>%
      head(8)
    
    coef_lines <- paste0(
      "<li><strong>", tidy_fit$term, "</strong>: ",
      round(tidy_fit$estimate, 3), "</li>",
      collapse = ""
    )
    
    HTML(paste0(
      "<p><strong>Model:</strong> OLS (dependent variable = Republican share)</p>",
      "<p><strong>Largest standardized coefficients:</strong></p>",
      "<ul>", coef_lines, "</ul>",
      "<p><em>Coefficients are on standardized predictors; a one-SD increase ",
      "in the predictor changes GOP share by the value shown.</em></p>"
    ))
  })
  
  # --- 4. Demographic-effect line chart --------------------------------
  output$coefPlot <- renderPlotly({
    # Keep the eight most influential predictors (largest absolute β)
    top_vars <- coefs %>%
      group_by(term) %>%
      summarise(max_abs = max(abs(estimate)), .groups = "drop") %>%
      arrange(desc(max_abs)) %>%
      head(8) %>%
      pull(term)
    
    p <- ggplot(filter(coefs, term %in% top_vars),
                aes(x = year, y = estimate, colour = term)) +
      geom_line(size = 1.1) +
      geom_point(size = 2) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      scale_x_continuous(breaks = c(2008, 2012, 2016, 2020)) +
      labs(y = "Standardised coefficient (β)",
           x = NULL,
           colour = "Predictor",
           title = "Demographic influence on GOP share over time") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # --- 5. Coefficient data table ---------------------------------------
  output$coefTable <- renderDT({
    coefs %>%
      arrange(year, desc(abs(estimate))) %>%
      mutate(across(c(estimate, std.error), round, 3),
             p.value = signif(p.value, 3)) %>%
      datatable(options = list(pageLength = 25), rownames = FALSE)
  })
  
  # =====  TAB 3 — 2016 → 2020 SHIFT DEMOGRAPHICS  =======================
  # 3-A Diverging bar chart ---------------------------------------------
  output$shiftBar <- renderPlotly({
    bar_df <- shift_df %>% arrange(diff_pp)
    
    p <- ggplot(bar_df,
                aes(x = diff_pp, y = reorder(term, diff_pp),
                    fill = diff_pp > 0,
                    text = paste0(term,
                                  "<br>Trump-shift counties: ",
                                  percent(mean_pos, 0.1),
                                  "<br>Biden-shift counties: ",
                                  percent(mean_neg, 0.1)))) +
      geom_col() +
      scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "blue"),
                        labels = c("Shift to Biden", "Shift to Trump"),
                        name   = NULL) +
      labs(x = "Difference in means (percentage-points)",
           y = NULL,
           title = "Demographic gaps between Trump-shift and Biden-shift counties") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text") %>% layout(showlegend = FALSE)
  })
  
  # Key takeaways (HTML) -------------------------------------------------
  output$shiftTake1 <- renderUI({
    HTML(paste0(
      "<h4>Key takeaways</h4><ul>",
      takeaway_bullets(shift_df, 5),
      "</ul>",
      "<p>Positive bars → demographic is more prevalent where the vote ",
      "moved <span style='color:red'>toward Trump</span>.<br>",
      "Negative bars → more prevalent where the vote ",
      "moved <span style='color:blue'>toward Biden</span>.</p>",
      "<p><strong>Shift counties</strong> have a ≥1 SD change in Trump vote ",
      "share from 2016 → 2020.</p>"
    ))
  })
  
  # 3-B Bubble / volcano plot -------------------------------------------
  output$shiftBubble <- renderPlotly({
    bub_df <- mutate(
      shift_df,
      logp    = -log10(p_value),
      sig_col = if_else(sig, "Significant", "NS")
    )
    
    p <- ggplot(bub_df,
                aes(x = diff_pp, y = logp,
                    size = abs(diff_pp), colour = sig_col,
                    text = paste0(term,
                                  "<br>Δ = ", round(diff_pp, 2), " pp",
                                  "<br>p = ", signif(p_value, 3)))) +
      geom_point(alpha = 0.8) +
      scale_colour_manual(values = c(Significant = "darkorange",
                                     NS = "grey70")) +
      labs(x = "Difference in means (pp)",
           y = "-log10(p)",
           colour = NULL,
           size = "|Δ| (pp)",
           title = "Magnitude vs. significance of demographic gaps") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  # Bubble-plot commentary ----------------------------------------------
  output$shiftTake2 <- renderUI({
    # Three most / least significant demographics
    most_sig  <- shift_df %>% arrange(p_value) %>% head(3)
    least_sig <- shift_df %>% arrange(desc(p_value)) %>% head(3)
    
    make_li <- function(df) {
      paste0(
        "<li>", df$term, " (p = ", signif(df$p_value, 3), ")</li>",
        collapse = ""
      )
    }
    
    HTML(paste0(
      "<h4>What stands out?</h4>",
      "<p><strong>Most statistically distinctive demographics</strong></p>",
      "<ul>", make_li(most_sig), "</ul>",
      "<p><strong>Least statistically distinctive demographics</strong></p>",
      "<ul>", make_li(least_sig), "</ul>",
      "<p>The vertical axis of the bubble plot is −log<sub>10</sub>(p). ",
      "Higher values → stronger evidence that the demographic truly differs ",
      "between Trump-shift and Biden-shift counties.</p>"
    ))
  })
}

# ── Run the app ─────────────────────────────────────────────────────────
shinyApp(ui, server)

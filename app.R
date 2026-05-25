# ---- 1. Libraries -----------------------------------------------------------

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(tibble)
  library(purrr)
  library(ggplot2)
  library(broom)
  library(htmltools)
})


# ---- 2. Helpers -------------------------------------------------------------

standardize <- function(x) as.numeric(scale(x))

fmt_p <- function(p) {
  if (is.na(p)) return("—")
  if (p < .001) "< .001" else sprintf("%.3f", p)
}

sig_stars <- function(p) {
  if (is.na(p)) ""
  else if (p < .001) "***"
  else if (p < .01)  "**"
  else if (p < .05)  "*"
  else ""
}

slope_label <- function(s) {
  if (s <= -0.30) "Negative relationship"
  else if (s < -0.05) "Slight negative relationship"
  else if (s <= 0.05) "Near-zero relationship"
  else if (s < 0.30) "Positive relationship"
  else "Strong positive relationship"
}

predict_moderation <- function(cf, x, m) {
  unname(cf["(Intercept)"] + cf["X"] * x + cf["M"] * m + cf["X:M"] * x * m)
}

numeric_cols <- function(df) {
  cols <- names(df)[vapply(df, is.numeric, logical(1))]
  cols[!grepl("(^id$|_id$)", cols, ignore.case = TRUE)]
}

default_choice <- function(choices, keywords, fallback_idx) {
  hit <- choices[tolower(choices) %in% tolower(keywords)]
  if (length(hit) > 0) return(hit[[1]])
  choices[[min(fallback_idx, length(choices))]]
}

plot_theme <- function(base = 14) {
  theme_minimal(base_size = base) +
    theme(
      plot.title       = element_text(face = "bold"),
      plot.subtitle    = element_text(color = "grey30"),
      legend.position  = "bottom",
      legend.title     = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey92"),
      axis.title       = element_text(face = "bold"),
      strip.text       = element_text(face = "bold")
    )
}

moderator_palette <- c(
  "Moderator = Mean - 1 SD" = "#6B7280",
  "Moderator = Mean"        = "#6B7280",
  "Moderator = Mean + 1 SD" = "#6B7280"
)

active_line_color <- "#D81B60"

example_data <- function(n = 300, seed = 42) {
  set.seed(seed)
  x <- rnorm(n)
  m <- 0.30 * x + rnorm(n, sd = 0.9)
  y <- 0.15 * x + 0.20 * m + 0.40 * x * m + rnorm(n, sd = 0.9)
  tibble(
    Predictor_X = x,
    Moderator_M = m,
    Outcome_Y   = y,
    Covariate_A = rnorm(n),
    Covariate_B = rnorm(n)
  )
}

metric_box <- function(title, output_id, tip = NULL) {
  div(
    class = "metric-box",
    title = tip,
    div(class = "metric-title", title),
    div(class = "metric-value", textOutput(output_id, inline = TRUE))
  )
}


# ---- 3. UI ------------------------------------------------------------------

ui <- page_navbar(
  title = "Moderation Regression Simulator",
  id    = "nav",
  theme = bs_theme(version = 5, bootswatch = "flatly"),

  header = tags$head(
    tags$style(HTML("
      .focal-tip                                              { color: #555; font-size: .82rem; margin-top: -6px; margin-bottom: .5rem; }
      .interp-card ul                                         { padding-left: 1.2rem; }
      /* Keep the interaction plot immediately visible without full-screen mode. */
      .plot-card .card-header                                 { padding: .4rem .75rem; }
      .plot-card .card-body                                   { padding: .25rem .5rem .4rem .5rem; }
      .metric-strip                                           { display: grid; grid-template-columns: repeat(5, minmax(0, 1fr)); gap: .35rem; margin: .35rem 0; }
      .metric-box                                             { border: 1px solid #d8dee6; border-radius: .3rem; background: #fff; padding: .28rem .4rem; min-width: 0; }
      .metric-title                                           { color: #5b6774; font-size: .68rem; font-weight: 700; line-height: 1.05; text-transform: uppercase; overflow-wrap: anywhere; }
      .metric-value                                           { color: #1c2b36; font-size: .95rem; font-weight: 700; line-height: 1.15; margin-top: .05rem; }
      #moderation_plot                                        { min-height: 340px; }
      @media (max-height: 760px) {
        #moderation_plot                                      { min-height: 300px; }
        .metric-box                                           { padding: .22rem .35rem; }
      }
      .adj-banner                                             { background: #fff8e1; border-left: 4px solid #f59e0b; padding: .4rem .6rem; margin-bottom: .55rem; font-size: .85rem; color: #6b4f00; border-radius: .15rem; }
      .btn-reset-adj                                          { background-color: #f59e0b; border-color: #d97706; color: #1c1208; font-weight: 600; letter-spacing: .01em; padding: .35rem .5rem; font-size: .82rem; margin-top: .25rem; margin-bottom: .4rem; box-shadow: 0 1px 2px rgba(0,0,0,.08); }
      .btn-reset-adj:hover                                    { background-color: #d97706; border-color: #b45309; color: #fff; }
      .btn-reset-adj:focus                                    { box-shadow: 0 0 0 .2rem rgba(245,158,11,.35); }
    "))
  ),

  nav_panel(
    "Model Builder",
    layout_sidebar(
      sidebar = sidebar(
        width = 320,
        title = "Controls",
        open = "desktop",

        accordion(
          open = c("Variables", "Display"),
          accordion_panel(
            "Data",
            fileInput("file", "Upload CSV file",
                      accept = c(".csv", "text/csv"),
                      width  = "100%"),
            actionButton("load_example", "Load built-in example",
                         class = "btn-outline-secondary btn-sm",
                         width = "100%"),
            helpText("Any tidy CSV with three or more numeric columns works.")
          ),
          accordion_panel(
            "Variables",
            uiOutput("predictor_ui"),
            uiOutput("moderator_ui"),
            uiOutput("outcome_ui")
          ),
          accordion_panel(
            "Display",
            checkboxInput("show_points",  "Show observed data points", TRUE),
            checkboxInput("show_reference_lines", "Show reference lines", TRUE)
          ),
          accordion_panel(
            "Coefficient adjustments",
            radioButtons(
              "simulation_mode",
              "Simulation mode",
              choices = c(
                "Visual only" = "visual",
                "Refit simulated regression" = "refit"
              ),
              selected = "visual"
            ),
            sliderInput("error_sd", "Random error / noise",
                        min = 0, max = 3, value = 0.9, step = 0.1),
            div(class = "focal-tip", textOutput("resid_sd_hint", inline = TRUE)),
            numericInput("sim_seed", "Simulation seed", value = 42, min = 1),
            sliderInput("adjust_b_iv", "Predictor coefficient adjustment (Delta b1)",
                        min = -1.5, max = 1.5, value = 0, step = 0.05),
            sliderInput("adjust_b_mod", "Moderator coefficient adjustment (Delta b2)",
                        min = -1.5, max = 1.5, value = 0, step = 0.05),
            sliderInput("adjust_b_int", "Interaction coefficient adjustment (Delta b3)",
                        min = -1.5, max = 1.5, value = 0, step = 0.05),
            actionButton("reset_adjustments",
                         HTML("&#x21BA; Reset coefficient adjustments"),
                         class = "btn-reset-adj",
                         width = "100%"),
            div(class = "focal-tip",
                "Adjustments change the plotted prediction equation only; model tables use the fitted coefficients.")
          )
        )
      ),
      # ---- Primary interaction plot -------------------------------------
      # Put the visualization first and size it against the viewport so the
      # fitted lines are usable without opening the full-screen card.
      card(
        class = "plot-card",
        full_screen = TRUE,
        card_header("Interaction plot"),
        uiOutput("mode_note"),
        plotOutput("moderation_plot", height = "calc(100vh - 260px)")
      ),
      div(
        class = "metric-strip",
        metric_box("Rows used", "n_rows"),
        metric_box("R²", "r2_text",
                   tip = "Variance explained by the model whose statistics are shown (original fit in Visual mode, refit in Refit mode)."),
        metric_box("Interaction b (z·z)", "interaction_b",
                   tip = "OLS coefficient on standardized X × M. This is the coefficient on a product of z-scores, not a standardized interaction effect in the Aiken–West / Friedrich sense."),
        metric_box("Interaction p-value", "interaction_p"),
        metric_box("Current IV slope", "current_slope_text")
      )
    )
  ),

  nav_panel(
    "Results",
    navset_card_tab(
      nav_panel(
        "Interpretation",
        div(class = "interp-card", uiOutput("interpretation"))
      ),
      nav_panel(
        "Model summary",
        tableOutput("coef_table"),
        tags$hr(),
        h6("Simple slopes of X at moderator levels"),
        tableOutput("slope_table")
      )
    )
  ),

  nav_panel(
    "How to use",
    card(
      card_header("Quick start"),
      markdown(paste(
        "**1.** Upload any CSV with at least three numeric columns, or click *Load built-in example*.",
        "**2.** Pick a **Predictor (X)**, **Outcome (Y)**, and **Moderator (M)**.",
        "**3.** Use **Selected Moderator Value** to move the highlighted conditional prediction line.",
        "**4.** The model `Y ~ X + M + X:M` refits automatically on standardized variables.",
        "**5.** Use **Visual only** mode to change the plotted equation without changing the statistics, or **Refit simulated regression** mode to generate a new outcome and refit the model.",
        sep = "\n\n"
      ))
    ),
    card(
      card_header("Performance notes"),
      markdown(
        "The selected moderator slider redraws only the highlighted line
while dragging. The fitted model plus scatter context are cached and reused,
so the teaching controls should feel smooth even on larger CSVs."
      )
    )
  )
)


# ---- 4. Server --------------------------------------------------------------

server <- function(input, output, session) {

  # ---- 4.2 Raw data ------------------------------------------------------
  raw_data <- reactiveVal(NULL)

  observeEvent(input$file, {
    req(input$file)
    df <- tryCatch(
      readr::read_csv(input$file$datapath, show_col_types = FALSE),
      error = function(e) NULL
    )
    validate(need(!is.null(df), "Could not read that CSV -- check the file format."))
    raw_data(df)
  })

  observeEvent(input$load_example, {
    raw_data(example_data())
  })

  observe({
    if (is.null(raw_data())) raw_data(example_data())
  })


  # ---- 4.3 Numeric variable choices --------------------------------------
  numeric_choices <- reactive({
    df <- raw_data()
    req(df)
    choices <- numeric_cols(df)
    validate(need(length(choices) >= 3,
                  "Need at least three numeric columns to build a moderation model."))
    choices
  })


  # ---- 4.4 Variable selectors + focal sliders ----------------------------
  output$predictor_ui <- renderUI({
    choices <- numeric_choices()
    sel <- default_choice(
      choices,
      c("Predictor_X", "BusPlanning", "Communication", "DiscFrequency",
        "WFC", "Intelligence", "X"),
      1
    )
    tagList(
      selectInput("predictor", "Predictor (X)", choices = choices, selected = sel),
      sliderInput("selected_x", "Evaluate predictions at this X location (z-score units)",
                  min = -3, max = 3, value = 0, step = 0.1, ticks = TRUE),
      div(class = "focal-tip",
          "Vertical guide line on the plot at this X value")
    )
  })
  output$outcome_ui <- renderUI({
    choices <- numeric_choices()
    sel <- default_choice(
      choices,
      c("Outcome_Y", "MoneyGrowth", "Performance", "InvestLikelihood",
        "PhysHealth", "Y"),
      length(choices)
    )
    selectInput("outcome", "Outcome (Y)", choices = choices, selected = sel)
  })
  
  output$moderator_ui <- renderUI({
    choices <- numeric_choices()
    sel <- default_choice(
      choices,
      c("Moderator_M", "Proactive", "Trust", "Credibility",
        "Resilience", "Conscientiousness", "M"),
      2
    )
    tagList(
      selectInput("moderator", "Moderator (M)", choices = choices, selected = sel),
      sliderInput("selected_m", "Selected Moderator Value: Low ← Mean → High (SD units)",
                  min = -3, max = 3, value = 0, step = 0.1, ticks = TRUE),
      div(class = "focal-tip", "-1 SD = low | 0 = mean | +1 SD = high")
    )
  })

  # ---- 4.4b Teaching-control reactives ----------------------------------
  selected_m_d    <- reactive(input$selected_m)
  selected_x_d    <- reactive(input$selected_x)
  adjust_b_iv_d   <- reactive(input$adjust_b_iv)
  adjust_b_mod_d  <- reactive(input$adjust_b_mod)
  adjust_b_int_d  <- reactive(input$adjust_b_int)

  observeEvent(input$reset_adjustments, {
    updateSliderInput(session, "adjust_b_iv",  value = 0)
    updateSliderInput(session, "adjust_b_mod", value = 0)
    updateSliderInput(session, "adjust_b_int", value = 0)
  })
  
  
  # ---- 4.5 Analysis-ready (standardized) data frame ---------------------
  model_df <- reactive({
    req(input$predictor, input$outcome, input$moderator)
    df_raw <- raw_data()
    req(df_raw)

    # When a new CSV loads, the variable dropdowns rebuild but their input
    # values can briefly still point at the previous dataset's columns. Wait
    # until the selected names exist in the current data before subsetting,
    # otherwise all_of() errors on the stale column names ("first load" crash).
    req(all(c(input$predictor, input$outcome, input$moderator) %in% names(df_raw)))

    validate(
      need(length(unique(c(input$predictor, input$outcome, input$moderator))) == 3,
           "Choose three different variables for X, Y, and M.")
    )

    df <- df_raw |>
      dplyr::select(all_of(c(input$predictor, input$outcome, input$moderator))) |>
      rename(X = !!input$predictor, Y = !!input$outcome, M = !!input$moderator) |>
      mutate(across(everything(), as.numeric)) |>
      drop_na()

    validate(
      need(nrow(df) >= 10, "Need at least 10 complete-case rows."),
      need(sd(df$X) > 0,   "Predictor has no variation."),
      need(sd(df$M) > 0,   "Moderator has no variation."),
      need(sd(df$Y) > 0,   "Outcome has no variation.")
    )

    df |> mutate(across(everything(), standardize))
  })


  # ---- 4.6 Fit the moderation model -------------------------------------
  original_fit <- reactive({
    lm(Y ~ X * M, data = model_df())
  })

  display_coef <- reactive({
    req(input$adjust_b_iv, input$adjust_b_mod, input$adjust_b_int)
    cf <- coef(original_fit())
    cf["X"]   <- cf["X"]   + adjust_b_iv_d()
    cf["M"]   <- cf["M"]   + adjust_b_mod_d()
    cf["X:M"] <- cf["X:M"] + adjust_b_int_d()
    cf
  })

  simulated_df <- reactive({
    req(input$error_sd, input$sim_seed)
    validate(
      need(!is.na(input$error_sd) && input$error_sd >= 0,
           "Random error / noise must be zero or greater."),
      need(!is.na(input$sim_seed), "Simulation seed is required.")
    )

    df <- model_df()
    cf <- display_coef()
    y_hat <- cf["(Intercept)"] +
      cf["X"] * df$X +
      cf["M"] * df$M +
      cf["X:M"] * df$X * df$M

    set.seed(input$sim_seed)
    # Burn a small prefix so the built-in example seed does not reuse X as noise.
    sim_error <- tail(rnorm(nrow(df) + 1000, mean = 0, sd = input$error_sd), nrow(df))
    df$Y_sim <- y_hat + sim_error
    df
  })

  simulated_fit <- reactive({
    lm(Y_sim ~ X * M, data = simulated_df())
  })

  active_fit <- reactive({
    req(input$simulation_mode)
    if (input$simulation_mode == "refit") {
      simulated_fit()
    } else {
      original_fit()
    }
  })

  plot_df <- reactive({
    if (isTRUE(input$simulation_mode == "refit")) {
      simulated_df() |> mutate(Y = Y_sim)
    } else {
      model_df()
    }
  })


  # ---- 4.7 Tidy summaries -----------------------------------------------
  fit_tidy   <- reactive(broom::tidy(active_fit(), conf.int = TRUE))
  fit_glance <- reactive(broom::glance(active_fit()))

  interaction_row <- reactive({
    fit_tidy() |> filter(term == "X:M") |> slice(1)
  })


  # ---- 4.8 Simple slopes ------------------------------------------------
  slopes_tbl <- reactive({
    m   <- active_fit()
    cf  <- coef(m)
    V   <- vcov(m)
    df_resid <- m$df.residual

    vals   <- c(-1, 0, 1)
    labels <- c("Low (-1 SD)", "Mean (0 SD)", "High (+1 SD)")

    purrr::map_dfr(seq_along(vals), function(i) {
      v <- vals[i]
      est <- unname(cf["X"] + cf["X:M"] * v)
      se  <- sqrt(V["X", "X"] + v^2 * V["X:M", "X:M"] + 2 * v * V["X", "X:M"])
      tst <- est / se
      pv  <- 2 * pt(abs(tst), df = df_resid, lower.tail = FALSE)
      tibble(
        Level   = labels[i],
        M_value = v,
        Slope   = est,
        SE      = se,
        CI_low  = est - qt(.975, df = df_resid) * se,
        CI_high = est + qt(.975, df = df_resid) * se,
        t       = tst,
        p       = pv
      )
    })
  })


  # ---- 4.9 Prediction grids ---------------------------------------------
  x_values <- reactive({
    df <- plot_df()
    seq(min(df$X), max(df$X), length.out = 120)
  })

  reference_lines_df <- reactive({
    cf <- display_coef()
    grid <- expand_grid(X = x_values(), M = c(-1, 0, 1))
    grid$fit <- predict_moderation(cf, grid$X, grid$M)
    grid$Level <- factor(
      grid$M,
      levels = c(-1, 0, 1),
      labels = c("Moderator = Mean - 1 SD", "Moderator = Mean", "Moderator = Mean + 1 SD")
    )
    grid
  })

  active_line_df <- reactive({
    cf <- display_coef()
    m_selected <- selected_m_d()
    req(m_selected)
    tibble(
      X = x_values(),
      M = m_selected,
      fit = predict_moderation(cf, x_values(), m_selected),
      Level = "Selected moderator value"
    )
  })
  
  focal_point <- reactive({
    cf <- display_coef()
    x_sel <- selected_x_d()
    m_sel <- selected_m_d()
    req(!is.null(x_sel), !is.null(m_sel))
    tibble(
      X   = x_sel,
      M   = m_sel,
      fit = predict_moderation(cf, x_sel, m_sel)
    )
  })
  

  current_slope <- reactive({
    cf <- display_coef()
    req(selected_m_d())
    unname(cf["X"] + cf["X:M"] * selected_m_d())
  })


  # ---- 4.10 Base plot (cached; independent of selected moderator) --------
  # This is the expensive part: scatter, reference lines, theme.
  # It only re-evaluates when the data, variables, or display toggles
  # change -- NOT when the user drags a slider.
  base_plot <- reactive({
    df   <- plot_df()
    grid <- reference_lines_df()
    base <- 15
    y_axis_label <- if (isTRUE(input$simulation_mode == "refit")) {
      sprintf("%s (simulated outcome)", input$outcome)
    } else {
      sprintf("%s (z)", input$outcome)
    }

    p <- ggplot()

    if (isTRUE(input$show_points)) {
      p <- p + geom_point(
        data = df, aes(X, Y), inherit.aes = FALSE,
        alpha = 0.32, color = "grey45"
      )
    }

    if (isTRUE(input$show_reference_lines)) {
      p <- p +
        geom_line(
          data = grid,
          aes(X, fit, color = Level, linetype = Level),
          linewidth = 0.85,
          alpha = 0.55
        )
    }

    p +
      scale_color_manual(values = moderator_palette, drop = FALSE) +
      scale_linetype_manual(
        values = c(
          "Moderator = Mean - 1 SD" = "dashed",
          "Moderator = Mean" = "solid",
          "Moderator = Mean + 1 SD" = "dashed"
        ),
        drop = FALSE
      ) +
      labs(
        title    = sprintf("Moderation: %s → %s by %s",
                           input$predictor, input$outcome, input$moderator),
        subtitle = "Reference lines stay fixed while the selected moderator line updates",
        x        = sprintf("%s (z)", input$predictor),
        y        = y_axis_label,
        color    = sprintf("%s reference", input$moderator),
        linetype = sprintf("%s reference", input$moderator)
      ) +
      plot_theme(base = base)
  })

  output$mode_note <- renderUI({
    if (isTRUE(input$simulation_mode == "refit")) {
      div(
        class = "adj-banner",
        "Regression simulator mode: adjusted coefficients generate a new outcome, the model is refit, and R², p-values, standard errors, and Interaction β update."
      )
    } else {
      div(
        class = "adj-banner",
        "Visual mode: sliders change only the plotted prediction equation. Model statistics remain from the original fitted regression."
      )
    }
  })


  # ---- 4.10b Final plot = cached base + lightweight active line ----------
  output$moderation_plot <- renderPlot({
    p <- base_plot()
    active <- active_line_df()
    fp <- focal_point()
    cf <- display_coef()
    slope <- current_slope()
    m_selected <- selected_m_d()
    x_selected <- selected_x_d()
    b1_disp <- unname(cf["X"])
    b3_disp <- unname(cf["X:M"])
    op_disp <- if (b3_disp < 0) "-" else "+"

    p +
      geom_vline(
        xintercept = c(-1, 0, 1),
        linetype = "dashed",
        color = "grey70",
        linewidth = 0.5,
        alpha = 0.6
      ) +
      geom_line(
        data = active,
        aes(X, fit),
        inherit.aes = FALSE,
        color = active_line_color,
        linewidth = 2.1
      ) +
      geom_vline(
        xintercept = x_selected,
        linetype   = "dotted",
        color      = active_line_color,
        linewidth  = 1.0,
        alpha      = 0.55
      ) +
      geom_point(
        data = fp,
        aes(X, fit),
        inherit.aes = FALSE,
        color  = active_line_color,
        fill   = "white",
        size   = 4.8,
        shape  = 21,
        stroke = 1.5
      ) +
      annotate(
        "label",
        x = max(active$X),
        y = active$fit[which.max(active$X)],
        label = sprintf(
          "M = %+.1f SD\nSlope = %+.3f\n%s",
          m_selected, slope, slope_label(slope)
        ),
        hjust = 1.02,
        vjust = -0.45,
        size = 15 * 0.25,
        color = active_line_color,
        fill = "white",
        linewidth = 0.25
      ) +
      labs(
        subtitle = sprintf(
          "Current IV slope = b1 + b3·M = %.3f %s %.3f·(%.1f) = %.3f   (%s)",
          b1_disp, op_disp, abs(b3_disp), m_selected, slope, slope_label(slope)
        )
      )
  }, res = 96)


  # ---- 4.11 KPI value boxes ---------------------------------------------
  output$n_rows        <- renderText(format(nrow(model_df()), big.mark = ","))
  output$r2_text       <- renderText(sprintf("%.3f", fit_glance()$r.squared))
  output$interaction_b <- renderText({
    ir <- interaction_row()
    sprintf("%+.3f%s", ir$estimate, sig_stars(ir$p.value))
  })
  output$interaction_p <- renderText(fmt_p(interaction_row()$p.value))
  output$current_slope_text <- renderText({
    s <- current_slope()
    sprintf("%.3f — %s", s, slope_label(s))
  })

  output$resid_sd_hint <- renderText({
    sprintf("Original model residual SD ≈ %.2f. Set noise near this to keep Refit comparable to the real data.",
            sigma(original_fit()))
  })

  # ---- 4.13 Coefficient table -------------------------------------------
  output$coef_table <- renderTable({
    td <- fit_tidy()
    td |>
      mutate(
        Term = dplyr::recode(term,
          "(Intercept)" = "Intercept",
          "X"   = sprintf("%s  (X)",   input$predictor),
          "M"   = sprintf("%s  (M)",   input$moderator),
          "X:M" = sprintf("%s × %s", input$predictor, input$moderator)
        ),
        Estimate     = sprintf("%+.3f", estimate),
        `Std. Error` = sprintf("%.3f", std.error),
        `95% CI`     = sprintf("[%+.2f, %+.2f]", conf.low, conf.high),
        t            = sprintf("%+.2f", statistic),
        p            = vapply(p.value, fmt_p,     character(1)),
        Sig          = vapply(p.value, sig_stars, character(1))
      ) |>
      dplyr::select(Term, Estimate, `Std. Error`, `95% CI`, t, p, Sig)
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%")


  # ---- 4.13 Simple-slopes table -----------------------------------------
  output$slope_table <- renderTable({
    st <- slopes_tbl()
    tibble(
      `Moderator level` = st$Level,
      `Slope of X`      = sprintf("%+.3f", st$Slope),
      SE                = sprintf("%.3f",  st$SE),
      `95% CI`          = sprintf("[%+.2f, %+.2f]", st$CI_low, st$CI_high),
      t                 = sprintf("%+.2f", st$t),
      p                 = vapply(st$p, fmt_p,     character(1)),
      Sig               = vapply(st$p, sig_stars, character(1))
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%")


  # ---- 4.14 Plain-language interpretation -------------------------------
  output$interpretation <- renderUI({
    td <- fit_tidy()
    st <- slopes_tbl()
    gl <- fit_glance()
    int <- td |> filter(term == "X:M") |> slice(1)

    # The Interpretation tab describes the model whose statistics are shown in
    # the tables/KPIs (active_fit): the original fit in Visual mode, the refit
    # in Refit mode. The plot itself uses display_coef (the adjusted equation),
    # so in Visual mode with non-zero adjustments the drawn line can differ from
    # these numbers -- the banner below flags that.
    cf <- coef(active_fit())
    m_selected <- selected_m_d()
    selected_slope <- unname(cf["X"] + cf["X:M"] * m_selected)
    low_slope  <- unname(cf["X"] + cf["X:M"] * -1)
    high_slope <- unname(cf["X"] + cf["X:M"] * 1)

    x_cross <- if (isTRUE(all.equal(unname(cf["X:M"]), 0))) {
      NA_real_
    } else {
      xc  <- unname(-cf["M"] / cf["X:M"])
      rng <- range(x_values())
      if (is.finite(xc) && xc >= rng[1] && xc <= rng[2]) xc else NA_real_
    }

    adj         <- c(adjust_b_iv_d(), adjust_b_mod_d(), adjust_b_int_d())
    adj_active  <- any(abs(adj) > 1e-8)
    visual_mode <- !isTRUE(input$simulation_mode == "refit")

    int_b <- int$estimate
    int_p <- int$p.value

    exists_text <- if (!is.na(int_p) && int_p < .05) {
      sprintf("Yes, the interaction is statistically significant (β = %+.3f, p = %s%s).",
              int_b, fmt_p(int_p), sig_stars(int_p))
    } else {
      sprintf("Not at conventional levels, the interaction is not significant (β = %+.3f, p = %s).",
              int_b, fmt_p(int_p))
    }

    direction_text <- if (is.na(int_b)) {
      "The interaction coefficient could not be estimated."
    } else if (int_b > 0) {
      sprintf("The X → Y relationship grows <b>more positive</b> as %s increases (positive interaction).",
              htmltools::htmlEscape(input$moderator))
    } else if (int_b < 0) {
      sprintf("The X → Y relationship grows <b>less positive (or more negative)</b> as %s increases (negative interaction).",
              htmltools::htmlEscape(input$moderator))
    } else {
      "The interaction has no observed direction."
    }

    mod_e <- htmltools::htmlEscape(input$moderator)
    prd_e <- htmltools::htmlEscape(input$predictor)
    out_e <- htmltools::htmlEscape(input$outcome)

    strength_text <- if (sign(low_slope) * sign(high_slope) < 0) {
      # Slopes have opposite signs across the moderator range.
      sprintf(
        "The %s to %s slope <b>changes sign</b> across the moderator range (%+.2f at low %s, %+.2f at high %s), so this is a <b>disordinal (crossover) interaction</b>: %s reverses the direction of the relationship, not just its strength.",
        prd_e, out_e, low_slope, mod_e, high_slope, mod_e, mod_e
      )
    } else if (isTRUE(all.equal(abs(high_slope), abs(low_slope)))) {
      "Across the reference range, the slope is about equally steep at low and high moderator values."
    } else if (abs(high_slope) > abs(low_slope)) {
      sprintf(
        "As %s increases, the slope gets steeper in the same direction, so the moderator <b>magnifies (strengthens)</b> the %s to %s relationship.",
        mod_e, prd_e, out_e
      )
    } else {
      sprintf(
        "As %s increases, the slope flattens toward zero, so the moderator <b>buffers (weakens)</b> the %s to %s relationship.",
        mod_e, prd_e, out_e
      )
    }

    crossover_text <- if (is.na(x_cross)) {
      "No crossover is implied inside the observed predictor range."
    } else {
      sprintf(
        "The displayed lines cross near %s = %+.2f SD, which suggests a possible crossover interaction.",
        htmltools::htmlEscape(input$predictor),
        x_cross
      )
    }

    describe_slope <- function(s, p, level_name) {
      sign_word <- if (s > 0) "positive" else if (s < 0) "negative" else "flat"
      sig_word  <- if (!is.na(p) && p < .05) "statistically significant"
                   else "not statistically significant"
      HTML(sprintf(
        "At <b>%s</b>, the slope of %s on %s is <b>%s</b> (β = %+.2f) and %s (p = %s).",
        level_name,
        htmltools::htmlEscape(input$predictor),
        htmltools::htmlEscape(input$outcome),
        sign_word, s, sig_word, fmt_p(p)
      ))
    }

    adj_banner <- if (visual_mode && adj_active) {
      div(
        class = "adj-banner",
        sprintf(
          "Heads up: the plotted line reflects your coefficient adjustments (Δb1 = %+.2f, Δb2 = %+.2f, Δb3 = %+.2f), but the statistics below come from the original fitted model. Switch to “Refit simulated regression” to make the statistics respond.",
          adj[1], adj[2], adj[3]
        )
      )
    } else NULL

    tagList(
      adj_banner,

      h6("Does moderation exist?"),
      p(exists_text),

      h6("How does the interaction reshape X → Y?"),
      p(HTML(direction_text)),

      h6("Slope at the selected moderator value"),
      p(sprintf(
        "At the selected moderator value (%+.1f SD), the model's slope of X on Y is %+.3f.",
        m_selected, selected_slope
      )),
      p(HTML(strength_text)),
      p(HTML(crossover_text)),

      h6("Slopes at low vs. high moderator"),
      tags$ul(
        tags$li(describe_slope(st$Slope[1], st$p[1], "Low moderator (−1 SD)")),
        tags$li(describe_slope(st$Slope[2], st$p[2], "Mean moderator (0 SD)")),
        tags$li(describe_slope(st$Slope[3], st$p[3], "High moderator (+1 SD)"))
      ),

      tags$hr(),
      tags$small(sprintf(
        "Model fit: R² = %.3f · Adjusted R² = %.3f · n = %d",
        gl$r.squared, gl$adj.r.squared, gl$nobs
      ))
    )
  })
}


# ---- 5. Launch --------------------------------------------------------------

shinyApp(ui, server)

# =============================================================================
# Maintenance LLM Tool — ROI Dashboard
# =============================================================================

library(shiny)
library(tidyverse)
library(scales)

# ── Load model objects ───────────────────────────────────────────────────────

loaded <- file.exists("bayesian_did_objects.rds")

if (loaded) {
  obj       <- readRDS("bayesian_did_objects.rds")
  att       <- obj$att
  roi       <- obj$roi
  wo        <- obj$wo
  tool_cost <- obj$tool_cost
}

# ── Theme ────────────────────────────────────────────────────────────────────

bg      <- "#0F172A"
surface <- "#1E293B"
border  <- "#334155"
txt     <- "#F1F5F9"
muted   <- "#94A3B8"
blue    <- "#3B82F6"
green   <- "#10B981"
amber   <- "#F59E0B"
red     <- "#EF4444"

card_css <- paste0(
  "background:", surface, "; border:1px solid ", border, ";",
  "border-radius:12px; padding:24px;"
)

kpi <- function(label, value, sub, color = txt) {
  div(style = card_css,
    div(style = paste0("font-size:11px; font-weight:700; letter-spacing:0.1em;
                        text-transform:uppercase; color:", muted, ";
                        margin-bottom:10px;"), label),
    div(style = paste0("font-size:2rem; font-weight:700; color:", color, ";"),
        value),
    div(style = paste0("font-size:13px; color:", muted, "; margin-top:6px;"),
        sub)
  )
}

dark_theme <- function() {
  theme_minimal(base_size = 12) +
  theme(
    plot.background  = element_rect(fill = surface, color = NA),
    panel.background = element_rect(fill = surface, color = NA),
    panel.grid.major = element_line(color = border, linewidth = 0.4),
    panel.grid.minor = element_blank(),
    text       = element_text(color = txt),
    axis.text  = element_text(color = muted, size = 10),
    axis.title = element_text(color = muted, size = 11),
    legend.position = "none"
  )
}

# ── UI ───────────────────────────────────────────────────────────────────────

ui <- fluidPage(
  style = paste0("background:", bg, "; min-height:100vh;
                  font-family:'Inter',sans-serif; padding:36px 40px;"),
  tags$head(
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;600;700&display=swap",
      rel  = "stylesheet"
    )
  ),

  # Header
  div(style = "margin-bottom:28px;",
    div(style = paste0("font-size:11px; font-weight:700; letter-spacing:0.12em;
                        text-transform:uppercase; color:", blue, ";
                        margin-bottom:6px;"),
        "Maintenance Operations · 2026"),
    div(style = paste0("font-size:1.8rem; font-weight:700; color:", txt, ";"),
        "LLM Work-Order Tool — ROI Dashboard"),
    div(style = paste0("font-size:13px; color:", muted, "; margin-top:4px;"),
        if (loaded)
          paste0(comma(nrow(wo |> filter(did == 1))),
                 " treated work orders · ",
                 n_distinct(wo$facility), " facilities · ",
                 "tool investment: $", comma(tool_cost))
        else "Knit the Rmd first to generate model results."
    )
  ),

  if (!loaded)
    div(style = paste0(card_css, " text-align:center; padding:60px;"),
        div(style = paste0("color:", muted, ";"),
            "No model file found. Knit bayesian_did_roi.Rmd first."))
  else tagList(

    # KPI row
    div(style = "display:grid; grid-template-columns:repeat(4,1fr);
                  gap:16px; margin-bottom:24px;",
      kpi("ROI",
          paste0(round(median(roi$roi_pct), 0), "%"),
          paste0("95%: ",
                 round(quantile(roi$roi_pct, .025), 0), "% to ",
                 round(quantile(roi$roi_pct, .975), 0), "%"),
          blue),
      kpi("Net Savings",
          paste0("$", comma(round(median(roi$net)))),
          paste0(round(mean(roi$roi_pct > 0) * 100, 0),
                 "% probability positive"),
          green),
      kpi("Overrun Reduction",
          paste0(abs(round(median(att$pct), 1)), "%"),
          "Causal effect on cost overruns",
          amber),
      kpi("Tool Investment",
          paste0("$", comma(tool_cost)),
          "License + setup + training + API",
          muted)
    ),

    # Charts
    div(style = "display:grid; grid-template-columns:1fr 1fr;
                  gap:16px;",
      div(style = card_css,
        div(style = paste0("font-size:11px; font-weight:700;
                            letter-spacing:0.1em; text-transform:uppercase;
                            color:", muted, "; margin-bottom:12px;"),
            "ROI Distribution"),
        plotOutput("p_roi", height = "280px")
      ),
      div(style = card_css,
        div(style = paste0("font-size:11px; font-weight:700;
                            letter-spacing:0.1em; text-transform:uppercase;
                            color:", muted, "; margin-bottom:12px;"),
            "Treatment Effect (Cost Overrun %)"),
        plotOutput("p_att", height = "280px")
      )
    )
  )
)

# ── Server ───────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  output$p_roi <- renderPlot({
    ggplot(roi, aes(roi_pct)) +
      geom_histogram(bins = 55, fill = blue, alpha = 0.85, color = NA) +
      geom_vline(xintercept = 0, color = red, linetype = "dashed", linewidth = 0.9) +
      geom_vline(xintercept = median(roi$roi_pct), color = amber, linewidth = 1.2) +
      annotate("rect",
        xmin = quantile(roi$roi_pct, .025),
        xmax = quantile(roi$roi_pct, .975),
        ymin = -Inf, ymax = Inf, alpha = 0.08, fill = blue) +
      annotate("text",
        x = median(roi$roi_pct), y = Inf, vjust = 2.2, hjust = -0.15,
        size = 4, color = amber,
        label = paste0("Median: ", round(median(roi$roi_pct), 0), "%")) +
      scale_x_continuous(labels = label_percent(scale = 1)) +
      labs(x = "Return on Investment", y = NULL) +
      dark_theme()
  }, bg = surface)

  output$p_att <- renderPlot({
    ggplot(att, aes(pct)) +
      geom_histogram(bins = 55, fill = green, alpha = 0.85, color = NA) +
      geom_vline(xintercept = 0, color = red, linetype = "dashed", linewidth = 0.9) +
      geom_vline(xintercept = median(att$pct), color = amber, linewidth = 1.1) +
      annotate("rect",
        xmin = quantile(att$pct, .025), xmax = quantile(att$pct, .975),
        ymin = -Inf, ymax = Inf, alpha = 0.08, fill = green) +
      annotate("text",
        x = median(att$pct), y = Inf, vjust = 2.2, hjust = -0.15,
        size = 4, color = amber,
        label = paste0("Median: ", round(median(att$pct), 1), "%")) +
      scale_x_continuous(labels = label_percent(scale = 1)) +
      labs(x = "% Change in Cost Overrun", y = NULL) +
      dark_theme()
  }, bg = surface)
}

shinyApp(ui, server)

library(dash)
library(dashBootstrapComponents)
library(ggplot2)
library(dplyr)
library(plotly)
library(purrr)

qwl_df <- readr::read_csv("./data/bei_vita_qwl_assessment.csv")
qwl_df$residence <- qwl_df$`Country of Residence`
residence_list <- unique(qwl_df[c("residence")])$residence

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

app$layout(
    dbcContainer(
        list(
          dccDropdown(
            id="col-select",
            options = residence_list %>% purrr::map(function(col) list(label = col, value = col)),
            value = "HK & Macau"
          ),
          dccGraph(id="hbarplot")
        )
    )
)


app$callback(
  output("hbarplot", "figure"),
  list(input("col-select", "value")),
  function(xcol) {
    col_name = "17. I experience MEANINGFULNESS at work ... (e.g. inspired, trusted, respected, purpose, seen and heard, acknowledged, fulfilled, growth, contribution to something greater, etc.)"
    
    plot_data <- qwl_df |>
      rename(analysis_col = col_name) |>
      filter(residence == "Japan") |>
      count(analysis_col)
    
    p <- ggplot(data=plot_data, aes(x=analysis_col)) +
      geom_bar() +
      geom_text(stat='count', aes(label=..count..)) +
      ggtitle("How healthy are the employees feeling overall?") +
      ggthemes::scale_color_tableau()

    ggplotly(p)
  }
)


app$run_server(host = '0.0.0.0')
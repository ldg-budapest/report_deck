library(mschart)

automatic_tick_spaces <- mschart:::axis_content_xml
monthly_tick_spaces <- function(...) {
  passed_args <- list(...)
  axis_string <- do.call(automatic_tick_spaces, passed_args)
  if (passed_args$is_x) {
    axis_string <- paste(
      axis_string,
      '<c:tickLblSkip val="12\"/>',
      sep=""
    )
  }
  axis_string
}

native_ms_simple_line <- function(
    input_data, x, y, group, labels=NULL, xlab="", ylab="", title=""
) {
  input_data %>%
    ms_linechart(
      x=x, y=y, group=group, labels=labels
    ) %>%
    chart_settings(scatterstyle = "marker") %>%
    chart_ax_y(limit_min = 0) %>%
    chart_data_labels(position = "t") %>%
    chart_labels_text(fp_text(font.size = 8)) %>%
    chart_labels(xlab=xlab, ylab=ylab, title=title) %>%
    chart_theme(
      axis_title_y = fp_text(font.size = 16),
      main_title = fp_text(font.size = 24)
    )
}

native_ms_survival_line <- function(
    input_data, x, y, group, labels=NULL, xlab="", ylab="", title=""
) {
  input_data %>%
    ms_linechart(
      x=x, y=y, group=group, labels=labels
    ) %>%
    chart_settings(style="line") %>%
    chart_ax_x(limit_min = 0, limit_max = 72) %>%
    chart_ax_y(limit_min = 0, limit_max = 1, num_fmt = "0%%") %>%
    chart_data_labels(position = "t") %>%
    chart_labels_text(fp_text(font.size = 8)) %>%
    chart_labels(xlab=xlab, ylab=ylab, title=title) %>%
    chart_theme(
      axis_title_x = fp_text(font.size = 16),
      axis_title_y = fp_text(font.size = 16),
      main_title = fp_text(font.size = 24)
    )
}

native_ms_survival_small <- function(
    input_data, x, y, group, labels=NULL, xlab="", ylab="", title=""
) {
  input_data %>%
    ms_linechart(
      x=x, y=y, group=group, labels=labels
    ) %>%
    chart_settings(style="line") %>%
    chart_ax_x(limit_min = 0, limit_max = 72) %>%
    chart_ax_y(limit_min = 0, limit_max = 1, num_fmt = "0%%") %>%
    chart_data_labels(position = "t") %>%
    chart_labels_text(fp_text(font.size = 8)) %>%
    chart_labels(xlab=xlab, ylab=ylab, title=title) %>%
    chart_theme(
      axis_title_x = fp_text(font.size = 10),
      axis_title_y = fp_text(font.size = 10),
      main_title = fp_text(font.size = 16)
    )
}

native_ms_small_bar <- function(
    input_data, x, y, group, labels=NULL, xlab="", ylab="", title=""
) {
  input_data %>%
    ms_barchart(
      x=x, y=y, group=group, labels=labels
    ) %>%
    chart_data_labels() %>%
    chart_labels_text(
      fp_text(bold = TRUE, font.size = 8)
    ) %>%
    chart_labels(xlab=xlab, ylab=ylab, title=title) %>%
    chart_settings(
      grouping = "clustered", table = TRUE
    ) %>%
    chart_theme(
      axis_title_y = fp_text(font.size = 10),
      main_title = fp_text(font.size = 12)
    )
}


native_ms_small_stacked_bar <- function(
    input_data, x, y, group, labels=NULL, xlab="", ylab="", title="", percent=FALSE
) {
  p <- input_data %>%
    ms_barchart(
      x=x, y=y, group=group, labels=labels
    ) %>%
    as_bar_stack(percent=percent) %>%
    chart_data_labels() %>%
    chart_labels_text(
      fp_text(bold = TRUE, font.size = 8)
    ) %>%
    chart_labels(xlab=xlab, ylab=ylab, title=title) %>%
    chart_settings(grouping = "percentStacked") %>%
    # as_bar_stack(percent=TRUE, dir="horizontal") %>%
    chart_theme(
      axis_title_y = fp_text(font.size = 10),
      main_title = fp_text(font.size = 12)
    )
  
  if(percent) p <- chart_ax_y(p, display=TRUE, num_fmt="0%%")
  
  p
}

native_ms_stacked_bar <- function(
    input_data, x, y, group, labels=NULL, xlab="", ylab="", title="", percent=FALSE
) {
  p <- input_data %>%
    ms_barchart(
      x=x, y=y, group=group, labels=labels
    ) %>%
    as_bar_stack(percent=percent) %>%
    chart_data_labels() %>%
    chart_labels_text(
      fp_text(bold = TRUE, font.size = 10)
    ) %>%
    chart_labels(xlab=xlab, ylab=ylab, title=title) %>%
    chart_settings(grouping = "percentStacked") %>%
    # as_bar_stack(percent=TRUE, dir="horizontal") %>%
    chart_theme(
      axis_title_x = fp_text(font.size = 12),
      axis_title_y = fp_text(font.size = 12),
      main_title = fp_text(font.size = 16)
    )
  
  if(percent) p <- chart_ax_y(p, display=TRUE, num_fmt="0%%")
  
  p
}

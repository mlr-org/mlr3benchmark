get_shaps <- function(model, task, data = NULL) {
  if (is.null(data)) {
    data = as.matrix(task$data(cols = task$feature_names))
  }

  p = predict(model, data,
              predcontrib = T, approxcontrib = F)
  p = p[,-ncol(p)]

  shaps = reshape2::melt(p)
  vars = reshape2::melt(data)
  relTruth = reshape2::melt(apply(data, 2, function(x)
    (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))))
  shap.frame = cbind(shaps, relTruth$value, vars$value)
  colnames(shap.frame) = c("id", "Var", "Shap", "relTruth","Truth")

  return(shap.frame)
}
get_interaction_shaps <- function(model, task, data = NULL) {
  if (is.null(data)) {
    data = as.matrix(task$data(cols = task$feature_names))
  }

  p = predict(model, data,
              predinteraction = TRUE)
  p = p[,colnames(data),colnames(data)]

  p = array(c(as.numeric(p), as.numeric(data)), dim = c(dim(p)[1:2], dim(p)[3] + 1),
            dimnames = list(NULL, colnames(data), c(colnames(data), "Truth")))

  return(p)
}


plot_shap_feature <- function(shaps, feature,
                              type = c("b", "p", "s"),
                              se = TRUE, xlab = feature,
                              smooth_color = "blue", smooth_fill = "lightblue",
                              smooth_lwd = 0.5, smooth_level = 0.95,
                              points_color = "black"){

  shaps = shaps[complete.cases(shaps), ]
  type = match.arg(type)
  if (type == "b") {
    ggplot2::ggplot(subset(shaps, Var == feature),
                    ggplot2::aes(x = Truth, y = Shap)) +
      ggplot2::geom_point(color = points_color) +
      ggplot2::geom_smooth(se = se, fill = smooth_fill, color = smooth_color,
                                                     level = smooth_level) +
      ggplot2::labs(x = xlab, y = "SHAP Value") +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        panel.grid = ggplot2::element_blank(),
        axis.line.x = ggplot2::element_line(),
        axis.line.y = ggplot2::element_line())
  } else if (type == "p") {
    ggplot2::ggplot(subset(shaps, Var == feature),
                    ggplot2::aes(x = Truth, y = Shap)) +
      ggplot2::geom_point(color = points_color) +
      ggplot2::labs(x = xlab, y = "SHAP Value") + theme_minimal() +
      ggplot2::theme(panel.grid = ggplot2::element_blank(),
                     axis.line.x = ggplot2::element_line(),
                     axis.line.y = ggplot2::element_line())
  } else {
    ggplot2::ggplot(subset(shaps, Var == feature),
                    ggplot2::aes(x = Truth, y = Shap)) +
      ggplot2::geom_smooth(se = se, lwd = smooth_lwd, fill = smooth_fill, color = smooth_color,
                  level = smooth_level) +
      ggplot2::labs(x = xlab, y = "SHAP Value") +
      ggplot2::theme_minimal() +
      ggplot2::theme(panel.grid = ggplot2::element_blank(),
                     axis.line.x = ggplot2::element_line(),
                     axis.line.y = ggplot2::element_line())
  }

}

plot_shap_summary <- function(shaps, low = "blue", high = "red",
                              jitter_w = 0, jitter_h = 0.2,
                              legend.h = 25, legend.w = 0.5,
                              show.mean = TRUE, mean.abs = TRUE, mean.round = 4,
                              show.legend = TRUE,
                              ord = NULL) {

  shaps = shaps[complete.cases(shaps), ]

  if (is.null(ord)) {
    ord = aggregate(Shap ~ Var, data = shaps, function(x) mean(abs(x)))
    shaps$Var = factor(shaps$Var, levels = as.character(ord[order(ord$Shap) ,1]))
  } else {
    shaps$Var = factor(shaps$Var, levels = ord)
  }

  shaps[shaps$id == "NA", c(1, 3:5)] = NA


  p = ggplot2::ggplot(shaps, ggplot2::aes(x = Shap, y = Var, colour = relTruth))  +
    ggplot2::geom_vline(xintercept = 0, colour = "darkgray") +
    ggplot2::geom_point() + ggplot2::geom_jitter(width = jitter_w, height = jitter_h) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank(),
          axis.line.x = ggplot2::element_line(),
          axis.text.y = ggplot2::element_text(size = 12),
          legend.text = ggplot2::element_text(size = 11),
          axis.text.x = ggplot2::element_text(size = 10)) +
    ggplot2::labs(x = "SHAP Value")

  if (show.legend) {
    p = p + ggplot2::scale_color_gradient(low = low, high = high,
                                 n.breaks = 2L,
                                 labels = c("Low", "High"),
                                 guide = ggplot2::guide_colorbar(barwidth = legend.w, barheight = legend.h,
                                                        title.position = "right",
                                                        title = "Feature Value",
                                                        title.theme = ggplot2::element_text(angle = 90,
                                                                                   hjust = 0.5)))
  } else {
    p = p + ggplot2::scale_color_gradient(low = low, high = high) +
      ggplot2::theme(legend.position = "none")
  }



  if (show.mean) {
    if (mean.abs) {
      shapmean = aggregate(Shap ~ Var, data = shaps, function(x) mean(abs(x)))[,2]
    } else {
      shapmean = aggregate(Shap ~ Var, data = shaps, mean)[,2]
    }

    p = p + ggplot2::geom_text(aes(x = x, y = y, colour = NULL),
                      data = data.frame(x = max(shaps$Shap) + 0.5, y = 1:length(shapmean)),
                      label = format(round(shapmean, mean.round), nsmall = mean.round))
  }

  p
}

plot_shap_importance <- function(shaps, relative = TRUE, bar_width = 0.4,
                                 low = "blue", high = "red", order = TRUE,
                                 legend.position = "none",
                                 legend.w = 0.5, legend.h = 10,
                                 legend_limit = NULL, xlim = NULL,
                                 show.mean = TRUE, mean.round = 4) {

  shaps = shaps[complete.cases(shaps), ]

  agg = cbind(aggregate(Shap ~ Var, data = shaps, function(x) mean(abs(x))),
              Mean = aggregate(Shap ~ Var, data = shaps, mean)[,2])

  if (relative) {
    agg$Shap = (agg$Shap/sum(agg$Shap))*100
  }

  if (order) {
    agg = agg[order(agg$Shap, decreasing = FALSE), ]
    agg$Var = factor(agg$Var, levels = agg$Var)
  }

  if (is.null(legend_limit)) {
    legend_limit = range(agg$Mean)
  }

  if (is.null(xlim)) {
    xlim = c(0, (max(agg$Shap) - (max(agg$Shap) %% 10)) + 10)
  }


  p = ggplot2::ggplot(agg,
                      ggplot2::aes(x = Shap, y = Var, fill = Mean)) +
    ggplot2::geom_bar(stat = "identity", width = bar_width) +
    ggplot2::scale_fill_gradient(low = low, high = high, n.breaks = 3,
                        limits = legend_limit,
                        guide = ggplot2::guide_colorbar(barwidth = legend.w, barheight = legend.h,
                                               title.position = "right",
                                               title = "Mean SHAP Value",
                                               title.theme = ggplot2::element_text(angle = 90,
                                                                          hjust = 0.5))) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank(),
                   axis.line.y = ggplot2::element_line(),
                   axis.text.y = ggplot2::element_text(size = 12),
          legend.text = ggplot2::element_text(size = 11),
          axis.text.x = ggplot2::element_text(size = 10),
          axis.ticks = ggplot2::element_line(),
          legend.position = legend.position) +
    ggplot2::xlim(xlim) +
    ggplot2::labs(x = "Feature Contribution (%)")

  if (show.mean) {
    p = p + ggplot2::geom_text(label = format(round(agg$Mean, mean.round), nsmall = mean.round),
                      hjust = -0.4)
  }

  p

}

plot_shap_sidebyside_importance <- function(shaps, bar_width = 0.4, ylim = NULL,
                                            legend.position = "top") {

  agg <- data.frame()
  for (i in seq_along(shaps)) {
    dat <- shaps[[i]][complete.cases(shaps[[i]]), ]
    dat[dat == "NA", c(1, 3:5)] <- NA
    shapi <- cbind(Data = names(shaps)[[i]],
                   aggregate(Shap ~ Var, data = dat, function(x) mean(abs(x)),
                             na.action = na.pass))
    shapi$Shap <- (shapi$Shap/sum(shapi$Shap, na.rm = TRUE)) * 100
    agg <- rbind(agg, shapi)
  }

  if (is.null(ylim)) {
    ylim = c(0, (max(agg$Shap, na.rm = TRUE) - (max(agg$Shap, na.rm = TRUE) %% 10)) + 10)
  }

  ord = order(agg[agg$Data == names(shaps)[[1]], "Shap"], decreasing = TRUE)
  agg$Var = factor(agg$Var, levels = levels(agg$Var)[ord])

  ggplot2::ggplot(agg,
                  ggplot2::aes(y = Shap, x = Var, fill = Data, group = Data)) +
    ggplot2::geom_bar(stat = "identity", width = bar_width, position = "dodge",
             color = "black") +
    ggplot2::geom_text(label = round(agg$Shap), position = ggplot2::position_dodge(bar_width),
              vjust = -1) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
          panel.grid = ggplot2::element_blank(),
          axis.line.y = ggplot2::element_line(),
          axis.text.x = ggplot2::element_text(size = 12),
          legend.text = ggplot2::element_text(size = 11),
          axis.text.y = ggplot2::element_text(size = 10),
          axis.ticks = ggplot2::element_line(),
          legend.title = ggplot2::element_blank(),
          legend.position = legend.position,
          legend.box = "horizontal") +
    ggplot2::ylim(ylim) +
    ggplot2::labs(y = "Feature Contribution (%)")

}

plot_shap_interaction <- function(shaps, shap_feature, intX_feature,
                                  intZ_feature = NULL, type = c("b", "p", "s"),
                                  se = TRUE, smooth_color = "blue", smooth_fill = "lightblue",
                                  smooth_lwd = 0.5, smooth_level = 0.95,
                                  low = "blue", high = "red") {

  type = match.arg(type)

  int_shaps = shaps[,intX_feature,shap_feature]
  Xtruth = shaps[,intX_feature,"Truth"]
  if (!is.null(intZ_feature)) {
    Ztruth = shaps[,intZ_feature,"Truth"]
  } else {
    Ztruth = Xtruth
  }
  data = data.frame(SHAPS = int_shaps, XTruth = Xtruth, ZTruth = Ztruth)

  p = ggplot2::ggplot(data, ggplot2::aes(x = XTruth, y = SHAPS, color = ZTruth)) +
    ggplot2::scale_color_gradient(low = low, high = high, n.breaks = 3,
                         guide = ggplot2::guide_colorbar(title = intZ_feature,
                                                title.position = "right",
                                                title.theme = ggplot2::element_text(angle = 90,
                                                                           hjust = 0.5))) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
          axis.line.y = ggplot2::element_line(),
          axis.line.x = ggplot2::element_line(),
          axis.ticks = ggplot2::element_line()) +
    ggplot2::labs(x = intX_feature,
         y = paste("SHAP Interaction Values\n", shap_feature, "X", intX_feature))

  if (type == "p" | type == "b") {
    p = p + ggplot2::geom_point()
  }
  if (type == "s" | type == "b") {
    p = p + ggplot2::geom_smooth(se = se, color = smooth_color, fill = smooth_fill,
                        lwd = smooth_lwd, level = smooth_level)
  }

  p

}

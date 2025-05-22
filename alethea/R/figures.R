# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.

# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.

#' Project's ggplot2 theme
#' @param base_size (numeric) Base size
#' @param base_family (character) Base family
#' @param base_line_size (numeric) Base line size
#' @param base_rect_size (numeric) Base rect size
#' @export

theme_alethea <- function(base_size = 16, base_family = "Helvetica",
                       base_line_size = base_size / 22,
                       base_rect_size = base_size / 22) {

  # The half-line (base-fontsize / 2) sets up the basic vertical
  # rhythm of the theme. Most margins will be set to this value.
  # However, when we work with relative sizes, we may want to multiply
  # `half_line` with the appropriate relative size. This applies in
  # particular for axis tick sizes. And also, for axis ticks and
  # axis titles, `half_size` is too large a distance, and we use `half_size/2`
  # instead.
  half_line <- base_size / 2

  # Set default palette to be colorblind friendly
  options(
      ggplot2.discrete.color = list(
          c("#FFC20A", "#0C7BDC"),
          c("#777777","#E69F00","#56B4E9","#009E73","F0E442","#0072B2",
            "#D55E00","#CC79A7","#DDDDDD")
      ),
      ggplot2.discrete.fill = list(
          c("#FFC20A", "#0C7BDC"),
          c("#777777","#E69F00","#56B4E9","#009E73","F0E442","#0072B2",
            "#D55E00","#CC79A7","#DDDDDD")
      )
  )

  # Set default figure size to be wider than it is long
  options(repr.plot.width=9, repr.plot.height=6)

  # Throughout the theme, we use three font sizes, `base_size` (`rel(1)`)
  # for normal, `rel(0.8)` for small, and `rel(1.2)` for large.

  t <- theme(
    # Elements in this first block aren't used directly, but are inherited
    # by others
    line =               element_line(
                           colour = "black", size = base_line_size,
                           linetype = 1, lineend = "butt"
                         ),
    rect =               element_rect(
                           fill = "white", colour = "black",
                           size = base_rect_size, linetype = 1
                         ),
    text =               element_text(
                            family = base_family, face = "plain",
                            colour = "black", size = base_size,
                            lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0,
                            margin = margin(), debug = FALSE
                         ),

    axis.line =          element_blank(),
    axis.line.x =        NULL,
    axis.line.y =        NULL,
    axis.text =          element_text(colour = "black", size = rel(0.8)),
    axis.text.x =        element_text(margin = margin(t = 0.8 * half_line / 2), vjust = 1),
    axis.text.x.top =    element_text(margin = margin(b = 0.8 * half_line / 2), vjust = 0),
    axis.text.y =        element_text(margin = margin(r = 0.8 * half_line / 2), hjust = 1),
    axis.text.y.right =  element_text(margin = margin(l = 0.8 * half_line / 2), hjust = 0),
    axis.ticks =         element_line(colour = "black", size = rel(0.5)),
    axis.ticks.length =  unit(half_line / 2, "pt"),
    axis.ticks.length.x = NULL,
    axis.ticks.length.x.top = NULL,
    axis.ticks.length.x.bottom = NULL,
    axis.ticks.length.y = NULL,
    axis.ticks.length.y.left = NULL,
    axis.ticks.length.y.right = NULL,
    axis.title.x =       element_text(
                           margin = margin(t = half_line / 2),
                           vjust = 1
                         ),
    axis.title.x.top =   element_text(
                           margin = margin(b = half_line / 2),
                           vjust = 0
                         ),
    axis.title.y =       element_text(
                           angle = 90,
                           margin = margin(r = half_line / 2),
                           vjust = 1
                         ),
    axis.title.y.right = element_text(
                           angle = -90,
                           margin = margin(l = half_line / 2),
                           vjust = 0
                         ),

    legend.background =  element_rect(colour = NA),
    legend.spacing =     unit(2 * half_line, "pt"),
    legend.spacing.x =    NULL,
    legend.spacing.y =    NULL,
    legend.margin =      margin(half_line, half_line, half_line, half_line),
    legend.key =         element_rect(fill = "white", colour = NA),
    legend.key.size =    unit(1.2, "lines"),
    legend.key.height =  NULL,
    legend.key.width =   NULL,
    legend.text =        element_text(size = rel(0.8)),
    legend.text.align =  NULL,
    legend.title =       element_text(hjust = 0),
    legend.title.align = NULL,
    legend.position =    "right",
    legend.direction =   NULL,
    legend.justification = "center",
    legend.box =         NULL,
    legend.box.margin =  margin(0, 0, 0, 0, "cm"),
    legend.box.background = element_blank(),
    legend.box.spacing = unit(2 * half_line, "pt"),

    panel.background = element_rect(fill = "white", colour = NA),
    panel.border     = element_rect(fill = NA, colour = "grey20"),
    panel.grid       =   element_line(colour = "black"),
    panel.grid.major =   element_line(size = rel(0.1)),
    panel.grid.minor =   element_line(size = rel(0.05)),
    panel.spacing =      unit(half_line, "pt"),
    panel.spacing.x =    NULL,
    panel.spacing.y =    NULL,
    panel.ontop    =     FALSE,

    strip.background =   element_rect(fill = "grey85", colour = "grey20"),
    strip.text       =   element_text(
                           colour = "black",
                           size = rel(0.8),
                           margin = margin(0.8 * half_line, 0.8 * half_line, 0.8 * half_line, 0.8 * half_line)
                         ),
    strip.text.x =       NULL,
    strip.text.y =       element_text(angle = -90),
    strip.text.y.left =  element_text(angle = 90),
    strip.placement =    "inside",
    strip.placement.x =  NULL,
    strip.placement.y =  NULL,
    strip.switch.pad.grid = unit(half_line / 2, "pt"),
    strip.switch.pad.wrap = unit(half_line / 2, "pt"),

    plot.background =    element_rect(colour = "white"),
    plot.title =         element_text( # font size "large"
                           size = rel(1.2),
                           hjust = 0, vjust = 1,
                           margin = margin(b = half_line)
                         ),
    plot.title.position = "panel",
#     plot.title.face =    "bold",
    plot.subtitle =      element_text( # font size "regular"
                           hjust = 0, vjust = 1,
                           margin = margin(b = half_line)
                         ),
    plot.caption =       element_text( # font size "small"
                           size = rel(0.8),
                           hjust = 1, vjust = 1,
                           margin = margin(t = half_line)
                         ),
    plot.caption.position = "panel",
#     plot.caption.face =  "italic",
    plot.tag =           element_text(
                           size = rel(1.2),
                           hjust = 0.5, vjust = 0.5
                         ),
    plot.tag.position =  'topleft',
    plot.margin =        margin(half_line, half_line, half_line, half_line),

    complete = TRUE
  )

}

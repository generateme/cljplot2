# ggplot2 counterparts of the Evolution notebook's "Smoothers with confidence bands" (section 3.3.1) loess and lm
# panels, for a visual comparison with cljplot2's chart/smooth.
#
# Run from the cljplot2 root:  Rscript dev/r/smooth_comparison.R
# Output: results/r/smooth-ggplot2-1000x380.png (the notebook image: docs/use_cases.evolution_files, section 3.3.1).
#
# Settings: ggplot2's geom_smooth defaults - loess span 0.75, degree 2, family "gaussian" (no robustness iterations),
# 95% band from predict(se = TRUE). cljplot2's chart/smooth uses the same span and degree but keeps fastmath's two
# robustness iterations (wider bands); `{:method-params {:iters 0}}` is the plain fit drawn here. The nested 50% band
# is a second geom_smooth layer (level = 0.5).

suppressPackageStartupMessages({
  library(ggplot2)
  library(patchwork)
})

air <- read.csv("notebooks/data/air-passengers.csv")
air$date <- as.Date(air$date)
chicks <- read.csv("notebooks/data/chick-weight.csv", colClasses = c(chick = "character", diet = "character"))

blue <- "#4575b4"

loess_panel <- ggplot(air, aes(date, passengers)) +
  geom_point(colour = blue, alpha = 0.7, size = 1) +
  geom_smooth(method = "loess", formula = y ~ x, level = 0.95, colour = blue, fill = blue, alpha = 0.3, linewidth = 0.4) +
  geom_smooth(method = "loess", formula = y ~ x, level = 0.5, colour = blue, fill = blue, alpha = 0.3, linewidth = 0.4) +
  labs(title = "loess, 50% / 95%", x = NULL, y = "passengers") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

lm_panel <- ggplot(chicks, aes(time, weight, colour = diet, fill = diet)) +
  geom_point(size = 0.8) +
  geom_smooth(method = "lm", formula = y ~ x, level = 0.95, alpha = 0.3, linewidth = 0.4) +
  labs(title = "lm per diet", x = "days", y = "weight, g") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

dir.create("results/r", recursive = TRUE, showWarnings = FALSE)
ggsave("results/r/smooth-ggplot2-1000x380.png", loess_panel + lm_panel, width = 1000, height = 380, units = "px", dpi = 100)
cat("saved results/r/smooth-ggplot2-1000x380.png\n")

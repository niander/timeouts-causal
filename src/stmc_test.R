
df <- tibble(margin = c(2,4,5,7,5), pts = c(2,2,1,2,-2))
df %>%
  mutate(pts.delta.before = lag(margin, delta) - lag(pts, delta),
         pts.delta.after = lead(margin, delta),
         slope.before = (margin - pts - pts.delta.before) / delta,
         slope.after = (pts.delta.after - margin) / delta,
         infl.score = slope.after - slope.before)

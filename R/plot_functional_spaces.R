plots_alpha <- mFD::alpha.multidim.plot(
  output_alpha_fd_multidim = alpha_fd_indices_fish,
  plot_asb_nm              = c("Bathypelagic"),
  ind_nm                   = c("fdis", "fric", "fdiv", 
                               "fspe", "fide", "feve"),
  faxes = NULL,
  faxes_nm = NULL,
  range_faxes = c(NA, NA),
  color_bg = "white",
  shape_sp = c(pool = 3, asb1 = 21, asb2 = 21),
  size_sp = c(pool = 1.3, asb1 = 0.3, asb2 = 0.3),
  color_sp = c(pool = "grey50", asb1 = "#3C685A", asb2 = NA),
  color_vert = c(pool = "grey50", asb1 = "#3C685A", asb2 = NA),
  fill_sp = c(pool = NA, asb1 = "#FFFFFF30", asb2 = "#FFFFFF30"),
  fill_vert = c(pool = NA, asb1 = "#3C685A", asb2 = NA),
  color_ch = c(pool = "grey", asb1 = "#3C685A", asb2 = NA),
  fill_ch = c(pool = "grey99", asb1 = "#3C685A", asb2 = NA),
  alpha_ch = c(pool = 1, asb1 = 0.3, asb2 = 0.3),
  shape_centroid_fdis = c(asb1 = 22, asb2 = 22),
  shape_centroid_fdiv = c(asb1 = 24, asb2 = 25),
  shape_centroid_fspe = 23,
  color_centroid_fspe = "black",
  color_sp_nm = "black",
  plot_sp_nm = NULL,
  fontface_sp_nm = "plain",
  save_file = FALSE,
  check_input = TRUE) 

plots_alpha$fric$PC1_PC2
ggsave("functional_space_PC1_bathy.png", path = "figures/soutenance", dpi = 700, height = 4, width = 4)


plots_alpha <- mFD::alpha.multidim.plot(
  output_alpha_fd_multidim = alpha_fd_indices_fish,
  plot_asb_nm              = c("Bathypelagic"),
  ind_nm                   = c("fdis", "fric", "fdiv", 
                               "fspe", "fide", "feve"),
  faxes = NULL,
  faxes_nm = NULL,
  range_faxes = c(NA, NA),
  color_bg = "grey92",
  shape_sp = c(pool = 3, asb1 = 21, asb2 = 21),
  size_sp = c(pool = 1.3, asb1 = 0.3, asb2 = 0.3),
  color_sp = c(pool = "grey50", asb1 = "black", asb2 = NA),
  color_vert = c(pool = "grey50", asb1 = "black", asb2 = NA),
  fill_sp = c(pool = NA, asb1 = "#FFFFFF30", asb2 = "#FFFFFF30"),
  fill_vert = c(pool = NA, asb1 = "black", asb2 = NA),
  color_ch = c(pool = "grey80", asb1 = "black", asb2 = NA),
  fill_ch = c(pool = "white", asb1 = NA, asb2 = NA),
  alpha_ch = c(pool = 1, asb1 = 0.3, asb2 = 0.3),
  shape_centroid_fdis = c(asb1 = 22, asb2 = 22),
  shape_centroid_fdiv = c(asb1 = 24, asb2 = 25),
  shape_centroid_fspe = 23,
  color_centroid_fspe = "black",
  color_sp_nm = "black",
  plot_sp_nm = NULL,
  fontface_sp_nm = "plain",
  save_file = FALSE,
  check_input = TRUE) 

plots_alpha$fric$PC3_PC4
ggsave("functional_space_PC3_tot.png", path = "figures", dpi = 700, height = 4, width = 4)







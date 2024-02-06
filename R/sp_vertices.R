
essai <- rownames (alpha_fd_indices_fish$details$asb_dist_nn_pool)

beta_plot_fish <- mFD::beta.multidim.plot(
  output_beta_fd_multidim = beta_fd_indices_fish,
  plot_asb_nm             = c("Lower mesopelagic", "Bathypelagic"),
  beta_family             = c("Jaccard"),
  plot_sp_nm              = essai,
  faxes                   = paste0("PC", 1:4),
  name_file               = NULL,
  faxes_nm                = NULL,
  range_faxes             = c(NA, NA),
  nm_size =2,
  check_input             = TRUE)

beta_plot_fish$PC3_PC4
ggsave("Lower_mesopelagic_Bathy_PC3.png", path = "figures", 
       dpi = 700, height = 10, width = 10)

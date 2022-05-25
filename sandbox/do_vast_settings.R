
do_vast_settings <- function(knots = 300, obs_model = c(2, 0), 
	eta1 = 1, eta2, anis = FALSE, bias = FALSE, FieldConfig, RhoConfig){

	settings <- FishStatsUtils::make_settings(
	  n_x = knots, # number of vertices in the SPDE mesh
	  Region = "User",
	  purpose = "index2", # use recommended defaults for an index of abundance
	  fine_scale = TRUE, # use bilinear interpolation from the INLA 'A' matrix
	  FieldConfig = FieldConfig,
	  RhoConfig = RhoConfig,
	  ObsModel = obs_model, # conventional logit-linked delta-Gamma; c(10, 2) for Tweedie
	  OverdispersionConfig = c(Eta1 = eta1, Eta2 = eta2),
	  bias.correct = bias,
	  use_anisotropy = anis,
	  max_cells = Inf, # use all grid cells from the extrapolation grid
	  knot_method = "samples" # "samples" or "grid"
	)
	return(settings)
}

my_apply_epsilon <- function (
    fit, 
    ADREPORT_name = "Index_ctl", 
    eps_name = "eps_Index_ctl", 
    data_function = identity, 
    inner.control = list(sparse = TRUE, lowrank = TRUE, trace = TRUE)
) 
{
    if (class(fit) != "fit_model") 
        stop("Check `fit` in `apply_epsilon`")
    if (!(ADREPORT_name %in% names(fit$Report))) 
        stop("Check `ADREPORT_name` in `apply_epsilon`")
    Data = fit$data_list
    Random = fit$tmb_list$Random
    Map = fit$tmb_list$Map
    New_params = fit$ParHat
    New_params[[eps_name]] = array(0, dim = dim(fit$Report[[ADREPORT_name]]))
    fixed = fit$par$par
    new_values = rep(0, prod(dim(New_params[[eps_name]])))
    names(new_values) = eps_name
    fixed = c(fixed, new_values)
    obj = MakeADFun(data = lapply(Data, FUN = data_function), 
        parameters = New_params, map = Map, random = Random, 
        intern = TRUE, inner.control = inner.control, DLL = "VAST_v14_0_1")
    obj$env$beSilent()
    gradient = obj$gr(fixed)
    SD = fit$par$SD
    if (is.null(SD$unbiased)) {
        SD$unbiased = list(value = NA, sd = NA, cov = array(NA, 
            c(1, 1)))
        SD$unbiased$value = SD$value
        SD$unbiased$value[] = NA
    } else {
        if (any(!is.na(SD$unbiased$value[ADREPORT_name]))) {
            warning(paste0("it appears that `", ADREPORT_name, 
                "` is already bias-corrected; using `apply_epsilon` seems inefficient and will replace existing values"))
        }
    }
    i = which(names(SD$unbiased$value) == ADREPORT_name)
    j = which(names(obj$par) == eps_name)
    if (length(i) == length(j)) {
        SD$unbiased$value[i] = gradient[j]
    }
    class(SD) = "sdreport"
    return(SD)
}

// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppEigen.h>
#include <Rcpp.h>

using namespace Rcpp;


RcppExport SEXP _rcpp_module_boot_stan_fit4cumhist_exgauss_mod();
RcppExport SEXP _rcpp_module_boot_stan_fit4cumhist_gamma_mod();
RcppExport SEXP _rcpp_module_boot_stan_fit4cumhist_gamma_fixed_mod();
RcppExport SEXP _rcpp_module_boot_stan_fit4cumhist_gamma_fixed_random_mod();
RcppExport SEXP _rcpp_module_boot_stan_fit4cumhist_gamma_random_mod();
RcppExport SEXP _rcpp_module_boot_stan_fit4cumhist_lognormal_mod();
RcppExport SEXP _rcpp_module_boot_stan_fit4cumhist_normal_mod();

static const R_CallMethodDef CallEntries[] = {
    {"_rcpp_module_boot_stan_fit4cumhist_exgauss_mod", (DL_FUNC) &_rcpp_module_boot_stan_fit4cumhist_exgauss_mod, 0},
    {"_rcpp_module_boot_stan_fit4cumhist_gamma_mod", (DL_FUNC) &_rcpp_module_boot_stan_fit4cumhist_gamma_mod, 0},
    {"_rcpp_module_boot_stan_fit4cumhist_gamma_fixed_mod", (DL_FUNC) &_rcpp_module_boot_stan_fit4cumhist_gamma_fixed_mod, 0},
    {"_rcpp_module_boot_stan_fit4cumhist_gamma_fixed_random_mod", (DL_FUNC) &_rcpp_module_boot_stan_fit4cumhist_gamma_fixed_random_mod, 0},
    {"_rcpp_module_boot_stan_fit4cumhist_gamma_random_mod", (DL_FUNC) &_rcpp_module_boot_stan_fit4cumhist_gamma_random_mod, 0},
    {"_rcpp_module_boot_stan_fit4cumhist_lognormal_mod", (DL_FUNC) &_rcpp_module_boot_stan_fit4cumhist_lognormal_mod, 0},
    {"_rcpp_module_boot_stan_fit4cumhist_normal_mod", (DL_FUNC) &_rcpp_module_boot_stan_fit4cumhist_normal_mod, 0},
    {NULL, NULL, 0}
};

RcppExport void R_init_bistablehistory(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

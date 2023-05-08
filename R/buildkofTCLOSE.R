
#' buildkofTCLOSE
#'
#' @param .path MEMORY PATH OF REPORT
#'
#' @return kofTCClose
#' @export
#' @import dplyr tidyr readxl

buildkofTCLOSE <- function(.path){
  kofTCClose <-
    .path %>%
    readxl::read_excel(.,
                           sheet = "TC | Cierre",
                           skip = 7,
                           col_types = c("numeric", "date", "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric"))
  kofTCClose <- kofTCClose[,-c(1,4,7,10,13,16,19,22,23,24,25,28,29,30,31,34,35,36,37,40,41,42,43,44,45,46,49:67)]
  kofTCClose <- kofTCClose %>% rename( 'Date' = `...2`,
                                       'P.Mx. / P.Ur.' = `P.Mx. / PhP....33`,
                                       'Bol. / USD' = `Bol. / USD...38`,
                                       'P.Mx. / Bol.' = `P.Mx. / Bol....39`,
                                       'P.Mx. / P.Col' = `PMx. / P.Col`)
  kofTCClose <- kofTCClose %>% mutate(Date = Date %>% lubridate::ymd())
  kofTCClose <- kofTCClose %>% filter(Date %>% lubridate::year() >= 2001)
  kofTCClose <- kofTCClose %>% mutate(`P.Mx. / P.Mx.` = 1)
  kofTCClose <- kofTCClose %>%
    pivot_longer(., !Date,  names_to = 'Moneda', values_to = 'TC') %>%
    mutate(across(everything(), .fns = ~replace_na(.,0)))
  kofTCClose <- kofTCClose %>%
    left_join(., tiposcambioMeta)

  return(kofTCClose)
}

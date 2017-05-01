#' visita ao site do GAE ou ao site do DATAUNIRIO
#' @param SITE
#' visita ao site do GAE ou ao site do DATAUNIRIO
#' @return abre o navegador e entra no site escolhido
#' @export

site <- function (SITE) {
  if (SITE=="GAE") {
    browseURL("http://gae.uniriotec.br/")
  } else if (SITE=="DATAUNIRIO") {
    browseURL("https://dataunirio.github.io/")
  } else {cat("Voce escreveu certo? \n Escreva site('GAE') para conhecer o site do Grupo de Apoio Estatistico - GAE \n Escreva site('DATAUNIRIO') para conhecer o site do DATAUNIRIO")
  }
}

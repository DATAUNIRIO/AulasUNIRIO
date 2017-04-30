#' instalar todos os pacotes do RCommander (Rcmdr), do Radiant e do Deducer
#' @importFrom utils install.packages choose.dir download.file data
# @importFrom datasets iris mtcars
# @import datasets iris mtcars
#' @param Pacote
#' instalar todos os pacotes do RCommander (Rcmdr), do Radiant e do Deducer
#' @return pacote instalado
#' @export

instalarpacote <- function (Pacote) {
  if (Pacote=="Rcmdr") {
    install.packages(c("rcmdcheck","Rcmdr","RcmdrMisc","RcmdrPlugin.BCA","RcmdrPlugin.BiclustGUI","RcmdrPlugin.coin","RcmdrPlugin.depthTools","RcmdrPlugin.DoE","RcmdrPlugin.doex","RcmdrPlugin.EACSPIR","RcmdrPlugin.EBM","RcmdrPlugin.EcoVirtual","RcmdrPlugin.epack","RcmdrPlugin.Export","RcmdrPlugin.EZR","RcmdrPlugin.FactoMineR","RcmdrPlugin.FuzzyClust","RcmdrPlugin.GWRM","RcmdrPlugin.HH","RcmdrPlugin.IPSUR","RcmdrPlugin.KMggplot2","RcmdrPlugin.lfstat","RcmdrPlugin.MA","RcmdrPlugin.mosaic","RcmdrPlugin.MPAStats","RcmdrPlugin.NMBU","RcmdrPlugin.orloca","RcmdrPlugin.PcaRobust","RcmdrPlugin.plotByGroup","RcmdrPlugin.pointG","RcmdrPlugin.qual","RcmdrPlugin.RMTCJags","RcmdrPlugin.ROC","RcmdrPlugin.sampling","RcmdrPlugin.SCDA","RcmdrPlugin.seeg","RcmdrPlugin.SLC","RcmdrPlugin.SM","RcmdrPlugin.sos","RcmdrPlugin.steepness","RcmdrPlugin.survival","RcmdrPlugin.TeachingDemos","RcmdrPlugin.temis","RcmdrPlugin.UCA"), dependencies=TRUE)
  } else if (Pacote=="radiant") {
    install.packages("radiant", repos = "https://radiant-rstats.github.io/minicran/", type = "binary")
  } else if (Pacote=="Deducer") {
    install.packages(c("JGR","Deducer","DeducerExtras","DeducerPlugInExample","DeducerPlugInScaling","DeducerSpatial","DeducerSurvival","DeducerText"), dependencies=TRUE)
  } else {
    cat("Voce escreveu o nome do pacote certo? \nPara instalar os pacotes do R, por favor, escreva \ninstalarpacote('Rcmdr') com o R maiusculo e entre aspas, \ninstalarpacote('radiant') com o r minusculo, ou \ninstalarpacote('Deducer') com D maiusculo. \n Ainda com dificuldades na instalacao? \no GAE/UNIRIO tem uma apostila de RCommander para baixar DE GRACA \n no site http://gae.uniriotec.br/7/Notas_de_aula_Rcmdr.pdf")
  }
}


#' Baixar a apostila do GAE/UNIRIO
#'
#' @param apostila
#' Baixar a apostila do GAE/UNIRIO
#' @return Donwload da Apostila do Prof. Felipe Rafael
#' @export

Baixarapostilaoulivro <- function (apostila) {
  if (apostila=="APOSTILAGAE1") {
    cat("Onde voce quer salvar a sua Apostila?")
    choose.dir()
    download.file("http://gae.uniriotec.br/7/Notas_de_aula_Rcmdr.pdf",destfile="Notas_de_aula_Rcmdr.pdf")
    cat("A sua apostila esta na pasta escolhida")
  } else if (apostila=="APOSTILAGAE2") {
    cat("Onde voce quer salvar a sua Apostila 'Rcmdr_alem_dos_menus'?")
    choose.dir()
    download.file("http://gae.uniriotec.br/7/Rcmdr_alem_dos_menus.pdf",destfile="Rcmdr_alem_dos_menus.pdf")
    cat("A sua apostila esta na pasta escolhida")
  } else if (apostila=="LIVRO1") {
    cat("Onde voce quer salvar o seu livro 'R para cientistas sociais'?")
    choose.dir()
    download.file("http://www.uesc.br/editora/livrosdigitais_20140513/r_cientistas.pdf",destfile="R_para_cientistas_sociais.pdf")
    cat("O seu livro esta na pasta escolhida")
  } else if (apostila=="LIVRO2") {
    cat("Onde voce quer salvar o seu livro 'Analise Exploratoria de Dados usando o R'?")
    choose.dir()
    download.file("http://www.uesc.br/editora/livrosdigitais2/analiseexploratoria_r.pdf.pdf",destfile="Analise_Exploratoria_de_Dados_usando_o_R.pdf")
    cat("O seu livro esta na pasta escolhida")
  } else {cat("Por favor, escreva \nBaixarapostilaoulivro('APOSTILAGAE'), \nBaixarapostilaoulivro('APOSTILAGAE2'), \nBaixarapostilaoulivro('LIVRO1'), \nBaixarapostilaoulivro('LIVRO2') \nem letras maiusculas e entre aspas para salvar o seu arquivo em PDF")
  }
}

#' Carrega o banco de dados em portugues
#'
#' @param CARROSEPAISES
#' Carrega o banco de dados em portugues. Todavia voc?? precisa escrever data(mtcars) ou data(iris) antes para funcionar e remove(mtcars) para remover o original
#' @return Banco de dados CARROS ou PAISES
#' @export

bd <- function (CARROSEPAISES) {
  if (CARROSEPAISES=="CARROS") {
    CARROS<<-mtcars
    colnames(CARROS) <<- c("Kmporlitro","Cilindros","Preco","HP","Amperagem_circ_eletrico",
                           "Peso","RPM","Tipodecombustivel","TipodeMarcha","NumdeMarchas","NumdeValvulas")
    nomes<-c("Km por litro","Numero de Cilindros","Preco",
             "HP = Horse Power (potencia do motor)","Amperagem_circ_eletrico = Amperagem media (o principal indicador da bateria)","Peso (em toneladas) do Carro",
             "RPM = Rotacoes Por Minuto","Tipo de combustivel (0 = Gasolina, 1 = Alcool)","Tipo de C??mbio (0 = Automatico, 1 = Manual)",
             "Numero de Marchas","Numero de Valvulas")
    attr(CARROS, "variable.labels") <<- nomes
    dicionariodedados<-attr(CARROS, "variable.labels")
    dicionariodedados
  }
  else if (CARROSEPAISES=="PAISES") {
    PAISES<<-iris
    colnames(PAISES) <<- c("ParticipacaoPolitica","EficaciadoEstado","Transparencia","Violencia","Regiao")
    levels(PAISES$Regiao) <<- c("Africa","America_Latina","Europa")
    PAISES<<-data.frame(PAISES)
    nomes<-c("ParticipacaoPolitica = Nivel de Participacao Politica do Pais",
             "EficaciadoEstado = Nivel de Eficacia do do Estado do Pais",
             "Transparencia = Nivel de Transparencia/Accountability do Pais",
             "Violencia = Nivel de Violencia do Pais",
             "Regiao = Continente onde o pais esta localizado")
    attr(PAISES, "variable.labels") <<- nomes
    dicionariodedadospaises<-attr(PAISES, "variable.labels")
    dicionariodedadospaises
  }
  else {cat("Voce escreveu certo? Escreva bd('CARROS') ou bd('PAISES') em letras maiusculas e entre aspas. para ver se funcionou escreva head(CARROS) ou head(PAISES)")
  }
}

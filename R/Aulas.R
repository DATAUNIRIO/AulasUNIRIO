#' instalar todos os pacotes do RCommander (Rcmdr), do Radiant e do Deducer
#' @importFrom utils install.packages choose.dir download.file data installed.packages
#' @importFrom WDI WDI
#' @import WDI
# @importFrom datasets iris mtcars
# @import datasets iris mtcars
#' @param Pacote
#' instalar todos os pacotes do RCommander (Rcmdr), do Radiant e do Deducer
#' @return pacote instalado
#' @export

#.onAttach <- function(libname, pkgname) { packageStartupMessage("Welcome to my package") }
#.onLoad <- function(libname = find.package("AulasUNIRIO"), pkgname = "AulasUNIRIO") {
#  packageStartupMessage("Visite os nossos Sites:")
#}

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
    setwd(choose.dir())
    download.file("http://gae.uniriotec.br/7/Notas_de_aula_Rcmdr.pdf",destfile="Notas_de_aula_Rcmdr.pdf")
    cat("A sua apostila esta na pasta escolhida")
  } else if (apostila=="APOSTILAGAE2") {
    cat("Onde voce quer salvar a sua Apostila 'Rcmdr_alem_dos_menus'?")
    setwd(choose.dir())
    download.file("http://gae.uniriotec.br/7/Rcmdr_alem_dos_menus.pdf",destfile="Rcmdr_alem_dos_menus.pdf")
    cat("A sua apostila esta na pasta escolhida")
  } else if (apostila=="LIVRO1") {
    cat("Onde voce quer salvar o seu livro 'R para cientistas sociais'?")
    setwd(choose.dir())
    download.file("http://www.uesc.br/editora/livrosdigitais_20140513/r_cientistas.pdf",destfile="R_para_cientistas_sociais.pdf")
    cat("O seu livro esta na pasta escolhida")
  } else if (apostila=="LIVRO2") {
    cat("Onde voce quer salvar o seu livro 'Analise Exploratoria de Dados usando o R'?")
    setwd(choose.dir())
    download.file("http://www.uesc.br/editora/livrosdigitais2/analiseexploratoria_r.pdf",destfile="Analise_Exploratoria_de_Dados_usando_o_R.pdf")
    cat("O seu livro esta na pasta escolhida")
  } else {cat("Por favor, escreva \nBaixarapostilaoulivro('APOSTILAGAE'), \nBaixarapostilaoulivro('APOSTILAGAE2'), \nBaixarapostilaoulivro('LIVRO1'), \nBaixarapostilaoulivro('LIVRO2') \nem letras maiusculas e entre aspas para salvar o seu arquivo em PDF")
  }
}

#' Carrega o banco de dados em portugues
#'
#' @param CARROSEPAISES
#' Carrega o banco de dados em portugues. Todavia voc?? precisa escrever data(mtcars) ou data(iris) antes para funcionar e remove(mtcars) para remover o original
#' @return Banco de dados chamados CARROS, PAISES ou WBDATA (Banco de dados do WorldBank - precisa de Internet)
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
  else if (CARROSEPAISES=="WBDATA") {
    if( !is.element("WDI", installed.packages()[,1]) )
      install.packages("WDI")
    inds <- c('SP.DYN.TFRT.IN','SP.DYN.LE00.IN', 'SP.POP.TOTL',
              'NY.GDP.PCAP.CD', 'SE.ADT.1524.LT.FE.ZS')
    indnams <- c("taxadefecundidade", "expectativadevida", "populacao",
                 "PIB.per.capita.Current.USD", "estudo.15.ate.25.anos")
    wdiData <<- WDI(country="all", indicator=inds,
                   start=1960, end=format(Sys.Date(), "%Y"), extra=TRUE)
    colnum <<- match(inds, names(wdiData))
    names(wdiData)[colnum] <<- indnams

    indsWB <- c('SE.XPD.TOTL.GD.ZS','SE.ADT.LITR.ZS','SP.POP.TOTL','VC.IHR.PSRC.P5','SG.GEN.PARL.ZS',
                'SG.NOD.CONS','IC.BUS.EASE.XQ','LP.LPI.OVRL.XQ','IC.REG.DURS','IC.REG.PROC','IC.TAX.TOTL.CP.ZS',
                'IC.LGL.CRED.XQ','IC.BUS.NREG','IC.BUS.NDNS.ZS','IQ.CPA.SOCI.XQ','IQ.CPA.PUBS.XQ','IQ.CPA.STRC.XQ',
                'IQ.CPA.ECON.XQ','SH.STA.ACSN','SH.TBS.INCD','SH.DYN.AIDS.ZS','SH.XPD.TOTL.ZS','SH.MED.BEDS.ZS',
                'SP.ADO.TFRT','SP.DYN.LE00.IN','SP.DYN.TFRT.IN','SP.DYN.IMRT.IN','EN.POP.SLUM.UR.ZS','SL.UEM.TOTL.ZS',
                'NY.GDP.PCAP.PP.CD','NY.GDP.MKTP.CD','FR.INR.RINR','SM.POP.NETM','SM.POP.REFG.OR','SM.POP.REFG')

    indnamsWB <- c("GastoGovernoEducacao","TaxaAlfabetizacaoAdulto","Populacao","Homicidios","ProporcaoMulheresparlamento",
                   "Generonaconstituicao","IndiceFacilidadeNegocios","IndiceperformanceLogistica","Tempoparainiciarempresa",
                   "Procedimetosiniciarempresa","Impostostotais","Indicedireitoslegais","NumerodeNovosnegocios","DensidadeNovosnegocios",
                   "Inclusaosocial","gestaosetorpublico","politicasestruturais","gestaoeconomica","percentualSaneamento",
                   "Turberculose","HIV","GastosSAUDE","LeitosHospitais","FecundidadeAdolecente","EspectativadeVida","FecundidadeTotal",
                   "MortalidadeInfantil","PopulacaoFavelas","TaxadeDesemprego","PIBperCapita","PIB","Taxadejuros","Imigracao","Refugiadoorigem",
                   "Refugiadosasilo")

    nomesWB<- c(
      "GastoGovernoEducacao        Government expenditure on education, total (% of GDP)",
      "TaxaAlfabetizacaoAdulto     Adult literacy rate, population 15+ years, both sexes (%)",
      "Populacao                   Population, total",
      "Homicidios                  Intentional homicides (per 100,000 people) USAR O ANO DE 2012",
      "ProporcaoMulheresparlamento Proportion of seats held by women in national parliaments (%)",
      "Generonaconstituicao        Nondiscrimination clause mentions gender in the constitution (1=yes; 0=no)",
      "IndiceFacilidadeNegocios    Ease of doing business index (1=most business-friendly regulations)",
      "IndiceperformanceLogistica  Logistics performance index: Overall (1=low to 5=high)",
      "Tempoparainiciarempresa     Time required to start a business (days)",
      "Procedimetosiniciarempresa  Start-up procedures to register a business (number)",
      "Impostostotais              Total tax rate (% of commercial profits)",
      "Indicedireitoslegais        Strength of legal rights index (0=weak to 12=strong)",
      "NumerodeNovosnegocios       New businesses registered (number)",
      "DensidadeNovosnegocios      New business density (new registrations per 1,000 people ages 15-64)",
      "Inclusaosocial              CPIA policies for social inclusion/equity cluster average (1=low to 6=high)",
      "gestaosetorpublico          CPIA public sector management and institutions cluster average (1=low to 6=high)",
      "politicasestruturais        CPIA structural policies cluster average (1=low to 6=high)",
      "gestaoeconomica             CPIA economic management cluster average (1=low to 6=high)",
      "percentualSaneamento        Improved sanitation facilities (% of population with access)",
      "Turberculose                Incidence of tuberculosis (per 100,000 people)",
      "HIV                         Prevalence of HIV, total (% of population ages 15-49)",
      "GastosSAUDE                 Health expenditure, total (% of GDP)",
      "LeitosHospitais             Hospital beds (per 1,000 people)",
      "FecundidadeAdolecente       Adolescent fertility rate (births per 1,000 women ages 15-19)",
      "EspectativadeVida           Life expectancy at birth, total (years)",
      "FecundidadeTotal            Fertility rate, total (births per woman)",
      "MortalidadeInfantil         Mortality rate, infant (per 1,000 live births)",
      "PopulacaoFavelas            Population living in slums (% of urban population)",
      "TaxadeDesemprego            Unemployment, total (% of total labor force)",
      "PIBperCapita                GDP per capita, PPP (current international $)",
      "PIB                         GDP (current US$)",
      "Taxadejuros                 Real interest rate (%)",
      "Imigracao                   Net migration",
      "Refugiadoorigem             Refugee population by country or territory of origin",
      "Refugiadosasilo             Refugee population by country or territory of asylum")

    wdiData2 <<- WDI(country="all", indicator=indsWB,
                    start=2010, end=format(Sys.Date(), "%Y"), extra=TRUE)
    colnum <<- match(indsWB, names(wdiData2))
    names(wdiData2)[colnum] <<- indnamsWB
    attr(wdiData2, "variable.labels") <<- nomesWB
    dicionariodedadosWB<-attr(wdiData2, "variable.labels")
    BancoagregadoWB<- subset(wdiData2, income=='Aggregates')
    BancodesagregadoWB<- subset(wdiData2, income!='Aggregates')
  }
  else {cat("Voce escreveu certo? Escreva bd('CARROS'), bd('PAISES'), bd('WBDATA') em letras maiusculas e entre aspas. para ver se funcionou escreva head(CARROS) ou head(PAISES)")
  }
}

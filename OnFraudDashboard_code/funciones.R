source("./config.R")


endpoint <- devolver.endpoint()
prefix <-
  'PREFIX pproc:<http://contsem.unizar.es/def/sector-publico/pproc#>
PREFIX schema:<http://schema.org/>
PREFIX pc:<http://purl.org/procurement/public-contracts#>
PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX owl:<http://www.w3.org/2002/07/owl#>
PREFIX org:<http://www.w3.org/ns/org#>
PREFIX pproc:<http://contsem.unizar.es/def/sector-publico/pproc#>
PREFIX dcterms:<http://purl.org/dc/terms/>
PREFIX rdfs:<http://www.w3.org/2000/01/rdf-schema#>
PREFIX gr:<http://purl.org/goodrelations/v1>
PREFIX foaf:<http://xmlns.com/foaf/0.1/>
PREFIX dc:<http://purl.org/dc/elements/1.1/>'

sparqlVistaGeneral <-   paste(
  prefix,
  'SELECT *
  where{
  ?contract rdf:type pproc:Contract.
  ?contract pc:contractingAuthority ?unit.
  ?unit dcterms:description ?desc.
  ?contract dcterms:description ?description.
  ?contract pc:supplier ?organization.
  ?organization rdf:type org:Organization.
  ?organization dcterms:identifier ?identifier.
  ?contract pc:actualPrice ?bp.
  ?bp rdf:type pproc:BundlePriceSpecification;
  <gr:hasCurrencyValue> ?value .
  }
  ORDER BY (?identifier)')

sparqlConcentracionProveedores <-
  paste(
    prefix,
    'SELECT ?identifier, ?description, count(*) as ?numerodecontrataciones, sum(?value) as ?sumavalor
      where{
      ?contract rdf:type pproc:Contract.
      ?contract dcterms:description ?description.
        ?contract pc:supplier ?organization.
      ?organization rdf:type org:Organization.
      ?organization dcterms:identifier ?identifier.
      ?contract pc:actualPrice ?bp.
      ?bp rdf:type pproc:BundlePriceSpecification;
      <gr:hasCurrencyValue> ?value .
      }
      ORDER BY (?identifier)
      ')


vector_es_outlier_IQR <- function (datos, indice.de.columna, coef = 1.5) {
  columna.datos = datos[, indice.de.columna]
  cuartil.primero = quantile(columna.datos)[2]  
  cuartil.tercero = quantile(columna.datos)[4]
  iqr = cuartil.tercero - cuartil.primero
  extremo.superior.outlier = (iqr * coef) + cuartil.tercero
  extremo.inferior.outlier = cuartil.primero - (iqr * coef)
  es.outlier  = columna.datos > extremo.superior.outlier |
    columna.datos < extremo.inferior.outlier
  return (es.outlier)
}

descriptivo<-function()
{
  qd <- SPARQL(endpoint,sparqlVistaGeneral)
  df <- qd$results
  print("desriptivo cargado")
  return (df)
  
}

concentracion<-function()
{
  qd <- SPARQL(endpoint,sparqlConcentracionProveedores)
  df <- qd$results
  print("concentracion cargado")
  return (df)
}

numeroContratos <- function(df)
{
  return (sum(df$numerodecontrataciones))
}
valorContratosObras <-function(df)
{
  return (sum((df%>%filter(description=='Contrato de obras'))$value))
}

valorContratosSumServ <-function(df)
{
  return (sum((df%>%filter(description=='Contrato de servicios' |description=='Contrato de suministros'))$value))
}

valorContratos<-function(df)
{
  return (sum(df$sumavalor))
}

devolverContratos<-function(df)
{
  return(df%>%filter(description=='Contrato de obras'| description=='Contrato de servicios' |description=='Contrato privado' |description=='Contrato de suministros' )%>%select(description,numerodecontrataciones)%>%group_by(description,numerodecontrataciones)%>%summarize(contar=n()))
}

getOutliersSuministros<-function(df)
{
  suministros<-select(df,identifier,description,numerodecontrataciones,sumavalor) %>%filter(numerodecontrataciones==1 &description=='Contrato de suministros' )
  sum.outliers<-vector_es_outlier_IQR(suministros,4)
  return (length(sum.outliers[sum.outliers==TRUE])) 
}

getDFOutliersSuministros<-function(df)
{
  suministros<-select(df,identifier,description,numerodecontrataciones,sumavalor) %>%filter(numerodecontrataciones==1 &description=='Contrato de suministros' )
  sum.outliers<-vector_es_outlier_IQR(suministros,4)
  return (as.data.frame(suministros[sum.outliers==TRUE,]$identifier))
}

getOutliersServicios<-function(df)
{
  servicios<-select(df,identifier,description,numerodecontrataciones,sumavalor) %>%filter(numerodecontrataciones==1 &description=='Contrato de servicios' )
  serv.outliers<-vector_es_outlier_IQR(servicios,4)
  return (length(serv.outliers[serv.outliers==TRUE])) 
}

getDFOutliersServicios<-function(df)
{
  servicios<-select(df,identifier,description,numerodecontrataciones,sumavalor) %>%filter(numerodecontrataciones==1 &description=='Contrato de servicios' )
  serv.outliers<-vector_es_outlier_IQR(servicios,4)
  return (as.data.frame(servicios[serv.outliers==TRUE,]$identifier))
}


getDFOutliers<-function(df)
{
  servicios<-select(df,contract,identifier,description,numerodecontrataciones,sumavalor) %>%filter(numerodecontrataciones==1 &description=='Contrato de servicios' )
  serv.outliers<-vector_es_outlier_IQR(servicios,5)
  tmp1<- (as.data.frame(servicios[serv.outliers==TRUE,]))
  
  privado<-select(df,contract,identifier,description,numerodecontrataciones,sumavalor) %>%filter(numerodecontrataciones==1 &description=='Contrato privado' )
  priv.outliers<-vector_es_outlier_IQR(privado,5)
  tmp2<-as.data.frame(privado[priv.outliers==TRUE,])
  obras<-select(df,contract,identifier,description,numerodecontrataciones,sumavalor) %>%filter(numerodecontrataciones==1 &description=='Contrato de obras' )
  obras.outliers<-vector_es_outlier_IQR(obras,5)
  tmp3<- (as.data.frame(obras[obras.outliers==TRUE,]))
  
  suministros<-select(df,contract,identifier,description,numerodecontrataciones,sumavalor) %>%filter(numerodecontrataciones==1 &description=='Contrato de suministros' )
  sum.outliers<-vector_es_outlier_IQR(suministros,5)
  tmp4<- (as.data.frame(suministros[sum.outliers==TRUE,]))
  
  return(tmp1%>%union(tmp2)%>%union(tmp3)%>%union(tmp4))
  
}



getOutliersPrivados<-function(df)
{
  privado<-select(df,identifier,description,numerodecontrataciones,sumavalor) %>%filter(numerodecontrataciones==1 &description=='Contrato privado' )
  priv.outliers<-vector_es_outlier_IQR(privado,4)
  return (length(priv.outliers[priv.outliers==TRUE])) 
}

getDFOutliersPrivados<-function(df)
{
  privado<-select(df,identifier,description,numerodecontrataciones,sumavalor) %>%filter(numerodecontrataciones==1 &description=='Contrato privado' )
  priv.outliers<-vector_es_outlier_IQR(privado,4)
  return (as.data.frame(privado[priv.outliers==TRUE,]$identifier))
}

getOutliersObras<-function(df)
{
  obras<-select(df,identifier,description,numerodecontrataciones,sumavalor) %>%filter(numerodecontrataciones==1 &description=='Contrato de obras' )
  obras.outliers<-vector_es_outlier_IQR(obras,4)
  return (length(obras.outliers[obras.outliers==TRUE]))
 }

getDFOutliersObras<-function(df)
{
  obras<-select(df,identifier,description,numerodecontrataciones,sumavalor) %>%filter(numerodecontrataciones==1 &description=='Contrato de obras' )
  obras.outliers<-vector_es_outlier_IQR(obras,4)
  return (as.data.frame(obras[obras.outliers==TRUE,]$identifier))

}
numeroProveedores <-function(df)
{
  return (nrow(df))
}

volumenTipoContratos<-function(df)
{
 return (df%>%select(description,value)%>%group_by(description)%>%summarize(suma=sum(value), count=n()))
}

getDFSumServCercanosUmbral <- function (general){

servysum.cercanos.umbral<-select(general,contract,identifier,description,value) %>%filter((description=='Contrato de suministros'|description=='Contrato de servicios'|description=='Contrato privado')&value>=17200 )
if (nrow(servysum.cercanos.umbral)>0)
  servysum.cercanos.umbral$contract <- getExpediente(servysum.cercanos.umbral$contract) 
else
  servysum.cercanos.umbral[1,]<-list("n/a","n/a","n/a",0)
return (servysum.cercanos.umbral)
}

getDFObrasCercanosUmbral <- function (general){
  
  obras<-select(general,contract,identifier,description,value) %>%filter((description=='Contrato de obras')&value>=48000 )

  if (nrow(obras)>0)
    obras$contract<-getExpediente(obras$contract)
  else
  {
    obras[1,]<-list("n/a","n/a","n/a",0)  
  }
    
  return (obras)
}


getExpediente<-function(exps)
{
  if (length(exps)==0)
    return (c("sin expedientes"))
  return(sub(pattern=">","",sub(pattern = "<http://purl.org/purchase2pay/individuals/","",x = exps)))
}

getDFObrasAgregadas<-function(df){
obras<-select(df,identifier,description,numerodecontrataciones,sumavalor) %>%filter(numerodecontrataciones>1 &description=='Contrato de obras'&sumavalor>50000)
if (nrow(obras)==0)
{
  obras[1,]<-list("n/a","n/a","n/a",0)  
}
return (obras)
}

getDFSumServAgregados<-function(df){
servicios<-select(df,identifier,description,numerodecontrataciones,sumavalor) %>%filter(numerodecontrataciones>1 &(description=='Contrato de suministros'|description=='Contrato de servicios'|description=='Contrato privado')&sumavalor>=18000 )
if (nrow(servicios)==0)
{
  servicios[1,]<-list("n/a","n/a","n/a",0)  
}
return(servicios)
}

anotar <- function (matriz)
{
  .jinit()
  rdf<-devolver.rdf()
  .jaddClassPath(rdf)
  task<-.jnew("rdfapi/OnFraudManager")
  print(task)
  strings <-task$anotar(.jarray(matriz,dispatch=T))
  resultados <- read.csv(text = readLines(textConnection(strings)), sep = ",", header = TRUE)
  return (resultados)
}

matrizPonderadaExpedientes<-function(matriz,esquema)
{
  
  matrizTransformada<-dcast(matriz, contract~redflag);
  for (i in 1:length(esquema)){
  matrizTransformada[,(i+1)]<-matrizTransformada[,(i+1)]*esquema[i]
  }
  matrizTransformada$total <- rowSums(matrizTransformada[, c(2:ncol(matrizTransformada))])
  colnames(matrizTransformada)<-gsub("http://purl.org/purchase2pay/individuals/","",colnames(matrizTransformada))
  return (matrizTransformada)
}

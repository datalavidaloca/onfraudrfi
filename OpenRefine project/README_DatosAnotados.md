#Acceso, transformación y explotación de datos abiertos sobre contrataciones públicas en el Ayuntamiento de Madrid
**Adaptación al TFM del trabajo de Web Semántica**
*Raul del Aguila*

##Consideraciones previas

El presente documento fue desarrollado en el contexto del trabajo final de la asignatura Web Semántica y Datos Enlazados. Este documento contiene la guía de anotación y publicación de los expedientes de contratos menores del Ayuntamiento de Madrid, realizado con motivo de dicha asignatura.

Estos datos serán utilizados posteriormente en la Prueba de Concepto de OnFraud. Por tanto, en lo relativo a la configuración del servidor es necesario referirse al fichero de configuración adjunto en este repositorio, y no al descrito en este documento. 

Asimismo, los contratos enlazados están cargados en la imagen docker que actúa como servidor semántico en la prueba de concepto.

En lo relativo a la publicación de estos datos y a la configuración del servidor, es necesario referirse al fichero CONFIG_ENTORNO del presente repositorio. Asimismo, el proyecto Open Refine también está disponible en el citado repositorio. 



##Introducción

Como recoge la [Open Knowledge Foundation](https://okfn.org/about/), los datos abiertos son una pieza fundamental para la transparencia en las organizaciones.  La transparencia implica que un ciudadano pueda ejercer el control sobre las acciones de los gobiernos e instituciones mediante al análisis de los datos que éstas han publicado. Es, por tanto, una buena base para comenzar la lucha contra el fraude.
El fraude implica engaño. La voluntad de ocultar una acción deshonesta para con la organización o con la sociedad es, sin duda, una de las principales características de quien perpetra un fraude.
Tras la reciente crisis económica, los ciudadanos, empresas y agentes sociales han tomado conciencia del grave impacto financiero (técnicamente hablando, perjuicio económico) que el fraude puede ocasionar a organizaciones públicas y privadas, así como administraciones públicas; el fraude es, sin duda, un ataque directo a la cuenta de resultados de cualquier tipo de organización que menoscaba además el impacto reputacional y la confianza de consumidores y ciudadanos en estas organizaciones.
Sin embargo, la lucha contra el fraude no es algo nuevo:
Instituciones como la ACFE (Association of Certified Fraud Examiners) llevan formando y elaborando estándares de facto en lo relativo a procedimientos y tipología de fraude. 
Existen firmas de servicios profesionales que tienen departamentos especializados en la investigación y prevención de fraude como, por ejemplo, EY. Estas firmas utilizan procedimientos de análisis financiero, social network analytics, o minería de datos, entre otros, en el contexto de grandes investigaciones de fraude.
A nivel regulatorio, normativas como la FCPA americana o la UK Bribery Act británica que establecen sanciones para todas aquellas organizaciones que no hayan puesto los medios necesarios para prevenir la corrupción. A nivel nacional, las sucesivas reformas del Código Penal español desde el 2010 se han ido alineando con la normativa internacional, llegando la jurisprudencia nacional a establecer como requerimiento la existencia de controles informáticos para dicha prevención.
La adjudicación de contratos es una de las principales áreas de análisis en la lucha contra el fraude. El presente  trabajo tiene por objeto el acceso a una fuente de datos abiertos relacionadas con la adjudicación de contratos por parte de una administración pública, su transformación de acuerdo con un modelo ontológico, su publicación y un ejemplo de explotación de estos datos.
La explotación de estos datos se realizará de acuerdo a la búsqueda de patrones de fraude sencillos. En cualquier caso, las conclusiones que se obtengan del análisis de estos datos deben considerarse como una aproximación académica y su finalidad no es la conclusión sobre la eventual existencia de fraude, aspecto para lo que se deberían tener en cuenta otras fuentes de datos (alta de proveedores, procedimientos de licitación, tesorería, entre otros) y realizarse procedimientos de muy diversa naturaleza (análisis forense, entrevistas formales, etc).

##Fuente de datos

Se ha accedido al portal de datos abiertos del [Ayuntamiento de Madrid](https://datos.madrid.es/)  y se ha accedido a los datos contratos menores del [ejercicio 201/7](http://datos.madrid.es/portal/site/egob/menuitem.c05c1f754a33a9fbe4b2e4b284f1a5a0/?vgnextoid=139afaf464830510VgnVCM1000000b205a0aRCRD&vgnextchannel=374512b9ace9f310VgnVCM100000171f5a0aRCRD&vgnextfmt=default) . La imagen siguiente contiene una captura de pantalla de esta página web, accedida el 9 de enero de 2018

![captura de pantalla de datos abiertos](https://github.com/datalavidaloca/Curso2017-2018/blob/master/RaulDelAguila/imagenes/capt1.png?raw=true)

En este sentido, los datos que vamos a descargar, enlazar y explotar son los relativos a los contratos *menores* contratados por el Ayuntamiento de Madrid.
En el contexto de la contratación de carácter público y de acuerdo con la [Ley de Contratos del Sector Público](Ley de Contratos del Sector Público 9/2017: https://www.boe.es/boe/dias/2017/11/09/pdfs/BOE-A-2017-12902.pdf)  los contratos menores son aquellos inferiores a 50.000, si son contratos de obras,  ó 18.000 euros si son contratos de servicios o suministros (importes sin IVA). Es necesario señalar que la si bien la última modificación de la Ley es de noviembre de 2017, la versión vigente en el momento de las contrataciones recogidas en el fichero que contiene los contratos menores la aprobada a través de [Real Decreto 
tivo 3/2011](://www.boe.es/boe/dias/2011/11/16/pdfs/BOE-A-2011-17887.pdf).
En cuanto a la aprobación y tramitación del expediente, la citada Ley recoge que  en los contratos menores la tramitación del expediente sólo exigirá la aprobación del gasto y la incorporación al mismo de la factura correspondiente, que deberá reunir los requisitos que las normas de desarrollo de esta Ley establezcan; en el caso del contrato menor de obras, deberá añadirse, además, el presupuesto de las obras, sin perjuicio de que deba existir el correspondiente proyecto cuando normas específicas así lo requieran el trabajo afecte a la estabilidad, seguridad o estanqueidad de la obra.
Este aspecto es muy relevante. Un típico patrón de fraude a analizar es la fragmentación de contratos, mediante sucesivos contratos, novaciones o renovaciones de los mismos,  con objeto de no tener que pasar los requerimientos establecidos para contratos no menores, en concreto, la obligatoriedad de establecer un proceso licitador.
En cuanto a la contratación, de acuerdo con el citado Real Decreto, los contratos menores podrán adjudicarse directamente a cualquier empresario con capacidad de obrar y que cuente con la habilitación profesional necesaria para realizar la prestación, cumpliendo con las normas que se acaban de describir.
Una vez expuesta la fuente de datos que vamos a analizar y adelantado el objeto posterior del análisis, pasamos a describir los datos:
###Descripción de los datos
La web desde la que se ha descargado el catálogo de datos, no especifica el formato de cada uno de los ficheros.  La única descripción que recoge de los datos es la siguiente:
>Contratos menores contabilizados en 2017: listado generado a 15/12/2017, diferenciando los contratos menores que fueron aprobados y contabilizados el año 2016 y los que tras la carga de plurianuales tienen gasto en 2017, de los generados a partir del 1 de enero de 2017 (estos últimos tendrán vacío el campo "Documento 2016").

Dado que no se dispone de un descriptivo de los datos, es necesario realizar un análisis en profunidad sobre los mismos. Si bien se puede utilizar OpenRefine para realizar el análisis de los datos, en vez de utilizar esta herramienta para realizar este análisis descriptivo inicial se ha utilizado un script SQL que analiza la estructura, rango y valores de las tablas. El código de este script puede consultarse en el siguiente [enlace](https://raw.githubusercontent.com/datalavidaloca/Curso2017-2018/master/RaulDelAguila/profiling.sql)

El resultado del análisis se encuentra recogido en la [tabla 1](https://github.com/datalavidaloca/Curso2017-2018/RaulDelAguila/tablas/Tabla 1 Análisis de los datos.docx).

Como se puede comprobar en la tabla 1, existen muchas observaciones que no disponen de datos y otros con una frecuencia de aparición muy baja que, con independencia de la connotación estadística de los mismos, podrían considerarse como anómalos.
En el contexto de una investigación o análisis de fraude, no se deben alterar los datos recibidos por el cliente. Es decir, se ha de trabajar con esos datos aunque la calidad no sea adecuada. Por ejemplo, en el caso de datos con valor nulo, la gestión de esos datos debe realizarse durante la explotación y la incidencia podría devenir en una recomendación sobre calidad (dado que esas ineficiencias pueden ser aprovechadas para la comisión de un fraude), pero no se ha de suponer que se debe modificar esos datos previo al análisis.
Asimismo, este primer análisis ya nos ofrece información muy rica sobre los datos que se han de analizar. Por ejemplo, se puede comprobar que la mayor parte de los contratos menores se han concedido en el ámbito de la Salud. Asimismo, los contratos de Servicios y Suministros suponen la práctica totalidad de los contratos seleccionados.
Otra conclusión preliminar que se puede obtener es que  no existen muchos contratos que sean cercanos al valor del importe máximo o mínimo. Esto ya es un primer indicador de una posible irregularidad, esto es, valores extremos de importe justo por debajo del límite de contratación.
Cabe señalar según se puede comprobar en la tabla 1, que no existen valores únicos que sirvan como identificador. Así, por ejemplo, el expediente 115/2017/01534 aparece 30 veces, con diferente descripción.
Una vez que ya hemos accedido e inspeccionado nuestros datos, vamos a analizar la licencia de los datos y, en caso de poder ser procesados y analizados, trabajaremos sobre los vocabularios formalizados u ontologías que pueden representar estos datos para su publicación y explotación posterior.

##Licencia de los datos

No existe especificación de licencia recogida en la página Web del Ayuntamiento de Madrid. No obstante lo anterior, sí especifica [la página web](http://datos.madrid.es/portal/site/egob/menuitem.400a817358ce98c34e937436a8a409a0/?vgnextoid=eba412b9ace9f310VgnVCM100000171f5a0aRCRD&vgnextchannel=eba412b9ace9f310VgnVCM100000171f5a0aRCRD&vgnextfmt=default) las condiciones de uso de los [datos](http://datos.madrid.es/portal/site/egob/menuitem.3efdb29b813ad8241e830cc2a8a409a0/?vgnextoid=108804d4aab90410VgnVCM100000171f5a0aRCRD&vgnextchannel=b4c412b9ace9f310VgnVCM100000171f5a0aRCRD&vgnextfmt=default). Las características más relevantes son:

>(...)
>Autorización de reutilización y cesión no exclusiva de derechos de propiedad intelectual

>Las presentes condiciones generales permiten la reutilización de los documentos y datos sometidos a ellas para fines comerciales y no comerciales.  (...). Por ejemplo, la reutilización autorizada incluye actividades como la copia, difusión, modificación, adaptación, extracción, reordenación y combinación de la información.
(...)
>Esta autorización conlleva, asimismo, la cesión gratuita y no exclusiva de los derechos de propiedad intelectual, en su caso, correspondientes a tales documentos, autorizándose la realización de actividades de reproducción, distribución, comunicación pública o transformación, necesarias para desarrollar la actividad de reutilización autorizada, en cualquier modalidad y bajo cualquier formato, para todo el mundo y por el plazo máximo permitido por la Ley.
>(...)
>Condiciones generales para la reutilización
Son de aplicación las siguientes condiciones generales para la reutilización de los documentos sometidos a ellas:
1.	Está prohibido desnaturalizar el sentido de la información.
2.	Debe citarse la fuente de los documentos objeto de la reutilización. Esta cita podrá realizarse de la siguiente manera: "Origen de los datos: Ayuntamiento de Madrid (o, en su caso, órgano administrativo, organismo o entidad de que se trate)"
3.	Debe mencionarse la fecha de la última actualización de los documentos objeto de la reutilización, siempre y cuando estuviera incluida en el documento original.
4.	No se podrá indicar, insinuar o sugerir que el Ayuntamiento de Madrid participa, patrocina o apoya la reutilización que se lleve a cabo con la información.
5.	Deben conservarse, no alterarse ni suprimirse los metadatos sobre la fecha de actualización y las condiciones de reutilización aplicables incluidos, en su caso, en 
6.	el documento puesto a disposición para su reutilización.
6.	En caso de información anonimizada por protección de datos personales u otros motivos, está expresamente prohibido realizar labores de re-identificación de personas a partir de estos datos y otras fuentes de datos e información posibles, pasadas, actuales o futuras.
>Exclusión de responsabilidad
>(...)
>El Ayuntamiento de Madrid no será responsable del uso que de su información hagan las personas y/o empresas que reutilicen datos, ni tampoco de los daños sufridos o pérdidas económicas que, de forma directa o indirecta, produzcan o puedan producir perjuicios económicos, materiales o sobre datos, provocados por el uso de la información reutilizada.
>(...)
>Responsabilidad de las personas y/o empresas que reutilicen datos
El agente reutilizador se halla sometido a la normativa aplicable en materia de reutilización de la información del sector público, incluyendo el régimen sancionador previsto en el artículo 11 de la Ley 37/2007, de 16 de noviembre, sobre Reutilización de la Información del Sector Público.
>(...)

Cabe señalar que si bien podemos reutilizar estos datos, estamos sujetos a la Ley 37/2007, de 16 de noviembre, sobre Reutilización de la Información del Sector Público. En concreto, es necesario señalar que la citada Ley establece que:
>La utilización de los conjuntos de datos se realizará por parte de los usuarios o agentes de la reutilización bajo su responsabilidad y riesgo, correspondiéndoles en exclusiva a ellos responder frente a terceros por daños que pudieran derivarse de ella

##Transformación de los datos
Antes de implementar o reutilizar una ontología, vamos a transformar los datos disponibles de forma que se adapten mejor a la representación del dominio que se pretende modelar (contratos menores en el contexto de una adjudicación pública).
###IVA
Los importes recogidos en esta tabla son superiores a los 50.000 euros. Esto quiere decir que el importe recogido en la tabla lleva incluido el IVA.
La primera transformación, esto es, diferenciar los importes sin IVA,  la vamos a realizar con OpenRefine porque sí incluye conocimiento que debería representarse en el dominio de los contratos menores (recuérdese que los límites de aprobación están en 50 y 18 mil euros). 
Para ello, añadiremos una nueva columna sobre la columna de Importe a la que llamaramoe `Importe sin IVA` con la siguiente expresión grel: `value*0,79` (es decir, estamos asumiendo un IVA del 21%)
###Unicidad de los datos: creación de la columna 'Expediente'
Como se ha expuesto con anterioridad, no existe una columna a la que se pueda considerar útil como PK, esto es, no existe un valor único por fila. Esto es debido a que en realidad, hay expedientes que están presentes en más de un registro. 

Para resolver este problema, se han realizado las siguientes acciones:
1. Reordenación de las columnas de forma que `Nº de Expediente` esté la primera e identificación de registros mediante una operación  `Blank Down`.
2. Creación de una nueva columna, `Expediente`, basada en la anterior. Sobre esta columna se modifica el Nº de Expediente a una expresión del tipo `Exp-NºExpediente-Nºde fila en registro` La expresión grel utilizada es la siguiente: `'Exp-'+cells["Nº expediente"].value.replace(".","").replace("/","-").replace("E11","")+"-"+(row.index-row.record.fromRowIndex+1)`

### Otras modificaciones
Como se ha expuesto con anterioridad, las transformaciones de datos han de limitarse a las mínimas indispensables para permitir la explotación de los datos en el contexto de un análisis de fraude. Por este motivo, el resto de las transformaciones fueron para reconocer fechas o números en el resto de columnas.
##Publicación de los datos
Una vez accedida nuestra fuente de datos, y tras realizar las modificaciones oportunas para su posterior tratamiento, es necesario ocuparse de la publicación de los mismos.

A continuación, detallamos las diferentes acciones realizadas en este punto

###Identificación y reutilización de recursos de conocimiento

Para publicar y enlazar los datos abiertos, es necesario utilizar un vocabulario formalizado que sea capaz de representar los mismos en el formato del LOD. 
Para ello, vamos a analizar en primer lugar los requisitos que nuestra ontología debería resolver y posteriormente identificaremos aquellas ontologías ya existentes que pueden ayudarnos a representar nuestros datos.
Es necesario señalar en este punto que si bien podría desarrollarse un vocabulario formalizado u ontología, este trabajo no partirá de la necesidad de desarrollar dicho vocabulario sino que, en caso de encontrar un vocabulario que se pueda reutilizar, se utilizará ese vocabulario. El fin de la utilización de una ontología para este trabajo el de publicar datos sobre contratos y su posterior explotación.
A tal fin, basaremos nuestra selección y eventual desarrollo en las directrices establecidos en la metodología NeOn. Esta metodología encuentra entre sus fundamentos la reutilización de recursos de conocimiento, tanto ontológicos como no ontológicos.

####Especificación de requisitos

Mediante la especificación de requisitos ontológicos se ha pretendido formalizar el alcance de la ontología a desarrollar y los requisitos funcionales o de conocimiento y no funcionales (como usos previstos o lenguajes deimplementación) que la ontología debe satisfacer.
Ésta, es la parte más relevante de esta actividad dado que va a determinar en buena medida la necesidad de reutilización de recursos de conocimiento (si, por ejemplo, los términos identificados fueran muy específicos), qué escenarios de la metodología NeOn será necesario aplicar y facilita el diseño de la ontología.
En lo relativo a los requisitos funcionales se ha intentado obtener la mayor información posible sobre el dominio de la contratación pública y, más concretamente, de los contratos menores.
En este apartado, se ha procedido a elaborar una especificación de requisitos ontológicos basada en las directrices de la metodología NeOn, de acuerdo con las directrices establecidas en [Suárez Figueroa; 2010].
Cabe señalar que puesto que en el desarrollo de esta ontología no se disponía de usuarios expertos en los diferentes dominios, las tareas se han visto simplificadas, si bien debiera realizarse conjuntamente con usuarios expertos en el dominio. Por este motivo, la definición de los requisitos ontológicos se ha basado en el conocimiento que se disponía sobre adjudicación de contratación pública, así como en la explotación que se le pretende dar a la ontología, esto es, la identificación de patrones de fraude.
Asimismo y dado que el dominio es en realidad un conjunto muy limitado del dominio de las adjudicaciones de contratos públicos, se ha simplificado la elaboración del DERO (Documento de Especificación de Requisitos Ontológicos). De este modo, éstos se van a representar mediante una lista de preguntas de competencia.

Se pasa a detallar la especificación de requisitos:

**Propósito de la ontología**: La ontología debe modelar a nivel general contrataciones del sector público y, en concreto, los contratos menores.
**Alcance**: contratos menores, esto es, aquellos adjudicados sin necesidad de una fase previa de publicación y licitación.
**Lenguaje**: la ontología debe estar implementada en cualquier sintaxis basada en RDF.
**Usuarios previstos**: en el ámbito educativo, profesores y estudiantes, así como cualquier otra persona que desee utilizarla para analizar la contratación pública de las administraciones en el contexto de la web de datos enlazados.
**Usos previstos**: Enlazado y análisis de contratos menores en el contexto de la publicación de datos abiertos por parte de las administraciones públicas.
**Requisitos no funcionales**: la ontología debe reutilizar recursos de conocimiento ontológico. Deberá soportar el inglés y el español.
***REQUISITOS FUNCIONALES (PREGUNTAS DE COMPETENCIA)***
1.	¿Qué son los contratos menores?
a.	Los contratos menores son aquellos que pueden adjudicarse directamente a cualquier proveedor. 
2.	¿Quién realiza estas contrataciones?
	a) Las administraciones y organizaciones públicas con autoridad de contratación que desde una administración (como Ayuntamientos) u organización pública (empresas públicas) tengan dicha autoridad.
	b) Concretamente, estos contratos pueden venir de un departamento o unidad gestora.
3.	¿Quiénes son los proveedores a los que se puede contratar?
	a) Las empresas o profesional con capacidad de obrar que cuente con la habilitación profesional necesaria para realizar la prestación.
4.	¿Cuál es el límite de la cuantía que adjudicar mediante un contrato menor? 
	a) Se consideran contratos menores: - cuando se trate de contratos de obras de importe inferior a 50.000 euros (IVA no incluido) - cuando se trate de contratos de suministros o contratos servicios de importe inferior a 18.000 euros (IVA no incluido)
5.	¿Cómo se tramita el expediente de un contrato menor?
	a) Mediante la aprobación del gasto y la incorporación de la factura
	b) En el caso de los contratos de obras, los permisos y presupuesto de obras.
6.	¿Debe coincidir la fecha de facturación con la fecha de aprobación?
	a) No necesariamente.
7.	Cuál es la fecha de inicio de los servicios
	a) 	A partir de la fecha de aprobación
8.	¿Debe coincidir la fecha de aprobación con la de contabilización?
	a) No
9.	¿Cuál es la duración máxima de un contrato menor?
	a) Un año, no prorrogable

Nótese que con esta información ya podemos modelar la casuística de un contrato menor. Se podrían añadir más preguntas de competencia como, por ejemplo, cómo se identifica a un proveedor (con un NIF y un nombre), pero para nuestro contexto es más que suficiente.

Como se puede observar, el vocabulario identificado en las preguntas de competencia es bastante específico de los contratos menores. 

Por otro lado, de acuerdo con NeOn, es conveniente identificar los términos utilizados en las preguntas de competencia para tener una primera aproximación de los términos que han de representar nuestros vocabularios enlazados. A tal fin, se ha utilizado el portal LinguaKit. El resultado de esta consulta es el que se puede comprobar en las tablas del siguiente [documento](https://github.com/datalavidaloca/Curso2017-2018/blob/master/RaulDelAguila/tablas/Tabla%203-%20Frecuencia%20de%20t%C3%A9rminos.docx)

Cabe señalar que se debería utilizar un lexicón como EuroWordNet para la identificación de sinónimos y traducciones de cara a refinar más estos listados. Sin embargo y dado que los términos a publicar son escasos y no muy complicados, se ha decidido no utilizar esta herramienta.

###Identificación y selección de recursos ontológicos

Esta actividad comprende la búsqueda y reutilización de recursos de conocimiento, tanto ontológicos como no ontológicos. No obstante lo anterior, en este apartado se tratará la búsqueda y reutilización de recursos ontológicos
Cabe recordar que nuestro objetivo no es el desarrollo en sí de una ontología, sino el reutilizar esta ontología para un fin determinado que es el de publicar datos sobre contratos y su posterior explotación. Por tanto, la decisión en este punto no ha sido tanto qué ontologías reutilizar para el desarrollo de una nueva ontología, sino si es necesario desarrollar una ontología con las ya disponibles para realizar esta tarea de anotación y posterior explotación.
No obstante, el framework de selección de recursos de conocimiento establecido por Suárez Figueroa (2010) es aplicable para el objeto de este trabajo en este punto.
Asimismo, si bien se pueden reutilizar ontologías generales, como SKOS, este trabajo se ha centrado en la identificación de ontologías del dominio de contratación pública y pagos.
La división entre nombres, adjetivos y verbos obtenida en el DERO facilita esta labor y  el diseño posterior de la ontología al identificar en una primera instancia nombres, relaciones (object properties) y características (data properties) que deberán ser identificadas en nuestra ontología. Sin embargo, ésta debe tomarse como una primera aproximación dado que, por ejemplo, no todos los sustantivos son entidades y pueden ser atributos.
Gracias a esta primera aproximación a la ontología necesaria, se pueden identificar aquellos conceptos que desean ser representados en la misma. En este sentido, la elaboración del glosario de términos es de gran ayuda, también, para la búsqueda y selección de ontologías. Se ha realizado la búsqueda fundamentalmente sobre LinkedOpenVocabularies (LOV) y sobre Google.
Cabe señalar que si bien existen otros buscadores, como Swoogle o Schema.org, finalmente se utilizaron los dos anteriores dado que fueron los que mejores y más resultados proporcionaban.
A continuación se detallan las ontologías candidatas identificadas.

| Fuente | Búsqueda | Resultado |       URI           | Comentarios | 
|--------|----------|-----------|---------------------|-------------|
|   LOV  |  Public contract      | PC |http://lov.okfn.org/dataset/lov/vocabs/pc|Ontología para representar contratos públicos|
|   LOV  |    Procurement    | PPROC | http://lov.okfn.org/dataset/lov/vocabs?q=procurement|Ontología diseñada para representar y enlazar datos de sobre contratos públicos.|
|   LOV  | Payment    | PAY |http://lov.okfn.org/dataset/lov/vocabs/pay|Representa una ontología de tarea para representar los pagos.|
|   LOV  | Procurement | ORGES |http://lov.okfn.org/dataset/lov/vocabs/orges|Extensión de la ontología ORG para representar organizaciones públicas españolas.|
|   LOV  | Tender | loted |http://lov.okfn.org/dataset/lov/vocabs/loted|Vocabulario orientado a la publicación de ofertas y licitaciones|

En este punto, se han evaluado las ontologías respecto al marco establecido por Suárez Figueroa (2010) en su Tesis Doctoral, a saber,  :
1.	Comprobar si el alcance y propósito establecidos en el DERO son similares a los de la ontología candidata
2.	Comprobar si los requisitos no funcionales establecidos en el DERO se cubren en la ontología candidata
3.	Comprobar si los requisitos funcionales establecidos en el DERO se cubren con los conceptos de la ontología candidata:
	a)	Analizando si los términos esenciales de la nueva ontología aparecen en la ontología candidata.
	b)	Mediante medidas de precisión y exhaustividad de la terminología de la nueva ontología con los de la ontología.
4.	Determinando si la ontología candidata es capaz de responder las palabras de competencia.
5.	Asimismo, las ontologías a analizar han sido todas desarrolladas por instituciones o fuentes reconocidas.

Como punto de partida y de cara a facilitar la toma de decisiones sobre la ontología a reutilizar, NeOn propone realizar una tabla con cuatro criterios: alcance similar, propósito similar, requisitos funcionales y no funcionales cubiertos. Cada uno de estos cuatro criterios, toman un rango de valores (Sí, No, y Parcialmente en el caso de los requisitos o No Sabe en el caso de alcance y propósito) .

Según se especifica en [Suárez Figueroa; 2010], en el caso de que alguna ontología tuviera el valor “NO” en cuanto a alcance, propósito y requisitos funcionales, debería ser eliminada del criterio de decisión.

La siguiente tabla contiene el resultado de la evaluación:

|		 |     | Ontologías de dominio candidatas|
|--------|-----|---------------------------------|
|Criterio|Rango| PC | PPROC | PAY | ORGES | LOTED|
|Alcance similar|S/N/NS|	NS|S|NS|NS|NS|
|Propósito similar|S/N/NS|	NS|S|N|N|NS|
|Requisitos no funcionales cubiertos|S/Parcialmente/N|S|S|S|S|S|
|Requisitos funcionales cubiertos|	Si/Parcialmente/N|	|Parcialmente|Parcialmente|NO|NO|NO|


Como consecuencia de lo anterior, se ha decidido eliminar  PAY, ORGES y LOTED. Asimismo, las ontologías PC y PPROC son ambas ontologías candidatas para ser reutilizadas. NeOn propone una primera evaluación basada en los términos recogidos por ambas ontologías (incluso con medidas de precisión y exhaustividad), y en la cobertura de los mismos.
Se puede compprobar Ambas ontologías representan los conceptos recogidos en el DERO y son perfectamente capaz de ser utilizadas para realizar las anotaciones que debemos usar.  La selección de una u otra ontología sería un buen ejercicio teórico, pero dado que el conocimiento que debemos representar no es conceptualmente tan complejo y dado que ambas son capaces de representarlo, cualquier ejercicio a este respecto sería dotar de excesivo artificio a una decisión que debería tomarse por la utilidad y practicidad del recurso a reutilizar.
Por este motivo se ha optado por la selección de PPROC. Las razones de esta selección son las siguientes:
1.	PPROC está específicamente diseñada para para representar y enlazar datos de sobre contratos públicos.
2.	PPROC está basada, entre otras ontologías, en PC  o como GoodRelations en lo relativo esta últimaa la especificación del precio de los contratos. http://www.semantic-web-journal.net/system/files/swj895.pdf
3.	Asimismo PPROC está diseñada para dotar de transparencia a los procesos de licitación y adjudicación [Muñoz Soro et al; ]http://www.semantic-web-journal.net/system/files/swj1030.pdf
4.	Las diferentes ontologías han sido desarrolladas por comunidades de referencia y de hecho son accesibles a través de LOV.
5.	De este modo nuestro dataset incrementa su capacidad de interoperabilidad.
La licencia de PPROC es CC By  SA 4.0, lo que nos permite redistribuir la ontología bajo la misma licencia e incluir un enlace a la licencia.

###Estrategia de nombrado de recursos

Una vez que se ha decidido reutilizar esta ontología, se debe considerar la estrategia de nombrado de nuestros recursos. Por un lado, debemos respetar el modo de referenciar conceptos y propiedades de la ontología PPROC que vamos a reutilizar, a saber: 
http://contsem.unizar.es/def/sector-publico/pproc# Esto es lógico dado que normalmente se accederá de forma única a la ontología.

Por otro lado, para referenciar a los individuos, se ha seleccionado por utilizar la barra inclinada ('/'). Se ha seleccionado porque vamos a tener muchas instancias y además múltiples peticiones http, dado que consultaremos los individuos para su explotación.

Por otro lado, hemos utilizado un dominio para abstraer la localización concreta para abstraer los diferentes recursos de conocimiento del servidor concreto donde están alojados. La URI que se va a dar a las instancias seguirá la forma "http://purl.org/purchase2pay/individuals/".

###Transformación de datos enlazados a RDF

Mediante LODRefine, 1.0.3 se ha procedido a transformar los datos de acuerdo con el  modelo definido en la ontología PPROC [ver ejemplo](http://www.semantic-web-journal.net/system/files/swj1142.pdf).

Si bien PPROC es un modelo bastante más rico de lo que necesita el modelo de los contratos menores del Ayuntamiento de Madrid, como ya se ha expuesto, PPROC es capaz de adaptarse a las necesidades de publicación de nuestros datos y está diseñado para este fin.
PPROC define una jerarquía de contratos. Específicamente, no define un contrato menor de obra, suministro o servicio. Si nuestro dataset recogiera además de esta casuística otra tipología de contratos, como contratos marco, sería necesario extender la ontología añadiendo una restricción de dominio y modificando la taxonomía de los contratos para poder representar esta casuística.
Siendo el caso de que queremos transformar los datos para su posterior publicación y explotación, y puesto que únicamente tenemos una casuística de contratos, de cara a facilitar su interoperabilidad con otros datos enlazados de similares características (contratación pública) no se ha realizado esta transformación. Únicamente se ha añadido una etiqueta de descripción `dcterms:description`.
Asimismo, es necesario resaltar que en caso de haber sido necesaria esta transformación, habría sido necesario realizar una transformación más compleja que la que OpenRefine permite, es decir, habría que haberse realizado una transformación basada, por ejemplo, en JENA para poder asociar cada registro a una subclase concreta de la ontología.

El RDF generado está publicado en la web http://purl.org/purchase2pay/individuals/.

Asimismo, se puede comprobar la transformación en el proyecto de OpenRefine `Contratos-Menores.openrefine.tar.gz` accesible en este repositorio.

####Enlazado de datos

El enlazado de datos abiertos es un aspecto básico a la hora de poder facilitar un valor añadido a la explotación que podemos realizar de los mismos. De este modo, al asegurar que nuestros datos no están aislados, podemos mejorar la potencial interoperabilidad de nuestro dataset e incrementar los potenciales análisis que nuestra herramienta de prevención y detección de fraude pueda utilizar. 
En nuestro caso, hemos de pensar que nuestro dataset de forma explícita no ofrece un dato o variable que pueda ser utilizado para su enlazado posterior. Asimismo y como ya hemos expuesto con anterioridad, el dataset no describe la autoridad contratante, esto es, el Ayuntamiento de Madrid, sino los departamentos específicos.
Por esta razón se procedió a modelar, gracias a PPROC, la relación existente entre organización y departamento.
Asimismo, se hizo explícita la aparición del Ayuntamiento de Madrid. Esta es una entidad potencialmente enlazable a otros conjuntos de datos. De entre los conjuntos de datos disponibles, se seleccionó DBpedia para el enlazado de los mismos. . Gracias a la funcionalidad que dispone OpenRefine, se realizó el enlazado a DBPedia del Ayuntamiento de Madrid, a nivel de instancia.

###Publicación

Se ha utilizado dos servicios para publicar los datos:

+datahub.io: `http://datahub.io/datalavidaloca/contratos-menores-perfect-mayfly-72`
+ckan: `https://demo.ckan.org/dataset/348a657f-b18e-4798-9d66-ffcc156a50be/resource/3b0f1c0e-089f-42a6-9b79-2662359cccf5/download/contratos-menores.ttl`

A modo de ejemplo, se puede observar en la siguiente imagen ![imagen](https://github.com/datalavidaloca/Curso2017-2018/blob/master/RaulDelAguila/imagenes/capt3-datahub.png?raw=true) el proceso de publicación en datahub

Ninguno de estos dos servicios, proporciona un método de generar un vocabulario dcat para describir los datos de forma directa (ckan sí permite descargarse una extensión para ubuntu). Como no disponía de este ssoo y datahub parece no permitir la generación de este vocabulario, he usado el servicio de `entryscape.org` para generar un catálogo y un dataset en formato dcat.

El código original generado por este portal es el siguiente:

```
@prefix adms: <http://www.w3.org/ns/adms#> . 
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix odrs: <http://schema.theodi.org/odrs#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix dcterms: <http://purl.org/dc/terms/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix vcard: <http://www.w3.org/2006/vcard/ns#> .
@prefix store: <https://free.entryscape.com/store/> .
@prefix dcat: <http://www.w3.org/ns/dcat#> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix es: <http://entrystore.org/terms/> .
@prefix dc: <http://purl.org/dc/elements/1.1/> .


<https://free.entryscape.com/store/170/resource/2> a dcat:Catalog ;
	dcterms:title "Contratos-Menores"@es ;
	dcterms:description "Catálogo de datos de los contratos menores formalizados por el Ayto de Madrid en el ejercicio 2017"@es ;
	dcterms:publisher <https://free.entryscape.com/store/170/resource/1> ;
	dcat:dataset <https://free.entryscape.com/store/170/resource/3> ;
	dcterms:issued "2018-01-24"^^xsd:date ;
	dcterms:language <http://publications.europa.eu/resource/authority/language/SPA> ;
	dcterms:license <http://creativecommons.org/licenses/by-nc-sa/4.0/> ;
	foaf:homepage "http://purl.org/purchase2pay/individuals/" .

<https://free.entryscape.com/store/170/resource/1> a foaf:Agent ;
	foaf:name "Purchase2Pay publisher" .

<https://free.entryscape.com/store/170/resource/3> a dcat:Dataset ;
	dcterms:title "Contratos-Menores"@es ;
	dcterms:description "Contratos menores del Ayuntamiento de Madrid, a fecha 15-12-2017"@es ;
	dcterms:publisher <https://free.entryscape.com/store/170/resource/1> ;
	dcat:theme <http://publications.europa.eu/resource/authority/data-theme/GOVE> .

```
Dado que no tenemos una página web, hemos puesto como homepage la URL de los datos en lazados. En caso de ser una institución, se debería poner la página web de la institución.  


Como se puede comprobar, no indica nada de cómo se distribuyen los datos. Se podría complementar este código con el siguiente:

```
<https://free.entryscape.com/store/170/resource/3> a dcat:Dataset ;
	dcterms:title "Contratos-Menores"@es ;
	dcterms:description "Contratos menores del Ayuntamiento de Madrid, a fecha 15-12-2017"@es ;
	dcterms:publisher <https://free.entryscape.com/store/170/resource/1> ;
	dcat:theme <http://publications.europa.eu/resource/authority/data-theme/GOVE>;
	dcat:distribution <https://free.entryscape.com/store/170/resource/4>.
<https://free.entryscape.com/store/170/resource/4> a dcat:Distribution
	dcat:downloadURL: <http://datahub.io/datalavidaloca/contratos-menores-perfect-mayfly-72/r/contratos-menores-perfect-mayfly-72_zip.zip>;
    dct:title "Datos del dataset";
    dct:license:<http://creativecommons.org/licenses/by-nc-sa/4.0/>.
```
##Explotación
Se ha realizado un ejemplo de explotación de los datos para evaluar patrones de fragmentación de contratos menores. El ejemplo de aplicación se encuentra en el RMarkdown disponible en este repositorio.

Para poder realizar esta evaluación, se dio de alta un sparql endpoint basado en un servidor de app virtuoso sobre docker.

Los pasos llevados para poder realizar el setup de esta infraestructura fueron los siguientes:
`


***Inicio de la máquina docker***
`docker run -p 127.0.0.1:8890:8890 -p 127.0.0.1:1111:1111 -e DBA_PASSWORD="ROOT" -e SPARQL_UPDATE=true -e DEFAULT_GRAPH="http://purl.org/puchase2pay/individuals/" -v //DESKTOP-NCTP077/db:/data -d tenforce/virtuoso`

***Comprobamos que esté corriendo la máquina***
`docker ps`

***Carga de la tripleta*** 
`docker cp Contratos-Menores.ttl objective_jones:/usr/local/virtuoso-opensource/var/lib/virtuoso/db`

***Ejecutamos el terminal dentro de nuestro contenedor***
`docker exec -it objective_jones bash`

***Entramos en la consola de la BBDD***
`isql-v -U dba -P $DBA_PASSWORD`

***Cargamos el grafo en virtuoso***
`ld_dir('dumps', '*.ttl', 'http://purl.org/purchase2pay/individuals/');`

***Carga y ejecución***
`rdf_loader_run();`
 
```


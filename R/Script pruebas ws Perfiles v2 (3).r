## nombres variables: "Codigo_cliente",'Provincia_habitacion','Nivel_academico','Cant_propiedades_consolidado',
       ##                    'Mon_sal_liquido','Mon_sal_nominal','Num_dependientes','Num_edad_anos','flag_vehiculos','Genero')])

load("C:/Users/pmontenegro/Desktop/Pedro Montenegro M/Pedro Montenegro/casos-estad-sticos-1/R/basead2 (2).rdata")
load("C:/Users/pmontenegro/Desktop/Pedro Montenegro M/Pedro Montenegro/casos-estad-sticos-1/R/modelo2.Rdata")
load("C:/Users/pmontenegro/Desktop/Pedro Montenegro M/Pedro Montenegro/casos-estad-sticos-1/R/modelo3.Rdata")
load("C:/Users/pmontenegro/Desktop/Pedro Montenegro M/Pedro Montenegro/casos-estad-sticos-1/R/modelo4.Rdata")
baseAD2<- baseAD2 %>%
  select(Identificacion,
         Genero,
         Mon_sal_nominal,
         Mon_sal_liquido,
         Num_dependientes,
         Nivel_academico,
         flag_vehiculos,
         Cant_propiedades_consolidado,
         Num_edad_anos)
baseAD2$Mon_sal_nominal=ifelse(baseAD2$Mon_sal_nominal<=350000,'menor350',
                             ifelse(baseAD2$Mon_sal_nominal>350000 &baseAD2$Mon_sal_nominal<=420000,'de350a420',
                                    ifelse(baseAD2$Mon_sal_nominal>420000 & baseAD2$Mon_sal_nominal<=510000,'de420a510', 
                                           ifelse(baseAD2$Mon_sal_nominal>510000 & baseAD2$Mon_sal_nominal<=610000,'de510a610',
                                                  ifelse(baseAD2$Mon_sal_nominal>610000 & baseAD2$Mon_sal_nominal<=700000,'de610a700',
                                                         ifelse(baseAD2$Mon_sal_nominal>700000 &baseAD2$Mon_sal_nominal<=815000,'de700a815',
                                                                ifelse(baseAD2$Mon_sal_nominal>815000 & baseAD2$Mon_sal_nominal<=965000,'de815a965',
                                                                       ifelse(baseAD2$Mon_sal_nominal>965000 & baseAD2$Mon_sal_nominal<=1150000,'de965a1150'
                                                                              ,'mayor1150'))))))))




baseAD2$Cant_propiedades_consolidado=ifelse(baseAD2$Cant_propiedades_consolidado==0,'prop0',
                                            ifelse(baseAD2$Cant_propiedades_consolidado==1,'prop1','propmas2'))
#############
colnames(baseAD2)=c('Identificacion','genero','sal_bruto_cat','sal_liquido','num_dependientes_cat',
                    'nivel_academico','flag_vehiculos','Cant_propiedades_consolidado',
                    'edad')


baseAD2$num_dependientes_cat=ifelse(is.na(baseAD2$num_dependientes_cat)==TRUE,0,baseAD2$num_dependientes_cat)
baseAD2$num_dependientes_cat=as.numeric(baseAD2$num_dependientes_cat)
baseAD2$num_dependientes_cat=ifelse(baseAD2$num_dependientes_cat>=6,'mas_6',baseAD2$num_dependientes_cat)


baseAD2$num_dependientes_cat=as.character(baseAD2$num_dependientes_cat)

baseAD2$sal_liquido=ifelse(baseAD2$sal_liquido<=350000,'Menor 350',
                           ifelse(baseAD2$sal_liquido>350000 & baseAD2$sal_liquido<=600000,'De 350 a 600',
                                  ifelse(baseAD2$sal_liquido>600000 & baseAD2$sal_liquido<=1000000,'De 601 a 1 millon',
                                         ifelse(baseAD2$sal_liquido>1000000 &baseAD2$sal_liquido<=1500000,'De 1 millon a millon y medio',
                                                'Mayor de millon y medio'))))



baseAD2$nivel_academico= ifelse(baseAD2$nivel_academico=='PRIMARIA COMPLETA'|
                                  baseAD2$nivel_academico=='PRIMARIA INCOMPLETA','PRIMARIA',
                                ifelse(baseAD2$nivel_academico=='TECNICO'|
                                         baseAD2$nivel_academico=='TECNICO MEDIO'|
                                         baseAD2$nivel_academico=='DIPLOMADO','TECNICO',
                                       baseAD2$nivel_academico))

baseAD2$genero=ifelse(baseAD2$genero=='Sin Definir','Masculino',baseAD2$genero)

baseAD2$edad=ifelse(baseAD2$edad<=25,'Menor 25',
                    ifelse(baseAD2$edad>25 &baseAD2$edad<=35,'De 26 a 35',
                           ifelse(baseAD2$edad>35 & baseAD2$edad<=50,'De 36 a 50',
                                  'Mayor de 51') ))

baseAD2$flag_vehiculos=ifelse(is.na(baseAD2$flag_vehiculos)==FALSE,baseAD2$flag_vehiculos,'No tiene')
baseAD2$Cant_propiedades_consolidado=ifelse(is.na(baseAD2$Cant_propiedades_consolidado)==FALSE,baseAD2$Cant_propiedades_consolidado,'prop0')
baseAD2$PUNT_MOD21=predict(mod2,baseAD2)
baseAD2$PUNT_MOD31=predict(mod3,baseAD2)
baseAD2$PUNT_MOD41=predict(mod4,baseAD2)


baseAD2$puntaje3= baseAD2$PUNT_MOD21+baseAD2$PUNT_MOD31+baseAD2$PUNT_MOD41



###prueba puntos
baseAD2$perfil4=ifelse(baseAD2$puntaje3>=0.07293339,'5',
                       ifelse(baseAD2$puntaje3>=0.05453881 & baseAD2$puntaje3<0.07293339,'4',
                              ifelse(baseAD2$puntaje3>=0.03729671 & baseAD2$puntaje3<0.05453881,'3',
                                     ifelse(baseAD2$puntaje3<0.03729671 & baseAD2$puntaje3>0.02218088,'2',
                                            '1'))))

original=subset(baseAD2,baseAD2$Identificacion=='101960418')


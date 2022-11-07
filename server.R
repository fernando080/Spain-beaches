path = "env.R"
source(path, encoding = "UTF-8")

server <- function(input, output, session) {
    
    datasetInput <- reactive({
        data <- read.csv("www/data/playas_espana.csv", header = T, encoding = "UTF-8")
        data$Provincia <- stringi::stri_trans_general(data$Provincia, "Latin-ASCII")
        data$Comunidad_Autonoma <- stringi::stri_trans_general(data$Comunidad_Autonoma, "Latin-ASCII")
        data$Aparcamiento <- stringi::stri_trans_general(data$Aparcamiento, "Latin-ASCII")
        data$Aparcamiento_seguridad <- stringi::stri_trans_general(data$Aparcamiento_seguridad, "Latin-ASCII")
        data$Aparcamiento_num_plazas <- stringi::stri_trans_general(data$Aparcamiento_num_plazas, "Latin-ASCII")
        data$Grado_urbanizacion <- stringi::stri_trans_general(data$Grado_urbanizacion, "Latin-ASCII")
        data$Grado_ocupacion <- stringi::stri_trans_general(data$Grado_ocupacion, "Latin-ASCII")
        data
    })
    
    output$mymap <- renderLeaflet({
        
        m <- leaflet() %>% setView(lat  = 40.416775, lng = -3.703790, zoom = 4) %>% 
            addTiles() %>% 
            addMarkers(data = datasetInput(), lng = ~Coordenada_X, lat = ~Coordenada_Y)
        
    })
    
    output$Hospi <- renderPlot({
        
        data <- datasetInput()
        data_hospi <- data[!(is.na(data$Distancia_Hospital)), ]
        data_hospi$Comunidad_Autonoma = as.factor(data_hospi$Comunidad_Autonoma)
        
        data_hospi <- levels_change_CCAA('Comunidad_Autonoma', data_hospi)
        
        med_dis_hop_prov <- aggregate(round(data_hospi$Distancia_Hospital
                                            , 2),
                                      list(data_hospi$Provincia, 
                                           data_hospi$Comunidad_Autonoma), 
                                      mean)
        colnames(med_dis_hop_prov) <- c('Provincia', 'Comunidad', 'value')
        med_dis_hop_prov$value <- round(med_dis_hop_prov$value, 2)
        
        ggplot(med_dis_hop_prov, aes(fill = Provincia, y = value, 
                                     x = Comunidad, label = value)) + 
            geom_bar(position="stack", stat="identity") +
            geom_text(size = 6, position = position_stack(vjust = 0.5)) +
            scale_fill_viridis(discrete = T) +
            ggtitle("Distance to Hospital Study by Province and Community") +
            theme(plot.title = element_text(size = 20, face = "bold")) +
            xlab("")
    })
    
    
    output$Dep <- renderPlot({
        
        data <- datasetInput()
        data_dep <- data[!(is.na(data$Distancia_Puerto_Deportivo)), ]
        data_dep$Comunidad_Autonoma = as.factor(data_dep$Comunidad_Autonoma)
        
        data_dep <- levels_change_CCAA('Comunidad_Autonoma', data_dep)
        
        med_dis_dep_prov <- aggregate(round(data_dep$Distancia_Puerto_Deportivo,
                                            2),
                                      list(data_dep$Provincia, 
                                           data_dep$Comunidad_Autonoma), 
                                      mean)
        colnames(med_dis_dep_prov) <- c('Provincia', 'Comunidad', 'value')
        med_dis_dep_prov$value <- round(med_dis_dep_prov$value, 2)
        
        ggplot(med_dis_dep_prov, aes(fill = Provincia, y = value, 
                                     x = Comunidad, label = value)) + 
            geom_bar(position="stack", stat="identity") +
            geom_text(size = 6, position = position_stack(vjust = 0.5)) +
            scale_fill_viridis(discrete = T) +
            ggtitle("Study Distance to Sports Center by Province and Community") +
            theme(plot.title = element_text(size = 20, face = "bold")) +
            xlab("")
        
    })
    
    dataTApa <- reactive({
        
        data_par <- datasetInput()
        
        data_par <- data_par[, c('Provincia','Comunidad_Autonoma','Aparcamiento',
                                  'Aparcamiento_seguridad', 'Aparcamiento_num_plazas')]
        
    })
    
    output$apar1 <- renderPlot({
        
        dataTApa = dataTApa()
        
        g1 = ggplot(dataTApa, aes(Provincia , ..count.., fill = Aparcamiento)) +
            geom_bar()  + 
            coord_flip() +
            ggtitle("Beaches parking by province and community") + 
            theme(plot.title = element_text(size = 18, face = "bold")) +
            facet_grid(Comunidad_Autonoma ~ ., scales = "free", space = "free")
        
        g2 = ggplot(dataTApa, aes(Provincia,..count..,fill=Aparcamiento_seguridad)) +
            geom_bar()  + 
            coord_flip() +
            ggtitle("Beach Security Parking by Province and Community") + 
            theme(plot.title = element_text(size = 18, face = "bold")) +
            facet_grid(Comunidad_Autonoma ~ ., scales = "free", space = "free")
        
        
        grid.arrange(g1, g2, ncol = 1, nrow = 2)
    },height = 1000)
    
    
    output$apar2 <- renderPlot({
        
        ggplot(dataTApa(), aes(x = Provincia, y = ..count..,
                             fill=Aparcamiento_num_plazas)) +
            geom_bar()  + 
            coord_flip() +
            ggtitle("Parking Number of Spaces by Province and Community") + 
            theme(plot.title = element_text(size = 18, face = "bold")) +
            facet_grid(Comunidad_Autonoma ~ ., scales = "free", space = "free")
        
    },height = 1000)
    
    dataTUrb <- reactive({
        
        data_urb <- datasetInput()[, c('Provincia','Comunidad_Autonoma',
                                  'Grado_urbanizacion')]
        data_urb <- data_urb[order(data_urb$Comunidad_Autonoma),]
        
    })
    
    output$urb1a <- renderPlot({
        
        dataTUrb = dataTUrb()
        dataTUrb$Comunidad_Autonoma = as.factor(dataTUrb$Comunidad_Autonoma)
        
        Com_Aut = levels(dataTUrb$Comunidad_Autonoma)
        l = length(Com_Aut)
        
        plot_list = list()
        
        for (i in 1:l) {
            df = dataTUrb[dataTUrb$Comunidad_Autonoma == Com_Aut[i],]
            df$Provincia = as.factor(as.character(df$Provincia))
            
            data.m = melt(table(df[,c(1,3)])) 
            names(data.m)[3] = "count"
            
            # calculate percentage:
            m1 = ddply(data.m, .(Provincia), summarize, ratio=count/sum(count))
            
            #order data frame (needed to comply with percentage column):
            m2 = data.m[order(data.m$Provincia),]
            
            # combine them:
            mydf = data.frame(m2,ratio=m1$ratio)
            
            # create bar plot
            mydf = ddply(mydf,.(Provincia),transform,position=cumsum(count)-0.5*count) 
            mydf = mydf[!(is.nan(mydf$ratio)),]
            
            p = ggplot(mydf, aes(x="", y=ratio, group=Grado_urbanizacion, 
                                 color=Grado_urbanizacion, fill=Grado_urbanizacion)) + 
                geom_bar(width = 1,color = "black",stat = "identity") +
                coord_polar(theta = "y", start=0) +
                facet_wrap(~Provincia)+ 
                ggtitle(toupper(Com_Aut[i])) +
                xlab(" ") + 
                ylab(" ") +
                theme(axis.text = element_blank(),
                      axis.ticks = element_blank(),
                      panel.grid  = element_blank(),
                      plot.title = element_text(size = 14,
                                                face = "bold",
                                                hjust = 0.5,
                                                vjust = 1),
                      panel.border = element_rect(colour = "black",fill=NA,size=1))+
                force_panelsizes(rows = unit(1.5, "in"),
                                 cols = unit(1.125, "in"))
            
            plot_list[[i]] = p
        }
        
        do.call("grid.arrange", c(plot_list, ncol=4))
    },height = 1000)
    
    dataTOcu <- reactive({
        
        data_ocu <- datasetInput()[, c('Provincia','Comunidad_Autonoma',
                                       'Grado_ocupacion')]
        data_ocu <- data_ocu[order(data_ocu$Comunidad_Autonoma),]
        
    })
    
    output$ocu1a <- renderPlot({
        
        dataTOcu = dataTOcu()
        dataTOcu$Comunidad_Autonoma = as.factor(dataTOcu$Comunidad_Autonoma)
        
        
        Com_Aut = levels(dataTOcu$Comunidad_Autonoma)
        l = length(Com_Aut)
        
        plot_list = list()
        
        for (i in 1:l) {
            df = dataTOcu[dataTOcu$Comunidad_Autonoma == Com_Aut[i],]
            df$Provincia = as.factor(as.character(df$Provincia))
            
            data.m = melt(table(df[,c(1,3)])) 
            names(data.m)[3] = "count"
            
            # calculate percentage:
            m1 = ddply(data.m, .(Provincia), summarize, ratio=count/sum(count))
            
            #order data frame (needed to comply with percentage column):
            m2 = data.m[order(data.m$Provincia),]
            
            # combine them:
            mydf = data.frame(m2,ratio=m1$ratio)
            
            # create bar plot
            mydf = ddply(mydf,.(Provincia),transform,position=cumsum(count)-0.5*count) 
            mydf = mydf[!(is.nan(mydf$ratio)),]
            
            p = ggplot(mydf, aes(x="", y=ratio, group=Grado_ocupacion, 
                                 color=Grado_ocupacion, fill=Grado_ocupacion)) + 
                geom_bar(width = 1,color = "black",stat = "identity") +
                coord_polar(theta = "y", start=0) +
                facet_wrap(~Provincia)+ 
                ggtitle(toupper(Com_Aut[i])) +
                xlab(" ") + 
                ylab(" ") +
                theme(axis.text = element_blank(),
                      axis.ticks = element_blank(),
                      panel.grid  = element_blank(),
                      plot.title = element_text(size = 14,
                                                face = "bold",
                                                hjust = 0.5,
                                                vjust = 1),
                      panel.border = element_rect(colour = "black",fill=NA,size=1))+
                force_panelsizes(rows = unit(1.5, "in"),
                                 cols = unit(1.125, "in"))
            
            plot_list[[i]] = p
        }
        
        do.call("grid.arrange", c(plot_list, ncol=4))
    },height = 1000)
    
    dataTwid <- reactive({
        data_anc <- datasetInput()[, c('Provincia','Comunidad_Autonoma',
                                  'Anchura')]
        data_anc <- data_anc[!((data_anc$Anchura =="") | (is.nan(data_anc$Anchura))), ]
        data_anc$Comunidad_Autonoma = as.factor(data_anc$Comunidad_Autonoma)
        
        data_anc = levels_change_CCAA('Comunidad_Autonoma', data_anc)
    })
    
    output$width <- renderPlot({
        
        g1 = ggplot(dataTwid(), aes(x = Provincia, y = Anchura, fill = Provincia)) +
            geom_violin(position=position_dodge(),draw_quantiles=c(0.5)) +
            geom_boxplot(width=0.1,
                         color="black",
                         position = position_dodge(width =0.9))+
            labs(title="Width Distribution in Provinces",
                 x="Provinces", 
                 y="Width of the Beaches") +
            theme(plot.title = element_text(size = 20, face = "bold")) +
            facet_grid(~Comunidad_Autonoma, scales = "free", space = "free")+
            theme(axis.text = element_blank(),
                  axis.ticks = element_blank(),
                  axis.text.x = element_text(angle = 90, hjust = 1, vjust=.1),
                  axis.title.y = element_text(size = 20, face = "bold"))
        
        g2 = ggplot(dataTwid(), aes(x = '', y = Anchura)) +
            geom_violin(position=position_dodge(),draw_quantiles=c(0.5))+
            labs(title="Total Width",
                 x="Total",
                 y = "") +
            theme(plot.title = element_text(size = 20, face = "bold")) 
        
        grid.arrange(g1, g2, nrow = 1, widths = c(3, 1))
        
    },height = 500)
    
    dataTlen <- reactive({
        data_lon <- datasetInput()[, c('Provincia','Comunidad_Autonoma',
                                  'Longitud')]
        data_lon <- data_lon[!((data_lon$Longitud =="") | (is.nan(data_lon$Longitud))), ]
        data_lon$Comunidad_Autonoma = as.factor(data_lon$Comunidad_Autonoma)
        
        data_lon = levels_change_CCAA('Comunidad_Autonoma', data_lon)
    })
    
    output$len <- renderPlot({
        
        g1 = ggplot(dataTlen(), aes(x = Provincia, y = Longitud, fill = Provincia)) +
            geom_violin(position=position_dodge(),draw_quantiles=c(0.5)) +
            geom_boxplot(width=0.1,
                         color="black",
                         position = position_dodge(width =0.9))+
            labs(title="Distribution of Length in Provinces",
                 x="Provinces", 
                 y="Length of Beaches") +
            theme(plot.title = element_text(size = 20, face = "bold")) +
            facet_grid(~Comunidad_Autonoma, scales = "free", space = "free")+
            theme(axis.text = element_blank(),
                  axis.ticks = element_blank(),
                  axis.text.x = element_text(angle = 90, hjust = 1, vjust=.1),
                  axis.title.y = element_text(size = 20, face = "bold"))
        
        g2 = ggplot(dataTlen(), aes(x = '', y = Longitud)) +
            geom_violin(position=position_dodge(),draw_quantiles=c(0.5))+
            labs(title="Total Length",
                 x="Total",
                 y = "") +
            theme(plot.title = element_text(size = 20, face = "bold")) 
        
        grid.arrange(g1, g2, nrow = 1, widths = c(3, 1))
    },height = 500)
    
    dataTlenAn <- reactive({
        
        data_lonAn <- datasetInput()[, c('Provincia','Comunidad_Autonoma',
                                    'Longitud')]
        data_lonAn <- data_lonAn[!((data_lonAn$Longitud =="") | 
                                       (is.nan(data_lonAn$Longitud))), ]
        
        data_lonAn <- dplyr::filter(data_lonAn, grepl("andaluc", Comunidad_Autonoma, fixed = TRUE))
    })
    
    output$partcase <- renderPlot({
        
        ggplot(dataTlenAn(), aes(x = Provincia, y = Longitud, fill = Provincia)) +
            geom_violin() + 
            geom_jitter(height = 0, width = 0.1, size = 0.1) +
            labs(title="Distribution of Length in Provinces",
                 x="Provinces", 
                 y="Length of Andalucia's Beaches") +
            theme(axis.text = element_blank(),
                  axis.ticks = element_blank(),
                  axis.text.x = element_text(angle = 90, hjust = 1, vjust=.1),
                  axis.title.y = element_text(size = 14, face = "bold"),
                  plot.title = element_text(size = 14, face = "bold"))
    },height = 500)
    
    output$partcase2 <- renderPlot({
        
        data_lonAn <- dataTlenAn()
        
        g1 = ggplot(data_lonAn, aes(x=Longitud)) + 
            geom_histogram(aes(y=..density..), colour="black", fill="white")+
            geom_density(alpha=.2, fill="#1C2AFF") +
            ggtitle("Andalucia") +
            theme(axis.text.y=element_blank(),
                  axis.title.y=element_blank(),
                  plot.title = element_text(face = "bold"))
        
        g2 = ggplot(dplyr::filter(data_lonAn, grepl("huelva", Provincia, fixed = TRUE)), aes(x=Longitud)) + 
            geom_histogram(aes(y=..density..), colour="black", fill="white")+
            geom_density(alpha=.2, fill="#12DA1E") +
            ggtitle("Huelva") +
            theme(axis.text.y=element_blank(),
                  axis.title=element_blank(),
                  plot.title = element_text(face = "bold"))
        
        g3 = ggplot(dplyr::filter(data_lonAn, grepl("diz", Provincia, fixed = TRUE)), aes(x=Longitud)) + 
            geom_histogram(aes(y=..density..), colour="black", fill="white")+
            geom_density(alpha=.2, fill="#F5FF61") +
            ggtitle("Cadiz") +
            theme(axis.text.y=element_blank(),
                  axis.title=element_blank(),
                  plot.title = element_text(face = "bold"))
        
        g4 = ggplot(dplyr::filter(data_lonAn, grepl("laga", Provincia, fixed = TRUE)), aes(x=Longitud)) + 
            geom_histogram(aes(y=..density..), colour="black", fill="white")+
            geom_density(alpha=.2, fill="#63C5FF") +
            ggtitle("Malaga") +
            theme(axis.text.y=element_blank(),
                  axis.title=element_blank(),
                  plot.title = element_text(face = "bold"))
        
        g5 = ggplot(dplyr::filter(data_lonAn, grepl("granada", Provincia, fixed = TRUE)), aes(x=Longitud)) + 
            geom_histogram(aes(y=..density..), colour="black", fill="white")+
            geom_density(alpha=.2, fill="#FF6666") +
            ggtitle("Granada") +
            theme(axis.text.y=element_blank(),
                  axis.title=element_blank(),
                  plot.title = element_text(face = "bold"))
        
        g6 = ggplot(dplyr::filter(data_lonAn, grepl("almer", Provincia, fixed = TRUE)), aes(x=Longitud)) + 
            geom_histogram(aes(y=..density..), colour="black", fill="white")+
            geom_density(alpha=.2, fill="#F450FF") +
            ggtitle("Almeria") +
            theme(axis.text.y=element_blank(),
                  axis.title=element_blank(),
                  plot.title = element_text(face = "bold"))
        
        grid.arrange(g1,g2, g3, g4, g5, g6, 
                     nrow = 2,
                     layout_matrix = rbind(c(1,1,1,1,1), c(2,3,4,5,6)))
    },height = 500)
}

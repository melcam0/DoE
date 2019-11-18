rm(list = ls())

vif<-function (M){
  M = as.data.frame(M)
  if (colnames(M)[1] == "(Intercept)") 
    M = M[, -1]
  z = rep(NA, ncol(M))
  names(z) = colnames(M)
  for (i in 1:ncol(M)) {
    z[i] = 1/(1 - summary(lm(M[, i] ~ ., data = M[, -i,drop=FALSE]))$r.squared)
  }
  return(z)
}

server <- function (input , output, session ){
  
  observeEvent(input$openModal, {
    showModal(
      modalDialog(title = "Autori:",size = 's',easyClose = TRUE,footer = NULL,
                  
                  tags$img(src = base64enc::dataURI(file = "GC.jpg", mime = "image/jpg")),
                  
                  
                  HTML((paste(" "," ","Giorgio Marrubini","email: giorgio.marrubini@unipv.it"," ",
                              'Camillo Melzi','email: camillomelzi@gmail.com',sep="<br/>"))))
    )
  })
  
  observeEvent(input$quit,{
    stopApp()
  })

  if (!interactive()) {
    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
  }

# Fattoriale completo -----------------------------------------------------------------

  output$fatt_compl_titolo<-renderUI({
    HTML("Fattoriale completo 2 <sup>",input$fatt_compl_k,"<sup>" )
  })
  
  dis_fatt_compl<-reactive({
    req(input$fatt_compl_k)
    dis<-as.list(NULL)
    for(i in 1:input$fatt_compl_k){
      dis[[i]]=c(-1,1)
    }
    df<-expand.grid(dis)
    var<-c(NULL)
    for(i in 1:input$fatt_compl_k){
      var[i]<-paste("x",i,sep="")
      df[,i]<-as.integer(df[,i])
    }
    colnames(df)<-var
    exp<-seq(1,nrow(df))
    df<-cbind.data.frame('Exp#'=exp,df)
    df
  })

  output$fatt_compl_download <- downloadHandler(
    filename = "fatt_compl.xlsx", 
    content = function(file) {
    write.xlsx(dis_fatt_compl(),file,colNames=TRUE)
    })

  output$fatt_compl_dis<-renderTable({
    validate(need(input$fatt_compl_k>0,'Inserire un numero di fattori > 0'))
    req(input$fatt_compl_k)
    dis<-as.list(NULL)
    for(i in 1:input$fatt_compl_k){
      dis[[i]]=c(-1,1)
    }
    df<-expand.grid(dis)
    var<-c(NULL)
    for(i in 1:input$fatt_compl_k){
      var[i]<-paste("x",i,sep="")
      df[,i]<-as.integer(df[,i])
    }
    colnames(df)<-var
    exp<-seq(1,nrow(df))
    df<-cbind.data.frame('Exp#'=exp,df)
    df
  })
  
  output$fatt_compl_figura_dis<-renderPlot({
    validate(need(input$fatt_compl_k<=3,''))
    require(ggplot2)
    lati <- data.frame(
      ind = c(1, 2, 
              2, 3, 
              3, 4, 
              4, 1, 
              5, 6, 
              6, 7, 
              7, 8, 
              8, 5, 
              1, 5, 
              2, 6, 
              3, 7, 
              4, 8),
      group = rep(1:12, each = 2)
    )
    a<-1/3
    vertici<-cbind.data.frame(x=c(0,0,1,1,a,a,1+a,1+a),
                              y=c(0,1,1,0,a,1+a,1+a,a))
    df<-as.data.frame(NULL)
    for(i in 1:nrow(lati)){
      df[i,1]<-vertici$x[lati$ind[i]]
      df[i,2]<-vertici$y[lati$ind[i]]
      df[i,3]<-lati$group[i]
    }
    colnames(df)<-c('x','y','gruppi')
    
    if(input$fatt_compl_k==1){
      quadrato<-df[c(8,7),]
      etichette<-c("exp 1","exp 2")
      gr<- ggplot(quadrato, aes(x, y, group = gruppi,label=etichette)) +ylim(-0.5,0.5)+
         geom_polygon(fill = NA, colour='black') +
         scale_linetype_manual(values = c('TRUE' = "dotted", 'FALSE' = 'solid')) +
         scale_size_manual(values = c('TRUE' = 0.2, 'FALSE' = 0.5)) +
         theme_void() +
         theme(legend.position = 'none') +
         coord_equal()+
         geom_label(size=7)+
         annotate(geom="text", x=0.5, y=0, label="+", colour="red",
                 size=7)+
         geom_segment(aes(x=0.25,y=-0.1,xend=0.75,yend=-0.1),arrow = arrow(length = unit(0.5, "cm")),colour="grey")+
         annotate(geom="text", x=0.5, y=-0.15, label="x1",size=5)+xlim(-0.5,1.5)
     print(gr)
    }
    
    if(input$fatt_compl_k==2){
      quadrato<-df[1:8,]
      etichette<-c("exp 1","exp 2","exp 3","exp 4",0,0,0,0)[c(8,7,3,5,4,6,2,1)]
      gr<-ggplot(quadrato, aes(x, y, group = gruppi,label=etichette)) +
      geom_polygon(fill = NA, colour='black') +
      scale_linetype_manual(values = c('TRUE' = "dotted", 'FALSE' = 'solid')) +
      scale_size_manual(values = c('TRUE' = 0.2, 'FALSE' = 0.5)) +
      theme_void() +
      theme(legend.position = 'none') +
      coord_equal()+
      geom_label(size=7)+
      annotate(geom="text", x=0.5, y=0.5, label="+", colour="red",
               size=7)+
      geom_segment(aes(x=-0.1,y=0.25,xend=-0.1,yend=0.75),arrow = arrow(length = unit(0.5, "cm")),colour="grey")+
      annotate(geom="text", x=-0.15, y=0.5, label="x2",size=5)+
      geom_segment(aes(x=0.25,y=-0.1,xend=0.75,yend=-0.1),arrow = arrow(length = unit(0.5, "cm")),colour="grey")+
      annotate(geom="text", x=0.5, y=-0.15, label="x1",size=5)+xlim(-0.5,1.5)
      print(gr)
    }
    
    if(input$fatt_compl_k==3){
      hidden<-c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,TRUE,FALSE,
                FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,
                FALSE,FALSE)
      etichette<-c(rep(0,16),c("exp 1", "exp 2","exp 3", "exp 4","exp 5", "exp 6","exp 7", "exp 8"))
      etichette<-etichette[c(1:16,17,21,19,23,20,24,18,22)]
      gr<-ggplot(df, aes(x, y, group = gruppi,label=etichette)) +
        geom_polygon(fill = NA, colour='black', aes(linetype = hidden,  size = hidden)) +
        scale_linetype_manual(values = c('TRUE' = "dotted", 'FALSE' = 'solid')) +
        scale_size_manual(values = c('TRUE' = 0.2, 'FALSE' = 0.5)) +
        theme_void() +
        theme(legend.position = 'none') +
        coord_equal()+
        geom_label(size=7)+
        annotate(geom="text", x=2/3, y=2/3, label="+", colour="red",
                 size=7)+
        geom_segment(aes(x=-0.1,y=0.25,xend=-0.1,yend=0.75),arrow = arrow(length = unit(0.5, "cm")),colour="grey")+
        annotate(geom="text", x=-0.15, y=0.5, label="x2",size=5)+
        geom_segment(aes(x=0.25,y=-0.1,xend=0.75,yend=-0.1),arrow = arrow(length = unit(0.5, "cm")),colour="grey")+
        annotate(geom="text", x=0.5, y=-0.15, label="x1",size=5)+
        geom_segment(aes(x=1.235702-0.09,y=0,xend=1.471405-0.09,yend=0.2357023),arrow = arrow(length = unit(0.5, "cm")),colour="grey")+
        annotate(geom="text", x=1.35, y=0.1, label="x3",size=5)+xlim(-0.5,1.5)
      print(gr)
    }
  })
  
  output$fatt_compl_matrdisp<-renderTable({
    req(input$fatt_compl_k)
    validate(need(input$fatt_compl_k>0,''))
    dis<-as.list(NULL)
    for(i in 1:input$fatt_compl_k){
      dis[[i]]=c(-1,1)
    }
    df<-expand.grid(dis)
    var<-c(NULL)
    for(i in 1:input$fatt_compl_k){
      var[i]<-paste("x",i,sep="")
      df[,i]<-as.integer(df[,i])
    }
    colnames(df)<-var
    m<-input$fatt_compl_k
    lin<-NULL
    if(m>1){
      for(i in 2:m){
        x<-paste("*x",i,sep="")
        lin<-paste(lin,x)
      }
    }
    frm<-formula(paste('y~x1',lin))
    y<-rep(0,2^input$fatt_compl_k)
    df<-cbind.data.frame(df,y=y)
    mod<-lm(frm,df)
    X<-model.matrix(mod)
    D<-solve(t(X)%*%X)
    D
  })

  output$fatt_compl_modello<-renderText({
    req(input$fatt_compl_k)
    validate(need(input$fatt_compl_k>0,''))
    dis<-as.list(NULL)
    for(i in 1:input$fatt_compl_k){
      dis[[i]]=c(-1,1)
    }
    df<-expand.grid(dis)
    var<-c(NULL)
    for(i in 1:input$fatt_compl_k){
      var[i]<-paste("x",i,sep="")
      df[,i]<-as.integer(df[,i])
    }
    colnames(df)<-var
    m<-input$fatt_compl_k
    lin<-NULL
    if(m>1){
      for(i in 2:m){
        x<-paste("*x",i,sep="")
        lin<-paste(lin,x)
      }
    }
    frm<-formula(paste('y~x1',lin))
    y<-rep(0,2^input$fatt_compl_k)
    df<-cbind.data.frame(df,y=y)
    mod<-lm(frm,df)
    coeff<-attr(mod$coefficients, 'names')
    n<-length(coeff)
    modello<-'y ~ 1'
    for (i in 2:n){
      modello<-paste(modello,'+', coeff[i])
    }
    modello
  })

  output$fatt_compl_selvar<-renderUI({
    validate(need(input$fatt_compl_k>2,''))
      var<-c(NULL)
    for(i in 1:input$fatt_compl_k){
      var[i]<-paste("x",i,sep="")
    }
    selectInput("fatt_compl_selvar", label = h5("Selezionare 2 variabili"), 
                choices = var, 
                multiple = TRUE,selected = var[1:2])
  })
  
  output$fatt_compl_selvar_spazio<-renderUI({
    validate(need(input$fatt_compl_k==2,''))
    br()
  })

  output$fatt_compl_fixvar<-renderUI({
    validate(need(input$fatt_compl_k>2,''))
    req(input$fatt_compl_selvar)
    vl<-'0'
    if(input$fatt_compl_k>3){
      for(i in 2:(input$fatt_compl_k-2)){
      vl<-paste(vl,'0')
    }
    }
    var<-c(NULL)
    for(i in 1:input$fatt_compl_k){
      var[i]<-paste("x",i,sep="")
    }
    col<-c(1,2)
    if(input$fatt_compl_k>2 & length(input$fatt_compl_selvar)>=2){
      col<-which(var%in%input$fatt_compl_selvar==TRUE)
      col_fix<-which(var%in%var[col][1:2]==FALSE)
    }
    var_fix<-c(NULL)
    for(i in 1:length(col_fix)){
      var_fix<-paste(var_fix,var[col_fix][i])
    }
    if(length(col_fix)==1){
      txt<-paste('valore della  variabile',var[col_fix])
    }else{
      txt<-paste('valori delle  variabili',var_fix, '(separati da spazio)')
    }
    textInput(inputId = "fatt_compl_fixvar",label = h5(txt),value = vl)
  })

  output$fatt_compl_livellolev<-renderPlot({
    validate(need(input$fatt_compl_k>1,''))
    req(input$fatt_compl_k)
   # req(input$fatt_compl_fixvar)
    var<-c(NULL)
    for(i in 1:input$fatt_compl_k){
      var[i]<-paste("x",i,sep="")
    }
    dis<-as.list(NULL)
    for(i in 1:input$fatt_compl_k){
      dis[[i]]=c(-1,1)
    }
    dsg<-expand.grid(dis)
    for(i in 1:input$fatt_compl_k){
      dsg[,i]<-as.integer(dsg[,i])
    }
    colnames(dsg)<-var
    dt<-as.list(NULL)
    for(i in 1:2){
      dt[[i]]=seq(-1,1,0.1)
    }
    dt_exp<-expand.grid(dt)
    data<-matrix(0,441,input$fatt_compl_k)

    col<-c(1,2)
    if(input$fatt_compl_k>2 & length(input$fatt_compl_selvar)>=2){
      req(input$fatt_compl_fixvar)
      col<-which(var%in%input$fatt_compl_selvar==TRUE)
      col_fix<-which(var%in%var[col][1:2]==FALSE)
      fix<-as.numeric(unlist(strsplit(input$fatt_compl_fixvar," ")))
      for(i in 1:(input$fatt_compl_k-2))data[,col_fix[i]]<-rep(fix[i],441)
      
    }
    data[,col[1]]<-dt_exp[,1]
    data[,col[2]]<-dt_exp[,2]
    data<-as.data.frame(data)
    colnames(data)<-var

    m<-input$fatt_compl_k
    lin<-NULL
    if(m>1){
      for(i in 2:m){
        x<-paste("*x",i,sep="")
        lin<-paste(lin,x)
      }
    }
    
    frm<-formula(paste('~x1',lin))
    P=model.matrix(frm,data = dsg)
    X=model.matrix(frm,data = data)
    Q=X%*%solve(t(P)%*%P)%*%t(X)
    
    if(nrow(Q)==0){
      plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
      text(0.5,0.5,'inserire il valore delle variabili costanti',cex = 1.6, col = "red")
    }else{
    Lev=data.frame(data,"L"=diag(Q))
    colnames(Lev)[col[1]]<-'x'
    colnames(Lev)[col[2]]<-'y'
    lattice::contourplot(L~x*y,data=Lev,cuts=15,main='Plot of Leverage: Contour Plot',cex.main=0.8,
                         xlab=var[col[1]],ylab=var[col[2]],col='blue',labels=list(col="blue",cex=0.9),
                         aspect=1)
    }
  })

  output$fatt_compl_suplev<-renderPlot({
    validate(need(input$fatt_compl_k>1,''))
    req(input$fatt_compl_k)
    #req(input$fatt_compl_fixvar)
    var<-c(NULL)
    for(i in 1:input$fatt_compl_k){
      var[i]<-paste("x",i,sep="")
    }
    dis<-as.list(NULL)
    for(i in 1:input$fatt_compl_k){
      dis[[i]]=c(-1,1)
    }
    dsg<-expand.grid(dis)
    for(i in 1:input$fatt_compl_k){
      dsg[,i]<-as.integer(dsg[,i])
    }
    colnames(dsg)<-var
    dt<-as.list(NULL)
    for(i in 1:2){
      dt[[i]]=seq(-1,1,0.1)
    }
    dt_exp<-expand.grid(dt)
    data<-matrix(0,441,input$fatt_compl_k)
    col<-c(1,2)
    if(input$fatt_compl_k>2 & length(input$fatt_compl_selvar)>=2){
      req(input$fatt_compl_fixvar)
      col<-which(var%in%input$fatt_compl_selvar==TRUE)
      col_fix<-which(var%in%var[col][1:2]==FALSE)
      fix<-as.numeric(unlist(strsplit(input$fatt_compl_fixvar," ")))
      for(i in 1:(input$fatt_compl_k-2))data[,col_fix[i]]<-rep(fix[i],441)
    }
    data[,col[1]]<-dt_exp[,1]
    data[,col[2]]<-dt_exp[,2]
    data<-as.data.frame(data)
    colnames(data)<-var
    data<-as.data.frame(data)
    colnames(data)<-var
    m<-input$fatt_compl_k
    lin<-NULL
    if(m>1){
      for(i in 2:m){
        x<-paste("*x",i,sep="")
        lin<-paste(lin,x)
      }
    }
    frm<-formula(paste('~x1',lin))
    P=model.matrix(frm,data = dsg)
    X=model.matrix(frm,data = data)
    Q=X%*%solve(t(P)%*%P)%*%t(X)
    
    if(nrow(Q)==0){
      plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
      text(0.5,0.5,'inserire il valore delle variabili costanti',cex = 1.6, col = "red")
    }else{
      Lev=data.frame(data,"L"=diag(Q))
      colnames(Lev)[col[1]]<-'x'
      colnames(Lev)[col[2]]<-'y'
      req(input$fatt_compl_lv_z)
      req(input$fatt_compl_lv_x)
      lattice::wireframe(L~x*y,data=Lev,drape=TRUE,col.regions = colorRampPalette(c("yellow","green","blue"))(256),
                         at=seq(min(Lev$L),max(Lev$L),(max(Lev$L)-min(Lev$L))/256),
                         
                         screen=list(z=input$fatt_compl_lv_z,x=-input$fatt_compl_lv_x),
                         
                         main='Plot of Leverage',cex.main=0.8,xlab=var[col[1]],
                         ylab=var[col[2]],zlab=paste('Response')) 
    }   
  })
  
  output$fatt_compl_lv_z<-renderUI({
    sliderInput('fatt_compl_lv_z',label = 'Rotazione orizzontale',min = 0,max = 360,value = 30,step = 10)
  })
  
  output$fatt_compl_lv_x<-renderUI({
    sliderInput('fatt_compl_lv_x',label = 'Rotazione verticale',min = 0,max = 90,value = 60,step = 10)
  })

  output$fatt_compl_risptext<-renderUI({
    h4(paste('Risposte (',2^input$fatt_compl_k,', separate da spazio)',sep=''))
  })
  
  output$fatt_compl_figura_risp<-renderPlot({
    validate(need(input$fatt_compl_k<=3,''))
    validate(need(length(as.numeric(unlist(strsplit(input$fatt_compl_risp," "))))==2^input$fatt_compl_k,''))
    require(ggplot2)
    lati <- data.frame(
      ind = c(1, 2, 
              2, 3, 
              3, 4, 
              4, 1, 
              5, 6, 
              6, 7, 
              7, 8, 
              8, 5, 
              1, 5, 
              2, 6, 
              3, 7, 
              4, 8),
      group = rep(1:12, each = 2)
    )
    a<-1/3
    vertici<-cbind.data.frame(x=c(0,0,1,1,a,a,1+a,1+a),
                              y=c(0,1,1,0,a,1+a,1+a,a))
    df<-as.data.frame(NULL)
    for(i in 1:nrow(lati)){
      df[i,1]<-vertici$x[lati$ind[i]]
      df[i,2]<-vertici$y[lati$ind[i]]
      df[i,3]<-lati$group[i]
    }
    colnames(df)<-c('x','y','gruppi')
    
    if(input$fatt_compl_k==1){
      quadrato<-df[c(8,7),]
      etichette<-as.numeric(unlist(strsplit(input$fatt_compl_risp," ")))
      gr<- ggplot(quadrato, aes(x, y, group = gruppi,label=etichette)) +ylim(-0.5,0.5)+
        geom_polygon(fill = NA, colour='black') +
        scale_linetype_manual(values = c('TRUE' = "dotted", 'FALSE' = 'solid')) +
        scale_size_manual(values = c('TRUE' = 0.2, 'FALSE' = 0.5)) +
        theme_void() +
        theme(legend.position = 'none') +
        coord_equal()+
        geom_label(size=7)+
        annotate(geom="text", x=0.5, y=0, label="+", colour="red",
                 size=7)+
        geom_segment(aes(x=0.25,y=-0.1,xend=0.75,yend=-0.1),arrow = arrow(length = unit(0.5, "cm")),colour="grey")+
        annotate(geom="text", x=0.5, y=-0.15, label="x1",size=5)+xlim(-0.5,1.5)
      print(gr)
    }
    
    if(input$fatt_compl_k==2){
      quadrato<-df[1:8,]
      risp<-as.numeric(unlist(strsplit(input$fatt_compl_risp," ")))
      etichette<-c(risp,0,0,0,0)[c(8,7,3,5,4,6,2,1)]
      gr<-ggplot(quadrato, aes(x, y, group = gruppi,label=etichette)) +
        geom_polygon(fill = NA, colour='black') +
        scale_linetype_manual(values = c('TRUE' = "dotted", 'FALSE' = 'solid')) +
        scale_size_manual(values = c('TRUE' = 0.2, 'FALSE' = 0.5)) +
        theme_void() +
        theme(legend.position = 'none') +
        coord_equal()+
        geom_label(size=7)+
        annotate(geom="text", x=0.5, y=0.5, label="+", colour="red",
                 size=7)+
        geom_segment(aes(x=-0.1,y=0.25,xend=-0.1,yend=0.75),arrow = arrow(length = unit(0.5, "cm")),colour="grey")+
        annotate(geom="text", x=-0.15, y=0.5, label="x2",size=5)+
        geom_segment(aes(x=0.25,y=-0.1,xend=0.75,yend=-0.1),arrow = arrow(length = unit(0.5, "cm")),colour="grey")+
        annotate(geom="text", x=0.5, y=-0.15, label="x1",size=5)+xlim(-0.5,1.5)
      print(gr)
    }
    
    if(input$fatt_compl_k==3){
      hidden<-c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,TRUE,FALSE,
                FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,
                FALSE,FALSE)
      risp<-as.numeric(unlist(strsplit(input$fatt_compl_risp," ")))
      etichette<-c(rep(0,16),risp)
      etichette<-etichette[c(1:16,17,21,19,23,20,24,18,22)]
      gr<-ggplot(df, aes(x, y, group = gruppi,label=etichette)) +
        geom_polygon(fill = NA, colour='black', aes(linetype = hidden,  size = hidden)) +
        scale_linetype_manual(values = c('TRUE' = "dotted", 'FALSE' = 'solid')) +
        scale_size_manual(values = c('TRUE' = 0.2, 'FALSE' = 0.5)) +
        theme_void() +
        theme(legend.position = 'none') +
        coord_equal()+
        geom_label(size=7)+
        annotate(geom="text", x=2/3, y=2/3, label="+", colour="red",
                 size=7)+
        geom_segment(aes(x=-0.1,y=0.25,xend=-0.1,yend=0.75),arrow = arrow(length = unit(0.5, "cm")),colour="grey")+
        annotate(geom="text", x=-0.15, y=0.5, label="x2",size=5)+
        geom_segment(aes(x=0.25,y=-0.1,xend=0.75,yend=-0.1),arrow = arrow(length = unit(0.5, "cm")),colour="grey")+
        annotate(geom="text", x=0.5, y=-0.15, label="x1",size=5)+
        geom_segment(aes(x=1.235702-0.09,y=0,xend=1.471405-0.09,yend=0.2357023),arrow = arrow(length = unit(0.5, "cm")),colour="grey")+
        annotate(geom="text", x=1.35, y=0.1, label="x3",size=5)+xlim(-0.5,1.5)
      print(gr)
    }
    
  }) 
  
  output$fatt_compl_coeff<-renderPrint({
    validate(need(length(as.numeric(unlist(strsplit(input$fatt_compl_risp," "))))==2^input$fatt_compl_k,''))
    req(input$fatt_compl_k)
    dis<-as.list(NULL)
    for(i in 1:input$fatt_compl_k){
      dis[[i]]=c(-1,1)
    }
    df<-expand.grid(dis)
    var<-c(NULL)
    for(i in 1:input$fatt_compl_k){
      var[i]<-paste("x",i,sep="")
      df[,i]<-as.integer(df[,i])
    }
    colnames(df)<-var
    y<- as.numeric(unlist(strsplit(input$fatt_compl_risp," ")))
    df<-cbind.data.frame(df,y=y)
    m<-input$fatt_compl_k
    lin<-NULL
    if(m>1){
      for(i in 2:m){
        x<-paste("*x",i,sep="")
        lin<-paste(lin,x)
      }
    }
    frm<-formula(paste('y~x1',lin))
    mod<-lm(frm,df)
    round(mod$coefficients,2)
  })

  output$fatt_compl_grcoeff<-renderPlot({
    validate(need(length(as.numeric(unlist(strsplit(input$fatt_compl_risp," "))))==2^input$fatt_compl_k,''))
    req(input$fatt_compl_k)
    dis<-as.list(NULL)
    for(i in 1:input$fatt_compl_k){
      dis[[i]]=c(-1,1)
    }
    df<-expand.grid(dis)
    var<-c(NULL)
    for(i in 1:input$fatt_compl_k){
      var[i]<-paste("x",i,sep="")
      df[,i]<-as.integer(df[,i])
    }
    colnames(df)<-var
    y<- as.numeric(unlist(strsplit(input$fatt_compl_risp," ")))
    df<-cbind.data.frame(df,y=y)
    m<-input$fatt_compl_k
    lin<-NULL
    if(m>1){
      for(i in 2:m){
        x<-paste("*x",i,sep="")
        lin<-paste(lin,x)
      }
    }
    frm<-formula(paste('y~x1',lin))
    mod<-lm(frm,df)
    require(ggplot2)
    df_coeff<-data.frame(nome=names(mod$coefficients[-1]),
                         valore=mod$coefficients[-1])
    if(length(as.numeric(unlist(strsplit(input$fatt_compl_misind," "))))<2){
      ggplot(data = df_coeff,aes(x =nome,y=valore))+
        xlab("")+ylab("")+theme_light()+
        geom_bar(fill="red",stat="identity")+
        scale_x_discrete(limits=names(mod$coefficients[-1]))
    }else{
      x<-as.numeric(unlist(strsplit(input$fatt_compl_misind," ")))
      gdl<-length(x)-1
      q<-qt(p = 0.975,df = gdl)
      s<-sd(x)
      X<-model.matrix(mod)
      D<-solve(t(X)%*%X)
      d<-diag(D)

     df_coeff<-cbind.data.frame(df_coeff,inf=mod$coefficients[-1]-q*s*sqrt(d[-1]),sup=mod$coefficients[-1]+q*s*sqrt(d[-1]))
     ggplot(data = df_coeff,aes(x =nome,y=valore))+
       xlab("")+ylab("")+theme_light()+
       geom_bar(fill="red",stat="identity")+
       scale_x_discrete(limits=names(mod$coefficients[-1]))+
       geom_errorbar(aes(ymin=inf, ymax=sup),
                    width=0.2, colour="green3")
    }
  })
  
  output$fatt_compl_grsigncoeff<-renderPlot({
    validate(need(length(as.numeric(unlist(strsplit(input$fatt_compl_risp," "))))==2^input$fatt_compl_k,''))
    req(input$fatt_compl_k)
    dis<-as.list(NULL)
    for(i in 1:input$fatt_compl_k){
      dis[[i]]=c(-1,1)
    }
    df<-expand.grid(dis)
    var<-c(NULL)
    for(i in 1:input$fatt_compl_k){
      var[i]<-paste("x",i,sep="")
      df[,i]<-as.integer(df[,i])
    }
    colnames(df)<-var
    y<- as.numeric(unlist(strsplit(input$fatt_compl_risp," ")))
    df<-cbind.data.frame(df,y=y)
    m<-input$fatt_compl_k
    lin<-NULL
    if(m>1){
      for(i in 2:m){
        x<-paste("*x",i,sep="")
        lin<-paste(lin,x)
      }
    }
    frm<-formula(paste('y~x1',lin))
    mod<-lm(frm,df)
    
    FrF2::DanielPlot(mod,alpha = 0.05,main='',pch=19,cex.fac=1,col="blue",datax = FALSE)
    qqline(y = coef(mod)[-1],datax = FALSE,col="blue",lty=2)
    })

  output$fatt_compl_prev_df<-renderPrint({
    validate(need(length(as.numeric(unlist(strsplit(input$fatt_compl_risp," "))))==2^input$fatt_compl_k,''))
    req(input$fatt_compl_k)
    if(length(as.numeric(unlist(strsplit(input$fatt_compl_prev," "))))==input$fatt_compl_k){
      dis<-as.list(NULL)
      for(i in 1:input$fatt_compl_k){
        dis[[i]]=c(-1,1)
      }
      df<-expand.grid(dis)
      var<-c(NULL)
      for(i in 1:input$fatt_compl_k){
        var[i]<-paste("x",i,sep="")
        df[,i]<-as.integer(df[,i])
      }
      colnames(df)<-var
      y<- as.numeric(unlist(strsplit(input$fatt_compl_risp," ")))
      df<-cbind.data.frame(df,y=y)
      m<-input$fatt_compl_k
      lin<-NULL
      if(m>1){
        for(i in 2:m){
          x<-paste("*x",i,sep="")
          lin<-paste(lin,x)
        }
      }
      frm<-formula(paste('y~x1',lin))
      mod<-lm(frm,df)
      x<- as.numeric(unlist(strsplit(input$fatt_compl_prev," ")))
      nd<-rbind.data.frame(x)
      colnames(nd)<-var
      prev<-predict(object = mod,newdata=nd)
      attr(prev,'names')<-c('previsione')
      prev
    }else{
      cat(paste('inserire le',input$fatt_compl_k,'coord. del punto'))
    }
  })
  
  output$fatt_compl_intprev<-renderPrint({
    validate(need(length(as.numeric(unlist(strsplit(input$fatt_compl_risp," "))))==2^input$fatt_compl_k &
                    length(as.numeric(unlist(strsplit(input$fatt_compl_misind," "))))>0 &
                    length(as.numeric(unlist(strsplit(input$fatt_compl_prev," "))))==input$fatt_compl_k,''))

    x<-as.numeric(unlist(strsplit(input$fatt_compl_misind," ")))
    if(length(x)==1){
      cat('Almeno 2 misure')
    }else{
      gdl<-length(x)-1
      q<-qt(p = 0.975,df = gdl)
      q1<-qt(p = 0.995,df = gdl)
      q2<-qt(p = 0.9995,df = gdl)
      s<-sd(x)
      req(input$fatt_compl_k)
      dis<-as.list(NULL)
      for(i in 1:input$fatt_compl_k){
        dis[[i]]=c(-1,1)
      }
      df<-expand.grid(dis)
      var<-c(NULL)
      for(i in 1:input$fatt_compl_k){
        var[i]<-paste("x",i,sep="")
        df[,i]<-as.integer(df[,i])
      }
      colnames(df)<-var
      y<- as.numeric(unlist(strsplit(input$fatt_compl_risp," ")))
      df<-cbind.data.frame(df,y=y)
      m<-input$fatt_compl_k
      lin<-NULL
      if(m>1){
        for(i in 2:m){
          x<-paste("*x",i,sep="")
          lin<-paste(lin,x)
        }
      }
      frm<-formula(paste('y~x1',lin))
      mod<-lm(frm,df)
      X<-model.matrix(mod)
      
      #  leverage nel punto previsione    
      p<-as.numeric(unlist(strsplit(input$fatt_compl_prev," ")))
      data<-as.data.frame(matrix(p,1,input$fatt_compl_k))
      colnames(data)<-var
      lin<-NULL
      if(m>1){
        for(i in 2:m){
          x<-paste("*x",i,sep="")
          lin<-paste(lin,x)
        }
      }
      frm<-formula(paste('~x1',lin))
      P<-model.matrix(frm,data=data)
      d<-P%*%solve(t(X)%*%X)%*%t(P)
      
      prev<-predict(mod,newdata = data)
      df<-data.frame(prev-q*s*sqrt(d),prev+q*s*sqrt(d),
                     prev-q1*s*sqrt(d),prev+q1*s*sqrt(d),
                     prev-q2*s*sqrt(d),prev+q2*s*sqrt(d))
      
      df<-cbind.data.frame(round(df,3))
      colnames(df)<-c('2.5%','97.5%','0.5%','99.5%','0.05%','99.95%')
      df
    }
  })

  output$fatt_compl_misind_media<-renderPrint({
    validate(need(length(as.numeric(unlist(strsplit(input$fatt_compl_misind," "))))>1,''))
    x<-as.numeric(unlist(strsplit(input$fatt_compl_misind," ")))
    mod<-lm(x~1,as.data.frame(x))
    sm<-summary(mod)
    media<-predict(mod,interval = 'confidence')[1,]
    attr(media,'names')<-c('media','2.5%','97.5%')
    round(media,3)
  })
  
  output$fatt_compl_misind_sd<-renderPrint({
    validate(need(length(as.numeric(unlist(strsplit(input$fatt_compl_misind," "))))>1,''))
    x<-as.numeric(unlist(strsplit(input$fatt_compl_misind," ")))
    mod<-lm(x~1,as.data.frame(x))
    sm<-summary(mod)
    df<-c(round(sm$sigma,3))
    attr(df,'names')<-c('dev.st.')
    df
  })
  
  output$fatt_compl_misind_gdl<-renderPrint({
    validate(need(length(as.numeric(unlist(strsplit(input$fatt_compl_misind," "))))>1,''))
    x<-as.numeric(unlist(strsplit(input$fatt_compl_misind," ")))
    gdf<-(length(as.numeric(unlist(strsplit(input$fatt_compl_misind," "))))-1)
    df<-c(gdf)
    attr(df,'names')<-c( 'gdl')
    df
  })
  
  output$fatt_compl_stimint_txt<-renderUI({
    validate(need(length(as.numeric(unlist(strsplit(input$fatt_compl_misind," "))))>0 &
                    length(as.numeric(unlist(strsplit(input$fatt_compl_risp," "))))==2^input$fatt_compl_k,''))
    h4('Stima per intervallo')
  })
 
  output$fatt_compl_stimint<-renderPrint({
    validate(need(length(as.numeric(unlist(strsplit(input$fatt_compl_misind," "))))>0 &
                    length(as.numeric(unlist(strsplit(input$fatt_compl_risp," "))))==2^input$fatt_compl_k,''))
    x<-as.numeric(unlist(strsplit(input$fatt_compl_misind," ")))
    if(length(x)==1){
      cat('Almeno 2 misure')
    }else{
      gdl<-length(x)-1
      q<-qt(p = 0.975,df = gdl)
      q1<-qt(p = 0.995,df = gdl)
      q2<-qt(p = 0.9995,df = gdl)
      s<-sd(x)
      req(input$fatt_compl_k)
      dis<-as.list(NULL)
      for(i in 1:input$fatt_compl_k){
        dis[[i]]=c(-1,1)
      }
      df<-expand.grid(dis)
      var<-c(NULL)
      for(i in 1:input$fatt_compl_k){
        var[i]<-paste("x",i,sep="")
        df[,i]<-as.integer(df[,i])
      }
      colnames(df)<-var
      y<- as.numeric(unlist(strsplit(input$fatt_compl_risp," ")))
      df<-cbind.data.frame(df,y=y)
      m<-input$fatt_compl_k
      lin<-NULL
      if(m>1){
        for(i in 2:m){
          x<-paste("*x",i,sep="")
          lin<-paste(lin,x)
        }
      }
      frm<-formula(paste('y~x1',lin))
      mod<-lm(frm,df)
      X<-model.matrix(mod)
      D<-solve(t(X)%*%X)
      d<-diag(D)
      
      pval<-pt(abs(mod$coefficients/(s*sqrt(d))),df = gdl,lower.tail = FALSE)*2
      df<-data.frame(mod$coefficients,mod$coefficients-q*s*sqrt(d),mod$coefficients+q*s*sqrt(d),
                     mod$coefficients-q1*s*sqrt(d),mod$coefficients+q1*s*sqrt(d),
                     mod$coefficients-q2*s*sqrt(d),mod$coefficients+q2*s*sqrt(d),
                     pval)
      lab<-rep('',nrow(df))
      for (i in 1:nrow(df)){
        if(pval[i]<0.1)lab[i]<-'.'
        if(pval[i]<0.05)lab[i]<-'*'
        if(pval[i]<0.01)lab[i]<-'**'
        if(pval[i]<0.001)lab[i]<-'***'
      }
      
      df<-cbind.data.frame(round(df[,1:7],3),round(df[,8],4),lab)
      colnames(df)<-c('val','2.5%','97.5%','0.5%','99.5%','0.05%','99.95%','p-value','')
      df[,-1]
    }
  })

  output$fatt_compl_mod_selvar<-renderUI({
    validate(need(input$fatt_compl_k>2,''))
    var<-c(NULL)
    for(i in 1:input$fatt_compl_k){
      var[i]<-paste("x",i,sep="")
    }
    selectInput("fatt_compl_mod_selvar", label = h5("Selezionare 2 variabili"), 
                choices = var, 
                multiple = TRUE,selected = var[1:2])
  })
  
  output$fatt_compl_mod_fixvar<-renderUI({
    validate(need(input$fatt_compl_k>2,''))
    req(input$fatt_compl_mod_selvar)
    vl<-'0'
    if(input$fatt_compl_k>3){
      for(i in 2:(input$fatt_compl_k-2)){
        vl<-paste(vl,'0')
      }
    }

    var<-c(NULL)
    for(i in 1:input$fatt_compl_k){
      var[i]<-paste("x",i,sep="")
    }
    col<-c(1,2)
    if(input$fatt_compl_k>2 & length(input$fatt_compl_mod_selvar)>=2){
      col<-which(var%in%input$fatt_compl_mod_selvar==TRUE)
      col_fix<-which(var%in%var[col][1:2]==FALSE)
    }
    var_fix<-c(NULL)
    for(i in 1:length(col_fix)){
      var_fix<-paste(var_fix,var[col_fix][i])
    }
    if(length(col_fix)==1){
      txt<-paste('valore della  variabile',var[col_fix])
    }else{
      txt<-paste('valori delle  variabili',var_fix, '(separati da spazio)')
    }
    textInput(inputId = "fatt_compl_mod_fixvar",label = h5(txt),value = vl)
  })
  
  output$fatt_compl_livellorisp<-renderPlot({
    validate(need(length(as.numeric(unlist(strsplit(input$fatt_compl_risp," "))))==2^input$fatt_compl_k,''))
    req(input$fatt_compl_k)
    var<-c(NULL)
    for(i in 1:input$fatt_compl_k){
      var[i]<-paste("x",i,sep="")
    }
    dis<-as.list(NULL)
    for(i in 1:input$fatt_compl_k){
      dis[[i]]=c(-1,1)
    }
    dsg<-expand.grid(dis)
    for(i in 1:input$fatt_compl_k){
      dsg[,i]<-as.integer(dsg[,i])
    }
    colnames(dsg)<-var
    y<-as.numeric(unlist(strsplit(input$fatt_compl_risp," ")))
    dsg<-cbind.data.frame(dsg,y=y)
    dt<-as.list(NULL)
    for(i in 1:2){
      dt[[i]]=seq(-1,1,0.1)
    }
    dt_exp<-expand.grid(dt)
    data<-matrix(0,441,input$fatt_compl_k)
    col<-c(1,2)
    if(input$fatt_compl_k>2 & length(input$fatt_compl_mod_selvar)>=2){
      input$fatt_compl_mod_fixvar
      col<-which(var%in%input$fatt_compl_mod_selvar==TRUE)
      col_fix<-which(var%in%var[col][1:2]==FALSE)
      fix<-as.numeric(unlist(strsplit(input$fatt_compl_mod_fixvar," ")))
      for(i in 1:(input$fatt_compl_k-2))data[,col_fix[i]]<-rep(fix[i],441)
      
    }
    data[,col[1]]<-dt_exp[,1]
    data[,col[2]]<-dt_exp[,2]
    data<-as.data.frame(data)
    colnames(data)<-var

    m<-input$fatt_compl_k
    lin<-NULL
    if(m>1){
      for(i in 2:m){
        x<-paste("*x",i,sep="")
        lin<-paste(lin,x)
      }
    }
    frm<-formula(paste('y~x1',lin))
    mod<-lm(frm,dsg)
    Pred=data.frame(data,P=predict(mod,newdata=data))
    colnames(Pred)[col[1]]<-'x'
    colnames(Pred)[col[2]]<-'y'
    colore<-c("blue","green3","red","black","purple1")
    cl<-as.integer(input$fatt_compl_livellorisp_col)
    
    if(is.na(Pred$P[1])==TRUE){
      plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
      text(0.5,0.5,'inserire il valore delle variabili costanti',cex = 1.6, col = "red")
    }else{
    lattice::contourplot(P~x*y,data=Pred,cuts=15,main='Response Surface: Contour Plot',cex.main=0.8,
                         xlab=var[col[1]],ylab=var[col[2]],col=colore[cl],
                         labels=list(col=colore[cl],cex=0.9),
                         aspect=1)
    }
  })

  output$fatt_compl_livellorisp_col<-renderUI({
    validate(need(length(as.numeric(unlist(strsplit(input$fatt_compl_risp," "))))==2^input$fatt_compl_k,''))
    selectInput("fatt_compl_livellorisp_col", label = h3(""), 
                choices = list("blu" = 1, "verde" = 2, "rosso" = 3,"nero" = 4,"viola" = 5), 
                selected = 1,width="130px")
  })
  
  output$fatt_compl_suprisp<-renderPlot({
    validate(need(length(as.numeric(unlist(strsplit(input$fatt_compl_risp," "))))==2^input$fatt_compl_k,''))
    req(input$fatt_compl_k)
    var<-c(NULL)
    for(i in 1:input$fatt_compl_k){
      var[i]<-paste("x",i,sep="")
    }
    dis<-as.list(NULL)
    for(i in 1:input$fatt_compl_k){
      dis[[i]]=c(-1,1)
    }
    dsg<-expand.grid(dis)
    for(i in 1:input$fatt_compl_k){
      dsg[,i]<-as.integer(dsg[,i])
    }
    colnames(dsg)<-var
    y<-as.numeric(unlist(strsplit(input$fatt_compl_risp," ")))
    dsg<-cbind.data.frame(dsg,y=y)
    dt<-as.list(NULL)
    for(i in 1:2){
      dt[[i]]=seq(-1,1,0.1)
    }
    dt_exp<-expand.grid(dt)
    data<-matrix(0,441,input$fatt_compl_k)

    col<-c(1,2)
    if(input$fatt_compl_k>2 & length(input$fatt_compl_mod_selvar)>=2){
      req(input$fatt_compl_mod_fixvar)
      col<-which(var%in%input$fatt_compl_mod_selvar==TRUE)
      col_fix<-which(var%in%var[col][1:2]==FALSE)
      fix<-as.numeric(unlist(strsplit(input$fatt_compl_mod_fixvar," ")))
      for(i in 1:(input$fatt_compl_k-2))data[,col_fix[i]]<-rep(fix[i],441)
      
    }
    data[,col[1]]<-dt_exp[,1]
    data[,col[2]]<-dt_exp[,2]
    data<-as.data.frame(data)
    colnames(data)<-var

    m<-input$fatt_compl_k
    lin<-NULL
    if(m>1){
      for(i in 2:m){
        x<-paste("*x",i,sep="")
        lin<-paste(lin,x)
      }
    }
    frm<-formula(paste('y~x1',lin))
    mod<-lm(frm,dsg)
    Pred=data.frame(data,P=predict(mod,newdata=data))
    colnames(Pred)[col[1]]<-'x'
    colnames(Pred)[col[2]]<-'y'
    
    if(is.na(Pred$P[1])==TRUE){
      plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
      text(0.5,0.5,'inserire il valore delle variabili costanti',cex = 1.6, col = "red")
    }else{
      req(input$fatt_compl_rp_z)
      req(input$fatt_compl_rp_x)
    lattice::wireframe(P~x*y,data=Pred,drape=TRUE,col.regions = colorRampPalette(c("yellow","green","blue"))(256),
                       at=seq(min(Pred$P),max(Pred$P),(max(Pred$P)-min(Pred$P))/256),
                       screen=list(z=input$fatt_compl_rp_z,x=-input$fatt_compl_rp_x),
                       main='Response Surface',cex.main=0.8,xlab=var[col[1]],
                       ylab=var[col[2]],zlab=paste('Response')) 
    }
  })
  
  output$fatt_compl_rp_z<-renderUI({
    sliderInput('fatt_compl_rp_z',label = 'Rotazione orizzontale',min = 0,max = 360,value = 30,step = 10)
  })
  
  output$fatt_compl_rp_x<-renderUI({
    sliderInput('fatt_compl_rp_x',label = 'Rotazione verticale',min = 0,max = 90,value = 60,step = 10)
  })
  
# Frazionario -----------------------------------------------------------------  
  output$logo <- renderImage({list(src = "Risoluzione.png",width=1050, height=400)}, deleteFile = FALSE)
  
  dis_frazionario<-reactive({
    require(FrF2)
    fattori<-c("x1")
    for(i in 2:input$frazion_k) fattori<-paste(fattori,",x",i,sep='')
    fattori<-unlist(strsplit(fattori,","))
    dis<-FrF2(nruns = 2^(input$frazion_k-input$frazion_p),nfactors = input$frazion_k,
          factor.names = fattori,randomize = FALSE)
    dis
  })
  
  output$frazion_titolo<-renderUI({
    require(FrF2)
    ris<-c("I","II","III","IV","V","VI","VII","VIII","IX","X")
    r<-min(nchar(unlist(generators(dis_frazionario()))))-1
    HTML("Frazionario 2 <sup>",input$frazion_k,"-",input$frazion_p,"</sup> <sub ><sub>",ris[r],"</sub></sub>" )
  })
  
  output$frazion_generat<-renderPrint({
    require(FrF2)
    unlist(generators(dis_frazionario()))
  })
  
  output$frazion_download <- downloadHandler(
    filename = "frazionario.xlsx", 
    content = function(file) {
      dis<-attr(dis_frazionario(),"desnum")
      exp<-seq(1,nrow(dis))
      dis<-cbind.data.frame('Exp#'=exp,dis)
      write.xlsx(dis, file ,colNames=TRUE)
    })

  output$frazion_dis<-renderTable(digits=0,{
    dis<-attr(dis_frazionario(),"desnum")
    exp<-seq(1,nrow(dis))
    dis<-cbind.data.frame('Exp#'=exp,dis)
  })
  
  output$frazion_modello<-renderText({
    req(input$frazion_k)
    m<-input$frazion_k
    lin<-NULL
    if(m>1){
      for(i in 2:m){
        x<-paste("*x",i,sep="")
        lin<-paste(lin,x)
      }
    }
    frm<-formula(paste('y~x1',lin))
    y<-rep(0,2^input$fatt_compl_k)
    dis<-attr(dis_frazionario(),"desnum")
    df<-cbind.data.frame(dis,y=y)
    mod<-lm(frm,df)
    coeff<-names(mod$effects)
    n<-sum(!is.na(mod$coefficients))
    modello<-'y ~ 1'
    for (i in 2:n){
      modello<-paste(modello,'+', coeff[i])
    }
    modello
  })
  
  output$frazion_confusioni<-renderPrint({
    req(input$frazion_k)
    require(FrF2)
    m<-input$frazion_k
    lin<-NULL
    if(m>1){
      for(i in 2:m){
        x<-paste("*x",i,sep="")
        lin<-paste(lin,x)
      }
    }
    frm<-formula(paste('y~x1',lin))
    y<-rep(0,2^input$fatt_compl_k)
    dis<-attr(dis_frazionario(),"desnum")
    df<-cbind.data.frame(dis,y=y)
    mod<-lm(frm,df)
    aliases(mod)
  })

  output$frazion_matrdisp<-renderTable({
    req(input$frazion_k)
    m<-input$frazion_k
    lin<-NULL
    if(m>1){
      for(i in 2:m){
        x<-paste("*x",i,sep="")
        lin<-paste(lin,x)
      }
    }
    frm<-formula(paste('y~x1',lin))
    y<-rep(0,2^input$fatt_compl_k)
    dis<-attr(dis_frazionario(),"desnum")
    df<-cbind.data.frame(dis,y=y)
    mod<-lm(frm,df)
    coeff<-names(mod$effects)
    n<-sum(!is.na(mod$coefficients))
    frm<-' ~ '
    for (i in 2:n){
      frm<-paste(frm,'+', coeff[i])
    }
    frm<-as.formula(frm)
    X<-model.matrix(frm,as.data.frame(dis))
    D<-solve(t(X)%*%X)
    D 
  })

  output$frazion_selvar<-renderUI({
    validate(need(input$frazion_k>2,''))
    var<-c(NULL)
    for(i in 1:input$frazion_k){
      var[i]<-paste("x",i,sep="")
    }
    selectInput("frazion_selvar", label = h5("Selezionare 2 variabili"), 
                choices = var, 
                multiple = TRUE,selected = var[1:2])
  })
  
  output$frazion_selvar_spazio<-renderUI({
    validate(need(input$frazion_k==2,''))
    br()
  })
  
  output$frazion_fixvar<-renderUI({
    validate(need(input$frazion_k>2,''))
    req(input$frazion_selvar)
    vl<-'0'
    if(input$frazion_k>3){
      for(i in 2:(input$frazion_k-2)){
        vl<-paste(vl,'0')
      }
    }
    var<-c(NULL)
    for(i in 1:input$frazion_k){
      var[i]<-paste("x",i,sep="")
    }
    col<-c(1,2)
    if(input$frazion_k>2 & length(input$frazion_selvar)>=2){
      req(input$frazion_selvar)
      col<-which(var%in%input$frazion_selvar==TRUE)
      col_fix<-which(var%in%var[col][1:2]==FALSE)
    }
    var_fix<-c(NULL)
    for(i in 1:length(col_fix)){
      var_fix<-paste(var_fix,var[col_fix][i])
    }
    if(length(col_fix)==1){
      txt<-paste('valore della  variabile',var[col_fix])
    }else{
      txt<-paste('valori delle  variabili',var_fix, '(separati da spazio)')
    }
    textInput(inputId = "frazion_fixvar",label = h5(txt),value = vl)
  })
  
  output$frazion_livellolev<-renderPlot({
    validate(need(input$frazion_k>2,''))
    req(input$frazion_k)
    req(input$frazion_fixvar)
    var<-c(NULL)
    for(i in 1:input$frazion_k){
      var[i]<-paste("x",i,sep="")
    }

    dis<-attr(dis_frazionario(),"desnum")
    dsg<-cbind.data.frame(dis)
    
    dt<-as.list(NULL)
    for(i in 1:2){
      dt[[i]]=seq(-1,1,0.1)
    }
    dt_exp<-expand.grid(dt)
    data<-matrix(0,441,input$frazion_k)
    col<-c(1,2)
    if(input$frazion_k>2 & length(input$frazion_selvar)>=2){
      req(input$frazion_fixvar)
      col<-which(var%in%input$frazion_selvar==TRUE)
      col_fix<-which(var%in%var[col][1:2]==FALSE)
      fix<-as.numeric(unlist(strsplit(input$frazion_fixvar," ")))
      for(i in 1:(input$frazion_k-2))data[,col_fix[i]]<-rep(fix[i],441)
      
    }
    data[,col[1]]<-dt_exp[,1]
    data[,col[2]]<-dt_exp[,2]
    data<-as.data.frame(data)
    colnames(data)<-var
    
    m<-input$frazion_k
    lin<-NULL
    if(m>1){
      for(i in 2:m){
        x<-paste("*x",i,sep="")
        lin<-paste(lin,x)
      }
    }
    frm<-formula(paste('y~x1',lin))
    y<-rep(0,2^input$fatt_compl_k)
    df<-cbind.data.frame(dis,y=y)
    mod<-lm(frm,df)
    coeff<-names(mod$effects)
    n<-sum(!is.na(mod$coefficients))
    frm<-' ~ '
    for (i in 2:n){
      frm<-paste(frm,'+', coeff[i])
    }
    frm<-as.formula(frm)
    
    P=model.matrix(frm,data = dsg)
    X=model.matrix(frm,data = data)
    Q=X%*%solve(t(P)%*%P)%*%t(X)
    
    if(nrow(Q)==0){
      plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
      text(0.5,0.5,'inserire il valore delle variabili costanti',cex = 1.6, col = "red")
    }else{
    Lev=data.frame(data,"L"=diag(Q))
    colnames(Lev)[col[1]]<-'x'
    colnames(Lev)[col[2]]<-'y'
    
    lattice::contourplot(L~x*y,data=Lev,cuts=15,main='Plot of Leverage: Contour Plot',cex.main=0.8,
                         xlab=var[col[1]],ylab=var[col[2]],col='blue',labels=list(col="blue",cex=0.9),
                         aspect=1)}
  })
  
  output$frazion_suplev<-renderPlot({
    validate(need(input$frazion_k>1,''))
    req(input$frazion_k)
    req(input$frazion_selvar)
    req(input$frazion_fixvar)
    var<-c(NULL)
    for(i in 1:input$frazion_k){
      var[i]<-paste("x",i,sep="")
    }

    dis<-attr(dis_frazionario(),"desnum")
    dsg<-cbind.data.frame(dis)

    dt<-as.list(NULL)
    for(i in 1:2){
      dt[[i]]=seq(-1,1,0.1)
    }
    dt_exp<-expand.grid(dt)
    data<-matrix(0,441,input$frazion_k)
    col<-c(1,2)
    if(input$frazion_k>2 & length(input$frazion_selvar)>=2){
      req(input$frazion_fixvar)
      col<-which(var%in%input$frazion_selvar==TRUE)
      col_fix<-which(var%in%var[col][1:2]==FALSE)
      fix<-as.numeric(unlist(strsplit(input$frazion_fixvar," ")))
      for(i in 1:(input$frazion_k-2))data[,col_fix[i]]<-rep(fix[i],441)
      
    }
    data[,col[1]]<-dt_exp[,1]
    data[,col[2]]<-dt_exp[,2]
    data<-as.data.frame(data)
    colnames(data)<-var

    m<-input$frazion_k
    lin<-NULL
    if(m>1){
      for(i in 2:m){
        x<-paste("*x",i,sep="")
        lin<-paste(lin,x)
      }
    }
    frm<-formula(paste('y~x1',lin))
    y<-rep(0,2^input$fatt_compl_k)
    df<-cbind.data.frame(dis,y=y)
    mod<-lm(frm,df)
    coeff<-names(mod$effects)
    n<-sum(!is.na(mod$coefficients))
    frm<-' ~ '
    for (i in 2:n){
      frm<-paste(frm,'+', coeff[i])
    }
    frm<-as.formula(frm)

    P=model.matrix(frm,data = dsg)
    X=model.matrix(frm,data = data)
    Q=X%*%solve(t(P)%*%P)%*%t(X)
    
    if(nrow(Q)==0){
      plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
      text(0.5,0.5,'inserire il valore delle variabili costanti',cex = 1.6, col = "red")
    }else{
    Lev=data.frame(data,"L"=diag(Q))
    colnames(Lev)[col[1]]<-'x'
    colnames(Lev)[col[2]]<-'y'
    req(input$frazion_lv_z)
    req(input$frazion_lv_x)
    lattice::wireframe(L~x*y,data=Lev,drape=TRUE,col.regions = colorRampPalette(c("yellow","green","blue"))(256),
                       at=seq(min(Lev$L),max(Lev$L),(max(Lev$L)-min(Lev$L))/256),
                       screen=list(z=input$frazion_lv_z,x=-input$frazion_lv_x),
                       main='Plot of Leverage',cex.main=0.8,xlab=var[col[1]],
                       ylab=var[col[2]],zlab=paste('Response')) }
  })
  
  output$frazion_lv_z<-renderUI({
    sliderInput('frazion_lv_z',label = 'Rotazione orizzontale',min = 0,max = 360,value = 30,step = 10)
  })
  
  output$frazion_lv_x<-renderUI({
    sliderInput('frazion_lv_x',label = 'Rotazione verticale',min = 0,max = 90,value = 60,step = 10)
  })
  
  output$frazion_risptext<-renderUI({
    h4(paste('Risposte (',2^(input$frazion_k-input$frazion_p),', separate da spazio)',sep=''))
  })

  output$frazion_coeff<-renderPrint({
    validate(need(length(as.numeric(unlist(strsplit(input$frazion_risp," "))))==2^(input$frazion_k-input$frazion_p),''))
    req(input$frazion_k)

    var<-c(NULL)
    for(i in 1:input$frazion_k){
      var[i]<-paste("x",i,sep="")
    }
    
    dis<-attr(dis_frazionario(),"desnum")
    dsg<-cbind.data.frame(dis)
    
    y<- as.numeric(unlist(strsplit(input$frazion_risp," ")))
    df<-cbind.data.frame(dsg,y=y)

    m<-input$frazion_k
    lin<-NULL
    if(m>1){
      for(i in 2:m){
        x<-paste("*x",i,sep="")
        lin<-paste(lin,x)
      }
    }
    frm<-formula(paste('y~x1',lin))
    df<-cbind.data.frame(dis,y=y)
    mod<-lm(frm,df)
    coeff<-names(mod$effects)
    n<-sum(!is.na(mod$coefficients))
    frm<-'y ~ 1'
    for (i in 2:n){
      frm<-paste(frm,'+', coeff[i])
    }
    frm<-as.formula(frm)
    mod<-lm(frm,df)
    round(mod$coefficients,2)
  })

  output$frazion_grcoeff<-renderPlot({
    validate(need(length(as.numeric(unlist(strsplit(input$frazion_risp," "))))==2^(input$frazion_k-input$frazion_p),''))
    req(input$frazion_k)
    
    var<-c(NULL)
    for(i in 1:input$frazion_k){
      var[i]<-paste("x",i,sep="")
    }
    
    dis<-attr(dis_frazionario(),"desnum")
    dsg<-cbind.data.frame(dis)
    
    y<- as.numeric(unlist(strsplit(input$frazion_risp," ")))
    df<-cbind.data.frame(dsg,y=y)

    m<-input$frazion_k
    lin<-NULL
    if(m>1){
      for(i in 2:m){
        x<-paste("*x",i,sep="")
        lin<-paste(lin,x)
      }
    }
    frm<-formula(paste('y~x1',lin))
    df<-cbind.data.frame(dis,y=y)
    mod<-lm(frm,df)
    coeff<-names(mod$effects)
    n<-sum(!is.na(mod$coefficients))
    frm<-'y ~ 1'
    for (i in 2:n){
      frm<-paste(frm,'+', coeff[i])
    }
    frm<-as.formula(frm)
    mod<-lm(frm,df)

    require(ggplot2)
    df_coeff<-data.frame(nome=names(mod$coefficients[-1]),
                         valore=mod$coefficients[-1])
    if(length(as.numeric(unlist(strsplit(input$frazion_misind," "))))<2){
      ggplot(data = df_coeff,aes(x =nome,y=valore))+
        xlab("")+ylab("")+theme_light()+
        geom_bar(fill="red",stat="identity")+
        scale_x_discrete(limits=names(mod$coefficients[-1]))
    }else{
      x<-as.numeric(unlist(strsplit(input$frazion_misind," ")))
      gdl<-length(x)-1
      q<-qt(p = 0.975,df = gdl)
      s<-sd(x)
      
      X<-model.matrix(mod)
      D<-solve(t(X)%*%X)
      d<-diag(D)
      
      df_coeff<-cbind.data.frame(df_coeff,inf=mod$coefficients[-1]-q*s*sqrt(d[-1]),sup=mod$coefficients[-1]+q*s*sqrt(d[-1]))
      ggplot(data = df_coeff,aes(x =nome,y=valore))+
        xlab("")+ylab("")+theme_light()+
        geom_bar(fill="red",stat="identity")+
        scale_x_discrete(limits=names(mod$coefficients[-1]))+
        geom_errorbar(aes(ymin=inf, ymax=sup),
                      width=0.2, colour="green3")
    }
  }) 
  
  output$frazion_grsigncoeff<-renderPlot({
    validate(need(length(as.numeric(unlist(strsplit(input$frazion_risp," "))))==2^(input$frazion_k-input$frazion_p),''))
    req(input$frazion_k)
    
    var<-c(NULL)
    for(i in 1:input$frazion_k){
      var[i]<-paste("x",i,sep="")
    }
    
    dis<-attr(dis_frazionario(),"desnum")
    dsg<-cbind.data.frame(dis)
    
    y<- as.numeric(unlist(strsplit(input$frazion_risp," ")))
    df<-cbind.data.frame(dsg,y=y)
    
    m<-input$frazion_k
    lin<-NULL
    if(m>1){
      for(i in 2:m){
        x<-paste("*x",i,sep="")
        lin<-paste(lin,x)
      }
    }
    frm<-formula(paste('y~x1',lin))
    df<-cbind.data.frame(dis,y=y)
    mod<-lm(frm,df)
    coeff<-names(mod$effects)
    n<-sum(!is.na(mod$coefficients))
    frm<-'y ~ 1'
    for (i in 2:n){
      frm<-paste(frm,'+', coeff[i])
    }
    frm<-as.formula(frm)
    mod<-lm(frm,df)
    FrF2::DanielPlot(mod,alpha = 0.05,main='',pch=19,cex.fac=1,col="blue",datax = FALSE)
    qqline(y = coef(mod)[-1],datax = FALSE,col="blue",lty=2)
  })

  output$frazion_prev_df<-renderPrint({
    validate(need(length(as.numeric(unlist(strsplit(input$frazion_risp," "))))==2^(input$frazion_k-input$frazion_p),''))
    req(input$frazion_k)
    if(length(as.numeric(unlist(strsplit(input$frazion_prev," "))))==input$frazion_k){

      var<-c(NULL)
      for(i in 1:input$frazion_k){
        var[i]<-paste("x",i,sep="")
      }
      
      dis<-attr(dis_frazionario(),"desnum")
      dsg<-cbind.data.frame(dis)
      
      y<- as.numeric(unlist(strsplit(input$frazion_risp," ")))
      df<-cbind.data.frame(dsg,y=y)
      
      m<-input$frazion_k
      lin<-NULL
      if(m>1){
        for(i in 2:m){
          x<-paste("*x",i,sep="")
          lin<-paste(lin,x)
        }
      }
      frm<-formula(paste('y~x1',lin))
      df<-cbind.data.frame(dis,y=y)
      mod<-lm(frm,df)
      coeff<-names(mod$effects)
      n<-sum(!is.na(mod$coefficients))
      frm<-'y ~ 1'
      for (i in 2:n){
        frm<-paste(frm,'+', coeff[i])
      }
      frm<-as.formula(frm)
      mod<-lm(frm,df)

      x<- as.numeric(unlist(strsplit(input$frazion_prev," ")))
      nd<-rbind.data.frame(x)
      colnames(nd)<-var
      prev<-predict(object = mod,newdata=nd)
      attr(prev,'names')<-c('previsione')
      prev
    }else{
      cat(paste('inserire le',input$frazion_k,'coord. del punto'))
    }
  })
  
  output$frazion_intprev<-renderPrint({
    validate(need(length(as.numeric(unlist(strsplit(input$frazion_risp," "))))==2^(input$frazion_k-input$frazion_p) &
                    length(as.numeric(unlist(strsplit(input$frazion_misind," "))))>0 &
                    length(as.numeric(unlist(strsplit(input$frazion_prev," "))))==input$frazion_k,''))
    x<-as.numeric(unlist(strsplit(input$frazion_misind," ")))
    if(length(x)==1){
      cat('Almeno 2 misure')
    }else{
      gdl<-length(x)-1
      q<-qt(p = 0.975,df = gdl)
      q1<-qt(p = 0.995,df = gdl)
      q2<-qt(p = 0.9995,df = gdl)
      s<-sd(x)
      req(input$frazion_k)
      var<-c(NULL)
      for(i in 1:input$frazion_k){
        var[i]<-paste("x",i,sep="")
      }
      
      dis<-attr(dis_frazionario(),"desnum")
      dsg<-cbind.data.frame(dis)
      
      y<- as.numeric(unlist(strsplit(input$frazion_risp," ")))
      df<-cbind.data.frame(dsg,y=y)
      
      m<-input$frazion_k
      lin<-NULL
      if(m>1){
        for(i in 2:m){
          x<-paste("*x",i,sep="")
          lin<-paste(lin,x)
        }
      }
      frm<-formula(paste('y~x1',lin))
      df<-cbind.data.frame(dis,y=y)
      mod<-lm(frm,df)
      coeff<-names(mod$effects)
      n<-sum(!is.na(mod$coefficients))
      frm<-'y ~ 1'
      for (i in 2:n){
        frm<-paste(frm,'+', coeff[i])
      }
      frm<-as.formula(frm)
      mod<-lm(frm,df)
      X<-model.matrix(mod)
      
      #  leverage nel punto previsione    
      p<-as.numeric(unlist(strsplit(input$frazion_prev," ")))
      data<-as.data.frame(matrix(p,1,input$frazion_k))
      colnames(data)<-var
      frm<-'~ 1'
      for (i in 2:n){
        frm<-paste(frm,'+', coeff[i])
      }
      frm<-as.formula(frm)
      P<-model.matrix(frm,data=data)
      d<-P%*%solve(t(X)%*%X)%*%t(P)
      
      prev<-predict(mod,newdata = data)
      df<-data.frame(prev-q*s*sqrt(d),prev+q*s*sqrt(d),
                     prev-q1*s*sqrt(d),prev+q1*s*sqrt(d),
                     prev-q2*s*sqrt(d),prev+q2*s*sqrt(d))
      
      df<-cbind.data.frame(round(df,3))
      colnames(df)<-c('2.5%','97.5%','0.5%','99.5%','0.05%','99.95%')
      df
    }
  })

  output$frazion_misind_media<-renderPrint({
    validate(need(length(as.numeric(unlist(strsplit(input$frazion_misind," "))))>1,''))
    x<-as.numeric(unlist(strsplit(input$frazion_misind," ")))
    mod<-lm(x~1,as.data.frame(x))
    sm<-summary(mod)
    media<-predict(mod,interval = 'confidence')[1,]
    attr(media,'names')<-c('media','2.5%','97.5%')
    round(media,3)
  })

  output$frazion_misind_sd<-renderPrint({
    validate(need(length(as.numeric(unlist(strsplit(input$frazion_misind," "))))>1,''))
    x<-as.numeric(unlist(strsplit(input$frazion_misind," ")))
    mod<-lm(x~1,as.data.frame(x))
    sm<-summary(mod)
    df<-c(round(sm$sigma,3))
    attr(df,'names')<-c('dev.st.')
    df
  })
  
  output$frazion_misind_gdl<-renderPrint({
    validate(need(length(as.numeric(unlist(strsplit(input$frazion_misind," "))))>1,''))
    x<-as.numeric(unlist(strsplit(input$frazion_misind," ")))
    gdf<-(length(as.numeric(unlist(strsplit(input$frazion_misind," "))))-1)
    df<-c(gdf)
    attr(df,'names')<-c( 'gdl')
    df
  })
  
  output$frazion_stimint_txt<-renderUI({
    validate(need(length(as.numeric(unlist(strsplit(input$frazion_misind," "))))>0 &
                    length(as.numeric(unlist(strsplit(input$frazion_risp," "))))==2^(input$frazion_k-input$frazion_p),''))
    h4('Stima per intervallo')
  })

  output$frazion_stimint<-renderPrint({
    validate(need(length(as.numeric(unlist(strsplit(input$frazion_misind," "))))>0 &
                    length(as.numeric(unlist(strsplit(input$frazion_risp," "))))==2^(input$frazion_k-input$frazion_p),''))
    x<-as.numeric(unlist(strsplit(input$frazion_misind," ")))
    if(length(x)==1){
      cat('Almeno 2 misure')
    }else{
      gdl<-length(x)-1
      q<-qt(p = 0.975,df = gdl)
      q1<-qt(p = 0.995,df = gdl)
      q2<-qt(p = 0.9995,df = gdl)
      s<-sd(x)
      req(input$frazion_k)
      var<-c(NULL)
      for(i in 1:input$frazion_k){
        var[i]<-paste("x",i,sep="")
      }
      
      dis<-attr(dis_frazionario(),"desnum")
      dsg<-cbind.data.frame(dis)
      
      y<- as.numeric(unlist(strsplit(input$frazion_risp," ")))
      df<-cbind.data.frame(dsg,y=y)
      
      m<-input$frazion_k
      lin<-NULL
      if(m>1){
        for(i in 2:m){
          x<-paste("*x",i,sep="")
          lin<-paste(lin,x)
        }
      }
      frm<-formula(paste('y~x1',lin))
      df<-cbind.data.frame(dis,y=y)
      mod<-lm(frm,df)
      coeff<-names(mod$effects)
      n<-sum(!is.na(mod$coefficients))
      frm<-'y ~ 1'
      for (i in 2:n){
        frm<-paste(frm,'+', coeff[i])
      }
      frm<-as.formula(frm)
      mod<-lm(frm,df)
      
      X<-model.matrix(mod)
      D<-solve(t(X)%*%X)
      d<-diag(D)

      pval<-pt(abs(mod$coefficients/(s*sqrt(d))),df = gdl,lower.tail = FALSE)*2
      df<-data.frame(mod$coefficients,mod$coefficients-q*s*sqrt(d),mod$coefficients+q*s*sqrt(d),
                     mod$coefficients-q1*s*sqrt(d),mod$coefficients+q1*s*sqrt(d),
                     mod$coefficients-q2*s*sqrt(d),mod$coefficients+q2*s*sqrt(d),
                     pval)
      lab<-rep('',nrow(df))
      for (i in 1:nrow(df)){
        if(pval[i]<0.1)lab[i]<-'.'
        if(pval[i]<0.05)lab[i]<-'*'
        if(pval[i]<0.01)lab[i]<-'**'
        if(pval[i]<0.001)lab[i]<-'***'
      }
      
      df<-cbind.data.frame(round(df[,1:7],3),round(df[,8],4),lab)
      colnames(df)<-c('val','2.5%','97.5%','0.5%','99.5%','0.05%','99.95%','p-value','')
      df[,-1]
    }
  })

  output$frazion_mod_selvar<-renderUI({
    validate(need(input$frazion_k>2,''))
    var<-c(NULL)
    for(i in 1:input$frazion_k){
      var[i]<-paste("x",i,sep="")
    }
    selectInput("frazion_mod_selvar", label = h5("Selezionare 2 variabili"), 
                choices = var, 
                multiple = TRUE,selected = var[1:2])
  })
  
  output$frazion_mod_fixvar<-renderUI({
    validate(need(input$frazion_k>2,''))
    req(input$frazion_mod_selvar)
    vl<-'0'
    if(input$frazion_k>3){
      for(i in 2:(input$frazion_k-2)){
        vl<-paste(vl,'0')
      }
    }
    var<-c(NULL)
    for(i in 1:input$frazion_k){
      var[i]<-paste("x",i,sep="")
    }
    col<-c(1,2)
    if(input$frazion_k>2 & length(input$frazion_mod_selvar)>=2){
      req(input$frazion_mod_selvar)
      col<-which(var%in%input$frazion_mod_selvar==TRUE)
      col_fix<-which(var%in%var[col][1:2]==FALSE)
    }
    var_fix<-c(NULL)
    for(i in 1:length(col_fix)){
      var_fix<-paste(var_fix,var[col_fix][i])
    }
    if(length(col_fix)==1){
      txt<-paste('valore della  variabile',var[col_fix])
    }else{
      txt<-paste('valori delle  variabili',var_fix, '(separati da spazio)')
    }
    textInput(inputId = "frazion_mod_fixvar",label = h5(txt),value = vl)
  })
  
  output$frazion_livellorisp<-renderPlot({
    validate(need(length(as.numeric(unlist(strsplit(input$frazion_risp," "))))==2^(input$frazion_k-input$frazion_p),''))
    req(input$frazion_k)

    var<-c(NULL)
    for(i in 1:input$frazion_k){
      var[i]<-paste("x",i,sep="")
    }
    
    dis<-attr(dis_frazionario(),"desnum")
    dsg<-cbind.data.frame(dis)
    
    y<- as.numeric(unlist(strsplit(input$frazion_risp," ")))
    df<-cbind.data.frame(dsg,y=y)
    
    m<-input$frazion_k
    lin<-NULL
    if(m>1){
      for(i in 2:m){
        x<-paste("*x",i,sep="")
        lin<-paste(lin,x)
      }
    }
    frm<-formula(paste('y~x1',lin))
    df<-cbind.data.frame(dis,y=y)
    mod<-lm(frm,df)
    coeff<-names(mod$effects)
    n<-sum(!is.na(mod$coefficients))
    frm<-'y ~ 1'
    for (i in 2:n){
      frm<-paste(frm,'+', coeff[i])
    }
    frm<-as.formula(frm)
    mod<-lm(frm,df)

    dt<-as.list(NULL)
    for(i in 1:2){
      dt[[i]]=seq(-1,1,0.1)
    }
    dt_exp<-expand.grid(dt)
    data<-matrix(0,441,input$frazion_k)
    col<-c(1,2)
    if(input$frazion_k>2 & length(input$frazion_mod_selvar)>=2){
      col<-which(var%in%input$frazion_mod_selvar==TRUE)
      col_fix<-which(var%in%var[col][1:2]==FALSE)
      fix<-as.numeric(unlist(strsplit(input$frazion_mod_fixvar," ")))
      for(i in 1:(input$frazion_k-2))data[,col_fix[i]]<-rep(fix[i],441)
      
    }
    data[,col[1]]<-dt_exp[,1]
    data[,col[2]]<-dt_exp[,2]
    data<-as.data.frame(data)
    colnames(data)<-var

    Pred=data.frame(data,P=predict(mod,newdata=data))
    colnames(Pred)[col[1]]<-'x'
    colnames(Pred)[col[2]]<-'y'
    colore<-c("blue","green3","red","black","purple1")
    cl<-as.integer(input$frazion_livellorisp_col)
    
    if(is.na(Pred$P[1])==TRUE){
      plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
      text(0.5,0.5,'inserire il valore delle variabili costanti',cex = 1.6, col = "red")
    }else{   
    lattice::contourplot(P~x*y,data=Pred,cuts=15,main='Response Surface: Contour Plot',cex.main=0.8,
                         xlab=var[col[1]],ylab=var[col[2]],col=colore[cl],
                         labels=list(col=colore[cl],cex=0.9),
                         aspect=1)}
  })
  
  output$frazion_livellorisp_col<-renderUI({
    validate(need(length(as.numeric(unlist(strsplit(input$frazion_risp," "))))==2^(input$frazion_k-input$frazion_p),''))
    selectInput("frazion_livellorisp_col", label = h3(""), 
                choices = list("blu" = 1, "verde" = 2, "rosso" = 3,"nero" = 4,"viola" = 5), 
                selected = 1,width="130px")
  })

  output$frazion_suprisp<-renderPlot({
    validate(need(length(as.numeric(unlist(strsplit(input$frazion_risp," "))))==2^(input$frazion_k-input$frazion_p),''))
    req(input$frazion_k)

    var<-c(NULL)
    for(i in 1:input$frazion_k){
      var[i]<-paste("x",i,sep="")
    }
    
    dis<-attr(dis_frazionario(),"desnum")
    dsg<-cbind.data.frame(dis)
    
    y<- as.numeric(unlist(strsplit(input$frazion_risp," ")))
    df<-cbind.data.frame(dsg,y=y)
    
    m<-input$frazion_k
    lin<-NULL
    if(m>1){
      for(i in 2:m){
        x<-paste("*x",i,sep="")
        lin<-paste(lin,x)
      }
    }
    frm<-formula(paste('y~x1',lin))
    df<-cbind.data.frame(dis,y=y)
    mod<-lm(frm,df)
    coeff<-names(mod$effects)
    n<-sum(!is.na(mod$coefficients))
    frm<-'y ~ 1'
    for (i in 2:n){
      frm<-paste(frm,'+', coeff[i])
    }
    frm<-as.formula(frm)
    mod<-lm(frm,df)

    dt<-as.list(NULL)
    for(i in 1:2){
      dt[[i]]=seq(-1,1,0.1)
    }
    dt_exp<-expand.grid(dt)
    data<-matrix(0,441,input$frazion_k)
    col<-c(1,2)
    if(input$frazion_k>2 & length(input$frazion_mod_selvar)>=2){
      col<-which(var%in%input$frazion_mod_selvar==TRUE)
      col_fix<-which(var%in%var[col][1:2]==FALSE)
      fix<-as.numeric(unlist(strsplit(input$frazion_mod_fixvar," ")))
      for(i in 1:(input$frazion_k-2))data[,col_fix[i]]<-rep(fix[i],441)
      
    }
    data[,col[1]]<-dt_exp[,1]
    data[,col[2]]<-dt_exp[,2]
    data<-as.data.frame(data)
    colnames(data)<-var

    Pred=data.frame(data,P=predict(mod,newdata=data))
    colnames(Pred)[col[1]]<-'x'
    colnames(Pred)[col[2]]<-'y'
    
    if(is.na(Pred$P[1])==TRUE){
      plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
      text(0.5,0.5,'inserire il valore delle variabili costanti',cex = 1.6, col = "red")
    }else{
      req(input$frazion_rp_z)
      req(input$frazion_rp_x)
    lattice::wireframe(P~x*y,data=Pred,drape=TRUE,col.regions = colorRampPalette(c("yellow","green","blue"))(256),
                       at=seq(min(Pred$P),max(Pred$P),(max(Pred$P)-min(Pred$P))/256),
                       screen=list(z=input$frazion_rp_z,x=-input$frazion_rp_x),
                       main='Response Surface',cex.main=0.8,xlab=var[col[1]],
                       ylab=var[col[2]],zlab=paste('Response'))} 
  })
  
  output$frazion_rp_z<-renderUI({
    sliderInput('frazion_rp_z',label = 'Rotazione orizzontale',min = 0,max = 360,value = 30,step = 10)
  })
  
  output$frazion_rp_x<-renderUI({
    sliderInput('frazion_rp_x',label = 'Rotazione verticale',min = 0,max = 90,value = 60,step = 10)
  })
  
  # Plackett-Burman -----------------------------------------------------------------
  
  output$pb_titolo<-renderUI({
    HTML("Plackett-Burman" )
  })
  
  n_pb<-reactive({
    n<-0 
    for (m in 1:10000){
      if (input$pb_k<4*m){
        n<-4*m
        break()
      }
    }
    n
  })
  
  output$pb_n<-renderUI({
    selectInput("pb_n", label = "n° esperimenti",width = "25%", 
                choices = list(n_pb(),n_pb()+4), 
                selected = 1)
  })

  output$dis16<-renderUI({
    validate(need(as.integer(input$pb_n)==16,""))
    radioButtons("dis16", label = h3(""),
                 choices = list("Plackett-Burman" = 1, "Box-Tyssedal" = 2), 
                 selected = 1)
  })
                    
  dis_pb<-reactive({
    require(FrF2)
    req(input$pb_k)
    req(input$pb_n)
    validate(need(input$pb_k<as.integer(input$pb_n),""))
    fattori<-c("x1")
    for(i in 2:input$pb_k) fattori<-paste(fattori,",x",i,sep='')
    fattori<-unlist(strsplit(fattori,","))
    n<-as.integer(input$pb_n)
    plbur<-pb(nruns = n,factor.names = fattori,randomize = FALSE,boxtyssedal=FALSE)
    if(as.integer(input$pb_n)==16 ){
      req(input$dis16)
      bs<-input$dis16 ==2
      plbur<-pb(nruns = n,factor.names = fattori,randomize = FALSE,boxtyssedal=bs)
    } 
    attr(plbur,"desnum")
  })
  
  output$pb_download <- downloadHandler(
    filename = "pb.xlsx", 
    content = function(file) {
      dis<-dis_pb()
      exp<-seq(1,nrow(dis))
      dis<-cbind.data.frame('Exp#'=exp,dis)
      write.xlsx(dis, file,colNames=TRUE)
    })
  
  output$pb_dis<-renderTable(digits=0,{
    dis<-dis_pb()
    exp<-seq(1,nrow(dis))
    dis<-cbind.data.frame('Exp#'=exp,dis)
  })

  output$pb_modello<-renderText({
    req(input$pb_k)
    req(input$pb_n)
    m<-length(colnames(dis_pb())) 
    lin<-NULL
    if(m>1){
      for(i in 2:m){
        x<-paste("+",colnames(dis_pb())[i],sep="")
        lin<-paste(lin,x)
      }
    }
    modello<-paste('y~1+x1',lin)
    modello
  })

  output$pb_matrdisp<-renderTable({
    req(input$pb_k)
    req(input$pb_n)
    m<-length(colnames(dis_pb())) 
    lin<-NULL
    if(m>1){
      for(i in 2:m){
        x<-paste("+",colnames(dis_pb())[i],sep="")
        lin<-paste(lin,x)
      }
    }
    lin<-paste('y~x1',lin,sep="")
    frm<-formula(lin)
    n<-as.integer(input$pb_n)
    y<-rep(0,n)
    df<-cbind.data.frame(dis_pb(),y=y)
    mod<-lm(frm,df)
    X<-model.matrix(mod)
    D<-solve(t(X)%*%X)
    D 
  })
  
  output$pb_suggerimento<-renderUI({
    potenze<-c(8,16,32,64,128,256,512,1024,2048,4096,8192,16384)
    exp<-c(3,4,5,6,7,8,9,10,11,12,13,14)
    validate(need(as.integer(input$pb_n)%in%potenze,""))
    exp<-exp[potenze%in%as.integer(input$pb_n)]
    if(as.integer(input$pb_n)!=16){
      HTML("<hr><hr><hr><hr>
           <h5>Il disegno di Plackett-Burman con", as.integer(input$pb_n), "esperimenti 
           coincide con il disegno frazionario 2 <sup>",input$pb_k,"-",input$pb_k-exp,"</sup><sub><sub> III </sub></sub>.<h5>
           <h5> Potresti prendere in considerazione l'ipotesi di aumentare il numero di esperimenti a",as.integer(input$pb_n)+4,".<h5>
           <h5> Assicurati di prendere in considerazione la struttura alias per l'interpretazione! <h5>")
    }else{
      req(input$dis16)
      if(input$dis16 ==1){
        HTML("<h5>Il disegno di Plackett-Burman con", as.integer(input$pb_n), "esperimenti 
             coincide con il disegno frazionario 2 <sup>",input$pb_k,"-",input$pb_k-exp,"</sup><sub><sub> III </sub></sub>.<h5>
             <h5> Potresti prendere in considerazione l'ipotesi di aumentare il numero di esperimenti a",as.integer(input$pb_n)+4,".<h5>
             <h5> Assicurati di prendere in considerazione la struttura alias per l'interpretazione! <h5>")
      }else{
        HTML("Disegno di Box e Tyssedal, che ha il vantaggio di creare alias di ogni interazione con diversi 
             effetti principali, come per i Plackett-Burmann non frazionari.")
      }
    }
  })

  output$pb_confusioni<-renderPrint({
    req(input$pb_k)
    req(input$pb_n)
    m<-length(colnames(dis_pb())) 
    lin<-NULL
    if(m>1){
      for(i in 2:m){
        x<-paste("+",colnames(dis_pb())[i],sep="")
        lin<-paste(lin,x)
      }
    }
    lin<-paste('x1',lin,sep="")
    lin<-paste("y~(",lin,")^2",sep="")
    frm<-formula(lin)
    n<-as.integer(input$pb_n)
    y<-rep(0,n)
    df<-cbind.data.frame(dis_pb(),y=y)
    mod<-lm(frm,df)
    X<-model.matrix(mod)
    m<-(n-1)*(n-2)/2+n
    X1<-X[,1:n]
    X2<-X[,(n+1):m]
    A<-solve(t(X1)%*%X1)%*%(t(X1)%*%X2)
    potenze<-c(8,16,32,64,128,256,512,1024,2048,4096,8192,16384)
    if(as.integer(input$pb_n)%in%potenze) print(A)
    if(!as.integer(input$pb_n)%in%potenze)round(A,2)
  })

  output$pb_risptext<-renderUI({
    h4(paste('Risposte (',input$pb_n,', separate da spazio)',sep=''))
  })

  output$pb_coeff<-renderPrint({
    validate(need(length(as.numeric(unlist(strsplit(input$pb_risp," "))))==input$pb_n,''))
    req(input$pb_k)
    m<-length(colnames(dis_pb()))
    var<-c(NULL)
    for(i in 1:m){
      var[i]<-colnames(dis_pb())[i]
    }
    dsg<-as.data.frame(dis_pb())
    y<- as.numeric(unlist(strsplit(input$pb_risp," ")))
    df<-cbind.data.frame(dsg,y=y)
    m<-length(colnames(dis_pb())) 
    lin<-NULL
    if(m>1){
      for(i in 2:m){
        x<-paste("+",colnames(dis_pb())[i],sep="")
        lin<-paste(lin,x)
      }
    }
    lin<-paste('y~x1',lin,sep="")
    frm<-formula(lin)
    mod<-lm(frm,df)
    round(mod$coefficients,2)
  })

  output$pb_grcoeff<-renderPlot({
    validate(need(length(as.numeric(unlist(strsplit(input$pb_risp," "))))==input$pb_n,''))
    req(input$pb_k)
    m<-length(colnames(dis_pb()))
    dsg<-as.data.frame(dis_pb())
    y<- as.numeric(unlist(strsplit(input$pb_risp," ")))
    df<-cbind.data.frame(dsg,y=y)
    m<-length(colnames(dis_pb())) 
    lin<-NULL
    if(m>1){
      for(i in 2:m){
        x<-paste("+",colnames(dis_pb())[i],sep="")
        lin<-paste(lin,x)
      }
    }
    lin<-paste('y~x1',lin,sep="")
    frm<-formula(lin)
    mod<-lm(frm,df)
    require(ggplot2)
    df_coeff<-data.frame(nome=names(mod$coefficients[-1]),
                         valore=mod$coefficients[-1])
    gr<-ggplot(data = df_coeff,aes(x =nome,y=valore))+
      xlab("")+ylab("")+theme_light()+
      geom_bar(fill="red",stat="identity")+
      scale_x_discrete(limits=names(mod$coefficients[-1]))
    gr
    if(as.integer(input$pb_n)-input$pb_k>1){
       mx<-max(abs(mod$coefficients)[(input$pb_k+1):m+1])
       gr<-gr+geom_hline(yintercept = mx,linetype="dashed", color = "blue")+
         geom_hline(yintercept = -mx,linetype="dashed", color = "blue")
     }
    gr
  }) 
  
  output$pb_grsigncoeff<-renderPlot({
    validate(need(length(as.numeric(unlist(strsplit(input$pb_risp," "))))==input$pb_n,''))
    
    m<-length(colnames(dis_pb()))
    dsg<-as.data.frame(dis_pb())
    y<- as.numeric(unlist(strsplit(input$pb_risp," ")))
    df<-cbind.data.frame(dsg,y=y)
    m<-length(colnames(dis_pb())) 
    lin<-NULL
    if(m>1){
      for(i in 2:m){
        x<-paste("+",colnames(dis_pb())[i],sep="")
        lin<-paste(lin,x)
      }
    }
    lin<-paste('y~x1',lin,sep="")
    frm<-formula(lin)
    mod<-lm(frm,df)
    
    coeff<-coef(mod)[-1]
    coeff<-coeff[1:input$pb_k]
    
    # costr dis pb tipo disegno
    require(FrF2)
    req(input$pb_k)
    req(input$pb_n)
    validate(need(input$pb_k<as.integer(input$pb_n),""))
    fattori<-c("x1")
    for(i in 2:input$pb_k) fattori<-paste(fattori,",x",i,sep='')
    fattori<-unlist(strsplit(fattori,","))
    n<-as.integer(input$pb_n)
    plbur<-pb(nruns = n,factor.names = fattori,randomize = FALSE,boxtyssedal=FALSE)
    dis.pb.resp <- add.response(plbur, y)
    
    FrF2::DanielPlot(dis.pb.resp,alpha = 0.05,main='',pch=19,cex.fac=1,col="blue",datax = FALSE)
    qqline(y = coeff,datax = FALSE,col="blue",lty=2)
  })

  output$pb_graf_alias<-renderPlot({
    validate(need(length(as.numeric(unlist(strsplit(input$pb_risp," "))))==input$pb_n,''))
    req(input$pb_k)
    req(input$pb_n)
    m<-length(colnames(dis_pb())) 
    lin<-NULL
    if(m>1){
      for(i in 2:m){
        x<-paste("+",colnames(dis_pb())[i],sep="")
        lin<-paste(lin,x)
      }
    }
    lin<-paste('y ~ x1',lin,sep="")
    k<-input$pb_k
    inter<-NULL
    for(i in 2:k-1){
      for(j in (i+1):k){
        inter<-paste(inter,"+",colnames(dis_pb())[i],":",colnames(dis_pb())[j],sep="")
      }
    }
    formula<-paste(lin,inter)
    frm<-formula(formula)
    n<-as.integer(input$pb_n)
    y<-rep(0,n)
    df<-cbind.data.frame(dis_pb(),y=y)
    mod<-lm(frm,df)
    X<-model.matrix(mod)
    m1<-n+(k-1)*k/2
    X1<-X[,1:n]
    X2<-X[,(n+1):m1]
    A<-solve(t(X1)%*%X1)%*%(t(X1)%*%X2)
    y<- as.numeric(unlist(strsplit(input$pb_risp," ")))
    df<-cbind.data.frame(dis_pb(),y=y)
    frm<-formula(lin)
    mod<-lm(frm,df)
    s<-n/2
    cf<-mod$coefficients[-1]
    cf_s<-sign(cf)
    cf<-abs(mod$coefficients[-1])
    r<-order(cf,decreasing = TRUE)[1:s]
    cf_s<-cf_s[r]
    A<-A[-1,]
    A<-A[r,]
    B<-cf_s*A
    int<-apply(B,2,"sum")
    int<-abs(int)
    int<-int[order(int,decreasing = TRUE)]
    int<-int[int!=0]
    df_coeff<-data.frame(nome=names(int),
                         valore=int)
    gr<-ggplot(data = df_coeff,aes(x =nome,y=valore))+
      xlab("")+ylab("")+theme_light()+
      geom_bar(fill="red",stat="identity")+
      scale_x_discrete(limits=names(int))+theme(axis.text.x = element_text(angle = 90, hjust = 1))
    gr
  })

  # CCD ----------------------------------------------------------------- 
  
  output$ccd_titolo<-renderUI({
    HTML("Central Composite Design" )
  })
  
  ccd_alfa<-reactive({
    k<-input$ccd_k
    n<-input$ccd_n
    if(input$ccd_alfa==1)a<-(2^k)^(1/4)
    if(input$ccd_alfa==2)a<-k^(1/2)
    if(input$ccd_alfa==3)a<-1
    if(input$ccd_alfa==4)a<-sqrt((sqrt((2^k+2*k+n)*2^k)-2^k)/2)
    a
  })
  
  dis_ccd<-reactive({
    require(rsm)
    rotd <- ccd(input$ccd_k, n0 = c(0,input$ccd_n), alpha = ccd_alfa(),randomize = FALSE)
    rotdm <- rotd[ , 3:(2+input$ccd_k)]
    rotdm<-as.data.frame(rotdm)
    var<-c(NULL)
    for(i in 1:input$ccd_k){
      var[i]<-paste("x",i,sep="")
    }
    colnames(rotdm)<-var
    rotdm
  })
  
  output$ccd_download <- downloadHandler(
    filename = "ccd.xlsx", 
    content = function(file) {
      dis<-dis_ccd()
      exp<-seq(1,nrow(dis))
      dis<-cbind.data.frame('Exp#'=exp,dis)
      write.xlsx(dis, file,colNames=TRUE)
    })

  output$ccd_dis<-renderTable({
    req(input$ccd_k)
    req(input$ccd_n)
    dis<-dis_ccd()
    exp<-seq(1,nrow(dis))
    dis<-cbind.data.frame('Exp#'=exp,dis)
  })
  
  output$ccd_figura_dis<-renderPlot({
    validate(need(input$ccd_k<=3,''))
    require(ggplot2)
    lati <- data.frame(
      ind = c(1, 2, 
              2, 3, 
              3, 4, 
              4, 1, 
              5, 6, 
              6, 7, 
              7, 8, 
              8, 5, 
              1, 5, 
              2, 6, 
              3, 7, 
              4, 8),
      group = rep(1:12, each = 2)
    )
    a<-1/3
    vertici<-cbind.data.frame(x=c(0,0,1,1,a,a,1+a,1+a),
                              y=c(0,1,1,0,a,1+a,1+a,a))
    df<-as.data.frame(NULL)
    for(i in 1:nrow(lati)){
      df[i,1]<-vertici$x[lati$ind[i]]
      df[i,2]<-vertici$y[lati$ind[i]]
      df[i,3]<-lati$group[i]
    }
    colnames(df)<-c('x','y','gruppi')
    if(input$ccd_k==2){
      quadrato<-df[1:8,]
     # etichette<-c("exp 1","exp 2","exp 3","exp 4",0,0,0,0)[c(8,7,3,5,4,6,2,1)]
      gr<-ggplot(quadrato, aes(x, y, group = gruppi)) +
        geom_polygon(fill = NA, colour='black') +
        scale_linetype_manual(values = c('TRUE' = "dotted", 'FALSE' = 'solid')) +
        scale_size_manual(values = c('TRUE' = 0.2, 'FALSE' = 0.5)) +
        theme_void() +
        theme(legend.position = 'none') +
        coord_equal(xlim=c(-0.7+0.5,0.7+0.5),ylim =c(-0.7+0.5,0.7+0.5) )+
      #  geom_label(size=7)+
       # annotate(geom="text", x=0.5, y=0.5, label="+", colour="red",
       #          size=7)+
        geom_segment(aes(x=-0.1,y=0.25,xend=-0.1,yend=0.75),arrow = arrow(length = unit(0.5, "cm")),colour="grey")+
        annotate(geom="text", x=-0.15, y=0.5, label="x2",size=5)+
        geom_segment(aes(x=0.25,y=-0.1,xend=0.75,yend=-0.1),arrow = arrow(length = unit(0.5, "cm")),colour="grey")+
        annotate(geom="text", x=0.5, y=-0.15, label="x1",size=5)+xlim(-0.5,1.5)+
        geom_point(col="blue",size=2)+
        geom_point(aes(x=0.5,y=0.5),col="blue",size=2)+
        geom_point(aes(x=0.5+ccd_alfa()/2,y=0.5),col="blue",size=2)+
        geom_point(aes(x=0.5-ccd_alfa()/2,y=0.5),col="blue",size=2)+
        geom_segment(aes(x=0.5+ccd_alfa()/2,y=0.5,xend=0.5-ccd_alfa()/2,yend=0.5),col="red",linetype=2)+
        geom_point(aes(x=0.5,y=0.5+ccd_alfa()/2),col="blue",size=2)+
        geom_point(aes(x=0.5,y=0.5-ccd_alfa()/2),col="blue",size=2)+
        geom_segment(aes(x=0.5,y=0.5+ccd_alfa()/2,xend=0.5,yend=0.5-ccd_alfa()/2),col="red",linetype=2)
      print(gr)
    }
    if(input$ccd_k==3){
      hidden<-c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,TRUE,FALSE,
                FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,
                FALSE,FALSE)
      #☺etichette<-c(rep(0,16),c("exp 1", "exp 2","exp 3", "exp 4","exp 5", "exp 6","exp 7", "exp 8"))
      #etichette<-etichette[c(1:16,17,21,19,23,20,24,18,22)]
      gr<-ggplot(df, aes(x, y, group = gruppi)) +
        geom_polygon(fill = NA, colour='black', aes(linetype = hidden,  size = hidden)) +
        scale_linetype_manual(values = c('TRUE' = "dotted", 'FALSE' = 'solid')) +
        scale_size_manual(values = c('TRUE' = 0.2, 'FALSE' = 0.5)) +
        theme_void() +
        theme(legend.position = 'none') +
        coord_equal(xlim=c(-0.85+2/3,0.7+0.85),ylim =c(-0.85+2/3,0.85+2/3))+
        #geom_label(size=7)+
        #annotate(geom="text", x=2/3, y=2/3, label="+", colour="red",
        #         size=7)+
        geom_segment(aes(x=-0.1,y=0.25,xend=-0.1,yend=0.75),arrow = arrow(length = unit(0.5, "cm")),colour="grey")+
        annotate(geom="text", x=-0.15, y=0.5, label="x2",size=5)+
        geom_segment(aes(x=0.25,y=-0.1,xend=0.75,yend=-0.1),arrow = arrow(length = unit(0.5, "cm")),colour="grey")+
        annotate(geom="text", x=0.5, y=-0.15, label="x1",size=5)+
        geom_segment(aes(x=1.235702-0.09,y=0,xend=1.471405-0.09,yend=0.2357023),arrow = arrow(length = unit(0.5, "cm")),colour="grey")+
        annotate(geom="text", x=1.35, y=0.1, label="x3",size=5)+
        geom_point(col="blue",size=2)+
        geom_point(aes(x=2/3,y=2/3),col="blue",size=2)+
        geom_point(aes(x=2/3+ccd_alfa()/2,y=2/3),col="blue",size=2)+
        geom_point(aes(x=2/3-ccd_alfa()/2,y=2/3),col="blue",size=2)+
        geom_segment(aes(x=2/3+ccd_alfa()/2,y=2/3,xend=2/3-ccd_alfa()/2,yend=2/3),col="red",linetype=2)+
        geom_point(aes(x=2/3,y=2/3+ccd_alfa()/2),col="blue",size=2)+
        geom_point(aes(x=2/3,y=2/3-ccd_alfa()/2),col="blue",size=2)+
        geom_segment(aes(x=2/3,y=2/3+ccd_alfa()/2,xend=2/3,yend=2/3-ccd_alfa()/2),col="red",linetype=2)+
        geom_point(aes(x=2/3+ccd_alfa()/2/3,y=2/3+ccd_alfa()/2/3),col="blue",size=2)+
        geom_point(aes(x=2/3-ccd_alfa()/2/3,y=2/3-ccd_alfa()/2/3),col="blue",size=2)+
        geom_segment(aes(x=2/3+ccd_alfa()/2/3,y=2/3+ccd_alfa()/2/3,xend=2/3-ccd_alfa()/2/3,yend=2/3-ccd_alfa()/2/3),col="red",linetype=2)
      print(gr)
    }
  })
  
  output$ccd_matrdisp<-renderTable({
    req(input$ccd_k)
    df<-dis_ccd()
    m<-input$ccd_k
    lin<-NULL
    if(m>1){
      for(i in 2:m){
        x<-paste("+x",i,sep="")
        lin<-paste(lin,x,sep="")
      }
    }
    lin<-paste('y~(x1',lin,")^2",sep="")
    quad<-NULL
    for(i in 2:m){
      x<-paste("+I(x",i,"^2)",sep="")
      quad<-paste(quad,x)
    }
    quad<-paste("I(x1^2)",quad,sep="")
    frm<-paste(lin,"+",quad,sep="")
    frm<-as.formula(frm)
    l<-nrow(df)
    y<-rep(0,l)
    df<-cbind.data.frame(df,y=y)
    mod<-lm(frm,df)
    X<-model.matrix(mod)
    D<-solve(t(X)%*%X)
    D
  })
  
  output$ccd_modello<-renderText({
    req(input$ccd_k)
    df<-dis_ccd()
    m<-input$ccd_k
    lin<-NULL
    if(m>1){
      for(i in 2:m){
        x<-paste("+x",i,sep="")
        lin<-paste(lin,x,sep="")
      }
    }
    lin<-paste('y~(x1',lin,")^2",sep="")
    frm<-as.formula(lin)
    l<-nrow(df)
    y<-rep(0,l)
    df<-cbind.data.frame(df,y=y)
    mod<-lm(frm,df)
    coeff<-attr(mod$coefficients, 'names')
    n<-length(coeff)
    modello<-'y ~ 1'
    for (i in 2:n){
      modello<-paste(modello,'+', coeff[i])
    }
    quad<-NULL
    for(i in 2:m){
      x<-paste("+x",i,"^2",sep="")
      quad<-paste(quad,x)
    }
    quad<-paste("+x1^2",quad,sep="")
    modello<-paste(modello,quad,sep="")
    modello
  })
  
  output$ccd_selvar<-renderUI({
    validate(need(input$ccd_k>2,''))
    var<-c(NULL)
    for(i in 1:input$ccd_k){
      var[i]<-paste("x",i,sep="")
    }
    selectInput("ccd_selvar", label = h5("Selezionare 2 variabili"), 
                choices = var, 
                multiple = TRUE,selected = var[1:2])
  })
  
  output$ccd_selvar_spazio<-renderUI({
    validate(need(input$ccd_k==2,''))
    br()
  })
  
  output$ccd_fixvar<-renderUI({
    validate(need(input$ccd_k>2,''))
    req(input$ccd_selvar)
    vl<-'0'
    if(input$ccd_k>3){
      for(i in 2:(input$ccd_k-2)){
        vl<-paste(vl,'0')
      }
    }
    var<-c(NULL)
    for(i in 1:input$ccd_k){
      var[i]<-paste("x",i,sep="")
    }
    col<-c(1,2)
    if(input$ccd_k>2 & length(input$ccd_selvar)>=2){
      col<-which(var%in%input$ccd_selvar==TRUE)
      col_fix<-which(var%in%var[col][1:2]==FALSE)
    }
    var_fix<-c(NULL)
    for(i in 1:length(col_fix)){
      var_fix<-paste(var_fix,var[col_fix][i])
    }
    if(length(col_fix)==1){
      txt<-paste('valore della  variabile',var[col_fix])
    }else{
      txt<-paste('valori delle  variabili',var_fix, '(separati da spazio)')
    }
    textInput(inputId = "ccd_fixvar",label = h5(txt),value = vl)
  })
  
  output$ccd_livellolev<-renderPlot({
    validate(need(input$ccd_k>1,''))
    req(input$ccd_k)
    dsg<-dis_ccd()
    var<-c(NULL)
    for(i in 1:input$ccd_k){
      var[i]<-paste("x",i,sep="")
    }
    dt<-as.list(NULL)
    for(i in 1:2){
      dt[[i]]=seq(-1,1,0.1)
    }
    dt_exp<-expand.grid(dt)
    data<-matrix(0,441,input$ccd_k)
    col<-c(1,2)
    if(input$ccd_k>2 & length(input$ccd_selvar)>=2){
      req(input$ccd_fixvar)
      col<-which(var%in%input$ccd_selvar==TRUE)
      col_fix<-which(var%in%var[col][1:2]==FALSE)
      fix<-as.numeric(unlist(strsplit(input$ccd_fixvar," ")))
      for(i in 1:(input$ccd_k-2))data[,col_fix[i]]<-rep(fix[i],441)
      
    }
    data[,col[1]]<-dt_exp[,1]
    data[,col[2]]<-dt_exp[,2]
    data<-as.data.frame(data)
    colnames(data)<-var
    m<-input$ccd_k
    lin<-NULL
    if(m>1){
      for(i in 2:m){
        x<-paste("+x",i,sep="")
        lin<-paste(lin,x,sep="")
      }
    }
    lin<-paste('~(x1',lin,")^2",sep="")
    quad<-NULL
    for(i in 2:m){
      x<-paste("+I(x",i,"^2)",sep="")
      quad<-paste(quad,x)
    }
    quad<-paste("I(x1^2)",quad,sep="")
    frm<-paste(lin,"+",quad,sep="")
    frm<-as.formula(frm)
    P=model.matrix(frm,data = dsg)
    X=model.matrix(frm,data = data)
    Q=X%*%solve(t(P)%*%P)%*%t(X)
    
    if(nrow(Q)==0){
      plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
      text(0.5,0.5,'inserire il valore delle variabili costanti',cex = 1.6, col = "red")
    }else{
    Lev=data.frame(data,"L"=diag(Q))
    colnames(Lev)[col[1]]<-'x'
    colnames(Lev)[col[2]]<-'y'
    lattice::contourplot(L~x*y,data=Lev,cuts=15,main='Plot of Leverage: Contour Plot',cex.main=0.8,
                         xlab=var[col[1]],ylab=var[col[2]],col='blue',labels=list(col="blue",cex=0.9),
                         aspect=1)}
  })
  
  output$ccd_suplev<-renderPlot({
    validate(need(input$ccd_k>1,''))
    req(input$ccd_k)
    dsg<-dis_ccd()
    var<-c(NULL)
    for(i in 1:input$ccd_k){
      var[i]<-paste("x",i,sep="")
    }
    dt<-as.list(NULL)
    for(i in 1:2){
      dt[[i]]=seq(-1,1,0.1)
    }
    dt_exp<-expand.grid(dt)
    data<-matrix(0,441,input$ccd_k)
    col<-c(1,2)
    if(input$ccd_k>2 & length(input$ccd_selvar)>=2){
      req(input$ccd_selvar)
      req(input$ccd_fixvar)
      col<-which(var%in%input$ccd_selvar==TRUE)
      col_fix<-which(var%in%var[col][1:2]==FALSE)
      fix<-as.numeric(unlist(strsplit(input$ccd_fixvar," ")))
      for(i in 1:(input$ccd_k-2))data[,col_fix[i]]<-rep(fix[i],441)
      
    }
    data[,col[1]]<-dt_exp[,1]
    data[,col[2]]<-dt_exp[,2]
    data<-as.data.frame(data)
    colnames(data)<-var
    m<-input$ccd_k
    lin<-NULL
    if(m>1){
      for(i in 2:m){
        x<-paste("+x",i,sep="")
        lin<-paste(lin,x,sep="")
      }
    }
    lin<-paste('~(x1',lin,")^2",sep="")
    quad<-NULL
    for(i in 2:m){
      x<-paste("+I(x",i,"^2)",sep="")
      quad<-paste(quad,x)
    }
    quad<-paste("I(x1^2)",quad,sep="")
    frm<-paste(lin,"+",quad,sep="")
    frm<-as.formula(frm)
    P=model.matrix(frm,data = dsg)
    X=model.matrix(frm,data = data)
    Q=X%*%solve(t(P)%*%P)%*%t(X)
    
    if(nrow(Q)==0){
      plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
      text(0.5,0.5,'inserire il valore delle variabili costanti',cex = 1.6, col = "red")
    }else{
    Lev=data.frame(data,"L"=diag(Q))
    colnames(Lev)[col[1]]<-'x'
    colnames(Lev)[col[2]]<-'y'
    req(input$ccd_lv_z)
    req(input$ccd_lv_x)
    lattice::wireframe(L~x*y,data=Lev,drape=TRUE,col.regions = colorRampPalette(c("yellow","green","blue"))(256),
                       at=seq(min(Lev$L),max(Lev$L),(max(Lev$L)-min(Lev$L))/256),
                       screen=list(z=input$ccd_lv_z,x=-input$ccd_lv_x),
                       main='Plot of Leverage',cex.main=0.8,xlab=var[col[1]],
                       ylab=var[col[2]],zlab=paste('Response'))}
  })
  
  
  
  
  output$ccd_lv_z<-renderUI({
    sliderInput('ccd_lv_z',label = 'Rotazione orizzontale',min = 0,max = 360,value = 30,step = 10)
  })
  
  output$ccd_lv_x<-renderUI({
    sliderInput('ccd_lv_x',label = 'Rotazione verticale',min = 0,max = 90,value = 60,step = 10)
  })
  
  
  output$ccd_risptext<-renderUI({
    h4(paste('Risposte (',nrow(dis_ccd()),', separate da spazio)',sep=''))
  })

  output$ccd_coeff<-renderPrint({
    validate(need(length(as.numeric(unlist(strsplit(input$ccd_risp," "))))==nrow(dis_ccd()),''))
    req(input$ccd_k)
    dis<-dis_ccd()
    var<-c(NULL)
    for(i in 1:input$ccd_k){
      var[i]<-paste("x",i,sep="")
    }
    y<- as.numeric(unlist(strsplit(input$ccd_risp," ")))
    df<-cbind.data.frame(dis,y=y)
    m<-input$ccd_k
    lin<-NULL
    if(m>1){
      for(i in 2:m){
        x<-paste("+x",i,sep="")
        lin<-paste(lin,x,sep="")
      }
    }
    lin<-paste('y~(x1',lin,")^2",sep="")
    
    quad<-NULL
    for(i in 2:m){
      x<-paste("+I(x",i,"^2)",sep="")
      quad<-paste(quad,x)
    }
    quad<-paste("I(x1^2)",quad,sep="")
    frm<-paste(lin,"+",quad,sep="")
    frm<-as.formula(frm)
    mod<-lm(frm,df)
    round(mod$coefficients,2)
  })

  output$ccd_grcoeff<-renderPlot({
    validate(need(length(as.numeric(unlist(strsplit(input$ccd_risp," "))))==nrow(dis_ccd()),''))
    req(input$ccd_k)
    dis<-dis_ccd()
    var<-c(NULL)
    for(i in 1:input$ccd_k){
      var[i]<-paste("x",i,sep="")
    }
    y<- as.numeric(unlist(strsplit(input$ccd_risp," ")))
    df<-cbind.data.frame(dis,y=y)
    m<-input$ccd_k
    lin<-NULL
    if(m>1){
      for(i in 2:m){
        x<-paste("+x",i,sep="")
        lin<-paste(lin,x,sep="")
      }
    }
    lin<-paste('y~(x1',lin,")^2",sep="")
    quad<-NULL
    for(i in 2:m){
      x<-paste("+I(x",i,"^2)",sep="")
      quad<-paste(quad,x)
    }
    quad<-paste("I(x1^2)",quad,sep="")
    frm<-paste(lin,"+",quad,sep="")
    frm<-as.formula(frm)
    mod<-lm(frm,df)
    require(ggplot2)
    df_coeff<-data.frame(nome=names(mod$coefficients[-1]),
                         valore=mod$coefficients[-1])
    s<-summary(mod)
    gdl<-s$df[2]
    q<-qt(p = 0.975,df = gdl)
    s<-s$sigma
    df_coeff<-cbind.data.frame(df_coeff,inf=confint(mod)[-1,1],sup=confint(mod)[-1,2])
    ggplot(data = df_coeff,aes(x =nome,y=valore))+
      xlab("")+ylab("")+theme_light()+
      geom_bar(fill="red",stat="identity")+
      scale_x_discrete(limits=names(mod$coefficients[-1]))+
      geom_errorbar(aes(ymin=inf, ymax=sup),
                    width=0.2, colour="green3")
    
  })

  output$ccd_prev_df<-renderPrint({
    validate(need(length(as.numeric(unlist(strsplit(input$ccd_risp," "))))==nrow(dis_ccd()),''))
    req(input$ccd_k)
    if(length(as.numeric(unlist(strsplit(input$ccd_prev," "))))==input$ccd_k){
      dis<-dis_ccd()
      var<-c(NULL)
      for(i in 1:input$ccd_k){
        var[i]<-paste("x",i,sep="")
      }
      y<- as.numeric(unlist(strsplit(input$ccd_risp," ")))
      df<-cbind.data.frame(dis,y=y)
      m<-input$ccd_k
      lin<-NULL
      if(m>1){
        for(i in 2:m){
          x<-paste("+x",i,sep="")
          lin<-paste(lin,x,sep="")
        }
      }
      lin<-paste('y~(x1',lin,")^2",sep="")
      quad<-NULL
      for(i in 2:m){
        x<-paste("+I(x",i,"^2)",sep="")
        quad<-paste(quad,x)
      }
      quad<-paste("I(x1^2)",quad,sep="")
      frm<-paste(lin,"+",quad,sep="")
      frm<-as.formula(frm)
      mod<-lm(frm,df)
      x<- as.numeric(unlist(strsplit(input$ccd_prev," ")))
      nd<-rbind.data.frame(x)
      colnames(nd)<-var
      prev<-predict(object = mod,newdata=nd)
      attr(prev,'names')<-c('previsione')
      prev
    }else{
      cat(paste('inserire le',input$ccd_k,'coord. del punto'))
    }
  })

  output$ccd_intprev<-renderPrint({
    validate(need(length(as.numeric(unlist(strsplit(input$ccd_risp," "))))==nrow(dis_ccd()) &
                    length(as.numeric(unlist(strsplit(input$ccd_prev," "))))==input$ccd_k,''))
    req(input$ccd_k)
    dis<-dis_ccd()
    var<-c(NULL)
    for(i in 1:input$ccd_k){
      var[i]<-paste("x",i,sep="")
    }
    y<- as.numeric(unlist(strsplit(input$ccd_risp," ")))
    df<-cbind.data.frame(dis,y=y)
    m<-input$ccd_k
    lin<-NULL
    if(m>1){
      for(i in 2:m){
        x<-paste("+x",i,sep="")
        lin<-paste(lin,x,sep="")
      }
    }
    lin<-paste('y~(x1',lin,")^2",sep="")
    
    quad<-NULL
    for(i in 2:m){
      x<-paste("+I(x",i,"^2)",sep="")
      quad<-paste(quad,x)
    }
    quad<-paste("I(x1^2)",quad,sep="")
    frm<-paste(lin,"+",quad,sep="")
    frm<-as.formula(frm)
    mod<-lm(frm,df)

    x<- as.numeric(unlist(strsplit(input$ccd_prev," ")))
    nd<-rbind.data.frame(x)
    colnames(nd)<-var
    prev<-predict(object = mod,newdata=nd,interval='confidence')
    prev1<-predict(object = mod,newdata=nd,interval='confidence',level = 0.99)
    prev2<-predict(object = mod,newdata=nd,interval='confidence',level = 0.999)

    df<-cbind.data.frame(prev,prev1,prev2)
    df<-cbind.data.frame(round(df[,-c(1,4,7)],3))
    colnames(df)<-c('2.5%','97.5%','0.5%','99.5%','0.05%','99.95%')
    df
  })

  output$ccd_misind_media<-renderPrint({
    validate(need(length(as.numeric(unlist(strsplit(input$ccd_misind," "))))>1,''))
    x<-as.numeric(unlist(strsplit(input$ccd_misind," ")))
    mod<-lm(x~1,as.data.frame(x))
    sm<-summary(mod)
    media<-predict(mod,interval = 'confidence')[1,]
    attr(media,'names')<-c('media','2.5%','97.5%')
    round(media,3)
  })
  
  output$ccd_misind_sd<-renderPrint({
    validate(need(length(as.numeric(unlist(strsplit(input$ccd_misind," "))))>1,''))
    x<-as.numeric(unlist(strsplit(input$ccd_misind," ")))
    mod<-lm(x~1,as.data.frame(x))
    sm<-summary(mod)
    df<-c(round(sm$sigma,3))
    attr(df,'names')<-c('dev.st.')
    df
  })
  
  output$ccd_misind_gdl<-renderPrint({
    validate(need(length(as.numeric(unlist(strsplit(input$ccd_misind," "))))>1,''))
    x<-as.numeric(unlist(strsplit(input$ccd_misind," ")))
    gdf<-(length(as.numeric(unlist(strsplit(input$ccd_misind," "))))-1)
    df<-c(gdf)
    attr(df,'names')<-c( 'gdl')
    df
  })
  
  output$ccd_stimint_txt<-renderUI({
    validate(need(length(as.numeric(unlist(strsplit(input$ccd_misind," "))))>0 &
                    length(as.numeric(unlist(strsplit(input$ccd_risp," "))))==nrow(dis_ccd()),''))
    h4('Stima per intervallo')
  })

  output$ccd_stimint<-renderPrint({
    validate(need(length(as.numeric(unlist(strsplit(input$ccd_risp," "))))==nrow(dis_ccd()),''))
      req(input$ccd_k)
      dis<-dis_ccd()
      var<-c(NULL)
      for(i in 1:input$ccd_k){
        var[i]<-paste("x",i,sep="")
      }
      y<- as.numeric(unlist(strsplit(input$ccd_risp," ")))
      df<-cbind.data.frame(dis,y=y)
      m<-input$ccd_k
      lin<-NULL
      if(m>1){
        for(i in 2:m){
          x<-paste("+x",i,sep="")
          lin<-paste(lin,x,sep="")
        }
      }
      lin<-paste('y~(x1',lin,")^2",sep="")
      
      quad<-NULL
      for(i in 2:m){
        x<-paste("+I(x",i,"^2)",sep="")
        quad<-paste(quad,x)
      }
      quad<-paste("I(x1^2)",quad,sep="")
      frm<-paste(lin,"+",quad,sep="")
      frm<-as.formula(frm)
      mod<-lm(frm,df)
      smr<-summary(mod)
      pval<- smr$coefficients[,4,drop=FALSE]
      df<-data.frame(confint(mod)[,1],confint(mod)[,2],
                     confint(mod,level = 0.99)[,1],confint(mod,level = 0.99)[,2],
                     confint(mod,level = 0.999)[,1],confint(mod,level = 0.999)[,2],pval)
      lab<-rep('',nrow(df))
      for (i in 1:nrow(df)){
        if(pval[i]<0.1)lab[i]<-'.'
        if(pval[i]<0.05)lab[i]<-'*'
        if(pval[i]<0.01)lab[i]<-'**'
        if(pval[i]<0.001)lab[i]<-'***'
      }
      df<-cbind.data.frame(round(df[,1:6],3),round(df[,7],4),lab)
      colnames(df)<-c('2.5%','97.5%','0.5%','99.5%','0.05%','99.95%','p-value','')
      df
  })

  output$ccd_mod_selvar<-renderUI({
    validate(need(input$ccd_k>2,''))
    var<-c(NULL)
    for(i in 1:input$ccd_k){
      var[i]<-paste("x",i,sep="")
    }
    selectInput("ccd_mod_selvar", label = h5("Selezionare 2 variabili"), 
                choices = var, 
                multiple = TRUE,selected = var[1:2])
  })
  
  output$ccd_mod_fixvar<-renderUI({
    validate(need(input$ccd_k>2,''))
    req(input$ccd_mod_selvar)
    vl<-'0'
    if(input$ccd_k>3){
      for(i in 2:(input$ccd_k-2)){
        vl<-paste(vl,'0')
      }
    }
    var<-c(NULL)
    for(i in 1:input$ccd_k){
      var[i]<-paste("x",i,sep="")
    }
    col<-c(1,2)
    if(input$ccd_k>2 & length(input$ccd_mod_selvar)>=2){
      col<-which(var%in%input$ccd_mod_selvar==TRUE)
      col_fix<-which(var%in%var[col][1:2]==FALSE)
    }
    var_fix<-c(NULL)
    for(i in 1:length(col_fix)){
      var_fix<-paste(var_fix,var[col_fix][i])
    }
    if(length(col_fix)==1){
      txt<-paste('valore della  variabile',var[col_fix])
    }else{
      txt<-paste('valori delle  variabili',var_fix, '(separati da spazio)')
    }
    textInput(inputId = "ccd_mod_fixvar",label = h5(txt),value = vl)
  })

  output$ccd_livellorisp<-renderPlot({
    validate(need(length(as.numeric(unlist(strsplit(input$ccd_risp," "))))==nrow(dis_ccd()),''))
    req(input$ccd_k)
    req(input$ccd_livellorisp_col)
    dis<-dis_ccd()
    var<-c(NULL)
    for(i in 1:input$ccd_k){
      var[i]<-paste("x",i,sep="")
    }

    y<-as.numeric(unlist(strsplit(input$ccd_risp," ")))
    dsg<-cbind.data.frame(dis,y=y)
    dt<-as.list(NULL)
    for(i in 1:2){
      dt[[i]]=seq(-1,1,0.1)
    }
    dt_exp<-expand.grid(dt)
    data<-matrix(0,441,input$ccd_k)
    col<-c(1,2)
    if(input$ccd_k>2 & length(input$ccd_mod_selvar)>=2){
      col<-which(var%in%input$ccd_mod_selvar==TRUE)
      col_fix<-which(var%in%var[col][1:2]==FALSE)
      fix<-as.numeric(unlist(strsplit(input$ccd_mod_fixvar," ")))
      for(i in 1:(input$ccd_k-2))data[,col_fix[i]]<-rep(fix[i],441)
      
    }
    data[,col[1]]<-dt_exp[,1]
    data[,col[2]]<-dt_exp[,2]
    data<-as.data.frame(data)
    colnames(data)<-var
    m<-input$ccd_k
    lin<-NULL
    if(m>1){
      for(i in 2:m){
        x<-paste("+x",i,sep="")
        lin<-paste(lin,x,sep="")
      }
    }
    lin<-paste('y~(x1',lin,")^2",sep="")
    
    quad<-NULL
    for(i in 2:m){
      x<-paste("+I(x",i,"^2)",sep="")
      quad<-paste(quad,x)
    }
    quad<-paste("I(x1^2)",quad,sep="")
    frm<-paste(lin,"+",quad,sep="")
    frm<-as.formula(frm)
    mod<-lm(frm,dsg)
    Pred=data.frame(data,P=predict(mod,newdata=data))
    colnames(Pred)[col[1]]<-'x'
    colnames(Pred)[col[2]]<-'y'
    colore<-c("blue","green3","red","black","purple1")
    cl<-as.integer(input$ccd_livellorisp_col)
    
    if(is.na(Pred$P[1])==TRUE){
      plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
      text(0.5,0.5,'inserire il valore delle variabili costanti',cex = 1.6, col = "red")
    }else{
    lattice::contourplot(P~x*y,data=Pred,cuts=15,main='Response Surface: Contour Plot',cex.main=0.8,
                         xlab=var[col[1]],ylab=var[col[2]],col=colore[cl],
                         labels=list(col=colore[cl],cex=0.9),
                         aspect=1)}
  })
  
  output$ccd_livellorisp_col<-renderUI({
    validate(need(length(as.numeric(unlist(strsplit(input$ccd_risp," "))))==nrow(dis_ccd()),''))
    selectInput("ccd_livellorisp_col", label = h3(""), 
                choices = list("blu" = 1, "verde" = 2, "rosso" = 3,"nero" = 4,"viola" = 5), 
                selected = 1,width="130px")
  })
  
  output$ccd_suprisp<-renderPlot({
    validate(need(length(as.numeric(unlist(strsplit(input$ccd_risp," "))))==nrow(dis_ccd()),''))
    req(input$ccd_k)
    dis<-dis_ccd()
    var<-c(NULL)
    for(i in 1:input$ccd_k){
      var[i]<-paste("x",i,sep="")
    }
    y<-as.numeric(unlist(strsplit(input$ccd_risp," ")))
    dsg<-cbind.data.frame(dis,y=y)
    dt<-as.list(NULL)
    for(i in 1:2){
      dt[[i]]=seq(-1,1,0.1)
    }
    dt_exp<-expand.grid(dt)
    data<-matrix(0,441,input$ccd_k)
    col<-c(1,2)
    if(input$ccd_k>2 & length(input$ccd_mod_selvar)>=2){
      col<-which(var%in%input$ccd_mod_selvar==TRUE)
      col_fix<-which(var%in%var[col][1:2]==FALSE)
      fix<-as.numeric(unlist(strsplit(input$ccd_mod_fixvar," ")))
      for(i in 1:(input$ccd_k-2))data[,col_fix[i]]<-rep(fix[i],441)
      
    }
    data[,col[1]]<-dt_exp[,1]
    data[,col[2]]<-dt_exp[,2]
    data<-as.data.frame(data)
    colnames(data)<-var
    m<-input$ccd_k
    lin<-NULL
    if(m>1){
      for(i in 2:m){
        x<-paste("+x",i,sep="")
        lin<-paste(lin,x,sep="")
      }
    }
    lin<-paste('y~(x1',lin,")^2",sep="")
    quad<-NULL
    for(i in 2:m){
      x<-paste("+I(x",i,"^2)",sep="")
      quad<-paste(quad,x)
    }
    quad<-paste("I(x1^2)",quad,sep="")
    frm<-paste(lin,"+",quad,sep="")
    frm<-as.formula(frm)
    mod<-lm(frm,dsg)
    Pred=data.frame(data,P=predict(mod,newdata=data))
    colnames(Pred)[col[1]]<-'x'
    colnames(Pred)[col[2]]<-'y'
    
    if(is.na(Pred$P[1])==TRUE){
      plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
      text(0.5,0.5,'inserire il valore delle variabili costanti',cex = 1.6, col = "red")
    }else{
      req(input$ccd_rp_z)
      req(input$ccd_rp_x)
    lattice::wireframe(P~x*y,data=Pred,drape=TRUE,col.regions = colorRampPalette(c("yellow","green","blue"))(256),
                       at=seq(min(Pred$P),max(Pred$P),(max(Pred$P)-min(Pred$P))/256),
                       screen=list(z=input$ccd_rp_z,x=-input$ccd_rp_x),
                       main='Response Surface',cex.main=0.8,xlab=var[col[1]],
                       ylab=var[col[2]],zlab=paste('Response'))
    }
  })
  
  output$ccd_rp_z<-renderUI({
    sliderInput('ccd_rp_z',label = 'Rotazione orizzontale',min = 0,max = 360,value = 30,step = 10)
  })
  
  output$ccd_rp_x<-renderUI({
    sliderInput('ccd_rp_x',label = 'Rotazione verticale',min = 0,max = 90,value = 60,step = 10)
  })
  
  # D-ottimale -----------------------------------------------------------------  
  output$d_opt_titolo<-renderUI({
    HTML("D-ottimale" )
  })
  output$d_opt_importa<-renderUI({
    validate(need(input$d_opt_pti_cand==2, ' '))
    radioButtons("d_opt_importa", label = "Importa matrice punti candidati:",
                 choices = list("Incolla" = 1, "Excel" = 2), selected = 1,inline = TRUE)
  })
  output$d_opt_importa_incolla_spazio<-renderUI({
    validate(need(input$d_opt_pti_cand==2, ' '))
    req(input$d_opt_importa==1)
    br()
  })
  output$d_opt_importa_incolla<-renderUI({
    validate(need(input$d_opt_pti_cand==2, ' '))
    req(input$d_opt_importa==1)
    actionButton("d_opt_incolla", label = "Incolla")
  })
  output$d_opt_importa_incolla_spazio1<-renderUI({
    validate(need(input$d_opt_pti_cand==2, ' '))
    req(input$d_opt_importa==1)
    hr()
  })
  output$d_opt_importa_excel_brws<-renderUI({
    validate(need(input$d_opt_pti_cand==2, ' '))
    req(input$d_opt_importa==2)
    fileInput("d_opt_file_xlsx",label='',
              multiple = FALSE,
              accept = c(".xlx",".xlsx"))
  })
  output$d_opt_costruisci<-renderUI({
    validate(need(input$d_opt_pti_cand==1, ' '))
    radioButtons("d_opt_costruisci", label = "Costruisci matrice assegnando:",
                 choices = list("Livelli variabili " = 1, "Passo griglia" = 2), selected = 1,inline = TRUE)
  })
  output$d_opt_nvar<-renderUI({
    validate(need(input$d_opt_pti_cand==1, ' '))
    numericInput("d_opt_nvar", label = "n° fattori", value = 2,min = 2,max=50,width = "30%")
  })
  output$d_opt_passo<-renderUI({
    validate(need(input$d_opt_pti_cand==1&input$d_opt_costruisci==2, ' '))
    textInput(inputId = "d_opt_passo",label = "passo griglia",value = "0.1",width = "50%")
  })
  output$d_opt_lev_var1<-renderUI({
    validate(need(input$d_opt_nvar>=2& input$d_opt_costruisci==1&input$d_opt_pti_cand==1&input$d_opt_pti_cand==1,""))
    textInput("d_opt_lev_var1", label = 'Livello delle variabili (separati da spazio)', value = "Livelli della variabile 1? (e.g. '-1 0 1')")
  })
  output$d_opt_lev_var2<-renderUI({
    validate(need(input$d_opt_nvar>=2& input$d_opt_costruisci==1&input$d_opt_pti_cand==1&input$d_opt_pti_cand==1,""))
    textInput("d_opt_lev_var2", label = ' ', value = "Livelli della variabile 2? (e.g. '-1 0 1')")
  })
  output$d_opt_lev_var3<-renderUI({
    validate(need(input$d_opt_nvar>=3& input$d_opt_costruisci==1&input$d_opt_pti_cand==1,""))
    textInput("d_opt_lev_var3", label = ' ', value = "Livelli della variabile 3? (e.g. '-1 0 1')")
  })
  output$d_opt_lev_var4<-renderUI({
    validate(need(input$d_opt_nvar>=4& input$d_opt_costruisci==1&input$d_opt_pti_cand==1,""))
    textInput("d_opt_lev_var4", label = ' ', value = "Livelli della variabile 4? (e.g. '-1 0 1')")
  })
  output$d_opt_lev_var5<-renderUI({
    validate(need(input$d_opt_nvar>=5& input$d_opt_costruisci==1&input$d_opt_pti_cand==1,""))
    textInput("d_opt_lev_var5", label = ' ', value = "Livelli della variabile 5? (e.g. '-1 0 1')")
  })
  output$d_opt_lev_var6<-renderUI({
    validate(need(input$d_opt_nvar>=6& input$d_opt_costruisci==1&input$d_opt_pti_cand==1,""))
    textInput("d_opt_lev_var6", label = ' ', value = "Livelli della variabile 6? (e.g. '-1 0 1')")
  })
  output$d_opt_lev_var7<-renderUI({
    validate(need(input$d_opt_nvar>=7& input$d_opt_costruisci==1&input$d_opt_pti_cand==1,""))
    textInput("d_opt_lev_var7", label = ' ', value = "Livelli della variabile 7? (e.g. '-1 0 1')")
  })
  output$d_opt_lev_var8<-renderUI({
    validate(need(input$d_opt_nvar>=8& input$d_opt_costruisci==1&input$d_opt_pti_cand==1,""))
    textInput("d_opt_lev_var8", label = ' ', value = "Livelli della variabile 8? (e.g. '-1 0 1')")
  })
  output$d_opt_lev_var9<-renderUI({
    validate(need(input$d_opt_nvar>=9& input$d_opt_costruisci==1&input$d_opt_pti_cand==1,""))
    textInput("d_opt_lev_var9", label = ' ', value = "Livelli della variabile 9? (e.g. '-1 0 1')")
  })
  output$d_opt_lev_var10<-renderUI({
    validate(need(input$d_opt_nvar>=10& input$d_opt_costruisci==1&input$d_opt_pti_cand==1,""))
    textInput("d_opt_lev_var10", label = ' ', value = "Livelli della variabile 10? (e.g. 10,15,20')")
  })
  output$d_opt_lev_var11<-renderUI({
    validate(need(input$d_opt_nvar>=11& input$d_opt_costruisci==1&input$d_opt_pti_cand==1,""))
    textInput("d_opt_lev_var11", label = ' ', value = "Livelli della variabile 11? (e.g. '-1 0 1')")
  })
  output$d_opt_lev_var12<-renderUI({
    validate(need(input$d_opt_nvar>=12& input$d_opt_costruisci==1&input$d_opt_pti_cand==1,""))
    textInput("d_opt_lev_var12", label = ' ', value = "Livelli della variabile 12? (e.g. '-1 0 1')")
  })
  output$d_opt_lev_var13<-renderUI({
    validate(need(input$d_opt_nvar>=13& input$d_opt_costruisci==1&input$d_opt_pti_cand==1,""))
    textInput("d_opt_lev_var13", label = ' ', value = "Livelli della variabile 13? (e.g. '-1 0 1')")
  })
  output$d_opt_lev_var14<-renderUI({
    validate(need(input$d_opt_nvar>=14& input$d_opt_costruisci==1&input$d_opt_pti_cand==1,""))
    textInput("d_opt_lev_var14", label = ' ', value = "Livelli della variabile 14? (e.g. '-1 0 1')")
  })
  output$d_opt_lev_var15<-renderUI({
    validate(need(input$d_opt_nvar>=15& input$d_opt_costruisci==1&input$d_opt_pti_cand==1,""))
    textInput("d_opt_lev_var15", label = ' ', value = "Livelli della variabile 15? (e.g. '-1 0 1')")
  })
  output$d_opt_lev_var16<-renderUI({
    validate(need(input$d_opt_nvar>=16& input$d_opt_costruisci==1&input$d_opt_pti_cand==1,""))
    textInput("d_opt_lev_var16", label = ' ', value = "Livelli della variabile 16? (e.g. '-1 0 1')")
  })
  output$d_opt_lev_var17<-renderUI({
    validate(need(input$d_opt_nvar>=17& input$d_opt_costruisci==1&input$d_opt_pti_cand==1,""))
    textInput("d_opt_lev_var17", label = ' ', value = "Livelli della variabile 17? (e.g. '-1 0 1')")
  })
  output$d_opt_lev_var18<-renderUI({
    validate(need(input$d_opt_nvar>=18& input$d_opt_costruisci==1&input$d_opt_pti_cand==1,""))
    textInput("d_opt_lev_var18", label = ' ', value = "Livelli della variabile 18? (e.g. '-1 0 1')")
  })
  output$d_opt_lev_var19<-renderUI({
    validate(need(input$d_opt_nvar>=19& input$d_opt_costruisci==1&input$d_opt_pti_cand==1,""))
    textInput("d_opt_lev_var19", label = ' ', value = "Livelli della variabile 19? (e.g. '-1 0 1')")
  })
  output$d_opt_lev_var10<-renderUI({
    validate(need(input$d_opt_nvar>=20& input$d_opt_costruisci==1&input$d_opt_pti_cand==1,""))
    textInput("d_opt_lev_var20", label = ' ', value = "Livelli della variabile 20? (e.g. '-1 0 1')")
  })
  output$d_opt_lev_var11<-renderUI({
    validate(need(input$d_opt_nvar>=21& input$d_opt_costruisci==1&input$d_opt_pti_cand==1,""))
    textInput("d_opt_lev_var21", label = ' ', value = "Livelli della variabile 21? (e.g. '-1 0 1')")
  })
  output$d_opt_lev_var12<-renderUI({
    validate(need(input$d_opt_nvar>=22& input$d_opt_costruisci==1&input$d_opt_pti_cand==1,""))
    textInput("d_opt_lev_var22", label = ' ', value = "Livelli della variabile 22? (e.g. '-1 0 1')")
  })
  output$d_opt_lev_var13<-renderUI({
    validate(need(input$d_opt_nvar>=23& input$d_opt_costruisci==1&input$d_opt_pti_cand==1,""))
    textInput("d_opt_lev_var23", label = ' ', value = "Livelli della variabile 23? (e.g. '-1 0 1')")
  })
  output$d_opt_lev_var14<-renderUI({
    validate(need(input$d_opt_nvar>=24& input$d_opt_costruisci==1&input$d_opt_pti_cand==1,""))
    textInput("d_opt_lev_var24", label = ' ', value = "Livelli della variabile 24? (e.g. '-1 0 1')")
  })
  output$d_opt_lev_var15<-renderUI({
    validate(need(input$d_opt_nvar>=25& input$d_opt_costruisci==1&input$d_opt_pti_cand==1,""))
    textInput("d_opt_lev_var25", label = ' ', value = "Livelli della variabile 25? (e.g. '-1 0 1')")
  })
  output$d_opt_lev_var16<-renderUI({
    validate(need(input$d_opt_nvar>=26& input$d_opt_costruisci==1&input$d_opt_pti_cand==1,""))
    textInput("d_opt_lev_var26", label = ' ', value = "Livelli della variabile 26? (e.g. '-1 0 1')")
  })
  output$d_opt_lev_var17<-renderUI({
    validate(need(input$d_opt_nvar>=27& input$d_opt_costruisci==1&input$d_opt_pti_cand==1,""))
    textInput("d_opt_lev_var27", label = ' ', value = "Livelli della variabile 27? (e.g. '-1 0 1')")
  })
  output$d_opt_lev_var18<-renderUI({
    validate(need(input$d_opt_nvar>=28& input$d_opt_costruisci==1&input$d_opt_pti_cand==1,""))
    textInput("d_opt_lev_var28", label = ' ', value = "Livelli della variabile 28? (e.g. '-1 0 1')")
  })
  output$d_opt_lev_var19<-renderUI({
    validate(need(input$d_opt_nvar>=29& input$d_opt_costruisci==1&input$d_opt_pti_cand==1,""))
    textInput("d_opt_lev_var29", label = ' ', value = "Livelli della variabile 29? (e.g. '-1 0 1')")
  })
  output$d_opt_lev_var10<-renderUI({
    validate(need(input$d_opt_nvar>=30& input$d_opt_costruisci==1&input$d_opt_pti_cand==1,""))
    textInput("d_opt_lev_var10", label = ' ', value = "Livelli della variabile 30? (e.g. '-1 0 1')")
  })
  output$d_opt_lev_var11<-renderUI({
    validate(need(input$d_opt_nvar>=31& input$d_opt_costruisci==1&input$d_opt_pti_cand==1,""))
    textInput("d_opt_lev_var11", label = ' ', value = "Livelli della variabile 31? (e.g. '-1 0 1')")
  })
  output$d_opt_lev_var12<-renderUI({
    validate(need(input$d_opt_nvar>=32& input$d_opt_costruisci==1&input$d_opt_pti_cand==1,""))
    textInput("d_opt_lev_var12", label = ' ', value = "Livelli della variabile 32? (e.g. '-1 0 1')")
  })
  output$d_opt_lev_var13<-renderUI({
    validate(need(input$d_opt_nvar>=33& input$d_opt_costruisci==1&input$d_opt_pti_cand==1,""))
    textInput("d_opt_lev_var13", label = ' ', value = "Livelli della variabile 33? (e.g. '-1 0 1')")
  })
  output$d_opt_lev_var14<-renderUI({
    validate(need(input$d_opt_nvar>=34& input$d_opt_costruisci==1&input$d_opt_pti_cand==1,""))
    textInput("d_opt_lev_var14", label = ' ', value = "Livelli della variabile 34? (e.g. '-1 0 1')")
  })
  output$d_opt_lev_var15<-renderUI({
    validate(need(input$d_opt_nvar>=35& input$d_opt_costruisci==1&input$d_opt_pti_cand==1,""))
    textInput("d_opt_lev_var15", label = ' ', value = "Livelli della variabile 35? (e.g. '-1 0 1')")
  })
  output$d_opt_lev_var16<-renderUI({
    validate(need(input$d_opt_nvar>=36& input$d_opt_costruisci==1&input$d_opt_pti_cand==1,""))
    textInput("d_opt_lev_var16", label = ' ', value = "Livelli della variabile 36? (e.g. '-1 0 1')")
  })
  output$d_opt_lev_var17<-renderUI({
    validate(need(input$d_opt_nvar>=37& input$d_opt_costruisci==1&input$d_opt_pti_cand==1,""))
    textInput("d_opt_lev_var17", label = ' ', value = "Livelli della variabile 37? (e.g. '-1 0 1')")
  })
  output$d_opt_lev_var18<-renderUI({
    validate(need(input$d_opt_nvar>=38& input$d_opt_costruisci==1&input$d_opt_pti_cand==1,""))
    textInput("d_opt_lev_var18", label = ' ', value = "Livelli della variabile 38? (e.g. '-1 0 1')")
  })
  output$d_opt_lev_var19<-renderUI({
    validate(need(input$d_opt_nvar>=39& input$d_opt_costruisci==1&input$d_opt_pti_cand==1,""))
    textInput("d_opt_lev_var19", label = ' ', value = "Livelli della variabile 39? (e.g. '-1 0 1')")
  })
  output$d_opt_lev_var10<-renderUI({
    validate(need(input$d_opt_nvar>=40& input$d_opt_costruisci==1&input$d_opt_pti_cand==1,""))
    textInput("d_opt_lev_var40", label = ' ', value = "Livelli della variabile 40? (e.g. '-1 0 1')")
  })
  output$d_opt_lev_var11<-renderUI({
    validate(need(input$d_opt_nvar>=41& input$d_opt_costruisci==1&input$d_opt_pti_cand==1,""))
    textInput("d_opt_lev_var41", label = ' ', value = "Livelli della variabile 41? (e.g. '-1 0 1')")
  })
  output$d_opt_lev_var12<-renderUI({
    validate(need(input$d_opt_nvar>=42& input$d_opt_costruisci==1&input$d_opt_pti_cand==1,""))
    textInput("d_opt_lev_var42", label = ' ', value = "Livelli della variabile 42? (e.g. '-1 0 1')")
  })
  output$d_opt_lev_var13<-renderUI({
    validate(need(input$d_opt_nvar>=43& input$d_opt_costruisci==1&input$d_opt_pti_cand==1,""))
    textInput("d_opt_lev_var43", label = ' ', value = "Livelli della variabile 43? (e.g. '-1 0 1')")
  })
  output$d_opt_lev_var14<-renderUI({
    validate(need(input$d_opt_nvar>=44& input$d_opt_costruisci==1&input$d_opt_pti_cand==1,""))
    textInput("d_opt_lev_var44", label = ' ', value = "Livelli della variabile 44? (e.g. '-1 0 1')")
  })
  output$d_opt_lev_var15<-renderUI({
    validate(need(input$d_opt_nvar>=45& input$d_opt_costruisci==1&input$d_opt_pti_cand==1,""))
    textInput("d_opt_lev_var45", label = ' ', value = "Livelli della variabile 45? (e.g. '-1 0 1')")
  })
  output$d_opt_lev_var16<-renderUI({
    validate(need(input$d_opt_nvar>=46& input$d_opt_costruisci==1&input$d_opt_pti_cand==1,""))
    textInput("d_opt_lev_var46", label = ' ', value = "Livelli della variabile 46? (e.g. '-1 0 1')")
  })
  output$d_opt_lev_var17<-renderUI({
    validate(need(input$d_opt_nvar>=47& input$d_opt_costruisci==1&input$d_opt_pti_cand==1,""))
    textInput("d_opt_lev_var47", label = ' ', value = "Livelli della variabile 47? (e.g. '-1 0 1')")
  })
  output$d_opt_lev_var18<-renderUI({
    validate(need(input$d_opt_nvar>=48& input$d_opt_costruisci==1&input$d_opt_pti_cand==1,""))
    textInput("d_opt_lev_var48", label = ' ', value = "Livelli della variabile 48? (e.g. '-1 0 1')")
  })
  output$d_opt_lev_var19<-renderUI({
    validate(need(input$d_opt_nvar>=49& input$d_opt_costruisci==1&input$d_opt_pti_cand==1,""))
    textInput("d_opt_lev_var49", label = ' ', value = "Livelli della variabile 49? (e.g. '-1 0 1')")
  })
  output$d_opt_lev_var50<-renderUI({
    validate(need(input$d_opt_nvar>=50& input$d_opt_costruisci==1&input$d_opt_pti_cand==1&input$d_opt_pti_cand==1,""))
    textInput("d_opt_lev_var50", label = ' ', value = "Livelli della variabile 50? (e.g. '-1 0 1')")
  })
  d_opt_cp_paste<-eventReactive(input$d_opt_incolla,{
    df<-tryCatch(read.DIF(file = "clipboard",header = TRUE,transpose = TRUE),
                 error = function(e) "Selezionare un dataset!")
    df
  })
  d_opt_cp_xls<-reactive({
    validate(need(input$d_opt_importa,''))
    req(input$d_opt_file_xlsx$datapath)
    df=read_excel(path = input$d_opt_file_xlsx$datapath,sheet = 1,col_names = TRUE)
    df<-as.data.frame(df)
    df
  })
  d_opt_cp_griglia<-reactive({
    req(input$d_opt_nvar)
    req(input$d_opt_passo)
    n<-input$d_opt_nvar
    delta<-as.numeric(unlist(strsplit(input$d_opt_passo," ")))
    s = seq(-1, 1, delta)
    l<-length(s)
    validate(need(l^n<=10000,"Aumentare il passo della griglia!\n "),
             errorClass = "myClass") 
    x = data.frame(x1 = s)
    df = data.frame((NULL))
    y = list("x1")
    for (i in 2:n) {
      x = data.frame(x, s)
      y[i] = paste("x", i, sep = "")
    }
    df = expand.grid(x)
    names(df) = y
    df
  })
  d_opt_cp_livelli<-reactive({
    req(input$d_opt_nvar)
    n<-input$d_opt_nvar
    lv<-list(NULL)
    nl<-rep(1,n)
    for(i in 1:n){
      lev<-paste0("input$d_opt_lev_var",i)
      req(!eval(parse(text=lev))==paste0('Livelli della variabile ',i,"? (e.g. '-1 0 1')"))
      if(eval(parse(text=lev))!='')lv[[i]]<-as.numeric(unlist(strsplit(eval(parse(text=lev))," ")))
      if(length(unlist(strsplit(eval(parse(text=lev))," ")))==0)nl[i]<-0
    }
    if(sum(nl)==n){
      cp<-expand.grid(lv)
      c_n<-"x1"
      for(i in 2:n){
        c_n<-paste(c_n,",x",i,sep="")
      }
      colnames(cp)<-unlist(strsplit(c_n,","))
    }else{
      cp<-"Missing levels of some variable"}
    cp
    
  })
  d_opt_cp1<-reactive({
    validate(need(input$d_opt_costruisci,''))
    if(input$d_opt_pti_cand==1 & input$d_opt_costruisci==1)df<-d_opt_cp_livelli()
    if(input$d_opt_pti_cand==1 & input$d_opt_costruisci==2)df<-d_opt_cp_griglia()
    df
  })
  d_opt_cp2<-reactive({
    validate(need(input$d_opt_importa,''))
    if(input$d_opt_pti_cand==2 & input$d_opt_importa==1)df<-d_opt_cp_paste()
    if(input$d_opt_pti_cand==2 & input$d_opt_importa==2)df<-d_opt_cp_xls()
    
    df
  })
  d_opt_cp<-reactive({
    if(input$d_opt_pti_cand==1)df<-d_opt_cp1()
    if(input$d_opt_pti_cand==2)df<-d_opt_cp2()
    if(sum(df[,1]==c(1:nrow(df)))==nrow(df))df<-df[,-1]
    df
  })
  output$d_opt_cp<-renderTable({
    validate(need(is.data.frame(d_opt_cp()),""),
             errorClass = "myClass") 
    if(input$d_opt_cp_ha==1)df<-head(d_opt_cp())
    if(input$d_opt_cp_ha==2)df<-d_opt_cp()
    df
  })
  output$d_opt_download <- downloadHandler(
    filename = "cp.xlsx", 
    content = function(file) {
      dis<-d_opt_cp()
      exp<-seq(1,nrow(dis))
      dis<-cbind.data.frame('Exp#'=exp,dis)
      write.xlsx(dis, file,colNames=TRUE)
    })
  d_opt_var_sel<-reactive({
    validate(need(is.data.frame(d_opt_cp()),""))
    var<-colnames(d_opt_cp())
    frm_lin<-paste0(var,collapse = '+')
    frm_lin<-paste0('-1+',frm_lin)
    quad<-paste0('I(',var,'^2)')
    frm_quad<-paste0(quad,collapse = '+')
    #frm_inter<-paste0(var,collapse = '*')
    frm_inter<-paste0('(',frm_lin,')^2')
    if(!'2'%in%input$d_opt_mod_tipo)frm<-frm_lin
    if('2'%in%input$d_opt_mod_tipo)frm<-paste0(frm_lin,'+',frm_quad,'+',frm_inter)
    frm<-paste0('~',frm)
    X<-model.matrix(as.formula(frm),d_opt_cp())
    n<-ncol(d_opt_cp())
    var<-colnames(X[,1:n])
    if('2'%in%input$d_opt_mod_tipo){
      sel_q<-rep(1,n)
      for(i in 1:n){
        if(length(unique(X[,i]))==2 | length(unique(X[,i]))==1)sel_q[i]=0
      }
      D<-as.data.frame(matrix(sel_q,ncol=n))
      colnames(D)<-colnames(X[,1:n])
      Y<-model.matrix(as.formula(paste0('~',frm_lin,'+',frm_quad,'-1')),D)
      m<-ncol(Y)
      var_quad<-colnames(Y)[(n+1):m][Y[1,(n+1):m]==1]
      sel_i<-rep(1,n)
      for(i in 1:n){
        if(length(unique(X[,i]))==2&min(unique(X[,i]))==0&max(unique(X[,i]))==1)sel_i[i]=0
      }
      D<-as.data.frame(matrix(sel_i,ncol=n))
      colnames(D)<-colnames(X[,1:n])
      Z<-model.matrix(as.formula(paste0('~',frm_lin,'+',frm_inter)),D)
      m<-ncol(Z)
      var_inter<-colnames(Z)[(n+1):m][Z[1,(n+1):m]==1]
      var<-c(var,var_quad,var_inter)
    }
    var
  })
  output$d_opt_mod_variabx<-renderUI({
    validate(need(is.data.frame(d_opt_cp()),""))
    selectizeInput(inputId = "d_opt_mod_variabx",label="Termini modello (x)",
                   #div("Termini modello (x)",style="font-weight: 400"),
                   choices = d_opt_var_sel(),
                   selected=d_opt_var_sel(),
                   multiple=TRUE)
  })
  d_opt_formula<-reactive({
    req(input$d_opt_mod_variabx)
    var<-input$d_opt_mod_variabx
    if('1'%in%input$d_opt_mod_tipo){
      frm<-paste0(var,collapse = '+')
    }
    if(!'1'%in%input$d_opt_mod_tipo){
      frm<-paste0(var,collapse = '+')
      frm<-paste0('-1+',frm)
    }
    frm<-paste0('~',frm)
    frm
  })
  output$d_opt_modello<-renderText({
    validate(need(is.data.frame(d_opt_cp()),""))
    req(input$d_opt_mod_variabx)
    var<-input$d_opt_mod_variabx
    var<-str_remove(var,"\\(")
    var<-str_remove(var,"I")
    var<-str_remove(var,"\\)")
    if('1'%in%input$d_opt_mod_tipo){
      frm<-paste0(var,collapse = '+')
      frm<-paste0('1+',frm)
    }
    if(!'1'%in%input$d_opt_mod_tipo){
      frm<-paste0(var,collapse = '+')
    }
    frm<-paste0('y ~ ',frm)
    frm
  })
  output$d_opt_Lnumexp<-renderUI({
    x<-model.matrix(formula(d_opt_formula()),d_opt_cp())
    r<-nrow(x)
    co<-ncol(x)
    numericInput(inputId = 'd_opt_Lnumexp',label = 'Numero minimo di esperimenti',value = co,min = co,width = "30%")
  })
  output$d_opt_Unumexp<-renderUI({
    req(input$d_opt_Lnumexp)
    req(d_opt_cp())
    x<-model.matrix(formula(d_opt_formula()),d_opt_cp())
    r<-nrow(x)
    co<-input$d_opt_Lnumexp
    
    numericInput(inputId = 'd_opt_Unumexp',label = 'Numero massimo di esperimenti',value = co,max=r,min=co,width = "30%")
  })
  d_opt_federov<-eventReactive(input$d_opt_calc,{
    req(input$d_opt_Unumexp)
    req(input$d_opt_Lnumexp)
    fmrl<-formula(d_opt_formula())
    data<-d_opt_cp()
    nTrials<-input$d_opt_Unumexp
    p <- input$d_opt_Lnumexp
    nRepeats<-10
    vif<-TRUE
    logD<-FALSE
    validate(need(input$d_opt_Unumexp>=input$d_opt_Lnumexp,""),
             errorClass = "myClass") 
      s = as.vector(NULL)
      t = as.vector(NULL)
      dis<-as.list(NULL)
      if (vif) 
        v = as.vector(NULL)
      n<-nTrials-p+1
      withProgress(message = 'D-ottimale:',value = 0, {
        for (i in p:nTrials) {
          incProgress(detail = paste("NumExp", i),amount = 1/n)
          Dis.opt<-tryCatch(AlgDesign::optFederov(fmrl, nRepeats = nRepeats, 
                                                  data = data, nTrials = i, criterion = "D"),
                            error = function(e) "errore")

          if(Dis.opt=="errore")next()
          s[i - (p - 1)] = i
          t[i - (p - 1)] = Dis.opt$D
          if (logD) 
            t[i - (p - 1)] = log10(Dis.opt$D)
          if (vif) 
            v[i - (p - 1)] = max(vif(model.matrix(fmrl, Dis.opt$design)))
          dis[[i - (p - 1)]]<-Dis.opt$design
        }
      })
      X = data.frame(NumExp = s, D = t, Vif.max=v)
    list(X=X,dis=dis)
  })
  output$d_opt_table<-renderTable(digits = 4,{
    validate(need(ncol(d_opt_federov()$X)>0 & sum(is.na(d_opt_federov()$X))==0 ,
                  'Trovata matrice con rango insufficiente \nRiprovare!'))
    d_opt_federov()$X
  })
  
  msg1<-eventReactive(input$d_opt_calc,{
    df<-tryCatch(d_opt_cp() ,
                 error = function(e) "1")
    validate(need(df!='1','Costruire la matrice dei punti candidati!'),errorClass = "myClass")
  })
  output$d_opt_msg1<-renderTable({
    msg1()
  })
  output$d_opt_graf_D<-renderPlot({
    validate(need(ncol(d_opt_federov()$X)>0 & sum(is.na(d_opt_federov()$X))==0,''))
    plot(d_opt_federov()$X[,1],d_opt_federov()$X[,2],col='red',type='b',xlab='Numero di esperimenti',
         ylab='D');grid()
  })
  output$d_opt_graf_Vif<-renderPlot({
    validate(need(ncol(d_opt_federov()$X)>0 & sum(is.na(d_opt_federov()$X))==0,''))
    maxinfl<-max(d_opt_federov()$X[,3])
    plot(d_opt_federov()$X[,1],d_opt_federov()$X[,3],col='red',type='b',xlab='Numero di esperimenti',
         ylim=c(1,max(maxinfl,8)),
         ylab='Vif.max');grid()
    abline(h = 4, lty = 2, col = "green4")
    abline(h = 8, lty = 2, col = "red")
  })
  output$d_opt_numexp<-renderUI({
    #exp<-d_opt_federov()$X[,1]
    #m<-min(exp)
    #M<-max(exp)
    req(input$d_opt_Unumexp)
    req(input$d_opt_Lnumexp)
    m<-input$d_opt_Lnumexp
    M<-input$d_opt_Unumexp
    numericInput(inputId = 'd_opt_numexp',label = 'Numero di esperimenti',value = m,min = m,max=M,width = "30%")
  })
  output$d_opt_dis_opt<-renderTable({
    req(input$d_opt_numexp)
    req(input$d_opt_Lnumexp)
    validate(need(ncol(d_opt_federov()$X)>0 & sum(is.na(d_opt_federov()$X))==0,''))
    dis<-d_opt_federov()$dis[[input$d_opt_numexp-(input$d_opt_Lnumexp-1)]]
    exp<-seq(1,nrow(dis))
    dis<-cbind.data.frame('Exp#'=exp,dis)
    dis
  })
  output$d_opt_dis_download <- downloadHandler(
    filename = "d_opt.xlsx", 
    content = function(file) {
      dis<-d_opt_federov()$dis[[input$d_opt_numexp-(input$d_opt_Lnumexp-1)]]
      exp<-seq(1,nrow(dis))
      dis<-cbind.data.frame('Exp#'=exp,dis)
      write.xlsx(dis, file,colNames=TRUE)
    })
  output$d_opt_vf<-renderTable({
    req(input$d_opt_numexp)
    req(input$d_opt_Lnumexp)
    validate(need(ncol(d_opt_federov()$X)>0 & sum(is.na(d_opt_federov()$X))==0,''))
    frml<-formula(d_opt_formula())
    dis<-d_opt_federov()$dis[[input$d_opt_numexp-(input$d_opt_Lnumexp-1)]]
    vf<-vif(model.matrix(frml, dis))
    cn<-attr(vf,'names')
    cn<-str_remove(cn,"\\(")
    cn<-str_remove(cn,"I")
    cn<-str_remove(cn,"\\)")
    attr(vf,'names')<-cn
    t(vf)
  })
  output$d_opt_ag_importa_incolla_spazio<-renderUI({
    req(input$d_opt_ag_importa==1)
    br()
  })
  output$d_opt_ag_importa_incolla<-renderUI({
    req(input$d_opt_ag_importa==1)
    actionButton("d_opt_ag_incolla", label = "Incolla")
  })
  output$d_opt_ag_importa_incolla_spazio1<-renderUI({
    req(input$d_opt_ag_importa==1)
    hr()
  })
  output$d_opt_ag_importa_excel_brws<-renderUI({
    req(input$d_opt_ag_importa==2)
    fileInput("d_opt_ag_file_xlsx",label='',
              multiple = FALSE,
              accept = c(".xlx",".xlsx"))
  })
  d_opt_ag_dis_paste<-eventReactive(input$d_opt_ag_incolla,{
    df<-tryCatch(read.DIF(file = "clipboard",header = TRUE,transpose = TRUE),
                 error = function(e) "Selezionare un dataset!")
    df
  })
  d_opt_ag_dis_xls<-reactive({
    req(input$d_opt_ag_file_xlsx$datapath)
    df=read_excel(path = input$d_opt_ag_file_xlsx$datapath,sheet = 1,col_names = TRUE)
    if(sum(df[,1]==c(1:nrow(df)))==nrow(df))df<-df[,-1]
    df
  })
  d_opt_ag_dis<-reactive({
    if(input$d_opt_ag_importa==1)df<-d_opt_ag_dis_paste()
    if(input$d_opt_ag_importa==2)df<-d_opt_ag_dis_xls()
    df
  })
  output$d_opt_dis_tbl<-renderTable({
    validate(need(is.data.frame(d_opt_ag_dis()),"Selezionare un dataset!\n "),
             errorClass = "myClass") 
    d_opt_ag_dis()
  })
  d_opt_ag_var_sel<-reactive({
    validate(need(is.data.frame(d_opt_ag_dis()) & is.data.frame(d_opt_cp()),""))
    df1<-d_opt_ag_dis()
    df2<-d_opt_cp()
    var<-colnames(df1)
    colnames(df2)=var 
    data<-rbind.data.frame(df1,df2) 
    var<-colnames(data)
    frm_lin<-paste0(var,collapse = '+')
    frm_lin<-paste0('-1+',frm_lin)
    quad<-paste0('I(',var,'^2)')
    frm_quad<-paste0(quad,collapse = '+')
    #frm_inter<-paste0(var,collapse = '*')
    frm_inter<-paste0('(',frm_lin,')^2')
    if(!'2'%in%input$d_opt_ag_mod_tipo)frm<-frm_lin
    if('2'%in%input$d_opt_ag_mod_tipo)frm<-paste0(frm_lin,'+',frm_quad,'+',frm_inter)
    frm<-paste0('~',frm)
    X<-model.matrix(as.formula(frm),data)
    n<-ncol(d_opt_ag_dis())
    var<-colnames(X[,1:n])
    if('2'%in%input$d_opt_ag_mod_tipo){
      sel_q<-rep(1,n)
      for(i in 1:n){
        if(length(unique(X[,i]))==2 | length(unique(X[,i]))==1)sel_q[i]=0
      }
      D<-as.data.frame(matrix(sel_q,ncol=n))
      colnames(D)<-colnames(X[,1:n])
      Y<-model.matrix(as.formula(paste0('~',frm_lin,'+',frm_quad,'-1')),D)
      m<-ncol(Y)
      var_quad<-colnames(Y)[(n+1):m][Y[1,(n+1):m]==1]
      sel_i<-rep(1,n)
      for(i in 1:n){
        if(length(unique(X[,i]))==2&min(unique(X[,i]))==0&max(unique(X[,i]))==1)sel_i[i]=0
      }
      D<-as.data.frame(matrix(sel_i,ncol=n))
      colnames(D)<-colnames(X[,1:n])
      Z<-model.matrix(as.formula(paste0('~',frm_lin,'+',frm_inter)),D)
      m<-ncol(Z)
      var_inter<-colnames(Z)[(n+1):m][Z[1,(n+1):m]==1]
      var<-c(var,var_quad,var_inter)
    }
    var
  })
  output$d_opt_ag_mod_variabx<-renderUI({
    validate(need(is.data.frame(d_opt_ag_dis()) & is.data.frame(d_opt_cp()),""))
    selectizeInput(inputId = "d_opt_ag_mod_variabx",label="Termini modello (x)",
                   #div("Termini modello (x)",style="font-weight: 400"),
                   choices = d_opt_ag_var_sel(),
                   selected=d_opt_ag_var_sel(),
                   multiple=TRUE)
  })
  d_opt_ag_formula<-reactive({
    req(input$d_opt_ag_mod_variabx)
    var<-input$d_opt_ag_mod_variabx
    if('1'%in%input$d_opt_ag_mod_tipo){
      frm<-paste0(var,collapse = '+')
    }
    if(!'1'%in%input$d_opt_ag_mod_tipo){
      frm<-paste0(var,collapse = '+')
      frm<-paste0('-1+',frm)
    }
    frm<-paste0('~',frm)
    frm
  })
  output$d_opt_ag_modello<-renderText({
    validate(need(is.data.frame(d_opt_ag_dis()) & is.data.frame(d_opt_cp()),""))
    req(input$d_opt_ag_mod_variabx)
    var<-input$d_opt_ag_mod_variabx
    var<-str_remove(var,"\\(")
    var<-str_remove(var,"I")
    var<-str_remove(var,"\\)")
    if('1'%in%input$d_opt_ag_mod_tipo){
      frm<-paste0(var,collapse = '+')
      frm<-paste0('1+',frm)
    }
    if(!'1'%in%input$d_opt_ag_mod_tipo){
      frm<-paste0(var,collapse = '+')
    }
    frm<-paste0('y ~ ',frm)
    frm
  })
  output$d_opt_ag_Lnumexp<-renderUI({
    x<-model.matrix(formula(d_opt_ag_formula()),d_opt_ag_dis())
    r<-nrow(x)
    co<-max(ncol(x),r)+1
    numericInput(inputId = 'd_opt_ag_Lnumexp',label = 'Numero minimo di esperimenti',value = co,min = co,width = "30%")
  })
  output$d_opt_ag_Unumexp<-renderUI({
    req(input$d_opt_ag_Lnumexp)
    x<-model.matrix(formula(d_opt_ag_formula()),d_opt_ag_dis())
    r<-nrow(x)+nrow(d_opt_cp())
    co<-input$d_opt_ag_Lnumexp
    numericInput(inputId = 'd_opt_ag_Unumexp',label = 'Numero massimo di esperimenti',value = co,max=r,min=co,width = "30%")
  })
  d_opt_ag_federov<-eventReactive(input$d_opt_ag_calc,{
    req(input$d_opt_ag_Unumexp)
    req(input$d_opt_ag_Lnumexp)
    fmrl<-formula(d_opt_ag_formula())
    df1<-d_opt_ag_dis()
    nr<-nrow(df1)
    df2<-d_opt_cp()
    var<-colnames(df1)
    colnames(df2)=var 
    data<-rbind.data.frame(df1,df2) 
    X<-model.matrix(fmrl,data)
    nTrials<-input$d_opt_ag_Unumexp
    p <- input$d_opt_ag_Lnumexp
    nRepeats<-10
    vif<-TRUE
    logD<-FALSE
    s = as.vector(NULL)
    t = as.vector(NULL)
    dis<-as.list(NULL)
    if (vif) 
      v = as.vector(NULL)
    n<-nTrials-p+1
    withProgress(message = 'D-ottimale:',value = 0, {
      for (i in p:nTrials) {
        incProgress(detail = paste("NumExp", i),amount = 1/n)
        Dis.opt<-tryCatch(AlgDesign::optFederov(fmrl, nRepeats = nRepeats,augment = TRUE,rows = 1:nr,
                                           data = data, nTrials = i, criterion = "D"),
                     error = function(e) "errore")
        if(Dis.opt=="errore")next()
        s[i - (p - 1)] = i
        t[i - (p - 1)] = Dis.opt$D
        if (logD) 
          t[i - (p - 1)] = log10(Dis.opt$D)
        if (vif) 
          v[i - (p - 1)] = max(vif(model.matrix(fmrl, Dis.opt$design)))
        dis[[i - (p - 1)]]<-Dis.opt$design
      }
    })
    X = data.frame(NumExp = s, D = t, Vif.max=v)
    list(X=X,dis=dis)
  })
  d_opt_ag_msg1<-eventReactive(input$d_opt_ag_calc,{
    df<-tryCatch(d_opt_ag_dis() ,
                 error = function(e) "errore")
    validate(need(df!='errore','Importare il disegno degli esperimenti eseguiti!'),errorClass = "myClass")
  })
  output$d_opt_ag_msg1_tb<-renderTable({
    d_opt_ag_msg1()
  })
  d_opt_ag_msg2<-eventReactive(input$d_opt_ag_calc,{
    df<-tryCatch(d_opt_cp() ,
                 error = function(e) "aiuto!")
    validate(need(df!='aiuto!','Importa matrice punti candidati!'),errorClass = "myClass")
  })
  output$d_opt_ag_msg2_tb<-renderTable({
    d_opt_ag_msg2()
  })
  d_opt_ag_msg3<-eventReactive(input$d_opt_ag_calc,{
    df<-tryCatch(d_opt_ag_dis() ,
                 error = function(e) "aiuto!")
    validate(need(is.data.frame(df)&df!='aiuto!','Selezionare un dataset!'),errorClass = "myClass")
  })
  output$d_opt_ag_msg3_tb<-renderTable({
    d_opt_ag_msg3()
  })
  output$d_opt_ag_table<-renderTable(digits = 4,{
   validate(need(ncol(d_opt_ag_federov()$X)>0 & sum(is.na(d_opt_ag_federov()$X))==0 ,
                 'Trovata matrice con rango insufficiente \n Riprovare!'))
    d_opt_ag_federov()$X
  })
  output$d_opt_ag_graf_D<-renderPlot({
    #validate(need(input$d_opt_ag_Unumexp>=input$d_opt_ag_Lnumexp," "),
             #errorClass = "myClass")
    validate(need(ncol(d_opt_ag_federov()$X)>0 & sum(is.na(d_opt_ag_federov()$X))==0,''))
    plot(d_opt_ag_federov()$X[,1],d_opt_ag_federov()$X[,2],col='red',type='b',xlab='Numero di esperimenti',
         ylab='D');grid()
  })
  output$d_opt_ag_graf_Vif<-renderPlot({
    #validate(need(input$d_opt_ag_Unumexp>=input$d_opt_ag_Lnumexp," "),
             #errorClass = "myClass")
    validate(need(ncol(d_opt_ag_federov()$X)>2 & sum(is.na(d_opt_ag_federov()$X))==0,''))
    maxinfl<-max(d_opt_ag_federov()$X[,3])
    plot(d_opt_ag_federov()$X[,1],d_opt_ag_federov()$X[,3],col='red',type='b',xlab='Numero di esperimenti',
         ylim=c(1,max(maxinfl,8)),
         ylab='Vif.max');grid()
    abline(h = 4, lty = 2, col = "green4")
    abline(h = 8, lty = 2, col = "red")
  })
  output$d_opt_ag_numexp<-renderUI({
    #exp<-d_opt_ag_federov()$X[,1]
    #m<-min(exp)
    #M<-max(exp)
    validate(need(ncol(d_opt_ag_federov()$X)>0 & sum(is.na(d_opt_ag_federov()$X))==0,''))
    req(input$d_opt_ag_Unumexp)
    req(input$d_opt_ag_Lnumexp)
    m<-input$d_opt_ag_Lnumexp
    M<-input$d_opt_ag_Unumexp
    numericInput(inputId = 'd_opt_ag_numexp',label = paste('Numero di esperimenti (di cui',nrow(d_opt_ag_dis()),
                                                           'già eseguiti)'),value = m,min = m,max=M,width = "30%")
  })
  output$d_opt_ag_expes<-renderUI({
    validate(need(ncol(d_opt_ag_federov()$X)>0 & sum(is.na(d_opt_ag_federov()$X))==0,''))
    paste('di cui',nrow(d_opt_ag_dis()),'exp. già eseguiti')
  })
  output$d_opt_ag_dis_opt<-renderTable({
    validate(need(ncol(d_opt_ag_federov()$X)>0 & sum(is.na(d_opt_ag_federov()$X))==0,''))
    req(input$d_opt_ag_numexp)
    req(input$d_opt_ag_Lnumexp)
    validate(need(input$d_opt_ag_numexp-(input$d_opt_ag_Lnumexp-1)>0,''))
    dis<-d_opt_ag_federov()$dis[[input$d_opt_ag_numexp-(input$d_opt_ag_Lnumexp-1)]]
    exp<-seq(1,nrow(dis))
    dis<-cbind.data.frame('Exp#'=exp,dis)
    dis
  })
  output$d_opt_ag_dis_download <- downloadHandler(
    filename = "d_opt_ag.xlsx", 
    content = function(file) {
      dis<-d_opt_ag_federov()$dis[[input$d_opt_ag_numexp-(input$d_opt_ag_Lnumexp-1)]]
      exp<-seq(1,nrow(dis))
      dis<-cbind.data.frame('Exp#'=exp,dis)
      write.xlsx(dis, file,colNames=TRUE)
    })
  output$d_opt_ag_vf<-renderTable({
    req(input$d_opt_ag_numexp)
    req(input$d_opt_ag_Lnumexp)
    validate(need(ncol(d_opt_ag_federov()$X)>2 & sum(is.na(d_opt_ag_federov()$X))==0,''))
    frml<-formula(d_opt_ag_formula())
    dis<-d_opt_ag_federov()$dis[[input$d_opt_ag_numexp-(input$d_opt_ag_Lnumexp-1)]]
    vf<-vif(model.matrix(frml, dis))
    cn<-attr(vf,'names')
    cn<-str_remove(cn,"\\(")
    cn<-str_remove(cn,"I")
    cn<-str_remove(cn,"\\)")
    attr(vf,'names')<-cn
    t(vf)
  })

  # Piano personalizzato -----------------------------------------------------------------  
  output$pp_titolo<-renderUI({
    HTML("Piano personalizzato" )
  })
  output$pp_importa_incolla_spazio<-renderUI({
    req(input$pp_importa==1)
    br()
  })
  output$pp_importa_incolla<-renderUI({
    req(input$pp_importa==1)
    actionButton("pp_incolla", label = "Incolla")
  })
  output$pp_importa_incolla_spazio1<-renderUI({
    req(input$pp_importa==1)
    hr()
  })
  output$pp_importa_excel_brws<-renderUI({
    req(input$pp_importa==2)
    fileInput("pp_file_xlsx",label='',
              multiple = FALSE,
              accept = c(".xlx",".xlsx"))
  })
  pp_dis_paste<-eventReactive(input$pp_incolla,{
    df<-tryCatch(read.DIF(file = "clipboard",header = TRUE,transpose = TRUE),
                 #warning = function(w) {print('warning')},
                 error = function(e) "Selezionare un dataset!")
    df
  })
  pp_dis_xls<-reactive({
    req(input$pp_file_xlsx$datapath)
    df=read_excel(path = input$pp_file_xlsx$datapath,sheet = 1,col_names = TRUE)
    df
  })
  pp_dis<-reactive({
    if(input$pp_importa==1)df<-pp_dis_paste()
    if(input$pp_importa==2)df<-pp_dis_xls()
    if(sum(df[,1]==c(1:nrow(df)))==nrow(df))df<-df[,-1]
    df
  })
  output$pp_dis<-renderTable({
    validate(need(is.data.frame(pp_dis()),"Selezionare un dataset!\n "),
             errorClass = "myClass") 
    pp_dis()
  })
  pp_var_sel<-reactive({
    validate(need(is.data.frame(pp_dis()),""))
    var<-colnames(pp_dis())
    frm_lin<-paste0(var,collapse = '+')
    frm_lin<-paste0('-1+',frm_lin)
    quad<-paste0('I(',var,'^2)')
    frm_quad<-paste0(quad,collapse = '+')
    #frm_inter<-paste0(var,collapse = '*')
    frm_inter<-paste0('(',frm_lin,')^2')
    if(!'2'%in%input$pp_mod_tipo)frm<-frm_lin
    if('2'%in%input$pp_mod_tipo)frm<-paste0(frm_lin,'+',frm_quad,'+',frm_inter)
    frm<-paste0('~',frm)
    X<-model.matrix(as.formula(frm),pp_dis())
    n<-ncol(pp_dis())
    var<-colnames(X[,1:n])
    if('2'%in%input$pp_mod_tipo){
      sel_q<-rep(1,n)
      for(i in 1:n){
        if(length(unique(X[,i]))==2 | length(unique(X[,i]))==1)sel_q[i]=0
      }
      D<-as.data.frame(matrix(sel_q,ncol=n))
      colnames(D)<-colnames(X[,1:n])
      Y<-model.matrix(as.formula(paste0('~',frm_lin,'+',frm_quad,'-1')),D)
      m<-ncol(Y)
      var_quad<-colnames(Y)[(n+1):m][Y[1,(n+1):m]==1]
      sel_i<-rep(1,n)
      for(i in 1:n){
        if(length(unique(X[,i]))==2&min(unique(X[,i]))==0&max(unique(X[,i]))==1)sel_i[i]=0
      }
      D<-as.data.frame(matrix(sel_i,ncol=n))
      colnames(D)<-colnames(X[,1:n])
      Z<-model.matrix(as.formula(paste0('~',frm_lin,'+',frm_inter)),D)
      m<-ncol(Z)
      var_inter<-colnames(Z)[(n+1):m][Z[1,(n+1):m]==1]
      var<-c(var,var_quad,var_inter)
    }
    var
  })
  output$pp_mod_variabx<-renderUI({
    validate(need(is.data.frame(pp_dis()),""))
    selectizeInput(inputId = "pp_mod_variabx",label="Termini modello (x)",
                   #div("Termini modello (x)",style="font-weight: 400"),
                   choices = pp_var_sel(),
                   selected=pp_var_sel(),
                   multiple=TRUE)
  })
  pp_formula<-reactive({
    req(input$pp_mod_variabx)
    var<-input$pp_mod_variabx
    if('1'%in%input$pp_mod_tipo){
      frm<-paste0(var,collapse = '+')
    }
    if(!'1'%in%input$pp_mod_tipo){
      frm<-paste0(var,collapse = '+')
      frm<-paste0('-1+',frm)
    }
    frm<-paste0('~',frm)
    frm
  })
  output$pp_modello<-renderText({
    validate(need(is.data.frame(pp_dis()),""))
    req(input$pp_mod_variabx)
    var<-input$pp_mod_variabx
    var<-str_remove(var,"\\(")
    var<-str_remove(var,"I")
    var<-str_remove(var,"\\)")
    if('1'%in%input$pp_mod_tipo){
      frm<-paste0(var,collapse = '+')
      frm<-paste0('1+',frm)
    }
    if(!'1'%in%input$pp_mod_tipo){
      frm<-paste0(var,collapse = '+')
    }
    frm<-paste0('y ~ ',frm)
    frm
  })
  output$pp_matrdisp<-renderTable({
    validate(need(is.data.frame(pp_dis()),""))
    X<-model.matrix(as.formula(pp_formula()),pp_dis())
    validate(need(qr(X)$rank==ncol(X),"Il programma è stato interrotto perché la matrice del modello ha rango insufficiente"))
    D<-solve(t(X)%*%X)
    D
  })
  output$pp_selvar<-renderUI({
    req(input$pp_mod_variabx)
    var<-colnames(pp_dis())
    var<-var[var%in%input$pp_mod_variabx]
    validate(need(length(var)>2,''))
    X<-model.matrix(as.formula(pp_formula()),pp_dis())
    validate(need(qr(X)$rank==ncol(X),""))
    selectInput("pp_selvar", label = h5("Selezionare 2 variabili"), 
                choices = var, 
                multiple = TRUE,selected = var[1:2])
  })
  output$pp_fixvar<-renderUI({
    req(input$pp_mod_variabx)
    req(input$pp_selvar)
    var<-colnames(pp_dis())
    var<-var[var%in%input$pp_mod_variabx]
    validate(need(length(var)>2,''))
    X<-model.matrix(as.formula(pp_formula()),pp_dis())
    if(qr(X)$rank<ncol(X))stop()
    vl<-'0'
    if(length(var)>3){
      for(i in 2:(length(var)-2)){
        vl<-paste(vl,'0')
      }
    }
    #var<-attr(pp_mod()$model,"names")[-1]
    col<-c(1,2)
    if(length(var)>2 & length(input$pp_selvar)>=2){
      col<-which(var%in%input$pp_selvar==TRUE)
      col_fix<-which(var%in%var[col][1:2]==FALSE)
    }
    var_fix<-c(NULL)
    for(i in 1:length(col_fix)){
      var_fix<-paste(var_fix,var[col_fix][i])
    }
    if(length(col_fix)==1){
      txt<-paste('valore della  variabile',var[col_fix])
    }else{
      txt<-paste('valori delle  variabili',var_fix, '(separati da spazio)')
    }
    textInput(inputId = "pp_fixvar",label = h5(txt),value = vl)
  })
  output$pp_livellolev<-renderPlot({
    req(input$pp_mod_variabx)
    var<-colnames(pp_dis())
    var<-var[var%in%input$pp_mod_variabx]
    validate(need(length(var)>1,''))
    X<-model.matrix(as.formula(pp_formula()),pp_dis())
    #if(qr(X)$rank<ncol(X))stop()
    validate(need(qr(X)$rank==ncol(X),''))
    dsg<-as.data.frame(pp_dis())
    dt<-as.list(NULL)
    for(i in 1:2){
      dt[[i]]=seq(-1,1,0.1)
    }
    dt_exp<-expand.grid(dt)
    data<-matrix(0,441,length(var))
    col<-c(1,2)
    if(length(var)>2 & length(input$pp_selvar)>=2){
      req(input$pp_fixvar)
      col<-which(var%in%input$pp_selvar==TRUE)
      col_fix<-which(var%in%var[col][1:2]==FALSE)
      fix<-as.numeric(unlist(strsplit(input$pp_fixvar," ")))
      for(i in 1:(length(var)-2))data[,col_fix[i]]<-rep(fix[i],441)
    }
    data[,col[1]]<-dt_exp[,1]
    data[,col[2]]<-dt_exp[,2]
    data<-as.data.frame(data)
    colnames(data)<-var
    #frm<-formula(paste(" ~ ", paste(input$pp_mod_variabx[!input$pp_mod_variabx%in%input$pp_selinteraz], collapse= "+")))
    frm<-as.formula(pp_formula())
    P=model.matrix(frm,data = dsg)
    X=model.matrix(frm,data = data)
    Q=X%*%solve(t(P)%*%P)%*%t(X)
    if(nrow(Q)==0){
      plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
      text(0.5,0.5,'inserire il valore delle variabili costanti',cex = 1.6, col = "red")
    }else{
      Lev=data.frame(data,"L"=diag(Q))
      colnames(Lev)[col[1]]<-'x'
      colnames(Lev)[col[2]]<-'y'
      lattice::contourplot(L~x*y,data=Lev,cuts=15,main='Plot of Leverage: Contour Plot',cex.main=0.8,
                           xlab=var[col[1]],ylab=var[col[2]],col='blue',labels=list(col="blue",cex=0.9),
                           aspect=1)}
  })
  output$pp_suplev<-renderPlot({
    req(input$pp_mod_variabx)
    var<-colnames(pp_dis())
    var<-var[var%in%input$pp_mod_variabx]
    validate(need(length(var)>1,''))
    X<-model.matrix(as.formula(pp_formula()),pp_dis())
    validate(need(qr(X)$rank==ncol(X),""))
    dsg<-as.data.frame(pp_dis())
    dt<-as.list(NULL)
    for(i in 1:2){
      dt[[i]]=seq(-1,1,0.1)
    }
    dt_exp<-expand.grid(dt)
    data<-matrix(0,441,length(var))
    col<-c(1,2)
    if(length(var)>2 & length(input$pp_selvar)>=2){
      req(input$pp_selvar)
      req(input$pp_fixvar)
      col<-which(var%in%input$pp_selvar==TRUE)
      col_fix<-which(var%in%var[col][1:2]==FALSE)
      fix<-as.numeric(unlist(strsplit(input$pp_fixvar," ")))
      for(i in 1:(length(var)-2))data[,col_fix[i]]<-rep(fix[i],441)
    }
    data[,col[1]]<-dt_exp[,1]
    data[,col[2]]<-dt_exp[,2]
    data<-as.data.frame(data)
    colnames(data)<-var
    #frm<-formula(paste(" ~ ", paste(input$pp_mod_variabx[!input$pp_mod_variabx%in%input$pp_selinteraz], collapse= "+")))
    frm<-formula(paste(" ~ ", paste(input$pp_mod_variabx, collapse= "+")))
    P=model.matrix(frm,data = dsg)
    X=model.matrix(frm,data = data)
    Q=X%*%solve(t(P)%*%P)%*%t(X)
    if(nrow(Q)==0){
      plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
      text(0.5,0.5,'inserire il valore delle variabili costanti',cex = 1.6, col = "red")
    }else{
      Lev=data.frame(data,"L"=diag(Q))
      colnames(Lev)[col[1]]<-'x'
      colnames(Lev)[col[2]]<-'y'
      req(input$pp_lv_z)
      req(input$pp_lv_x)
      lattice::wireframe(L~x*y,data=Lev,drape=TRUE,col.regions = colorRampPalette(c("yellow","green","blue"))(256),
                         at=seq(min(Lev$L),max(Lev$L),(max(Lev$L)-min(Lev$L))/256),
                         screen=list(z=input$pp_lv_z,x=-input$pp_lv_x),
                         main='Plot of Leverage',cex.main=0.8,xlab=var[col[1]],
                         ylab=var[col[2]],zlab=paste('Response'))}
  })
  
  output$pp_lv_z<-renderUI({
    sliderInput('pp_lv_z',label = 'Rotazione orizzontale',min = 0,max = 360,value = 30,step = 10)
  })
  
  output$pp_lv_x<-renderUI({
    sliderInput('pp_lv_x',label = 'Rotazione verticale',min = 0,max = 90,value = 60,step = 10)
  })
  
  output$pp_risptext<-renderUI({
    X<-model.matrix(as.formula(pp_formula()),pp_dis())
    if(qr(X)$rank<ncol(X))stop()
    h4(paste('Risposte (',nrow(pp_dis()),', separate da spazio)',sep=''))
  })
  pp_mod<-reactive({
    validate(need(length(as.numeric(unlist(strsplit(input$pp_risp," "))))==nrow(pp_dis()),''))
    y<-as.numeric(unlist(strsplit(input$pp_risp," ")))
    D<-cbind.data.frame(y=y,pp_dis())
    frm<-formula(paste0('y',pp_formula()))
    mod<-lm(frm,D)
    mod
  })
  output$pp_coeff<-renderPrint({
    X<-model.matrix(as.formula(pp_formula()),pp_dis())
    if(qr(X)$rank<ncol(X))stop()
    round(pp_mod()$coefficients,2)
  })

  output$pp_grcoeff<-renderPlot({
    X<-model.matrix(as.formula(pp_formula()),pp_dis())
    if(qr(X)$rank<ncol(X))stop()
    mod<-pp_mod()
    require(ggplot2)
    df_coeff<-data.frame(nome=names(mod$coefficients[-1]),
                         valore=mod$coefficients[-1])
    if(input$pp_resind==2){
      if(length(as.numeric(unlist(strsplit(input$pp_misind," "))))<2){
        ggplot(data = df_coeff,aes(x =nome,y=valore))+
          xlab("")+ylab("")+theme_light()+
          geom_bar(fill="red",stat="identity")+
          scale_x_discrete(limits=names(mod$coefficients[-1]))
      }else{
        x<-as.numeric(unlist(strsplit(input$pp_misind," ")))
        gdl<-length(x)-1
        q<-qt(p = 0.975,df = gdl)
        s<-sd(x)
        X<-model.matrix(mod)
        D<-solve(t(X)%*%X)
        d<-diag(D)
        df_coeff<-cbind.data.frame(df_coeff,inf=mod$coefficients[-1]-q*s*sqrt(d[-1]),sup=mod$coefficients[-1]+q*s*sqrt(d[-1]))
        ggplot(data = df_coeff,aes(x =nome,y=valore))+
          xlab("")+ylab("")+theme_light()+
          geom_bar(fill="red",stat="identity")+
          scale_x_discrete(limits=names(mod$coefficients[-1]))+
          geom_errorbar(aes(ymin=inf, ymax=sup),
                        width=0.2, colour="green3")
      }}else{
        sm<-summary(mod)
        if(is.na(sm$sigma)){
          ggplot(data = df_coeff,aes(x =nome,y=valore))+
            xlab("")+ylab("")+theme_light()+
            geom_bar(fill="red",stat="identity")+
            scale_x_discrete(limits=names(mod$coefficients[-1]))
        }else{
          df_coeff<-cbind.data.frame(df_coeff,inf=mod$coefficients[-1]-sm$coefficients[1,2],sup=mod$coefficients[-1]+sm$coefficients[1,2])
          ggplot(data = df_coeff,aes(x =nome,y=valore))+
            xlab("")+ylab("")+theme_light()+
            geom_bar(fill="red",stat="identity")+
            scale_x_discrete(limits=names(mod$coefficients[-1]))+
            geom_errorbar(aes(ymin=inf, ymax=sup),
                          width=0.2, colour="green3")
        }
         }
  })

  output$pp_grsigncoeff<-renderPlot({
    var<-attr(pp_mod()$model,"names")[-1]
    validate(need(length(var)==sum(substr(var,1,2)!='I('),''))
    mod<-pp_mod()
    FrF2::DanielPlot(mod,alpha = 0.05,main='',pch=19,cex.fac=1,col="blue",datax = FALSE)
    qqline(y = coef(mod)[-1],datax = FALSE,col="blue",lty=2)
  })
  
  output$pp_prev_df<-renderPrint({
    var<-attr(pp_mod()$model,"names")[-1]
    var<-var[substr(var,1,2)!='I(']
    if(length(as.numeric(unlist(strsplit(input$pp_prev," "))))==length(var)){
      mod<-pp_mod()
      x<- as.numeric(unlist(strsplit(input$pp_prev," ")))
      nd<-rbind.data.frame(x)
      colnames(nd)<-var
      prev<-predict(object = mod,newdata=nd)
      attr(prev,'names')<-c('previsione')
      prev
    }else{
      cat(paste('inserire le',length(var),'coord. del punto'))
    }
  })

  output$pp_intprev<-renderPrint({
    var<-attr(pp_mod()$model,"names")[-1]
    var<-var[substr(var,1,2)!='I(']
    validate(need(length(as.numeric(unlist(strsplit(input$pp_prev," "))))==length(var),''))
    req(input$pp_mod_variaby)
    req(input$pp_mod_variabx)
    if(input$pp_resind==1){
      mod<-pp_mod()
      smr<-summary(mod)
      if(!is.na(smr$sigma)){
      x<- as.numeric(unlist(strsplit(input$pp_prev," ")))
      nd<-rbind.data.frame(x)
      colnames(nd)<-var
      prev<-predict(object = mod,newdata=nd,interval='confidence')
      prev1<-predict(object = mod,newdata=nd,interval='confidence',level = 0.99)
      prev2<-predict(object = mod,newdata=nd,interval='confidence',level = 0.999)
      df<-cbind.data.frame(prev,prev1,prev2)
      df<-cbind.data.frame(round(df[,-c(1,4,7)],3))
      colnames(df)<-c('2.5%','97.5%','0.5%','99.5%','0.05%','99.95%')
      df
      }else{
        cat('Non ci sono gradi di libertà')
      }
    }else{
      require(input$pp_misind)
      x<-as.numeric(unlist(strsplit(input$pp_misind," ")))
      if(length(x)<=1){
        cat('Almeno 2 misure')
      }else{
        gdl<-length(x)-1
        q<-qt(p = 0.975,df = gdl)
        q1<-qt(p = 0.995,df = gdl)
        q2<-qt(p = 0.9995,df = gdl)
        s<-sd(x)
        mod<-pp_mod()
        X<-model.matrix(mod)
        p<-as.numeric(unlist(strsplit(input$pp_prev," ")))
        data<-as.data.frame(matrix(p,1,length(input$pp_mod_variabx)))
        colnames(data)<-var
        frml<-paste("~", paste(input$pp_mod_variabx, collapse= " + "))
        frml<-as.formula(frml)
        P<-model.matrix(frml,data=data)
        d<-P%*%solve(t(X)%*%X)%*%t(P)
        prev<-predict(mod,newdata = data)
        df<-data.frame(prev-q*s*sqrt(d),prev+q*s*sqrt(d),
                       prev-q1*s*sqrt(d),prev+q1*s*sqrt(d),
                       prev-q2*s*sqrt(d),prev+q2*s*sqrt(d))
        df<-cbind.data.frame(round(df,3))
        colnames(df)<-c('2.5%','97.5%','0.5%','99.5%','0.05%','99.95%')
        df
      }
    }
  })
  
  output$pp_misind_media<-renderPrint({
    validate(need(length(as.numeric(unlist(strsplit(input$pp_misind," "))))>1,''))
    x<-as.numeric(unlist(strsplit(input$pp_misind," ")))
    mod<-lm(x~1,as.data.frame(x))
    sm<-summary(mod)
    media<-predict(mod,interval = 'confidence')[1,]
    attr(media,'names')<-c('media','2.5%','97.5%')
    round(media,3)
  })
  
  output$pp_misind_sd<-renderPrint({
    validate(need(length(as.numeric(unlist(strsplit(input$pp_misind," "))))>1,''))
    x<-as.numeric(unlist(strsplit(input$pp_misind," ")))
    mod<-lm(x~1,as.data.frame(x))
    sm<-summary(mod)
    df<-c(round(sm$sigma,3))
    attr(df,'names')<-c('dev.st.')
    df
  })
  
  output$pp_misind_gdl<-renderPrint({
    validate(need(length(as.numeric(unlist(strsplit(input$pp_misind," "))))>1,''))
    x<-as.numeric(unlist(strsplit(input$pp_misind," ")))
    gdf<-(length(as.numeric(unlist(strsplit(input$pp_misind," "))))-1)
    df<-c(gdf)
    attr(df,'names')<-c( 'gdl')
    df
  })

  output$pp_stimint_txt<-renderUI({
    validate(need(length(as.numeric(unlist(strsplit(input$pp_misind," "))))>0,''))
    h4('Stima per intervallo')
  })
  
  output$pp_stimint_txt<-renderUI({
    #validate(need(length(as.numeric(unlist(strsplit(input$fatt_compl_misind," "))))>0 &
     #               length(as.numeric(unlist(strsplit(input$fatt_compl_risp," "))))==2^input$fatt_compl_k,''))
    h4('Stima per intervallo')
  })
  
  output$pp_stimint<-renderPrint({
    X<-model.matrix(as.formula(pp_formula()),pp_dis())
    if(qr(X)$rank<ncol(X))stop()
    if(input$pp_resind==1){
      mod<-pp_mod()
      smr<-summary(mod)
      pval<- smr$coefficients[,4,drop=FALSE]
      if(!is.na(smr$sigma)){
        df<-data.frame(confint(mod)[,1],confint(mod)[,2],
                       confint(mod,level = 0.99)[,1],confint(mod,level = 0.99)[,2],
                       confint(mod,level = 0.999)[,1],confint(mod,level = 0.999)[,2],pval)
        lab<-rep('',nrow(df))
        for (i in 1:nrow(df)){
          if(pval[i]<0.1)lab[i]<-'.'
          if(pval[i]<0.05)lab[i]<-'*'
          if(pval[i]<0.01)lab[i]<-'**'
          if(pval[i]<0.001)lab[i]<-'***'
        }
        df<-cbind.data.frame(round(df[,1:6],3),round(df[,7],4),lab)
        colnames(df)<-c('2.5%','97.5%','0.5%','99.5%','0.05%','99.95%','p-value','')
        df
        
      }else{
        cat('Non ci sono gradi di libertà')
      }

    }else{
      require(input$pp_misind)
      x<-as.numeric(unlist(strsplit(input$pp_misind," ")))
      if(length(x)<=1){
        cat('Almeno 2 misure')
      }else{
        gdl<-length(x)-1
        q<-qt(p = 0.975,df = gdl)
        q1<-qt(p = 0.995,df = gdl)
        q2<-qt(p = 0.9995,df = gdl)
        s<-sd(x)
        mod<-pp_mod()
        X<-model.matrix(mod)
        D<-solve(t(X)%*%X)
        d<-diag(D)
        pval<-pt(abs(mod$coefficients/(s*sqrt(d))),df = gdl,lower.tail = FALSE)*2
        df<-data.frame(mod$coefficients,mod$coefficients-q*s*sqrt(d),mod$coefficients+q*s*sqrt(d),
                       mod$coefficients-q1*s*sqrt(d),mod$coefficients+q1*s*sqrt(d),
                       mod$coefficients-q2*s*sqrt(d),mod$coefficients+q2*s*sqrt(d),
                       pval)
        lab<-rep('',nrow(df))
        for (i in 1:nrow(df)){
          if(pval[i]<0.1)lab[i]<-'.'
          if(pval[i]<0.05)lab[i]<-'*'
          if(pval[i]<0.01)lab[i]<-'**'
          if(pval[i]<0.001)lab[i]<-'***'
        }
        df<-cbind.data.frame(round(df[,1:7],3),round(df[,8],4),lab)
        colnames(df)<-c('val','2.5%','97.5%','0.5%','99.5%','0.05%','99.95%','p-value','')
        df[,-1]
      }
    }
  })
  
  output$pp_mod_selvar<-renderUI({
    req(input$pp_mod_variabx)
    var<-colnames(pp_dis())
    var<-var[var%in%input$pp_mod_variabx]
    validate(need(length(var)>2,''))
    X<-model.matrix(as.formula(pp_formula()),pp_dis())
    if(qr(X)$rank<ncol(X))stop()
    selectInput("pp_mod_selvar", label = h5("Selezionare 2 variabili"), 
                choices = var, 
                multiple = TRUE,selected = var[1:2])
  })
  
  output$pp_mod_fixvar<-renderUI({
    req(input$pp_mod_variabx)
    req(input$pp_mod_selvar)
    var<-colnames(pp_dis())
    var<-var[var%in%input$pp_mod_variabx]
    validate(need(length(var)>2,''))
    X<-model.matrix(as.formula(pp_formula()),pp_dis())
    if(qr(X)$rank<ncol(X))stop()
    vl<-'0'
    if(length(var)>3){
      for(i in 2:(length(var)-2)){
        vl<-paste(vl,'0')
      }
    }
    #var<-attr(pp_mod()$model,"names")[-1]
    #var<-var[substr(var,1,2)!='I(']
    col<-c(1,2)
    if(length(var)>2 & length(input$pp_mod_selvar)>=2){
      col<-which(var%in%input$pp_mod_selvar==TRUE)
      col_fix<-which(var%in%var[col][1:2]==FALSE)
    }
    var_fix<-c(NULL)
    for(i in 1:length(col_fix)){
      var_fix<-paste(var_fix,var[col_fix][i])
    }
    if(length(col_fix)==1){
      txt<-paste('valore della  variabile',var[col_fix])
    }else{
      txt<-paste('valori delle  variabili',var_fix, '(separati da spazio)')
    }
    textInput(inputId = "pp_mod_fixvar",label = h5(txt),value = vl)
  })
  
  output$pp_livellorisp<-renderPlot({
    X<-model.matrix(as.formula(pp_formula()),pp_dis())
    if(qr(X)$rank<ncol(X))stop()
    var<-attr(pp_mod()$model,"names")[-1]
    var<-var[substr(var,1,2)!='I(']
    validate(need(length(var)>1,''))
    req(input$pp_mod_variabx)
    dsg<-as.data.frame(pp_dis())
    dt<-as.list(NULL)
    for(i in 1:2){
      dt[[i]]=seq(-1,1,0.1)
    }
    dt_exp<-expand.grid(dt)
    data<-matrix(0,441,length(var))
    col<-c(1,2)

    if(length(var)>2 & length(input$pp_mod_selvar)>=2){
      col<-which(var%in%input$pp_mod_selvar==TRUE)
      col_fix<-which(var%in%var[col][1:2]==FALSE)
      fix<-as.numeric(unlist(strsplit(input$pp_mod_fixvar," ")))
      for(i in 1:(length(var)-2))data[,col_fix[i]]<-rep(fix[i],441)
      
    }
    data[,col[1]]<-dt_exp[,1]
    data[,col[2]]<-dt_exp[,2]
    data<-as.data.frame(data)
    colnames(data)<-var
    mod<-pp_mod()
    
    Pred=data.frame(data,P=predict(mod,newdata=data))
    colnames(Pred)[col[1]]<-'x'
    colnames(Pred)[col[2]]<-'y'
    colore<-c("blue","green3","red","black","purple1")
    cl<-as.integer(input$pp_livellorisp_col)
    
    if(is.na(Pred$P[1])==TRUE){
      plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
      text(0.5,0.5,'inserire il valore delle variabili costanti',cex = 1.6, col = "red")
    }else{
      lattice::contourplot(P~x*y,data=Pred,cuts=15,main='Response Surface: Contour Plot',cex.main=0.8,
                           xlab=var[col[1]],ylab=var[col[2]],col=colore[cl],
                           labels=list(col=colore[cl],cex=0.9),
                           aspect=1)
    }
  })
  
  output$pp_livellorisp_col<-renderUI({
    selectInput("pp_livellorisp_col", label = h3(""), 
                choices = list("blu" = 1, "verde" = 2, "rosso" = 3,"nero" = 4,"viola" = 5), 
                selected = 1,width="130px")
  })
  
  output$pp_suprisp<-renderPlot({
    X<-model.matrix(as.formula(pp_formula()),pp_dis())
    if(qr(X)$rank<ncol(X))stop()
    var<-attr(pp_mod()$model,"names")[-1]
    var<-var[substr(var,1,2)!='I(']
    validate(need(length(var)>1,''))
    req(input$pp_mod_variabx)
    dsg<-as.data.frame(pp_dis())
    dt<-as.list(NULL)
    for(i in 1:2){
      dt[[i]]=seq(-1,1,0.1)
    }
    dt_exp<-expand.grid(dt)
    data<-matrix(0,441,length(var))
    col<-c(1,2)
    
    if(length(var)>2 & length(input$pp_mod_selvar)>=2){
      col<-which(var%in%input$pp_mod_selvar==TRUE)
      col_fix<-which(var%in%var[col][1:2]==FALSE)
      fix<-as.numeric(unlist(strsplit(input$pp_mod_fixvar," ")))
      for(i in 1:(length(var)-2))data[,col_fix[i]]<-rep(fix[i],441)
      
    }
    data[,col[1]]<-dt_exp[,1]
    data[,col[2]]<-dt_exp[,2]
    data<-as.data.frame(data)
    colnames(data)<-var
    mod<-pp_mod()
    
    Pred=data.frame(data,P=predict(mod,newdata=data))
    colnames(Pred)[col[1]]<-'x'
    colnames(Pred)[col[2]]<-'y'
    
    if(is.na(Pred$P[1])==TRUE){
      plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
      text(0.5,0.5,'inserire il valore delle variabili costanti',cex = 1.6, col = "red")
    }else{
      req(input$pp_rp_z)
      req(input$pp_rp_x)
      lattice::wireframe(P~x*y,data=Pred,drape=TRUE,col.regions = colorRampPalette(c("yellow","green","blue"))(256),
                         at=seq(min(Pred$P),max(Pred$P),(max(Pred$P)-min(Pred$P))/256),
                         screen=list(z=input$pp_rp_z,x=-input$pp_rp_x),
                         main='Response Surface',cex.main=0.8,xlab=var[col[1]],
                         ylab=var[col[2]],zlab=paste('Response')) 
    }
  })
  
  output$pp_rp_z<-renderUI({
    sliderInput('pp_rp_z',label = 'Rotazione orizzontale',min = 0,max = 360,value = 30,step = 10)
  })
  
  output$pp_rp_x<-renderUI({
    sliderInput('pp_rp_x',label = 'Rotazione verticale',min = 0,max = 90,value = 60,step = 10)
  })

}








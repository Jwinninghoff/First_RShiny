shinyServer(function(input,output){
    
    data(CO2)
    set.seed(345)
    inTrain <- createDataPartition(CO2$Treatment,p=.6,list=F)
    training <- CO2[inTrain,]
    testing <- CO2[-inTrain,]
    modFit <- train(Treatment ~ ., method="gbm",data=training,verbose=FALSE)
    
    percent <- function(x){paste(if(is.na(x)||x<=0){0}else{round(100*x,digits = 2)},"%",sep="")}

    newt <- select(testing,uptake,conc)
    newt <- reactiveVal()
        
    output$Result <- reactive({
        percent(dim(filter(filter(testing, row.names(testing) %in% newt()),
            Treatment=="nonchilled"))[1]/(dim(filter(filter(testing, row.names(testing) %in% newt()),
            Treatment=="nonchilled"))[1]+dim(filter(filter(testing, row.names(testing) %in% newt()),
            Treatment=="chilled"))[1]))
    })
    output$Result1 <- reactive({
        percent(confusionMatrix(testing$Treatment, predict(modFit,
            testing))$overall[1])
        })
    
    observeEvent(event_data("plotly_hover"), {
        newo <- event_data("plotly_hover")$customdata
        testing_old_new <- c(newt(),newo)
        newt(unique(testing_old_new))
    })
    output$plot1 <- renderPlotly({
        plot_ly(training,x=~uptake,y=~conc,color=~Treatment, type="scatter", mode = "markers",size=2,
        colors = c("rosybrown","coral"))%>%
        layout(xaxis = list(title="uptake rates (umol/m^2 sec)"),yaxis=list(title = "CO2 concentrations (mL/L)"))
    })
    output$plot2 <- renderPlotly({
        plot_ly(testing,x=~uptake,y=~conc,color=~Treatment, type="scatter",mode = "markers",size=2,
        colors = c("rosybrown","coral"))%>%
        layout(xaxis = list(title="uptake rates (umol/m^2 sec)"),
        yaxis=list(title = "CO2 concentrations (mL/L)"))
    })
    output$plot3 <- renderPlotly({
        
        cols <- ifelse(row.names(testing)%in% newt(),"rosybrown","coral")
        
        testing%>% 
            plot_ly(x=~uptake,y=~conc, 
                    customdata=row.names(testing),marker = list(color=cols),
                    size=2)%>%add_markers()%>%
            layout(xaxis = list(title="uptake rates (umol/m^2 sec)"),
                   yaxis=list(title = "CO2 concentrations (mL/L)")) 
    })
    output$Accuracy1 <- renderTable({
        filter(testing,Treatment == "nonchilled")
    })
    
    observeEvent(input$reset1,{
        newt(NULL)
    })
    observeEvent(input$message, {
        if((dim(filter(filter(testing, row.names(testing) %in% newt()),
        Treatment=="nonchilled"))[1]+dim(filter(filter(testing, row.names(testing) %in% newt()),
        Treatment=="chilled"))[1])<dim(filter(testing,Treatment == "nonchilled"))[1]){shinyalert("Sorry!", 
        "This prediction is not complete.",confirmButtonText = "Continue!", confirmButtonCol = "#E30022", type = "error")}else{shinyalert("Good Work!", 
        "This prediction is complete. Please look at Better than Machine Learning tab on sidebar.",
        confirmButtonText = "Let's See!",confirmButtonCol = "#8DB600", type = "success")}
    })
    observeEvent(input$message1, {
        if(if(is.na((dim(filter(filter(testing, row.names(testing) %in% newt()),
        Treatment=="nonchilled"))[1]+dim(filter(filter(testing, row.names(testing) %in% newt()),
        Treatment=="chilled"))[1])*dim(filter(filter(testing, row.names(testing) %in% newt()),
        Treatment=="nonchilled"))[1]/(dim(filter(filter(testing, row.names(testing) %in% newt()),
        Treatment=="nonchilled"))[1]+dim(filter(filter(testing, row.names(testing) %in% newt()),
        Treatment=="chilled"))[1]))){TRUE}else{(dim(filter(filter(testing, row.names(testing) %in% newt()),
        Treatment=="nonchilled"))[1]+dim(filter(filter(testing, row.names(testing) %in% newt()),
        Treatment=="chilled"))[1])*dim(filter(filter(testing, row.names(testing) %in% newt()),
        Treatment=="nonchilled"))[1]/(dim(filter(filter(testing, row.names(testing) %in% newt()),
        Treatment=="nonchilled"))[1]+dim(filter(filter(testing, row.names(testing) %in% newt()),
        Treatment=="chilled"))[1])-dim(filter(filter(testing, row.names(testing) %in% newt()),
        Treatment=="chilled"))[1]}<16){shinyalert("Unfortunately!", 
        "No. But don't give up!", confirmButtonText = "Try Again!", confirmButtonCol = "#E30022", type = "error")}else{shinyalert("Wow!", 
        "Yes! You are a genius!", confirmButtonText = "Congratulation!",confirmButtonCol = "#8DB600",
        type = "success")}
    })
    observeEvent(input$summary1,{
        shinyalert("What is a Machine Learning?","The machine learning is a powerful tool that can make prediction. When it receives dataset, it will study it and make prediction of what the next plot looks like. 
                   
                   For instance, the researchers sample grass in two categories. One is control group and another one experiment group. The experiment group places in freezing-below temperature and the researchers measure how much each individual in this group absorbs carbon dioxide with how much they have. However, the result reveals that there is no pattern and too chaotic so that machine learning is here to make a prediction.", 
                   confirmButtonText = "See What's Next!",confirmButtonCol = "FF7E00", type="info")
    })
    observeEvent(input$summary2,{
        shinyalert("How to play?","Now, you are one of these researchers who dares to challenge this machine learning. The first step you have to do is to look at that plot. 
                   
                   The next step is to make your prediction of what the next plot looks like. 
                   
                   In order to defeat the machine learning, your prediction must be 100% accurate. Use mouse to hover particular points. Then, click Prediction Complete to see if your task is done.",
                   confirmButtonText = "Let's Play!",confirmButtonCol = "#FF7E00",type="info")
    })
    output$table1 = renderTable({
        select(filter(testing, row.names(testing) %in% newt()),Treatment,conc,uptake)
    })
    observeEvent(input$warning1,{
        shinyalert("Accuarcy: 100%", "So, your goal is to get your prediction 100% accurate. If you get wrong, you can always reset your prediction and try again.",
                   confirmButtonText = "You Better Win!",confirmButtonCol = "#FF7E00",type="info")
    })
})

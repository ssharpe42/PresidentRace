shinyServer(function(input, output, session) {
  
  values <- reactiveValues(data = DataList$All, model_data = NULL, costs = NULL, reset = FALSE)
  
  #Set default cost matricies for betting
  set_costs = function(){
      class_levels = sort(as.character(unique(values$data$WINNER)))
      win.prob = prop.table(table(values$data$WINNER)[class_levels])
      
      #Gain and loss based on average probability of winning
      bet =1/win.prob
      antibet = 1/(1- win.prob)
      n = length(class_levels)
      
      #Matrix of costs
      if(input$accuracy){
          costs = matrix(0, n, n)
          diag(costs)=1
      }else{
          costs = matrix(rep(-antibet,each=n),n,n)
          diag(costs)=bet
      }
      colnames(costs) = rownames(costs ) = class_levels
      values$costs = costs
  }
  #set train and test split
  set_train_test = reactive({
      set.seed(12345)
      test_indx = sample(1:nrow(values$model_data), size = 81)
      values$train = values$model_data[-test_indx, ]
      values$test= values$model_data[test_indx,]
  })
  #Switch data based on problem type
  observe({
      if(input$prob_type=='All'){
          values$data = DataList$All
      }else{
          values$data = DataList$Ted
      }
      #Set Cost Matrix
      set_costs()
  })
  #Output variable choices based on problem type
  output$var_choice = renderUI({
      selectizeInput('var_choice','Select your variables:', choices = names(values$data) , selected = names(values$data), multiple = T)
  })

  #Reset cost matrix
  observeEvent(input$reset,{
      print('Here')
      values$reset = TRUE
      set_costs()
      
  })
  
  # Cost Table
  observe({
      print(values$reset)
      if (!is.null(input$hot) & !values$reset) {
          costs = hot_to_r(input$hot)
          costs[is.na(costs)]=0
      } else {
          print('Second')
          if (is.null(values$costs) & !values$reset)
              costs <- costs
          else
              costs <- values$costs
      }
      values$costs <- costs
      print(costs)
  })
  
  output$hot <- renderRHandsontable({
      if (!is.null(values$costs))
          rhandsontable(values$costs, stretchH = "all")%>%
          hot_col(1:(ncol(values$costs)), format = "$0,0.00")%>%
          #https://jrowen.github.io/rhandsontable/#formatting
          hot_cols(renderer = "function (instance, td, row, col, prop, value, cellProperties) {
                   Handsontable.renderers.NumericRenderer.apply(this, arguments);
                   if (value < 0) {
                   td.style.background = 'pink';
                   } else if (value > 0) {
                   td.style.background = 'lightgreen';
                   }
  }")
  })
  observe({
      if(is.null(values$t.rpart)){
          hide('treeplot')
          hide('downloadPlot')
      }else{
          show('treeplot')
         show('downloadPlot')
      }
  })
  ## Run Model
  observeEvent(input$run, {
      if(!is.null(values$data)){
          values$model_data = values$data[, union(input$var_choice,'WINNER')]
      }
      #Set training and testing sets
      set_train_test()
      
      #Prepare model and task
      race.all = makeClassifTask(id = "president", data = values$train, target = "WINNER")
      resamp = makeResampleDesc("CV", iters = 5L)
      lrn = makeLearner("classif.rpart", predict.type = "prob")
      control.grid = makeTuneControlGrid() 
      
      
      bet.costs = makeCostMeasure(id = "bet.costs", name = "Betting costs", costs = values$costs,
                                  best = min(values$costs), worst = max(values$costs))
      #Prepare tree tuning parameters
      ps = makeParamSet(
          makeDiscreteParam("cp", values = c(0.01)),
          makeDiscreteParam("minsplit", values = c(5,10)),
          makeDiscreteParam("maxdepth", values = c(5,8))
      )
      ps = makeParamSet(
          makeDiscreteParam("cp", values = c(0.01,.03,0.05,.1)),
          makeDiscreteParam("minsplit", values = c(5,10,20,30, 50)),
          makeDiscreteParam("maxdepth", values = c(2,3,5,8))
      )
      
      withProgress(message = 'Tuning Model...', min = 0, max = 1, {
                       parallelStartSocket(4)
                       set.seed(1234)
                       res = tuneParams(lrn, task = race.all, resampling = resamp, control = control.grid, par.set = ps, measures = list(acc,logloss, bet.costs))
                       values$opt.grid = sapply(as.data.frame(res$opt.path) , function(x) as.numeric(as.character(x))) %>% data.frame()
                       parallelStop()
      })
     
      best = values$opt.grid %>% arrange(-bet.costs.test.mean) %>% slice(1) 
      lrn = setHyperPars(lrn, par.vals = list(cp = best$cp, minsplit = best$minsplit, maxdepth = best$maxdepth)) 
      values$t.rpart <- train(lrn, race.all)
      values$perf<-as.numeric(performance(predict(values$t.rpart, newdata = values$test), measures = list(bet.costs)))
  })
  
  output$treeplot <- renderPlot({
      if(!is.null(values$t.rpart)){
          prp(values$t.rpart$learner.model, digits = 4, varlen = 0, faclen = 10) 
          ddd<<-values$t.rpart
      }
  })
  
  output$profit = renderValueBox({
      if(input$accuracy){
          if(is.null(values$perf)){
              valueBox('0', 'Expected Races Correct')
          }else{
              valueBox(round(values$perf*81), 'Expected Races Correct')
          }
      }else{
          if(is.null(values$perf)){
              valueBox('$0', 'Expected Total Season Profit')
          }else{
              valueBox(dollar_format()(values$perf*81), 'Expected Total Season Profit')
          }
      }
  })
  output$profitgame = renderValueBox({
      if(input$accuracy){
          if(is.null(values$perf)){
              valueBox('0%', 'Expected Accuracy')
          }else{
              valueBox(percent_format()(values$perf), 'Expected Accuracy')
          }
      }else{
          if(is.null(values$perf)){
              valueBox('$0', 'Expected Profit Per Game')
          }else{
              valueBox(dollar_format()(values$perf), 'Expected Profit Per Game')
          }   
      }
  })
  output$downloadPlot <- downloadHandler(
      filename = function() { 'DecisionTree.pdf' },
      content = function(file) {
          pdf(file)
          prp(values$t.rpart$learner.model, digits = 7, varlen = 0, faclen = 11)
          dev.off()
      }
  )
})
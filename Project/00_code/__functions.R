
# Function to map NUTS2 codes to countries
mapping_nuts <- function(nuts_code) {
  if (grepl("^AL", nuts_code)) {
    return("Albania")
  } else if (grepl("^AT", nuts_code)) {
    return("Austria")
  } else if (grepl("^BA", nuts_code)) {
    return("Bosnia and Herzegovina")
  } else if (grepl("^BE", nuts_code)) {
    return("Belgium")
  } else if (grepl("^BG", nuts_code)) {
    return("Bulgaria")
  } else if (grepl("^CY", nuts_code)) {
    return("Cyprus")
  } else if (grepl("^CZ", nuts_code)) {
    return("Czech Republic")
  } else if (grepl("^DE", nuts_code)) {
    return("Germany")
  } else if (grepl("^DK", nuts_code)) {
    return("Denmark")
  } else if (grepl("^EE", nuts_code)) {
    return("Estonia")
  } else if (grepl("^EL", nuts_code)) {
    return("Greece")
  } else if (grepl("^ES", nuts_code)) {
    return("Spain")
  } else if (grepl("^FI", nuts_code)) {
    return("Finland")
  } else if (grepl("^FR", nuts_code)) {
    return("France")
  } else if (grepl("^HR", nuts_code)) {
    return("Croatia")
  } else if (grepl("^HU", nuts_code)) {
    return("Hungary")
  } else if (grepl("^IE", nuts_code)) {
    return("Ireland")
  } else if (grepl("^IT", nuts_code)) {
    return("Italy")
  } else if (grepl("^LT", nuts_code)) {
    return("Lithuania")
  } else if (grepl("^LU", nuts_code)) {
    return("Luxembourg")
  } else if (grepl("^LV", nuts_code)) {
    return("Latvia")
  } else if (grepl("^MD", nuts_code)) {
    return("Moldova")
  } else if (grepl("^ME", nuts_code)) {
    return("Montenegro")
  } else if (grepl("^MK", nuts_code)) {
    return("North Macedonia")
  } else if (grepl("^MT", nuts_code)) {
    return("Malta")
  } else if (grepl("^NL", nuts_code)) {
    return("Netherlands")
  } else if (grepl("^PL", nuts_code)) {
    return("Poland")
  } else if (grepl("^PT", nuts_code)) {
    return("Portugal")
  } else if (grepl("^RO", nuts_code)) {
    return("Romania")
  } else if (grepl("^RS", nuts_code)) {
    return("Serbia")
  } else if (grepl("^SE", nuts_code)) {
    return("Sweden")
  } else if (grepl("^SI", nuts_code)) {
    return("Slovenia")
  } else if (grepl("^SK", nuts_code)) {
    return("Slovakia")
  } else if (grepl("^TR", nuts_code)) {
    return("Turkey")
  } else if (grepl("^UK", nuts_code)) {
    return("United Kingdom")
  } else if (grepl("^XK", nuts_code)) {
    return("Kosovo")
  } else {
    return(NA)
  }
}


merge_geometries <- function(data, nuts, new_nuts, new_name) {
  combined_geometry <- data %>%
    filter(NUTS %in% nuts) %>%
    st_union()
  
  new_row <- data.frame(
    NUTS = new_nuts,
    Name = new_name,
    geometry = st_sfc(combined_geometry)
  )
  
  new_row <- st_as_sf(new_row, sf_column_name = "geometry", crs = st_crs(data))
  
  data <- data %>%
    filter(!NUTS %in% nuts) %>%
    bind_rows(new_row)
  
  return(data)
}

update_w_queen <- function(W.queen, region_pairs, value_pairs) {
  # Ensure the lengths of region_pairs and value_pairs match
  if (length(region_pairs) != length(value_pairs)) {
    stop("The lengths of region_pairs and value_pairs must match.")
  }
  
  for (i in seq_along(region_pairs)) {
    region1 <- region_pairs[[i]][1]
    region2 <- region_pairs[[i]][2]
    value <- value_pairs[i]
    
    row_index1 <- which(rownames(W.queen) == region1)
    col_index1 <- which(colnames(W.queen) == region2)
    row_index2 <- which(rownames(W.queen) == region2)
    col_index2 <- which(colnames(W.queen) == region1)
    
    if (length(row_index1) > 0 && length(col_index1) > 0 && length(row_index2) > 0 && length(col_index2) > 0) {
      W.queen[row_index1, col_index1] <- value
      W.queen[row_index2, col_index2] <- value
    } else {
      warning(paste("One or both regions not found in the matrix:", region1, region2))
    }
  }
  
  return(W.queen)
}

modify_NUTS <- function(nuts_code) {
  gsub("([A-Za-z]+)([0-9]+)", "\\1-\\2", nuts_code)
}


panel_unstack= function(stackeddata, tstep=NULL) {
  bigT=nrow(stackeddata);K=ncol(stackeddata); C=36;
  if (is.null(tstep)) tstep=bigT
  X1=aperm(array(as.vector(t(as.matrix(stackeddata))),dim=c(K,tstep,bigT/tstep, C)), perm=c(2,1,3,4))
  try(dimnames(X1)[[1]] <-  unique(sapply(strsplit(rownames(stackeddata),"_"),
                                          function(x) x[[2]])), silent=TRUE)
  try(dimnames(X1)[[2]] <-  colnames(stackeddata), silent=TRUE)
  try(dimnames(X1)[[3]] <-  unique(sapply(strsplit(rownames(stackeddata),"_"),
                                          function(x) x[[1]])), silent=TRUE)
  try(dimnames(X1)[[4]] <-  unique(sapply(strsplit(rownames(stackeddata), "-"), 
                                          function(x) substr(x[[1]], 1, 2))), silent=TRUE)
  return(X1)
}


panel_stack = function(array3d) {
  x1= apply(array3d,2,rbind)
  try(rownames(x1) <-  as.vector(sapply(dimnames(array3d)[[3]],
                                        FUN=function(x) paste(x, dimnames(array3d)[[1]], sep="_"))), silent=TRUE)
  return(as.data.frame(x1))
}

demean = function(x, margin) {
  if (!is.array(x)) stop("x must be an array/matrix")
  otherdims=(1:length(dim(x)))[-margin]
  sweep(x,otherdims,apply(x,otherdims,mean))
}


##------------------------


spatFilt.bms <-
  function(X.data,WList,burn=1000,iter=NA,nmodel=100,mcmc="bd",g="UIP",mprior="random",mprior.size=NA,user.int=TRUE,
           start.value=NA,logfile=FALSE,logstep=10000) {
    #                beta.save=TRUE,exact=NA,int=NA,printRes=NA,ask.set=NA,return.g.stats=NA,theta=NULL,prior.msize=NULL #deprecated function arguments, retained for compatibility with older versions
    
    require(BMS)
    # set this to true (cause of W stuff, we cannot use the inverse tricks speeding up the OLS calculations
    force.full.ols=TRUE
    
    g.stats=FALSE
    # rule out hyper-g and EBL estimation, set g manually to bric in that case
    exList=list("ebl","EBL","hyper","HYPER")
    if(length(unlist(lapply(exList,function(x) grep(x,g))))>0){
      g="bric"
      print("Hyper-g prior / EBL currently not implemented for 'spatFilt.bms', g has been set to 'g=bric'.")
    }
    
    # rule out enumeration, set mcmc="bd" in that case
    exList=list("enum","enumerate")
    if(length(unlist(lapply(exList,function(x) grep(x,mcmc))))>0){
      mcmc="bd"
      print("Enumeration currently not implemented for 'spatFilt.bms', mcmc has been set to 'mcmc=bd'.")
    }
    
    
    
    
    ### getting data dimensions ####################
    if (class(X.data)[[1]]=="formula") { X.data=stats::model.frame(X.data) }
    
    if (any(is.na(X.data))) {
      X.data=na.omit(X.data)
      if (nrow(X.data)<3) {stop("Too few data observations. Please provide at least three data rows without NA entries.") }
      warning("Argument 'X.data' contains NAs. The corresponding rows have not been taken into account.")
    }
    
    
    N<-nrow(X.data)
    K=ncol(X.data)-1
    maxk=N-3  #maximum number of admissible k per model
    wNr=length(WList) # Number of W matrices
    K.filt=sapply(WList,function(x) ncol(x))   #list of length of filted Data matrices
    
    ############################################################################################################################################
    #### User Checks: ##########################################################################################################################
    ############################################################################################################################################
    
    
    # check for deprecated arguments
    #  if (!is.na(exact)) { warning("Function argument 'exact' has been deprecated, please refer to function 'estimates.bma' instead.") }
    #  if (!is.na(int)) { mcmc=paste(mcmc,".int",sep=""); warning("Function argument 'int' has been deprecated, please add an 'int' to the argument 'mcmc' instead.") }
    #  if (!is.na(printRes)) { user.int=printRes; warning("Function argument 'printRes' has been deprecated, please refer to the argument 'user.int' instead.") }
    #  if (!is.na(ask.set)) { warning("Function argument 'ask.set' has been deprecated, with no replacement.") }
    #  if (!is.na(return.g.stats)) { g.stats=return.g.stats; warning("Function argument 'return.g.stats' has been renamed into 'g.stats'.") }
    #  if (!is.null(theta)) { mprior=theta; warning("Function argument 'theta' has been renamed into 'mprior'.") }
    #  if (!is.null(prior.msize)) { mprior.size=prior.msize; warning("Function argument 'prior.msize' has been renamed into 'mprior.size'.") }
    return.g.stats=g.stats; #theta=mprior; prior.msize=mprior.size
    
    if (nmodel[1]<=0|is.na(nmodel[1])) {dotop=FALSE;nmodel=0} else {dotop=TRUE}
    
    ######################################################################################################################################
    #assign the sampling procedure
    int=FALSE; is.enum=FALSE #int: whether interaction sampler is wanted; is.enum: whether the sampler is enumeration
    if (length(grep("int",mcmc,ignore.case=TRUE))) {int=TRUE}
    
    if (length(grep("enum",mcmc,ignore.case=TRUE)))  {
      is.enum=TRUE; sampling=BMS:::.iterenum
      if (K>maxk) sampling=BMS:::.iterenum.KgtN
    } else if(length(grep("bd",mcmc,ignore.case=TRUE))){
      sampling=switch(int+1,BMS:::.fls.samp,BMS:::.fls.samp.int)
    } else {
      sampling=switch(int+1,BMS:::.rev.jump,BMS:::.rev.jump.int)
    }
    
    ######################################################################################################################################
    # specific enumeration user checks & init
    if (is.enum) {
      #check for a start.value index to start enumeration from seomewhere in between (and not do all possible models)
      
      start.value2=0
      if (length(start.value)==1) {
        start.value2=suppressWarnings(as.integer(start.value))
        if (any(is.na(start.value2))|start.value2[[1]]<K+5|start.value2[[1]]<0|start.value2[[1]]>=(2^K-1)) {
          start.value=0;start.value2=0
        }  else {
          #if startvalue is an integer index satysfying above conditions then convert it into a 'draw' to start from
          start.value=.enum_fromindex(start.value2)
          start.value=c(numeric(K-length(start.value)),start.value)
        }
      } else { start.value=0 }
      #   do all the other enumeration stuff
      burn=0;  int=FALSE; mcmc="enum"; is.enum=TRUE
      
      if (K>maxk) {lastindex=2^K-1-sum(choose(K,(N-2):K))} else lastindex=2^K-1
      if (is.na(iter)) {iter=lastindex-start.value2} #default iter is the rest to complete the enumeration from start.value
      iter=min(iter,2^K-1-start.value2); # don't do too much!
      
      
    } else {
      if (is.na(iter)) {iter=3000}; #if no enumeration and iter not given, set to default value 3000
    }
    
    ######################################################################################################################################
    # generate logfile if desired
    if(logfile!=FALSE){
      if (is.character(logfile)) {
        sfilename=logfile}
      else {
        sfilename="test.log"
      }
      if (nchar(sfilename)>0) file.create(sfilename)
      logfile=TRUE
      cat(as.character(Sys.time()),": starting loop ... \n",append=TRUE, file=sfilename)  #write one line
      if (logstep!=10000) fact=logstep else fact=max(floor((burn+iter)/100),logstep)
    }
    ######################################################################################################################################
    #model prior Initialization
    
    #outsourced to stupid function for cleanliness
    pmplist=BMS:::.choose.mprior(mprior,mprior.size,K=K)
    mprior=pmplist$mp.mode;
    
    
    
    ######################################################################################################################################
    ######################################################################################################################################
    # subtract mean from all regressors as in FLS
    y<-as.matrix(X.data[,1])
    X<-as.matrix(X.data[,2:ncol(X.data)])
    y.mean=mean(y)
    y<-y-matrix(y.mean,N,1,byrow=TRUE)
    X.mean=colMeans(X)
    X<-X-matrix(X.mean,N,K,byrow=TRUE)
    
    
    # here demeaning of eigenvectors
    for(i in 1:wNr){
      Xmeans=colMeans(as.matrix(WList[[i]]))
      WList[[i]]=WList[[i]]-matrix(Xmeans,nrow(WList[[i]]),ncol(WList[[i]]),byrow=TRUE)
    }
    
    
    # multiply the whole matrix stuff out before going into the simulation loops
    XtX.big=crossprod(X)
    Xty.big=crossprod(X,y)
    yty = as.vector(crossprod(y))
    
    # user check:
    coreig=eigen(cor(X),symmetric=TRUE,only.values=TRUE)$values
    #  if (qr(XtX.big)$rank<(min(N,K)-1)) stop("The design matrix you provided seems to overly suffer from linear dependence; After adjusting for a constant, its rank is <min(N-1,K-1).\n This would almost certainly lead to problems in BMA sampling. Check whether you perhaps accidentally provided a constant term in X.data.")
    if (sum(coreig>1e-10)<min(K,(N-1))) { force.full.ols=TRUE }
    
    
    # now create big XtX lists and Xty lists (for each W an own list)
    # we use Frisch Waugh  here
    resY=sapply(WList, function(x) lm(y~x)$res)
    LmList=list()
    for(i in 1:length(WList)){
      LmList[[i]]=apply(X,2,function(x) lm(x~as.matrix(WList[[i]]))$res)
    }
    
    XtX.bigList<-Xty.bigList<-list()
    for(i in 1:wNr){
      XtX.bigList[[i]]=crossprod(LmList[[i]])
      Xty.bigList[[i]]=crossprod(LmList[[i]],y)
    }
    yty.big=apply(resY,2,crossprod)
    
    
    ######################################################################################################################################
    # for the case that X contains interaction terms
    if(int){
      if(length(grep("#",colnames(X.data),fixed=TRUE))==0) stop("Please separate column names of interaction terms by # (e.g. A#B)")
      mPlus=BMS:::.constr.intmat(X,K)
    }
    else{ mPlus<-NA }
    
    ######################################################################################################################################
    #gprior-Stuff Initialization
    
    # now initialize wNr gprior objects, for each object we have a different yty
    
    gprior.info=BMS:::.choose.gprior(g,N,K,g.stats) # gprior.info is a list that summarizes info about the choice of the g-prior
    
    # null lik calculation
    nullVec=((1-N)/2)*log(yty.big)    # this is wrong!!
    
    lprobcalc=list()
    for(i in 1:wNr){
      if (gprior.info$gtype=="EBL")  {
        lprobcalc[[i]]=BMS:::.lprob.eblocal.init(N=N,K=(K+K.filt[i]),yty=yty,return.g=gprior.info$return.g.stats,null.lik=nullVec[i])
      } else if (gprior.info$gtype=="hyper")  {
        lprobcalc[[i]]=BMS:::.lprob.hyperg.init(N=N,K=(K+K.filt[i]),yty=yty,f21a=gprior.info$hyper.parameter,return.gmoments=gprior.info$return.g.stats,null.lik=nullVec[i])
      } else {
        lprobcalc[[i]]=BMS:::.lprob.constg.init(g=gprior.info$g,N=N,K=(K+K.filt[i]),yty=yty,null.lik=nullVec[i])
      }
    }
    
    ######################################################################################################################################
    #The function Starter selects randomly a start matrix and runs a
    #regression. From this regression, the
    #start Design matrix is that for which the t-stats are >0.2. So we
    #can circumvent starting from a very bad start point.
    wSampleOld=ceiling(runif(1,min=0,max=wNr))
    wdraw=numeric(wNr); wdraw[wSampleOld]=1
    
    start.list=BMS:::.starter(K,start.value,y=resY[,wSampleOld],N=N,XtX.big=XtX.bigList[[wSampleOld]],Xty.big=Xty.bigList[[wSampleOld]],X=LmList[[wSampleOld]])
    molddraw=start.list$molddraw; start.position=start.list$start.position
    kold=sum(molddraw)
    position=(1:K)[molddraw==1]
    
    ########################################################################################################################################
    
    
    
    ################################################################################################
    #    Initializing                                                                              #
    ################################################################################################
    
    
    # initialize sampler-specific variables    ########################################
    # these are to select additional statistics (such as g)
    collect.otherstats=FALSE
    otherstats=numeric(0)
    add.otherstats=numeric(0)
    # initialize the vector for collecting the empirical shrinkage factor moments
    if (gprior.info$return.g.stats & !(gprior.info$is.constant)) { add.otherstats=gprior.info$shrinkage.moments; collect.otherstats=TRUE }
    cumsumweights=iter
    
    if (collect.otherstats) {addup=spatBMS:::.addup.mcmc.wotherstats} else {addup=spatBMS:::.addup.mcmc}
    if (is.enum) {
      cumsumweights=0
      if (collect.otherstats) {addup=spatBMS:::.addup.enum.wotherstats} else {addup=spatBMS:::.addup.enum}
    }
    environment(addup) <- environment()
    ##################################################################################
    
    
    
    ##initialize model variables with starter model ###################################
    
    ols.object=BMS:::.ols.terms2(positions=(1:K)[molddraw==1],yty=yty.big[[wSampleOld]],k=kold,N,K=K,XtX.big=XtX.bigList[[wSampleOld]],Xty.big=Xty.bigList[[wSampleOld]]) #OLS results from starter model
    
    lik.list=lprobcalc[[wSampleOld]]$lprob.all(ymy=ols.object$ymy, k=kold+K.filt[wSampleOld], bhat=ols.object$bhat, diag.inverse=ols.object$diag.inverse) #likelihood and expected values for starter model
    lprobold=lik.list$lprob
    b1=lik.list$b1new
    b2=lik.list$b2new
    
    ## calculate the posterior model probability for the first model
    pmpold=pmplist$pmp(ki=kold,mdraw=molddraw)
    ##################################################################################
    
    ## initialize top 10 function ####################################################
    
    #null.lik=((1-N)/2)*log(yty) # calculate Likelihood for NullModel
    
    
    #  if (beta.save<0) {bbeta2=FALSE} else { bbeta2=beta.save }
    # set beta save manually to TRUE  cause I need to save at least the W-index variable
    beta.save=TRUE
    topmods=topmod(nmaxregressors=(K+wNr),nbmodel=nmodel, lengthfixedvec=1,bbeta=beta.save)
    
    if (dotop) topmods$addmodel(mylik=pmpold+lprobold,vec01=c(molddraw,wdraw),vbeta=c(b1,NA),vbeta2=c(b2,NA),fixedvec=wSampleOld)
    # topmods$addmodel(mylik=pmp2,vec01=c(molddraw,wdraw),vbeta=c(b1,0),vbeta2=c((stdev^2+b1^2),0),fixedvec=wSampleOld)
    
    ##################################################################################
    
    
    
    
    
    ## Initialize the rest  ###########################################################
    null.count=0             #number the null model has been drawn
    models.visited=0         #how often a model has been accepted (in burn-ins and iterations)
    inccount=numeric(K)      #how often the respective covariate has been included
    msize=0                  #average model size
    k.vec=numeric(K)         #how often the respective model size has been accepted
    b1mo=numeric(K)          #holds aggregate first moment of all coefficients
    ab=numeric(K)            #Initialize them here
    b2mo=numeric(K)          #holds aggregate second moment of all coefficients
    bb=numeric(K)
    Wcount=numeric(wNr)
    possign=inccount         # the number of times the respective coefficent has been positive
    mnewdraw=numeric(K)      #holds the binary vector denoting the proposed model
    if (force.full.ols) {candi.is.full.object=TRUE} else {candi.is.full.object=FALSE} #candi.is.full: if TRUE, standard OLS, else OLS via Frisch-Waugh tricks
    bmo=numeric(4*K); bm=bmo #common placeholder for b1mo, b2mo, k.vec and possign
    if (is.enum) { addup() } # in case the sampler is enumeration then count the starting value as well (no burn-ins)
    
    
    ###############################################################################################################
    ###############################################################################################################
    
    
    
    
    
    
    
    
    
    
    
    
    
    #############################################################################################
    set.seed(as.numeric(Sys.time()))              #Set Seed randomly for number generator
    
    t1<-Sys.time()                                #Save time before going into the loop
    ###########################################################################################
    #START MAIN LOOP
    ###########################################################################################
    for (i in 1:(burn+iter)){
      
      if(logfile){ if (i %% fact==0) { cat(as.character(Sys.time()),":",i,"current draw \n",append=TRUE, file=sfilename)} } #write one line
      
      ##########################################################################################
      #Start sampling program
      ###########################################################################################
      
      
      # Regressor Sampling Start##################################################################################################################################
      #sample a model
      a=sampling(molddraw=molddraw,K=K,mPlus=mPlus,maxk=maxk,oldk=kold)
      mnewdraw=a$mnewdraw; positionnew=a$positionnew; knew=length(positionnew)
      
      #calculate prior model prob
      pmpnew=pmplist$pmp(ki=knew,mdraw=mnewdraw) # get the (log) model prior prob
      
      if (!is.enum) {
        if (int) {if (length(c(a$dropi,a$addi))>2|i<3|force.full.ols) {candi.is.full.object=TRUE} else {candi.is.full.object=FALSE}}
        #candi.is.full.object = TRUE if there were multiple regs dropped or added due to interaction terms
        
        if (candi.is.full.object) {
          ols.candidate = BMS:::.ols.terms2(positions=positionnew,yty=yty.big[[wSampleOld]],k=knew,N,K=K,XtX.big=XtX.bigList[[wSampleOld]],
                                            Xty.big=Xty.bigList[[wSampleOld]]) #in case of changing interaction terms, draw the big OLS stuff
          ymy.candi =ols.candidate$ymy
        } else {
          ymy.candi=ols.object$child.ymy(a$addi,a$dropi,k=knew) #if standard sampling, use Frisch-Waugh to get the new ResidSS (faster)
        }
        
        
        lprobnew = lprobcalc[[wSampleOld]]$just.loglik(ymy.candi,c(knew+K.filt[wSampleOld])) # get the log-likelihood out of the ResidSS
        
        #Now decide whether to accept candidate draw
        accept.candi = as.logical(log(.Internal(runif(1,0,1)))< lprobnew-lprobold + pmpnew-pmpold)
        
      } else {
        accept.candi=TRUE
        candi.is.full.object=FALSE
      }
      
      
      
      if(accept.candi){
        if (!candi.is.full.object) {
          # in case one has used Frisch-Waugh and the new model got accepted,
          # calculate the 'real' inverse in order not to make copying mistakes
          ols.res = ols.object$mutate(addix=a$addi, dropix=a$dropi, newpos=positionnew, newk=knew)
        } else {
          ols.object = ols.candidate
          ols.res = ols.candidate$full.results()
        }
        
        lik.list = lprobcalc[[wSampleOld]]$lprob.all(ols.res$ymy, c(knew+K.filt[wSampleOld]), ols.res$bhat, ols.res$diag.inverse)
        
        
        lprobold=lik.list$lprob
        position = positionnew
        pmpold=pmpnew # get posterior odds for new model  if accepted
        molddraw=mnewdraw
        kold=knew
        models.visited=models.visited+1 #does not account for revisiting models
      }
      
      
      # W Sampling Start##################################################################################################################################
      wSampleNew=(1:wNr)[-c(wSampleOld)][ceiling((runif(1)*(wNr-1)))] #
      #wSampleNew=ceiling(runif(1,min=0,max=wNr))
      
      
      # not necessary to calculate PMPs since we do not sample the Ws
      # pmpnew=pmplist$pmp(ki=kold,mdraw=molddraw) # get the (log) model prior prob
      
      
      # k is kold, only W changes to wSampleNew!!!
      ols.candidate = BMS:::.ols.terms2(positions=position,yty=yty.big[[wSampleNew]],k=kold,N,K=K,XtX.big=XtX.bigList[[wSampleNew]],
                                        Xty.big=Xty.bigList[[wSampleNew]]) #in case of changing interaction terms, draw the big OLS stuff
      ymy.candi =ols.candidate$ymy
      lprobnew = lprobcalc[[wSampleNew]]$just.loglik(ymy.candi,c(kold+K.filt[wSampleNew])) # get the log-likelihood out of the ResidSS
      
      # Now decide whether to accept candidate draw
      # accept.candi = as.logical(log(.Internal(runif(1,0,1)))< lprobnew-lprobold + pmpnew-pmpold)
      accept.candi = as.logical(log(.Internal(runif(1,0,1)))< lprobnew-lprobold )
      #
      
      
      if(accept.candi){
        ols.object = ols.candidate
        ols.res = ols.candidate$full.results()
        wSampleOld=wSampleNew
        lik.list = lprobcalc[[wSampleOld]]$lprob.all(ols.res$ymy, c(kold+K.filt[wSampleNew]), ols.res$bhat, ols.res$diag.inverse)
        lprobold=lik.list$lprob
        #position = positionnew   # no position change and no pmp change
        #pmpold=pmpnew # get posterior odds for new model  if accepted
        #molddraw=mnewdraw
        #kold=knew
        models.visited=models.visited+1 #does not account for revisiting models
        
      }
      
      
      
      
      
      # Collect Posterior Draws
      ########################################################################
      if (i>burn){
        b1=lik.list$b1new; b2=lik.list$b2new
        
        addup() #addup does iterative, cumulative sums of quantities of interest (betas, model size, etc.)
        wdraw=numeric(wNr); wdraw[wSampleOld]=1
        Wcount[wSampleOld]=Wcount[wSampleOld]+1;
        # add log(lik)*p(M) to topmodels
        if (dotop)  topmods$addmodel(mylik=pmpold+lprobold,vec01=c(molddraw,wdraw),vbeta=c(b1,NA),vbeta2=c(b2,NA),fixedvec=wSampleOld)
        
      }
    }
    ###########################################################################################
    #END MAIN LOOP
    ###########################################################################################
    
    
    ###########################################################################################
    #adjust the topmod object and calculate all the betas after sampling
    #similar to having set bbeta=TRUE, and bbeta2=TRUE in the call to .top10 above
    #if (dotop) topmods=.topmod.as.bbetaT(topmods,gprior.info,X.data)
    
    ###########################################################################################
    
    ###########################################################################################
    
    timed<-difftime(Sys.time(),t1)
    
    # do aggregating calculations
    if (is.enum) {iter=iter+1; models.visited=models.visited+1}
    bmo=matrix(bmo,4,byrow=TRUE); b1mo=bmo[1,]; b2mo=bmo[2,]; k.vec=bmo[3,]; possign=bmo[4,]; rm(bmo)
    
    post.inf=BMS:::.post.calc(gprior.info,add.otherstats,k.vec,null.count,X.data,topmods,b1mo,b2mo,iter,burn,inccount,models.visited,K,N,msize,timed,cumsumweights,mcmc,possign)
    
    
    
    # report disaggregated results
    wIdx=topmods$fixed_vec()
    tM=topmods$bool_binary()[1:(ncol(X.data)-1),]
    if(length(colnames(X.data)[-1])==nrow(tM)){
      rownames(tM)=colnames(X.data)[-1]
    }
    lt1=topmods$lik() - max(topmods$lik())    # do this to get only positive probabilities
    lt1=exp(lt1)/sum(exp(lt1))
    lt2=topmods$ncount()/sum(topmods$ncount())
    
    #rbind the probs to the tMmatrix
    tM=rbind(tM,wIdx,lt1,lt2)
    rownames(tM)[(nrow(tM)-2):nrow(tM)]=c("W-Index","PMP (Exact)","PMP (MCMC)")
    if(length(topmods$bool())==ncol(tM)){
      colnames(tM)=topmods$bool()
    }
    
    
    
    result=list(info=post.inf$info,arguments=BMS:::.construct.arglist(spatFilt.bms),topmod=w2topmod(topmods,wNr),wTopModels=tM,
                topmodOr=topmods,start.pos=sort(start.position),gprior.info=post.inf$gprior.info,mprior.info=pmplist,
                X.data=X.data,WList=WList, reg.names=post.inf$reg.names,bms.call=match.call(spatFilt.bms,sys.call(0)))
    
    
    class(result)=c("bma","spatFilt")
    
    # spatFilt extension
    names(Wcount)=names(WList)
    result$Wcount=Wcount
    ###########################################################################################
    
    # print results to console
    if(user.int){
      print(result)
      print(timed)
      BMS:::plot.bma(result) # do modelsize plot
    }
    
    return(invisible(result))
  }





pmpW.bma <-
  function(object){
    require(BMS);
    if(!all(class(object) %in% c("bma","spatFilt")))
      stop("Submit a spatFilt bma object")
    
    wTmat=object$wTopModels
    uN=unique(wTmat["W-Index",]);postSumE=sum(wTmat["PMP (Exact)",]);
    postSumV=sum(wTmat["PMP (MCMC)",]);
    pmp=NULL
    for(i in 1:length(uN)){
      idx=which(object$wTopModels["W-Index",]==uN[i])
      pmp=rbind(pmp,cbind(sum(object$wTopModels["PMP (Exact)",idx])/postSumE*100,
                          sum(object$wTopModels["PMP (MCMC)",idx])/postSumV*100))
    }
    
    colnames(pmp)=c("PMP (Exact)", "PMP (MCMC)")
    # rbind the matrices receiving zero posterior support for completeness
    rdx=which(!c(1:length(object$WList)) %in% uN)
    if(length(rdx)>0){
      pmp=rbind(pmp,matrix(0,ncol=2,nrow=length(rdx)))
      rownames(pmp)=names(object$WList)[c(uN,rdx)]
    }
    else{
      rownames(pmp)=names(object$WList)[uN]
    }
    return(pmp)
  }



moranTest.bma <-
  function (object, variants = "single", W, nmodel = NULL)
  {
    require(BMS)
    require(spdep)
    if (!all(class(object) %in% c("bma", "spatFilt")))
      stop("Submit a spatFilt bma object")
    if (!variants %in% c("single", "double"))
      variants = "single"
    if (!"listw" %in% class(W))
      stop("Please submit a neighborhood matrix object of class 'listw'")
    lmList = lmListEV = moran = moranEV = list()
    wTmat = object$wTopModels
    dataM = as.matrix(object$X.data)
    WeightList = object$WList
    mMat = wTmat[1:(ncol(dataM) - 1), ]
    if (is.null(nmodel) || nmodel > ncol(wTmat))
      nmodel = ncol(wTmat)
    if (variants == "double") {
      for (i in 1:nmodel) {
        xMat1 = cbind(dataM[, names(which(mMat[, i] == 1))])
        xMat2 = cbind(dataM[, names(which(mMat[, i] == 1))],
                      as.matrix(WeightList[[wTmat["W-Index", i]]]))
        lmList[[i]] = lm(dataM[, 1, drop = FALSE] ~ xMat1)
        lmListEV[[i]] = lm(dataM[, 1, drop = FALSE] ~ xMat2)
      }
      moran = lapply(lmList, function(x) lm.morantest(x, W))
      moranEV = lapply(lmListEV, function(x) lm.morantest(x,
                                                          W))
    }
    else {
      for (i in 1:nmodel) {
        xMat2 = cbind(dataM[, names(which(mMat[, i] == 1))],
                      as.matrix(WeightList[[wTmat["W-Index", i]]]))
        lmListEV[[i]] = lm(dataM[, 1, drop = FALSE] ~ xMat2)
      }
      moranEV = lapply(lmListEV, function(x) lm.morantest(x,
                                                          W))
    }
    results = list(moran = moran, moranEV = moranEV)
    return(results)
  }



w2topmod <-
  function(tmo,wNr){
    binmat=tmo$bool_binary()
    KW=nrow(binmat); K=KW-wNr
    newbins=t(unique(as.data.frame(t(binmat[1:K,]))))
    dblbinsix=duplicated(as.data.frame(t(binmat[1:K,])))
    if(!any(dblbinsix)){
      return(topmod(nbmodels=tmo$nbmodels,nmaxregressors=K,bbeta=TRUE,lengthfixedvec=wNr,liks=tmo$lik(),
                    ncounts=tmo$ncount(),modelbinaries=binmat[1:K,],betas=tmo$betas()[1:K,],betas2=tmo$betas2()[1:K,],fixed_vector=tmo$fixed_vec()))
    }
    # first look at unique topmodels and extract their statistics (likelihood, betas, betas2, etc.)
    else{
      newlik=tmo$lik()[!dblbinsix]
      newncount=tmo$ncount()[!dblbinsix]
      newbetas=tmo$betas()[1:K,!dblbinsix]
      newbetas2=tmo$betas2()[1:K,!dblbinsix]
      newfixedvec=binmat[-(1:K),!dblbinsix]
      #dblbins=binmat[1:K,dblbinsix]
      dblbins=binmat[1:K,dblbinsix,drop=FALSE]
      
      # now get aggregated statistics (lik, betas, etc); e.g. beta|w1, beta|w2, beta|w3...get pmp weighted (integrate W dimension out)
      for (i in 1:ncol(newbins)) {
        
        index.in.dblbins=!as.logical(colSums(abs(dblbins-newbins[,i])))
        if (any(index.in.dblbins)) {
          
          #thism=cbind(newbins[,i],dblbins)
          thism.ix.intmo=c(i,which(dblbinsix)[index.in.dblbins])
          thism.lik=tmo$lik()[thism.ix.intmo]
          thism.weights=exp(thism.lik)/sum(exp(thism.lik))
          newlik[i]=crossprod(thism.lik,thism.weights)[[1]]
          newncount[i]=sum(tmo$ncount()[thism.ix.intmo])
          newbetas[,i]=c(tmo$betas()[1:K,thism.ix.intmo]%*%thism.weights)
          newbetas2[,i]=c(tmo$betas2()[1:K,thism.ix.intmo]%*%thism.weights)
          newfixedvec[c(tmo$fixed_vector())[thism.ix.intmo],i]=1
        }
      }
    }
    
    return(topmod(nbmodels=tmo$nbmodels,nmaxregressors=K,bbeta=TRUE,lengthfixedvec=wNr,liks=newlik,ncounts=newncount,modelbinaries=newbins,betas=newbetas,betas2=newbetas2,fixed_vector=newfixedvec))
  }

.Random.seed <-
  c(403L, 176L, 1050431880L, 736627224L, 411018411L, 703395146L,
    1480023L, 2056853610L, -1204487193L, 747713775L, 1026098991L,
    769782763L, -944924064L, -1915329963L, -670890257L, -641007037L,
    666099051L, 972870585L, -861533935L, -1976758763L, 185274910L,
    -673807387L, 837092484L, 356024116L, -568771687L, 1508041325L,
    230856101L, 966632597L, 1184827506L, 215095737L, -288768885L,
    -811626335L, 1003384882L, -785856030L, -2133729359L, 482047299L,
    -1562849908L, 1430017783L, -1074201986L, 785028891L, 436073005L,
    -914732590L, -1901420997L, -1732429534L, -431120595L, 1202067857L,
    204769942L, -1852953352L, -1316692842L, -1291165896L, 57086160L,
    -76485933L, 1701306063L, -611539830L, -779808593L, 1557633654L,
    1767825998L, 639528474L, -347699986L, -606542993L, 1387274563L,
    525377908L, 33146287L, 1021268910L, 1160304499L, -2019845222L,
    879106296L, 1787671378L, -1247396943L, 528766235L, -1718902033L,
    942856371L, -1089660597L, -275440929L, 59248782L, -2079397972L,
    -934385997L, -314801814L, -1343771797L, 1896771606L, 1038557846L,
    -1119761031L, 679080667L, -1069385656L, 576542971L, 1958632433L,
    -1296613550L, 887219375L, -1256440147L, 966888435L, 201765009L,
    1211286748L, 61466589L, 724376718L, 189350141L, -1362299181L,
    -391698315L, -1355363075L, -1080167170L, 73422869L, -1944655859L,
    1030548639L, -1931789311L, -294045534L, 154962119L, 141646326L,
    2063921090L, 1972125615L, -1557646077L, 2132458613L, 30081264L,
    -169705259L, 406408035L, 1604861557L, 182832669L, -1989173758L,
    139352955L, 1852328960L, 936825989L, -50773770L, 598885004L,
    -922975621L, -431723200L, 1182116052L, 1733540706L, -1003443233L,
    1446048262L, -1409932438L, 924322939L, -750693401L, -1693658447L,
    -172607835L, -1948056897L, 634711288L, 1447716466L, 1245663992L,
    -1097842970L, 736116703L, -1703499086L, -1151446178L, 1765158115L,
    -1532297523L, -2119159211L, 198909741L, 42721254L, 820092200L,
    2145383439L, -1701042858L, -1492825929L, 593442352L, -1490914720L,
    -1541455044L, -1466869521L, -252551984L, 1736635229L, -288153979L,
    -1163712741L, -871814243L, -1312497958L, -1090247754L, -406232161L,
    -1514594634L, -1269576560L, -1615322391L, -107444999L, -864234779L,
    -172219373L, 1079769931L, 598351007L, -1617469616L, -1235517595L,
    -1661137823L, -1588095333L, 840941002L, -1730757359L, -1420140159L,
    320933639L, 1252572198L, -545405275L, 871798447L, 300704702L,
    -686535679L, -1850603303L, 1412717776L, -1401775756L, 925821330L,
    -689195487L, -1898407765L, 967618865L, -1444026656L, -1612353927L,
    -1803821972L, 832086615L, -1684936099L, 1268582165L, 1575472206L,
    -1539582152L, -1605383017L, 1253347953L, -1194756453L, 1167297094L,
    2105174886L, 831374487L, -1533039066L, -791357562L, -458591829L,
    1954488876L, -1017915267L, 1126490987L, -1522175523L, -81509750L,
    -406405317L, 1704425665L, 904289591L, 1103745246L, -1894341356L,
    -1627130587L, -922092225L, 324741999L, -1799778110L, 1443325737L,
    1625189535L, 118189040L, 1791576357L, 11777497L, -312538298L,
    -1070223368L, 1905263316L, -1448141087L, -1705981913L, 438440405L,
    -2105001803L, 105666065L, 1175850406L, -541156113L, -927041631L,
    555641934L, 616252061L, 869063732L, -295654198L, -1219467290L,
    -1495042564L, 1937939432L, 150057371L, 798030167L, 2111671058L,
    189275654L, 1770370676L, 1598916238L, -512455337L, -1525432448L,
    9956755L, -477209735L, 970697971L, -1745433402L, 709649421L,
    -17351714L, 493829456L, -1023956540L, -1926970713L, 1861522731L,
    10550644L, 1989492329L, 1564176778L, -678036509L, 717643354L,
    -986691328L, -782987580L, 1728399692L, 240939130L, -162101237L,
    -1798068964L, 858332737L, -881993856L, 1834319353L, 1818219814L,
    1843943011L, 992096600L, -2107997112L, -362829889L, -1859495L,
    -2044410713L, 1761309317L, -1645782294L, 1492181924L, 1739089130L,
    -306146443L, -1678915050L, 936191717L, -754182064L, 268766147L,
    1726769147L, -1657605244L, -486235107L, -2006404530L, 329020333L,
    -1344985887L, -391825930L, -1621668915L, 283168735L, 1465073671L,
    -1642588944L, -943591574L, -1018986445L, 1907087266L, 1371251920L,
    -51579417L, 1101549957L, 676704977L, -69645986L, -988013334L,
    80100054L, -651630782L, -986559600L, -166064861L, 192859356L,
    -48514438L, 1216645508L, 1632351778L, -370704153L, -712664351L,
    -1918160150L, -1821399176L, -1176997805L, 364116588L, -766813224L,
    -134595360L, 1034469140L, 1555001988L, -1841917617L, -1695013364L,
    -970151350L, 611760168L, 641848878L, -921028536L, 497023370L,
    -991685713L, 1639690196L, -2044831147L, 2012336629L, 1132617915L,
    -320338680L, 1038838088L, 824985449L, -1280767542L, -1111116267L,
    1683624449L, 521513631L, 1623570860L, 1277802299L, 249857320L,
    798142589L, 911111832L, 1275769896L, -1088611030L, 1558099293L,
    -1125050163L, 1446441454L, 943372812L, 1841889483L, -72282955L,
    1716939178L, -1788211799L, -80061452L, 1782935940L, 1344229529L,
    -373673705L, -1375423133L, 1344569884L, 1054627179L, 1467369339L,
    -802996740L, 533133446L, 1148517833L, -1278647995L, 1871775366L,
    -564688173L, -653966747L, 1685759255L, 1878595523L, 262160184L,
    -328932456L, 1358600759L, -1861120880L, 251972922L, 2059869515L,
    -2053824304L, 497629740L, -1629484984L, 299390437L, -118611580L,
    1217961179L, 1087075695L, 2100516564L, -361786696L, 888451876L,
    918168052L, -310361730L, -942222108L, 1890460353L, -1026017687L,
    -1508147457L, -1814702350L, -416548426L, -1385599647L, -1422820055L,
    -706886929L, -1149513752L, 1278692285L, 1844872436L, 1354043381L,
    -1965114299L, -911331290L, 1855646143L, -669068000L, 1696813955L,
    467763469L, 12516899L, -114479478L, 13356463L, -1306073842L,
    -1709066329L, 371391883L, -166991443L, 1613496417L, 1986548380L,
    -816512300L, 413883604L, -140119375L, 1383383946L, 890792812L,
    -1523658070L, -197775690L, -1471301361L, -98967325L, -1859371787L,
    -2118451444L, -1734408155L, 985256832L, 998835938L, 1385332044L,
    -1694193501L, 1280707464L, 694585558L, 1239308506L, 1852965925L,
    -601473830L, -647827195L, -507905488L, 126943491L, -1835368594L,
    -303868990L, -794637808L, 200460066L, -1396879305L, -1121843349L,
    -1133277673L, -1475850658L, 185046661L, 1200750519L, 832589886L,
    1236503413L, 1226972170L, -2042310717L, 1007521419L, 1864469825L,
    380918212L, 2052516432L, -266297924L, -1107148700L, -2130959240L,
    -665931313L, 1684251805L, 662107865L, 741798099L, -972797416L,
    -925190240L, 816343722L, -1840932878L, 1932966L, -1719097019L,
    -567633101L, 257686860L, -247858508L, -663434283L, 1477986340L,
    322679166L, -1260602296L, -269553367L, 148292567L, -981270818L,
    -595934488L, -364198075L, -1663617994L, -715568029L, -1233765156L,
    -894305393L, -910022779L, -1358529670L, 1035097146L, 2118278045L,
    -1167188276L, -1632138682L, -1606599088L, 1510567878L, -1761587873L,
    -17190884L, -1633093218L, 1674814427L, -784707610L, 1398413726L,
    791551250L, 1451211031L, -1516331506L, 703432667L, 2066628408L,
    976595856L, 40478810L, -1594141764L, -966274487L, -1960621468L,
    250254035L, -2007502468L, 239761933L, 1568976011L, -256224548L,
    881062849L, -15131161L, -777589263L, 456749356L, -317473410L,
    1904221315L, 1842518732L, 1668571917L, -789721082L, -516513454L,
    -505923212L, -1596632382L, -1546362379L, 1547181509L, -837699932L,
    442160922L, -1625777724L, -2047587636L, 1453756356L, 558897580L,
    -1114423682L, 847935537L, 728206752L, 292542093L, -285611637L,
    -117297667L, -799323035L, 1401822109L, 1932473823L, 1766183501L,
    -684069426L, 327244136L, -1613926410L, 1600864740L, -1507761498L,
    29786014L, -1942181117L, -1416243294L, -646717563L, 2037902653L,
    -1116934848L, -386310L, 632789058L, -1798619477L, 1620562716L,
    2026311689L, 1759667070L, 1048831051L, 1277827439L, 163744263L,
    79373700L, 1501113584L, 443509883L, 780014693L, 857652676L, -502245584L,
    -590796873L, -1656398285L, -1765399558L, 292497089L, 63212133L,
    907827959L, 563690172L, 2146472866L, 261224680L, 1402881722L,
    686841076L, -46601172L, 1928795854L, 1089397277L, -145618841L,
    1302736999L, 2084960418L, -445371748L, -1311998078L, -1310068412L,
    1649822288L, 1530045988L, -2051505211L, 1374847073L, -992993205L,
    -495517500L, -240785962L, 1731192017L, 1440466618L, -1010595712L,
    883911337L, -1398547454L, 1047023653L, 541877884L, -504670173L,
    -235968129L, 847677962L, 1462869927L, 1115438956L, 773967256L,
    562676020L, 117373551L, -2128717829L, -1734804448L)

# FUNCTIONS DESIGNED FOR BEING CALLED IN bms() #######################################
# these functions are only subfunction to be called inside bms()
# [therefore they necessitate the statement environemnt(SUBFUNCTION) <- environment() inside bms() ]
# the are defined outside of bms() for readability and modularity purposes

.addup.mcmc <- function() {
  
  inccount <<- inccount + molddraw #PIPs
  msize<<-msize + kold   # average size of models
  
  #for speed reasons, iterative adding with indexing should be done in one stacked vector
  if (kold!=0) {
    bm[c(position,K+position,2*K+kold,3*K+position)]=c(b1,b2,1,b1>0); bmo <<- bmo+bm
    #bmo is partitioned: first K entries have cum. b1 ("b1mo"), second K entries have cum. b2 ("b2mo"), third K entries have model size dist ("k.vec"), and fourth K entries are like inccount for positive betas (add up pos. sign covariate selections)
  } else {
    null.count<<-null.count+1
  }
}


.addup.mcmc.wotherstats <-  function() {
  
  inccount <<- inccount + molddraw #PIPs
  msize<<-msize + kold   # average size of models
  
  #for speed reasons, iterative adding with indexing should be done in one stacked vector
  if (kold!=0) {
    bm[c(position,K+position,2*K+kold,3*K+position)]=c(b1,b2,1,b1>0); bmo <<- bmo+bm
    #bmo is partitioned: first K entries have cum. b1 ("b1mo"), second K entries have cum. b2 ("b2mo"), third K entries have model size dist ("k.vec"), and fourth K entries are like inccount for positive betas (add up pos. sign covariate selections)
  } else {
    null.count<<-null.count+1
  }
  
  # collect e.g. estimated g-priors, etc
  otherstats<<-lik.list$otherstats; add.otherstats<<-add.otherstats + otherstats
}


.addup.enum <- function() {
  weight=  exp(pmpold+lprobold-null.lik)
  
  inccount <<- inccount + weight*molddraw #PIPs
  msize<<-msize + weight*kold   # average size of models
  cumsumweights<<-cumsumweights+weight #denominator to get at sum of PMPs=1
  
  #for speed reasons, iterative adding with indexing should be done in one stacked vector
  if (kold!=0) {
    bm[c(position,K+position,2*K+kold,3*K+position)]=weight*c(b1,b2,1,b1>0); bmo <<- bmo+bm
    #bmo is partitioned: first K entries have cum. b1 ("b1mo"), second K entries have cum. b2 ("b2mo"), third K entries have model size dist ("k.vec"), and fourth K entries are like inccount for positive betas (add up pos. sign covariate selections)
  } else {
    null.count<<-null.count+weight
  }
}


.addup.enum.wotherstats <- function() {
  
  weight=  exp(pmpold+lprobold-null.lik)
  inccount <<- inccount + weight*molddraw #PIPs
  msize<<-msize + weight*kold   # average size of models
  cumsumweights<<-cumsumweights+weight #denominator to get at sum of PMPs=1
  
  #for speed reasons, iterative adding with indexing should be done in one stacked vector
  if (kold!=0) {
    bm[c(position,K+position,2*K+kold,3*K+position)]=weight*c(b1,b2,1,b1>0); bmo <<- bmo+bm
    #bmo is partitioned: first K entries have cum. b1 ("b1mo"), second K entries have cum. b2 ("b2mo"), third K entries have model size dist ("k.vec"), and fourth K entries are like inccount for positive betas (add up pos. sign covariate selections)
  } else {
    null.count<<-null.count+weight
  }
  otherstats<<-lik.list$otherstats; add.otherstats<<-add.otherstats + weight*otherstats
}


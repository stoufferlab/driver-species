
# model averaged prediction
predict.glmulti <- function(object, select="all", newdata=NA, se.fit=FALSE, varweighting="Buckland", icmethod="Lukacs", alphaIC=0.05,...) 
{
	ww = exp(-(object@crits - object@crits[1])/2)
	ww=ww/sum(ww)
	cucu=function(i) sum(ww[1:i])
	wwc=lapply(1:length(ww),cucu)
	# what models are to be used
	whom=c()
	if (length(select)>1)
		whom=select
	else if (select=="all")
		whom=1:length(object@crits)
	else if (is.integer(select))
		whom = 1:select
	else if (is.numeric(select)) {
		if (select <=1) 
			whom = which(wwc<=select)
		else 
			whom = which(object@crits <= (object@crits[1]+select))
	}
	mods = object@crits[whom]
	formo = object@formulas[whom]
	hasobj = try(object@objects,TRUE)
	if (!inherits(hasobj,"try-error") &&length(object@objects) > 0) {
		# model objects included: no need to refit
		coffee = object@objects[whom]
	} else {
		# refit models
		coffee=list()
		for (i in formo) {
			ff=object@params$fitfunction
			cak=as.call(list(substitute(match.fun(ff)), formula=i, data=object@call$data))
			if (length(object@adi)>=1)
				for (j in 1:length(object@adi)) {
					cak[[length(names(cak))+1]] = object@adi[[j]]
					names(cak)[length(names(cak))] = names(object@adi)[j]
				}
			modf=eval(cak)
			coffee=c(coffee,list(modf))
		}
	}
	
	if (length(coffee)==1) {
		# only one model ! Do conditional inference for continuity
		warning("Only one candidate: standard conditional prediction was performed.")
		return(predict(coffee[[1]],se.fit=se.fit,...))
	}
	
	waou=ww[whom]/sum(ww[whom])
	# make predictions
	predicts=list()
	for (i in 1:length(formo)) {
		if(!is.data.frame(newdata))	{
			arlette = predict(coffee[[i]],se.fit=se.fit, ...)
			predicts = c(predicts, list(arlette))
		} else {
			arlette = predict(coffee[[i]], newdata=newdata, se.fit=se.fit, ...)
			predicts = c(predicts, list(arlette))
		}
	}
	# handle average predictions
	if (se.fit) { 
		preds=lapply(predicts,function(x) x[[1]])
	}  else preds = predicts
	nbpo = length(preds[[1]])
	all = t(matrix(unlist(preds), nrow=nbpo))
	dimnames(all)=list(c(), names(preds[[1]]) )
	# handle NA values
	nana = lapply(preds, is.na)
	nbok = numeric(nbpo)
	for (i in 1:length(preds)) {
		nbok = nbok + nana[[i]]
		preds[[i]][is.na(preds[[i]])] = 0
	}
	minou = matrix(waou%*%t(matrix(unlist(preds),nrow=nbpo)), dimnames=list(names(preds[[1]]),c() )) 
	
	# handle variances if appropriate
	mvar=NULL
	if (se.fit) {
		waouv=waou
		if (nbpo>1) for (i in 2:nbpo) waouv=rbind(waouv,waou) 
		waouv= matrix(t(waouv),nrow=length(whom),ncol=nbpo)
		predse=lapply(predicts,function(x) x[[2]])
		allse = t(matrix(unlist(predse), nrow=nbpo))
		dimnames(allse)=list(c(), names(predse[[1]]) )
		# handle NA values
		nana = lapply(predse, is.na)
		nbok2 = numeric(nbpo)
		for (i in 1:length(predse)) {
			nbok2 = nbok2 + nana[[i]]
			predse[[i]][is.na(predse[[i]])] = 0
		}
		# get degrees of freedom
		modelsdf = unlist(lapply(coffee,function(x) max(getfit(x)[,3])))
		
		# compute variance components
		if (varweighting=="Buckland") {
			squaredevs = ((all-t(matrix(rep(minou,length(whom)), nbpo, length(whom)))))^2
			condivars = allse^2
			avervar = matrix(((waou)%*%(sqrt(squaredevs+condivars)))^2, ncol=1, dimnames=list( names(predse[[1]]), c("Uncond. variance")))
		} else if (varweighting=="Johnson") {
			squaredevs = waou%*%(((all-t(matrix(rep(minou,length(whom)), nbpo, length(whom))))^2))
			condivars =  waou%*%((allse^2))
			avervar = matrix(condivars+squaredevs, ncol=1, dimnames=list(  names(predse[[1]]), c("Uncond. variance")))
		}	
		# move on to confidence intervals
		if (icmethod=="Burnham") {
			# uses Burnham & Anderson (2002) suggestion
			stuvals = (as.numeric(lapply(modelsdf,  function(x) qt(1-alphaIC/2, x)))/qnorm(1-alphaIC/2))^2
			adjsem= (matrix(rep(stuvals, nbpo), nrow=length(whom)))*allse^2
			adjsem = adjsem + (all-t(matrix(rep(minou, length(whom)), ncol=length(whom))))^2
			adjse = qnorm(1-alphaIC/2)*(waou%*%sqrt(adjsem))
			uncondIC = matrix(adjse, ncol=1,  dimnames=list( names(predse[[1]]), c(paste("+/- (alpha=", alphaIC, ")",sep=""))))
		} else if (icmethod=="Lukacs") {
			# uses Lukacs et al. (2010) student-like method
			# get degrees of freedom for each model
			averddf = sum(waou*modelsdf)
			uncondIC = matrix(sqrt(avervar)*qt(1-alphaIC/2,averddf), ncol=1,  dimnames=list( names(predse[[1]]), c(paste("+/- (alpha=", alphaIC, ")",sep=""))))
		} else {
			# uses standard gaussian interval 
			uncondIC = matrix(sqrt(avervar)*qnorm(1-alphaIC/2), ncol=1,  dimnames=list( names(predse[[1]]), c(paste("+/- (alpha=", alphaIC, ")",sep=""))))
		}
		mvar = cbind(avervar, uncondIC)
		
	}
	
	list(averages = t(minou), variability = mvar, omittedNA = sum(nbok))
	
}

# accessing fitted models for coefficients
setGeneric("getfit", function(object, ...) standardGeneric("getfit"))

setMethod("getfit","ANY", function(object, ...)
{
	summ = summary(object)
	summ1 = summ$coefficients
	didi=dimnames(summ1)
	if (is.null(didi[[1]])) {
		summ1 = matrix(rep(0,2), nrow=1, ncol=2, dimnames=list(c("NULLOS"),list("Estimate","Std. Error")))
		return(cbind(summ1, data.frame(df=c(0))))
	}
	summ1=summ1[,1:2]
	if (length(dim(summ1))==0) {
		didi = dimnames(summ$coefficients)
		summ1=matrix(summ1, nrow=1, ncol=2, dimnames=list(didi[[1]],didi[[2]][1:2]))
	}
	return(cbind(summ1, data.frame(df=rep(summ$df[2], length(summ$coefficients[,1])))))
	
})
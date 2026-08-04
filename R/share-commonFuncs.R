doList <- function(func,tryFunc, x, ...){
    options <- completeOptions(...)
    ## We do not create a new list unless
    ## there exists any sharable elements
    for(i in seq_along(x)){
        Elt <- func(x[[i]],...)
        if(!C_isSameObject(x[[i]],Elt)){
            x[[i]] <- Elt
        }
    }
    if(options$sharedAttributes){
        ## share attributes
        oldAttrs <- attributes(x)
        if(!is.null(oldAttrs)){
            ## Exclude names and class attributes.
            ## If attributes(x) contains the names attribute,
            ## attributes(attributes(x)) still contains the names attributes
            ## and will cause an infinit loop. We must ignore it.
            ## The later one is not worthy to share
            if(!all(names(oldAttrs) %in% c("class","names"))){
                attrs <- doAttributes(tryFunc, oldAttrs, ...)
                ## set the attributes only when it is different from the old one
                if(!C_isSameObject(oldAttrs,attrs)){
                    attributes(x) <- attrs
                }
            }
        }
    }
    x
}
doS4 <- function(func, x, ..., setS4InFunc = FALSE){
    ## Vector- and list-backed S4 objects store their inherited .Data in the
    ## underlying vector/list. Temporarily clear the S4 flag so it can be
    ## processed by the usual atomic/list dispatch.
    if(isSharableAtomic(x)||isSEXPList(x)){
        data <- C_setS4(x, FALSE)
        on.exit(C_setS4(data, TRUE), add = TRUE)
        if(isS4(data))
            stop("Unexpected error, cannot convert an S4 object to a non-S4 object")
        if(setS4InFunc && isSharableAtomic(data)){
            ## Convert a newly created ALTREP before it becomes referenced;
            ## otherwise Rf_asS4() must duplicate and may materialize it.
            result <- func(data,...,.setS4 = TRUE)
        }else{
            result <- func(data,...)
            result <- C_setS4(result, TRUE)
        }
    }else{
        ## Ordinary S4 objects are S4SXP containers, so process their slots.
        result <- x
        slots <- slotNames(result)
        for(i in slots){
            slot(result, i, check = FALSE) <- func(slot(x, i), ...)
        }
    }
    validObject(result)
    result
}

doEnvironment <- function(func,tryFunc, x, ...){
    options <- completeOptions(...)
    for(i in names(x)){
        x[[i]] <- func(x[[i]],...)
    }
    if(options$sharedAttributes){
        attributes(x) <- tryFunc(attributes(x),...)
    }
    x
}



doAttributes<-function(tryFunc, attrs, ...){
    attrNames <- names(attrs)
    idx <- which(!attrNames%in%c("class","names","levels"))
    for(i in idx){
        newAttri <- tryFunc(attrs[[i]], ...)
        if(!C_isSameObject(newAttri,attrs[[i]])){
            attrs[[i]] <- newAttri
        }
    }
    attrs
}

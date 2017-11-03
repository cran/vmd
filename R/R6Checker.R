#' R6 Class of Standard Checking Utilities
#'
#' Create a new R6 Class that contains public methods of standard checking utilities
#' @rdname checker
#' @export
checker = function(){
  R6Checker$new()
}

#' @rdname checker
#' @usage NULL
#' @export
R6Checker = R6::R6Class('Checker',inherit=NULL,
    public=list(
      initialize = function(){
      },
      #Check Has Names
      checkHasNames = function(x,objnm = deparse(substitute(x))){
        `if`(!is.character(names(x)), stop(sprintf("'%s' of type ['%s'] must be a named object",objnm,paste(class(x),collapse="', '")),call.=FALSE),invisible(x))
      },

      #Check All Names from A are in B
      checkAllNamesAreIn = function(A,B,objnmA = deparse(substitute(A)), objnmB = deparse(substitute(B))){
        self$checkHasNames(A,objnmA)
        self$checkHasNames(B,objnmB)
        missing = setdiff(names(A),names(B))
        `if`(length(missing) > 0,{
          stop(sprintf("All names in '%s' must also be in '%s'. Missing: ['%s']",
                       objnmA,
                       objnmB,
                       paste(missing,collapse=", ")),
               call.=FALSE)
        },invisible(A))
      },

      checkAllNamesAreUnique = function(x,objnm = deparse(substitute(A))){
        self$checkHasNames(x,objnm)
        `if`(!identical(length(names(x)),length(unique(names(x)))), stop(sprintf("All names in '%s' must be unique.",objnm),call.=FALSE), invisible(x))
      },

      #Throws error if not numeric scalar (length 1) that is positive
      checkPositiveNumericScalar = function(x, includeZero = FALSE, objnm = deparse(substitute(x))){
        self$checkNumericScalar(x,objnm = objnm)
        self$checkRange(x, 0, aop = `if`(includeZero,'>=','>'), objnm = objnm)
      },


      #Throws error if not integer scalar (length 1) that is positive
      checkPositiveIntegerScalar = function(x,includeZero = FALSE, objnm = deparse(substitute(x))){
        self$checkLength(x,1,objnm = objnm)
        self$checkInteger(x,objnm=objnm)
        self$checkRange(x, 0, aop = `if`(includeZero,'>=','>'), objnm = objnm)
      },


      #Throws error i not numeric scalar (length 1)
      checkNumericScalar = function(x, objnm = deparse(substitute(x))){
        self$checkLength(x,1,objnm = objnm)
        self$checkNumeric(x,objnm = objnm)
      },


      #Throws error if not logical scalar (length 1)
      checkLogicalScalar = function(x,objnm = deparse(substitute(x))){
        self$checkLength(x,1,objnm=objnm)
        self$checkLogical(x, objnm = objnm)
      },


      #Throws error if not character scalar (length 1)
      checkCharacterScalar = function(x,objnm = deparse(substitute(x))){
        self$checkLength(x,1,objnm = objnm)
        self$checkCharacter(x, objnm = objnm)
      },


      #Throws error if not scalar (length 1) or a specific type
      checkScalarOfType = function(x, type = enfArg('type'), objnm = deparse(substitute(x))){
        self$checkLength(x,1,objnm=objnm)
        self$checkClass(x,type,objnm=objnm)
      },

      #Throws error if not in range [0,1]
      checkFractional = function(x, objnm = deparse(substitute(x))){
        self$checkNumeric(x, objnm = objnm)
        self$checkRange(x,0,1,aop='>=',bop="<=", objnm = objnm)
      },

      #Throws error if not in range [0,1] and scalar (length 1)
      checkFractionalScalar = function(x, objnm){
        self$checkScalar(x, objnm = objnm)
        self$checkFractional(x, objnm = objnm)
      },

      #Throws error if x is not numeric
      checkNumeric = function(x,objnm = deparse(substitute(x))){
        `if`(!all(is.numeric(x)), stop(sprintf("'%s' must be numeric",objnm),call.=FALSE), invisible(x))
      },


      #Throws error if x is not integer
      checkInteger = function(x, objnm = deparse(substitute(x))){
        `if`(!all(is.integer(x)) & !(all(is.numeric(x)) & all(x %% 1 == 0)),
             stop(sprintf("'%s' must be integer",objnm),call.=FALSE), invisible(x))
      },

      #Throws Error if Not Integer and Not scalar
      checkIntegerScalar = function(x, objnm = deparse(substitute(x))){
        self$checkInteger(x,objnm = objnm)
        self$checkScalar(x,objnm = objnm)
      },

      #Throws error if x is not character
      checkCharacter = function(x, objnm = deparse(substitute(x))){
        `if`(!is.character(x), stop(sprintf("'%s' must be character",objnm),call.=FALSE), invisible(x))
      },


      #Throws error if x is not logical
      checkLogical = function(x, objnm = deparse(substitute(x))){
        `if`(!is.logical(x), stop(sprintf("'%s' must be logical",objnm),call.=FALSE), invisible(x))
      },


      #Throws error if x does not inherit type
      checkClass = function(x,type = enfArg('type'), objnm = deparse(substitute(x))){
        `if`(!inherits(x,type), stop(sprintf("'%s' must inherit one of ['%s']",objnm,paste(type,collapse="', '")),call.=FALSE), invisible(x))
      },

      #Throws error if x is not data.frame
      checkDataFrame = function(x,objnm = deparse(substitute(x))){
        self$checkClass(x,'data.frame',objnm = objnm)
      },

      #Check list, with option to conduct check recursively, in the event that the list may be a list of lists
      checkListOfClass = function(x, type = enfArg('type'), recursive = FALSE, objnm = deparse(substitute(x))){
        self$checkCharacterScalar(type, objnm = 'type')
        self$checkClass(x,'list', objnm)
        check = sapply( `if`(recursive, flatten(x), x) , function(o){ inherits(o,type) })
        `if`(!all(check), stop(sprintf("All '%s' objects must inherit from '%s'",objnm,type),call.=FALSE), invisible(x))
      },

      #Check Data Frame is Numeric
      checkNumericDataFrame = function(x,objnm = deparse(substitute(x))){
        self$checkDataFrame(x,objnm = objnm)
        `if`(!all(sapply(as.list(x),function(o)is.numeric(o))), stop(sprintf("All columns of '%s' must be numeric.",objnm),call.=FALSE), invisible(x))
      },

      #Check Length of x
      checkLength = function(x,n = 1,op='==',objnm = deparse(substitute(x))){
        self$checkIsIn(op,c('==','>','<','>=','<='))
        n = as.integer(unique(n))
        `if`(!{any(do.call(op,args=list(length(x),n)))},
             stop(sprintf("'%s' must be of length/s '%s' ['%s']",objnm,op,paste(n,collapse="', '")),call.=FALSE),
             invisible(x))
      },


      #Check if x is a scalar (length 1)
      checkScalar = function(x, objnm = deparse(substitute(x))){
        self$checkLength(x,1,objnm = objnm)
      },


      #Check if x is not null or is not NA
      checkNotNullOrNA = function(x,objnm = deparse(substitute(x))){
        `if`(is.null(x) || is.na(x),stop(sprintf("'%s' must not be NULL or NA",objnm),call.=FALSE),invisible(x))
      },

      #Check if x is non zero length
      checkNonZeroLength = function(x,objnm = deparse(substitute(x))){
        `if`(!length(x), stop(sprintf("'%s' must not be zero length",objnm),call.=FALSE), invisible(x))
      },

      #Check if x is in valid
      checkIsIn = function(x,valid = enfArg('valid'), objnm = deparse(substitute(x)),objnmB = deparse(substitute(valid))){
        self$checkNonZeroLength(valid,objnm = objnmB)
        self$checkNotNullOrNA(valid,objnm = objnm)
        `if`(!all(x %in% valid),
             stop(sprintf("All '%s' (%s) must be in: ['%s'], missing: '%s'",
                          objnm,
                          paste(x,collapse="', '"),
                          paste(valid,collapse="', '"),
                          paste(setdiff(x,valid),collapse="', '")
             ),call.=FALSE),
             invisible(x))
      },

      #Check if any x
      checkAny = function(x,objnm = deparse(substitute(x))){
        self$checkLogical(x,objnm=objnm)
        `if`(!any(x), stop(sprintf("At least one of '%s' must be TRUE.",objnm),call.=FALSE), invisible(x))
      },

      #Check if x is in the range governed by a and b, and aop (operator) and bop (operator)
      checkRange = function(x, a = -Inf, b = Inf, aop = '>=', bop = '<=', objnm = deparse(substitute(x))){

        #Check x, a and b are numeric
        self$checkNumeric(x,objnm = objnm)
        self$checkNumeric(a,objnm = sprintf("%s (lower bound)",objnm))
        self$checkNumeric(b,objnm = sprintf("%s (upper bound)",objnm))

        #Valid Operators
        vop   = c('>','>=','<','<=','==')

        #aop must be character scalar and a valid operator
        self$checkScalarOfType(aop,'character', objnm = objnm)
        self$checkIsIn(aop,vop, objnm = objmm)

        #bop must be character scalar and a valid operator
        self$checkScalarOfType(bop,'character', objnm = objnm)
        self$checkIsIn(bop,vop, objnm = objmm)

        a = `if`(is.na(a),-Inf,a)
        b = `if`(is.na(b),+Inf,b)
        `if`(!all(do.call(aop,args=list(x,a)) & do.call(bop,args=list(x,b))),
             stop(sprintf("'%s' must be in the range ['%s' %s %s] AND ['%s' %s %s]",
                          objnm,
                          objnm,aop,a,
                          objnm,bop,b),call.=FALSE),
             invisible(x))
      },


      #Check if x is positive
      checkPositive = function(x, objnm = deparse(substitute(x))){
        self$checkRange(x, a = 0, aop='>', objnm = objnm)
      },

      #Check if x is negative
      checkNegative = function(x, objnm = deparse(substitute(x))){
        self$checkRange(x, a = 0, aop="<", objnm = objnm)
      }
    ),
    private=list(

    )
)

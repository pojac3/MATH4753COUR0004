#' MATH4753COUR0004::myRandom()
#'
#' @param n The size of the sample
#' @param iter How many times to choose the sample
#' @param time Time in between displaying each graph. May be useful with an animation
#'
#' @return - Generates new bar graphs every time seconds
#'
#' @examples
#'
#' myRandom(1000,30,1)
#' The above example generates a new bar graph every second for 30 seconds based on the data gathered from 1000 iterations
myRandom=function(n, iter=10,time=0.5){
  for( i in 1:iter){
    #make a sample
    s=sample(1:10,n,replace=TRUE)
    # turn the sample into a factor
    sf=factor(s,levels=1:10)
    #make a barplot
    barplot(table(sf)/n,beside=TRUE,col=rainbow(10),
            main=paste("Example sample()", " iteration ", i, " n= ", n,sep="") ,
            ylim=c(0,0.2))

    #release the table
    Sys.sleep(time)
  }
}

dui155 <- function(y){
    T <- nrow(y)
    reg <- lm(y$TARGET[2:T]~y$IMBFM[1:T-1]+y$HPDYO[1:T-1]+y$XUPP[1:T-1]+y$RBEQZ[1:T-1]
              +y$UKSKO[1:T-1]+y$ZZICN[1:T-1]+y$OIRTT[1:T-1]+y$EGTZY[1:T-1])
    
    puigdemont <- tail(predict(reg),1)
    
    return( puigdemont )
}

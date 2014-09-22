package recfun

object hw01 {
  def pascal(c: Int, r: Int): Int = {
    
    def loop(row_acc:Int,col_acc:Int):Int = {
      if(row_acc < r & col_acc < c){
        loop(row_acc + 1, col_acc + 1)
      }else{
        row_acc + col_acc
      }
    }
    
    
    if((r == 0 ) | (c == 0)) 1
    else if( r == c) 1
    else loop(0,0)
    
  }                                               //> pascal: (c: Int, r: Int)Int
  
  pascal(1,3)                                     //> res0: Int = 2
  
}
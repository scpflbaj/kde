K3_Zeros <- function(A,B,C) {
  solutions<-c()
  if (is.numeric(A) & is.numeric(B) & is.numeric(C)) {
	  if ( A == 0.0 ) {
  		if ( B != 0.0 ) {
	  	  solutions[1] = -C/B
		    solutions[2] = solutions[1]
		    #check1=solutions[1]*solutions[1]*A+solutions[1]*B+C
		    #check2=solutions[2]*solutions[2]*A+solutions[2]*B+C
		    #print(paste("A:",toString(A)," B:",toString(B)," C:",toString(C)," Sol 1>>",toString(check1)," Sol2 >>",toString(check2)))
		  }
	  } else {
  		expr1 = B*B
  		expr2 = 4*A*C
  		if ( expr1 >= expr2 && A!=0.0 ) {
  		  rad = expr1-expr2
  		  sqroot = sqrt(rad)
  		  numerator1 = -B + sqroot
  		  numerator2 = -B - sqroot
  		  denominator <- 2*A
  		  solutions[1] = numerator1/denominator
  		  solutions[2] = numerator2/denominator
  		  #check1=solutions[1]*solutions[1]*A+solutions[1]*B+C
  		  #check2=solutions[2]*solutions[2]*A+solutions[2]*B+C
  		  #print(paste("A:",toString(A)," B:",toString(B)," C:",toString(C)," Sol 1>>",toString(check1)," Sol2 >>",toString(check2)))
  		}
	  }
  } else {
  	print("Invalid data detected")
  }
  return(sort(solutions))
}
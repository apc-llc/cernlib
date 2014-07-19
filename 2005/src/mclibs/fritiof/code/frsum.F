*
* $Id: frsum.F,v 1.1.1.1 1996/01/11 14:05:20 mclareni Exp $
*
* $Log: frsum.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:20  mclareni
* Fritiof
*
*
C................................................
 
      FUNCTION FRSUM(X)
 
C...Summation FRSUM=SUM (x^n/n*n!), used for integrating eikonal overlap func.
 
      FRSUM = 0.
      IF(X.EQ.0.) RETURN
 
      I = 0
      TERM = 1.
10    I = I+1
      TERM = TERM * (X) * MAX(1,I-1)/FLOAT(I**2)
      FRSUM = FRSUM + TERM
      IF(ABS(TERM).LT.MIN(1.E-6,1.E-6*ABS(X))) GOTO 100
 
      CALL FRLOOPU(*10,I,2000,'LOPFRSUM')
 
100   CONTINUE
 
      RETURN
      END

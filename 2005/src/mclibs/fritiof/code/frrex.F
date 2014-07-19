*
* $Id: frrex.F,v 1.1.1.1 1996/01/11 14:05:22 mclareni Exp $
*
* $Log: frrex.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:22  mclareni
* Fritiof
*
*
C..........................................................
 
      FUNCTION FRREX(X)
 
C.......To take care the over_under_flow problem in large exponentials
 
      ARG = MIN(ABS(X), 80.0)
      IF(X.LT.0.) ARG = -ARG
 
      FRREX = EXP(ARG)
 
      RETURN
      END

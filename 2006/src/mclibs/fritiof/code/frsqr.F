*
* $Id: frsqr.F,v 1.1.1.1 1996/01/11 14:05:21 mclareni Exp $
*
* $Log: frsqr.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:21  mclareni
* Fritiof
*
*
C********************************* END FRDOICT ***************************
 
C******************************** AUXILLIARY ROUTINES *******************
 
      REAL FUNCTION FRSQR( X, MESSAGE )
 
C....Optional character 'MESSAGE' helps to identify the source of error.
C....Allow a little numerical error margin...
 
      CHARACTER*(*) MESSAGE
      IFLAG = 0
      IF(X.LT.-0.001) THEN
      IFLAG = 1
      WRITE(6,*) X, ' --SQRT-NEGATIVE VALUE '
      write(6,100) MESSAGE
100   FORMAT( A )
      STOP 'FRSQR: `NEG-ROOT'''
      ENDIF
      FRSQR = SQRT(MAX(X,0.))
      RETURN
      END

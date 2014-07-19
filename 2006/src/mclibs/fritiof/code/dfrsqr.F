*
* $Id: dfrsqr.F,v 1.1.1.1 1996/01/11 14:05:22 mclareni Exp $
*
* $Log: dfrsqr.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:22  mclareni
* Fritiof
*
*
      DOUBLE PRECISION FUNCTION DFRSQR( DX, MESSAGE )
      IMPLICIT DOUBLE PRECISION (D)
      CHARACTER*(*) MESSAGE
      IFLAG = 0
      IF(DX.LT.-0.001D0) THEN
      IFLAG = 1
      WRITE(6,*) DX, ' --SQRT-NEGATIVE VALUE '
      write(6,100) MESSAGE
100   FORMAT( A )
      STOP 'DFRSQR: `NEG-D_ROOT'''
      ENDIF
      DFRSQR = DSQRT(DMAX1(DX,0.D0))
      RETURN
      END

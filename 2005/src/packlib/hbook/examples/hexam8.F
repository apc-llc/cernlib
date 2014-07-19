*
* $Id: hexam8.F,v 1.2 1996/06/06 09:52:46 cernlib Exp $
*
* $Log: hexam8.F,v $
* Revision 1.2  1996/06/06 09:52:46  cernlib
* Remove #ifdef (CERNLIB_EXAMPLES) and pilot.h where not used
*
* Revision 1.1.1.1  1996/01/16 17:07:50  mclareni
* First import
*
*
*CMZ :  4.17/02 18/12/92  12.39.38  by  Unknown
*-- Author :    John Allison   17/12/92
      SUBROUTINE HEXAM8
* Example of multiquadric smoothing, including a "simple" ntuple.
      DIMENSION X(3)
      CHARACTER*8 CHTAGS(3)
      DATA CHTAGS/'   X   ','   Y   ','   Z   '/
      DATA LOUT/31/
*
      CALL HBOOK1(10,'TEST1',100,-3.,3.,0.)
      CALL HBOOK2(20,'TEST2',20,-3.,3.,20,-3.,3.,250.)
      CALL HBOOKN(30,'N-TUPLE',3,' ',1000,CHTAGS)
*
      DO 10 I=1,1000
         CALL RANNOR(A,B)
         X(1)=A
         X(2)=B
         X(3)=A*A+B*B
         CALL HFN(30,X)
  10  CONTINUE
*
      CALL HPROJ1(10,30,0,0,1,999999,1)
      CALL HQUAD(10,'V',0,0.,0.,NPAR,CHI2,NDF,FMIN,FMAX,IERR)
      IF(IERR.NE.0)THEN
         WRITE (LOUT, 10000) IERR
      ELSE
         WRITE (LOUT, 10100) NPAR, CHI2, NDF
         WRITE (LOUT, 10200) FMIN, FMAX
      END IF
      CALL HPROJ2(20,30,0,0,1,999999,1,2)
      CALL HQUAD(20,'V',0,0.,0.,NPAR,CHI2,NDF,FMIN,FMAX,IERR)
      IF(IERR.NE.0)THEN
         WRITE (LOUT, 10000) IERR
      ELSE
         WRITE (LOUT, 10100) NPAR, CHI2, NDF
         WRITE (LOUT, 10200) FMIN, FMAX
      END IF
      IERR=0
**      CALL HQUAD(30,'V',0,0.,0.,NPAR,CHI2,NDF,FMIN,FMAX,IERR)
**      IF(IERR.NE.0)THEN
**         WRITE (LOUT, 10000) IERR
**      ELSE
**         WRITE (LOUT, 10100) NPAR, CHI2, NDF
**         WRITE (LOUT, 10200) FMIN, FMAX
**      END IF
*
10000 FORMAT (1X, 'Error', I5, ' in multiquadric smoothing.')
10100 FORMAT (1X, 'Multiquadric smoothing with', I4,  ' parameters.'/
     +3X, 'Chi-squared', G12.5, ' for', I7, ' degrees of freedom.')
10200 FORMAT (3X, 'Min/max event density:', 2G12.5)
*
      END

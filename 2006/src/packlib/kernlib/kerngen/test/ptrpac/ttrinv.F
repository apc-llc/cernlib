*
* $Id: ttrinv.F,v 1.1.1.1 1996/02/15 17:55:02 mclareni Exp $
*
* $Log: ttrinv.F,v $
* Revision 1.1.1.1  1996/02/15 17:55:02  mclareni
* Kernlib
*
*
#include "kerngent/pilot.h"
      SUBROUTINE TTRINV

#include "kerngent/mkcde.inc"
      DIMENSION AC(10), CC(10), QC(10), RC(10)
      INTEGER TINF(4), TINFS2, TINFS4
      DATA AC/1.,0.,0.,2.,0.,13.,4.,0.,23.,77./,
     1     CC/0.2,-8.,4.,0.,0.,0.,4.2,-2.,0.,1./,
     2     QC/5.,10.,0.25,0.,0.,0.,-1.,0.5,0.,1./,
     3     RC/1.45679,0.,0.,-0.191358,0.,0.1882716,-0.0185185,
     4             0., -0.0462963, 0.02777778 /
      DATA TINF/1000,4HINV ,1,4H4*4 /
      DATA TINFS2/4HSINV/, TINFS4/4H4*4 /
C-----------------------------------------------------------------------

      CALL NEWGUY ('TRINV -TRSINV.','TTRINV  ')
      ZERLEV = ZEROV(2)

      CALL TRINV  (CC,A,4)
      CALL MVERIF (11,A,QC,10)

      CALL UCOPY (CC,A,10)
      CALL TRINV  (A ,A,4)
      CALL MVERIF (12,A,QC,10)

      ZERLEV = ZEROV(4)
      CALL TRSINV (AC,A,4)
      CALL MVERIF (21,A,RC,10)

      CALL UCOPY (AC,A,10)
      CALL TRSINV (A ,A,4)
      CALL MVERIF (22,A,RC,10)
C - - - - - - - - - - - - - - - - - - - -- -  - -- - - - - - - - - - - -

      IF (ITIMES .EQ. 0)   RETURN
      CALL TIMED (TIMERD)
      DO 50  J=1,1000
   50 CALL TRINV  (CC,A,4)
      CALL TIMING (TINF)

      TINF(2) = TINFS2
      TINF(4) = TINFS4
      CALL TIMED (TIMERD)
      DO 100 J=1,1000
  100 CALL TRSINV (AC,A,4)
      CALL TIMING (TINF)

      RETURN
      END

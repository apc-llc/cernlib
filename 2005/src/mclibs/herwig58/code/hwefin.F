*
* $Id: hwefin.F,v 1.1.1.1 1996/03/08 17:02:11 mclareni Exp $
*
* $Log: hwefin.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:11  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.46  by  Unknown
*-- Author :
CDECK  ID>, HWEFIN.
*CMZ :-        -26/04/91  11.11.55  by  Bryan Webber
*-- Author :    Bryan Webber
C------------------------------------------------------------------------
      SUBROUTINE HWEFIN
C     TERMINAL CALCULATIONS ON ELEMENTARY PROCESS
C------------------------------------------------------------------------
#include "herwig58/herwig58.inc"
      DOUBLE PRECISION RNWGT,SPWGT,ERWGT
      WRITE (6,1)
    1 FORMAT(/10X,'OUTPUT ON ELEMENTARY PROCESS'/)
      IF (NWGTS.EQ.0) THEN
        WRITE (6,10)
   10   FORMAT(10X,'NO WEIGHTS GENERATED')
      ELSE
        RNWGT=1./FLOAT(NWGTS)
        AVWGT=WGTSUM*RNWGT
        SPWGT=SQRT(MAX(WSQSUM*RNWGT-AVWGT**2,ZERO))
        ERWGT=SPWGT*SQRT(RNWGT)
        IF (.NOT.NOWGT) WGTMAX=AVWGT
        IF (WGTMAX.EQ.ZERO) WGTMAX=ONE
        WRITE (6,11) NEVHEP,NWGTS,AVWGT,SPWGT,WBIGST,WGTMAX,IPROC,
     &               1000.*AVWGT,1000.*ERWGT,100.*AVWGT/WGTMAX
   11   FORMAT(1P,
     &         10X,'NUMBER OF EVENTS   = ',I11/
     &         10X,'NUMBER OF WEIGHTS  = ',I11/
     &         10X,'MEAN VALUE OF WGT  =',E12.4/
     &         10X,'RMS SPREAD IN WGT  =',E12.4/
     &         10X,'ACTUAL MAX WEIGHT  =',E12.4/
     &         10X,'ASSUMED MAX WEIGHT =',E12.4//
     &         10X,'PROCESS CODE IPROC = ',I11/
     &         10X,'CROSS SECTION (PB) =',G12.4/
     &         10X,'ERROR IN C-S  (PB) =',G12.4/
     &         10X,'EFFICIENCY PERCENT =',G12.4)
      ENDIF
      END

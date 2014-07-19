C-                                                                 -
C-  INPUT:                                                         -
C-  PRUNIT= unit number for printout                               -
C-  LISAFI,NISAF,CFL and IFL ignored                               -
C-                                                                 -
C-     SDP  Dec,1986                                               -
C-                                                                 -
C-------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
#include "isajet/izisaf.inc"
#include "isajet/zebcom.inc"
      INTEGER PRUNIT,LISAFI,LISAF
      INTEGER NISAF,IFL,ENDR
      CHARACTER CFL*(*)
C
      LISAF=LISAE
      IF(LISAF.GT.0) THEN   ! check bank exists
        CALL UHTOC('ISAF',ENDR,4,4)
        IF(IQ(LISAF-4).NE.ENDR) RETURN  ! not end-of-run record
C
C          Print titles
C
          WRITE(PRUNIT,100)
C
C   Print contents of bank
C
         WRITE(PRUNIT,101) IQ(LISAF+1),IQ(LISAF+2)
         WRITE(PRUNIT,102) Q(LISAF+3),Q(LISAF+4)
C
      ENDIF
      RETURN
  100 FORMAT('0',//,1X,80('-'),/,' ISAJET END RUN BANK (ISAF)',/)
  101 FORMAT(/,'  no. of events for this run=',I8,/,
     $         '  no. of events requested   =',I8)
  102 FORMAT(/,'  cross section (microbarns)=',E15.4,/,
     $         '       final weight         =',E15.4)
      END

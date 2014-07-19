C-
C-  INPUT:
C-  PRUNIT= unit number for printout
C-  LISABI,NISAB,CFL and IFL ignored
C-
C-     SDP  Jan,1986
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
#include "isajet/zebcom.inc"
      INTEGER PRUNIT,LISABI
      INTEGER NISAB,IFL,NREAC,K,BEGR
      CHARACTER CFL*(*)
      CHARACTER*8 REACTN(10)
      DATA REACTN/'(TWOJET)','(E+E-)','(DRLYAN)',
     1            '(MBIAS)','(SUSY)','(WPAIR)','(HIGGS)',
     1            ' ',' ','(GEANT)'/
C
      IF(LISAB.GT.0) THEN  ! check bank exists
        CALL UHTOC('ISAB',BEGR,4,4)
        IF(IQ(LISAB-4).NE.BEGR) RETURN  ! not begin-of-run record
C
C          Print titles
C
          WRITE(PRUNIT,100)
C
C   Print contents of bank
C
         WRITE(PRUNIT,101) IQ(LISAB+1)
         NREAC=IQ(LISAB+2)
         WRITE(PRUNIT,102) NREAC,REACTN(NREAC)
         WRITE(PRUNIT,103) IQ(LISAB+3),Q(LISAB+4)
         WRITE(PRUNIT,104) (Q(LISAB+K),K=5,22)
C
      ENDIF
      RETURN
  100 FORMAT('0',//,1X,80('-'),/,' ISAJET BEG. RUN BANK (ISAB)',/)
  101 FORMAT(/' Isajet version no.='I8)
  102 FORMAT(/' Reaction=',I3,2X,A8)
  103 FORMAT(/' NO. of events generated='I10,3X,' CM ENERGY='F12.3)
  104 FORMAT(/' PT  limits='6F15.3,/,' TH  limits='6F15.3,/,
     $ ' PHI limits='6F15.3)
      END

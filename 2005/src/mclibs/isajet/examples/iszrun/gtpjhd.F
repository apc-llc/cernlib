C-   Purpose and Methods :
C-      Fetch information from next bank in PJHD linear structure
C-
C-   Inputs  :
C-     LSUP= supporting link, should be 0 to get information
C-           from first bank in linear structure,
C-           and LPJHD from preceding bank otherwise.
C-   Outputs :
C-     LPJHD = structural link to PJHD providing information
C-     NALG  = PJET ALGORITHM NUMBER 1=DR CONE : 2 = CMS OPENING ANGLE
C-     NPJET = PJET NUMBER OF JETS
C-     DRCUT = PJET DR CUT OR OPEN_ANGLE_CUT
C-     ETCUT = PJET JET ET CUT
C-     MAXIT = PJET MAXIMUM NUMBER OF ITERATIONS
C-     IR    = PJET INITIAL RADIATION TAG - 0 USE IR - 1 NO IR
C-     MUON  = PJET MUON  TAG - 0 IF NO MUONS - 1 IF USE MUONS
C-
C-   Created 14-NOV-1989   Chip Stewart
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
#include "isajet/zebcom.inc"
#include "isajet/izpjhd.inc"
      INTEGER LSUP,LPJHD,LQPJHD
      REAL    DRCUT,ETCUT
      INTEGER NALG,NPJET,MAXIT,IR,MUON
C----------------------------------------------------------------------
      IF ( LSUP.EQ.0 ) LSUP= LISAE-IZPJHD
      LQPJHD=LQ(LSUP)
      IF(LQPJHD.NE.0) THEN
        NALG = IQ(LQPJHD+2)
        NPJET = IQ(LQPJHD+3)
        DRCUT =  Q(LQPJHD+4)
        ETCUT =  Q(LQPJHD+5)
        MAXIT = IQ(LQPJHD+6)
        IR    = IQ(LQPJHD+7)
        IF (IQ(LQPJHD+1).GT.1) THEN
          MUON  = IQ(LQPJHD+8)
        ELSE
          MUON = 0
        END IF
      ENDIF
      LPJHD=LQPJHD
  999 RETURN
      END

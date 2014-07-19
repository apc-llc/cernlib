*
* $Id: dzdspi.F,v 1.1.1.1 1996/03/04 16:13:02 mclareni Exp $
*
* $Log: dzdspi.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:02  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDSPI
#include "dzdzbrinc.inc"
#include "zebra/zebq.inc"
#include "zebra/mzca.inc"
#include "zebra/mzcb.inc"
#include "zebra/mzcc.inc"
#include "zebra/mzcn.inc"
#include "zebra/mzbits.inc"
      INTEGER IBANK, IBANK1, IIST,NPAR
      CHARACTER*80 CMD
      CHARACTER*(*) CMD1
*--
      CALL KUGETI(IBANK)
      IF(IBANK.EQ.0)GOTO 999
      CALL KUPATL(CMD,NPAR)
      GOTO 10
 
      ENTRY DZDSP1(IBANK1,CMD1)
      CMD=CMD1
      IBANK=IBANK1
10    CONTINUE
      CALL DZDIBR
      IIST=0
      IF(IBANK.LT.0)THEN
*        coming from graphics display
         IBANK=-IBANK
         IF(NLKUSE.EQ.0)GOTO 999
         IF(IBANK .GT. NLKUSE) GOTO 888
         CALL SBYT(ISTUSE,IIST,27,6)
         CALL MZSDIV(IIST,-7)
         IF(LQ(LTDISP(ISTUSE)+KQS-IBANK).NE.0)
     &      CALL DZDSH1(IIST,LQ(LTDISP(ISTUSE)+KQS-IBANK),CMD)
      ELSE
*        coming from browser
         CALL SBYT(IACTST,IIST,27,6)
         CALL MZSDIV(IIST,-7)
         IF(IBANK.GT.IQ(LTBROW(IACTST)+KQS-3))GOTO 888
         IIST=IQ(LTBROW(IACTST)+KQS+1)
         CALL MZSDIV(IIST,-7)
         IF(LQ(LTBROW(IACTST)+KQS-IBANK).NE.0)
     &      CALL DZDSH1(IIST,LQ(LTBROW(IACTST)+KQS-IBANK),CMD)
      ENDIF
      GOTO 999
888   WRITE(*,*)'Invalid bank id given', IBANK
999   END
*********************************************************************

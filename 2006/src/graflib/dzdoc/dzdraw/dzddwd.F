*
* $Id: dzddwd.F,v 1.1.1.1 1996/03/04 16:13:17 mclareni Exp $
*
* $Log: dzddwd.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:17  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDDWD(IXSTOR, L, CHOPT, IFIRST, ILAST,LUN)
*.
*...DZDDWD   dump data words with documentation
*.
*. INPUT     :  IXSTOR  store index
*.              L       link to bank to be dumped
*.              CHOPT   character option
*.                      'S' short display, variable name only
*.                      'U' suppress list of UNDEFD
*.                      'K' generate keep sequences
*.              IFIRST   first word to dump
*.              ILAST    last word
*.              LUN      output unit
      CHARACTER*(*) CHOPT
      CHARACTER*8 CHBANK
#include "dzdoc/bkwrp.inc"
#include "dzdoc/tapes.inc"
*         From DZEBRA
#include "zebra/zbcdk.inc"
#include "dzdprm.inc"
#include "zebra/qstore.inc"
      CALL DZDGDV(CHOPT,IDVERS)
      ENTRY DZDDW1(IXSTOR, L, CHOPT, IFIRST, ILAST,LUN)
      IF(L .LE. 0)THEN
        GOTO 999
      ENDIF
      IL1 = IQ(KQS+L-1)
      IF(ILAST .GT. 0)IL1 = MIN(IL1,ILAST)
      IF(IFIRST.GT.0)THEN
         IF1 = MIN(IFIRST,IL1)
      ELSE
         IF1 = 1
      ENDIF
 
      CALL UHTOC(IQ(KQS+L-4),4,CHBANK,4)
      LUP = LQ(KQS+L+1)
      IF(LUP .GT. 2)THEN
           CALL UHTOC(IQ(KQS+LUP-4),4,CHBANK(5:),4)
      ELSE
           CHBANK(5:8)='NONE'
      ENDIF
      IF(JBIT(IQ(KQS+L),IQDROP).NE.0)THEN
          WRITE(LUN,'(A)')' Bank '//CHBANK(1:4)//' is dropped'
      ELSE
          CALL DZDDOC(IXSTOR,L,CHBANK,CHOPT,IF1,IL1,LUN,' ',NDUMMY)
      ENDIF
999   END
 
************************************************************************

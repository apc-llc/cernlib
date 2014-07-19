*
* $Id: dzdtmp.F,v 1.1.1.1 1996/03/04 16:13:07 mclareni Exp $
*
* $Log: dzdtmp.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:07  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDTMP(IXSTOR,LT,LUN,CHOPT)
 
*.  generate templates of bank doc for a bank or a whole tree
*
*. INPUT     : IXSTOR   Store index
*.             LT       Link to top bank
*.             CHOPT    Character option:
*.             CHOPT    option:
*.                      'L' follow next links
*.                      'T' do it for a bank tree
*.                      'A' fill author tag
*.                      'V' fill version tag
*.                      'S' fill store tag
*.                      'D' fill division tag
*.
*. OUTPUT    : none
*.
*. AUTHOR    : O.Schaile
*. VERSION   : 1.00
*. CREATED   : 25-Jan-91
*. LAST MOD  :
*.
*.
*.**********************************************************************
*.
      CHARACTER*(*) CHOPT
*
#include "zebra/zebq.inc"
#include "zebra/mzca.inc"
#include "zebra/mzcb.inc"
      IF (JBYT(IXSTOR,27,6).NE.JQSTOR)  CALL MZSDIV (IXSTOR,-7)
*--------
*      IOPTAQ=INDEX(CHOPT,'A')
      IOPTLQ=INDEX(CHOPT,'L')
      IOPTTQ=INDEX(CHOPT,'T')
*      NBK=0
*
*       init links
*
      L=LT
      LORIG = L
      LUP = L
      LFROM = LQ(KQS+L+1)
 
*     get next down link
   10 CONTINUE
 
      JB = LUP - LORIG + 1
      NS = IQ(KQS+LUP -2)
      IF(JB .EQ. 1)THEN
*     treat this bank
*         NBK=NBK+1
         CALL DZDTM1(L,LUN,CHOPT)
         IF(IOPTTQ.EQ.0)GOTO 60
      ENDIF
*
   40 CONTINUE
      IF(JB .LE. NS)THEN
 
*        go down
         LD1 = LQ(KQS+ L - JB)
*        look if this link is used
         IF(LD1 .EQ. 0)THEN
            JB = JB + 1
            GOTO 40
         ENDIF
*        if previous down bank was same => skip
         IF(JB.GT.1)THEN
            LD2 = LQ(KQS+ L - JB +1)
            IF(LD2.NE.0)THEN
               IF(IQ(KQS+LD1-4).EQ.IQ(KQS+LD2-4))THEN
                  JB = JB + 1
                  GOTO 40
               ENDIF
            ENDIF
         ENDIF
         LUP = LD1
         LORIG = LD1
         L = LD1
         GOTO 10
      ENDIF
 
*     look if its linear structure
 
      IF(IOPTLQ.NE.0)THEN
         LN = LQ(KQS+L)
         IF(LN .NE. 0)THEN
            LUP = LN
            LORIG = LN
            L = LN
            GOTO 10
         ENDIF
      ENDIF
*     look if it is end of a linear structure
*     i.e. origin and up link are in different banks
 
   50   CONTINUE
      LUP = LQ(KQS+L+1)
      LORIG = LQ(KQS+L+2)
 
*     look if back at top bank
 
      IF(LUP .LE. 1) GOTO 60
 
*     look if it would move above top bank
 
      IF(LUP .EQ. LFROM)      GOTO 60
 
      NSU = IQ(KQS+LUP-2)
      IF(LORIG .GE. LUP .OR. LORIG .LT. LUP-NSU)THEN
*         its different, step back
          L = LORIG
          GOTO 50
      ENDIF
*     go up
      L = LUP
      GOTO 10
*     all done, but top bank
   60 CONTINUE
*      WRITE(IQPRNT,'(A,I8,A)')' Total # of banks treated: ',NBK
  999 END
*****************************************************************

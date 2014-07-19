*
* $Id: dzdlin.F,v 1.1.1.1 1996/03/04 16:12:54 mclareni Exp $
*
* $Log: dzdlin.F,v $
* Revision 1.1.1.1  1996/03/04 16:12:54  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDLIN
************************************************************************
*.                                                                     *
*...DZDLIN prepares the text part of the bank descriptor for output    *
*.                                                                     *
*.  DZDLIN uses the information encoded in Hollerith form in a ZEBRA   *
*.  bank to output it in a structured form on the output line.         *
*.                                                                     *
*. CALLS    : INDXBC,SPACES                                            *
*. CALLED   : DZDOCO                                                   *
*. COMMON   : CDLINE,DZDOCC,DZDTAP,IDLINE                              *
*.                                                                     *
*. AUTHOR   : M. Goossens DD/US                                        *
*. VERSION  : 1.02(14)  /  9 Jul 1986                                  *
*.                                                                     *
*.**********************************************************************
      SAVE
 
#include "dzdoc/bkwrp.inc"
#include "dzdoc/linout.inc"
#include "dzdoc/tapes.inc"
*         From DZEBRA
#include "zebra/zbcdk.inc"
C--     Working vector for text treatment
      PARAMETER (NWORKQ=256)
      CHARACTER CWORK*(NWORKQ),SPACES*(NWORKQ)
      DATA CWORK/' '/
 
C- INFORM: Flag: 1 Do not format line (blanks are significant)
C-               0 Collapse multiple blanks to one blank
C-               Specifying line-feeds induces the INFORM=1 setting
C-               hence allowing the user to specify his own page layout
 
 1001 CONTINUE
      INFORM = 0
   10 NWORKF = MIN((NWORKQ-IEWORK)/4,NWHOLL)
*--     Copy text to working buffer up to end or linefeed
      DO 50 IH=0,NWORKF-1
          IF (IQ(KQSP+LQBKD+INSERT+IH).EQ.IILFLF) THEN
              CALL UHTOC(IQ(KQSP+LQBKD+INSERT),4,
     X                   CWORK(IEWORK+1:),4*IH)
              INFORM = 1
              IEWORK = IEWORK + 4*IH
              NWHOLL = NWHOLL - IH - 1
              INSERT = INSERT + IH + 1
*--             Drop slash at end of text
              IEWORK = INDXBC(CWORK(:IEWORK),' ')
              IF (CWORK(IEWORK:IEWORK).EQ.'/')
     X               IEWORK=INDXBC(CWORK(:IEWORK-1),' ')
*----           Start transfer from working buffer to output
   20         IF (IEWORK.GT.NOUTF) THEN
*--                 Output as much as possible and cut at blank
                  DO 30 IW = NOUTF,1,-1
                      IF (CWORK(IW:IW).EQ.' ') THEN
*--                         Blank found -- cut sentence
                          IEND = IW
                                                           GO TO 40
                      ENDIF
   30             CONTINUE
*--                 Fallen through do loop - cut at end
                  IEND   = NOUTF
   40             COUT(INLINE:) = CWORK(1:IEND)
                  IEWORK = IEWORK - IEND
*--                 Copy remaining information in COUT forward
                  CWORK(1:) = CWORK(IEND+1:)
              ELSE
*--                 Enough room on output line to contain info
                  COUT(INLINE:) = CWORK(1:IEWORK)
                  IEWORK = 0
              ENDIF
*--             Empty the text buffer
              IF(IFLOPT(MPOSBQ).EQ.0)THEN
                 IF (IFLOPT(MPOSIQ).NE.0)COUT(1:3)='*B.'
                 WRITE (LUNUSR,'(A)') COUT
              ENDIF
              IF (IEWORK.GT.0) THEN
                  COUT   = ' '
                                                           GO TO 20
              ENDIF
              IF (NWHOLL.GT.0) THEN
                  COUT   = ' '
                                                           GO TO 10
              ELSE
                                                           GO TO 999
              ENDIF
          ENDIF
   50 CONTINUE
 
*----     Fallen through do loop and no linefeed found
 
      CALL UHTOC(IQ(KQSP+LQBKD+INSERT),4,CWORK(IEWORK+1:),4*NWORKF)
      IEWORK = IEWORK + 4*NWORKF
      NWHOLL = NWHOLL - NWORKF
      INSERT = INSERT + NWORKF
*--     Fine the last significant character
      IEWORK = INDXBC(CWORK(1:IEWORK),' ')
      IF (IEWORK.EQ.0) THEN
*--         Empty the text buffer
              IF(IFLOPT(MPOSBQ).EQ.0) THEN
                 IF (IFLOPT(MPOSIQ).NE.0)COUT(1:3)='*B.'
                 WRITE (LUNUSR,'(A)') COUT
              ENDIF
          COUT   = ' '
                                                           GO TO 999
      ELSE
         IF(CWORK(IEWORK:IEWORK).EQ.'/')INFORM=1
      ENDIF
      IF (INFORM.NE.0) THEN
*--         Blanks are significant on last line
          INFORM = 0
      ELSE
*--          Blanks not significant, hence collapse multiple blanks to 1
          CWORK  = SPACES(CWORK(:IEWORK),1)
          IEWORK = INDXBC(CWORK(1:IEWORK),' ')
      ENDIF
*----   Start transfer from working buffer to output
  110 IF (IEWORK.GT.NOUTF) THEN
*--         Output as much as possible and cut at blank
          DO 120 IW = NOUTF,1,-1
              IF (CWORK(IW:IW).EQ.' ') THEN
*--                 Blank found -- cut sentence
                  IEND = IW
                                                           GO TO 130
              ENDIF
  120     CONTINUE
*--         Fallen through do loop - cut at end
          IEND   = NOUTF
  130     COUT(INLINE:) = CWORK(1:IEND)
          IEWORK = IEWORK - IEND
*--         Copy remaining information in COUT forward
          CWORK(1:) = CWORK(IEND+1:)
      ELSE
*--         Enough room on output line to contain info
          COUT(INLINE:) = CWORK(1:IEWORK)
          IEWORK = 0
      ENDIF
*--     Empty the text buffer
      IF(IFLOPT(MPOSBQ).EQ.0)THEN
          IF (IFLOPT(MPOSIQ).NE.0)COUT(1:3)='*B.'
          WRITE (LUNUSR,'(A)') COUT
      ENDIF
      COUT   = ' '
      IF (NWHOLL.GT.0)                                     GO TO  10
      IF (IEWORK.GT.0)                                     GO TO 110
 
  999 END

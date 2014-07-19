*
* $Id: cdlind.F,v 1.1.1.1 1996/02/28 16:24:35 mclareni Exp $
*
* $Log: cdlind.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:35  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
#if !defined(CERNLIB__P3CHILD)
      SUBROUTINE CDLIND (IDATA, NTOT, CLINE, LENGTH)
*     ==============================================
*
************************************************************************
*                                                                      *
*        SUBR. CDLIND (IDATA, *NTOT*, CLINE*, LENGTH*)                 *
*                                                                      *
*   Decodes an encdoed array into a string of characters               *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     IDATA    Array containing the encoded information                *
*     NTOT     Current location of the array IDATA to be decoded       *
*     CLINE    Character string of maximum 80 characters               *
*     LENGTH   Length of the string                                    *
*                                                                      *
*   Called by CDAFRI                                                   *
*                                                                      *
*   Original Code : DPLINE in CMZ Package                              *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "hepdb/ckkeys.inc"
      CHARACTER       CLINE*(*), KLINE*80
      DIMENSION       LINE(20), IDATA(9)
      CHARACTER*8     KEYW(13)
      CHARACTER*1     CAR, KONE, KTWO*(3)
      DIMENSION       NCHKEY(13)
*
      DATA            KEYW /'RETURN','RETURN','CONTINUE','END','END',
     +                      '+SELF.','+SEL','+CDE','+SEQ','+KEE','+DEC',
     +                      'ELSE','ENDIF'/
      DATA            NCHKEY /6, 6, 8, 3, 3, 6, 4, 4, 4, 4, 4, 4, 5/
#include "zebra/q_jbit.inc"
* Ignoring t=pass
*
*     ------------------------------------------------------------------
*
      ICADRE = 0
      IWORD  = IDATA(NTOT+1)
      IF (IWORD.EQ.0) THEN
        LENGTH = 0
        CLINE  = ' '
        NTOT   = NTOT + 1
        GO TO 999
      ENDIF
*
      LENGTH = 0
      KLINE  = ' '
      IBIT31 = JBIT (IWORD, 31)
      IBIT32 = JBIT (IWORD, 32)
      IF (IBIT31.NE.0) THEN
        ITWO   = IDATA(NTOT+2)
        NWI    = 2
        CALL CDITOC (ITWO, KTWO, 1, 3)
      ELSE
        KTWO   = ' '
        NWI    = 1
      ENDIF
      KCODE  = JBYT (IWORD, 1, 8)
      IF (IBIT32.NE.0) THEN
*
* ***   Study the comment cards
*
        ICODE  = KCODE
        ICADRE = MOD (ICODE, 2)
        IPOINT = MOD (ICODE, 4)
        ICMMNT = ICODE/4
        IF (ICMMNT.EQ.1) THEN
          KONE   = 'C'
        ELSE IF (ICMMNT.EQ.2) THEN
          KONE   = 'c'
        ELSE
          KONE   = '*'
        ENDIF
        IF (IPOINT.GT.1) THEN
          KTWO   = '.'
        ENDIF
        KLINE  = KONE//KTWO
        LENGTH = 4
*
*  **   Is there a box ?
*
        IF (ICADRE.NE.0) THEN
          NWI    = NWI + 1
          IPOS1  = JBYT (IDATA(NTOT+NWI), 25, 8)
          IPOS2  = JBYT (IDATA(NTOT+NWI),  9, 8)
          IKLIN  = JBYT(IDATA(NTOT+NWI),1,8)
          CALL CDCHFI (IKLIN, KLINE(IPOS2:IPOS2))
          LENGTH = IPOS2
          IF (IPOS1.NE.0) THEN
            IKLIN  = JBYT(IDATA(NTOT+NWI),17,8)
            CALL CDCHFI (IKLIN, KLINE(IPOS1:IPOS1))
          ENDIF
        ENDIF
*
* ***   Is there a character to repeat?
*
        ICAR   = JBYT (IWORD, 9, 8)
        IF (ICAR.EQ.0) THEN
*
*  **     No character to repeat.
*
          IFWORD = JBYT (IWORD, 17, 7)
          ILASTW = JBYT (IWORD, 24, 7)
          IF (ILASTW.EQ.1) THEN
            NTOT   = NTOT + NWI
            GO TO 70
          ENDIF
*
          IF (ILASTW.GT.20 .OR. IFWORD.GT.ILASTW) GO TO 100
*
          DO 10 IW = IFWORD, ILASTW
            JW     = NTOT + NWI + IW - IFWORD + 1
            LINE(IW) = IDATA(JW)
   10     CONTINUE
          NTOT   = NTOT + NWI + ILASTW - IFWORD + 1
          GO TO 60
        ELSE
*
*  **     Character must be repeated.
*
          CALL CDCHFI (ICAR, CAR)
          IFIRST = JBYT (IWORD, 17, 7)
          ILAST  = JBYT (IWORD, 24, 7)
*
          IF (ILAST.GT.80 .OR. IFIRST.GT.ILAST)   GO TO 100
*
          DO 20 I = IFIRST, ILAST
            KLINE(I:I) = CAR
   20     CONTINUE
          NTOT   = NTOT + NWI
          IF (ICADRE.EQ.0) LENGTH = ILAST
          GO TO 70
        ENDIF
      ELSE
*
* ***   Special cases
*
        ICODE  = JBYT (IWORD, 24, 7)
        ICOD   = ICODE - 114
*
*  **   Case ENDIF or ELSE or END or CONTINUE or RETURN
*  **   Case +PATCH,+DECK,+KEEP,+SEQ,+CDE,+SELF
*
*        ICODE = 115  ICOD = 1  'RETURN with label'
*              = 116       = 2  'RETURN without label'
*              = 117         3  'CONTINUE'
*              = 118         4  'END without label'
*              = 119         5  'END with label'
*              = 120         6  '+SELF.'
*              = 121         7  '+SELF,'
*              = 122         8  '+CDE'
*              = 123         9  '+SEQ'
*              = 124        10  '+KEEP,'
*              = 125        11  '+DECK,'
*              = 126        12  'ELSE'
*              = 127        13  'ENDIF'
*
        IF (ICOD.LE.0)                            GO TO 40
        IF (ICOD.LT.6 .OR. ICOD.GT.11) THEN
          IFIRST = JBYT (IWORD, 17, 7)
          CLINE(1:IFIRST) = ' '
          IF (ICOD.EQ.1 .OR. ICOD.EQ.3 .OR. ICOD.EQ.5) THEN
            CALL CDCHFI (KCODE, KONE)
            IKLIN = JBYT (IWORD, 9, 8)
            CALL CDCHFI (IKLIN, CLINE(5:5))
            CLINE(:4)  = KONE//KTWO
          ENDIF
          LENGTH = IFIRST+NCHKEY(ICOD)-1
          CLINE(IFIRST:LENGTH) = KEYW(ICOD)
          NTOT   = NTOT + NWI
          GO TO 900
        ENDIF
*
* ***   Case +PATCH,+DECK,+KEEP,+SEQ,+CDE,+SELF
*
        IF (ICOD .EQ. 6) THEN
          CLINE(:6) = '+SELF.'
          NTOT   = NTOT + 1
          LENGTH = 6
          GO TO 900
        ENDIF
*
        IFWORD = 2
        KLINE(1:4) = KEYW(ICOD)
        ILASTW = JBYT (IWORD, 17, 7)
        DO 30 IW = 2, ILASTW
          LINE(IW) = IDATA(NTOT+IW)
   30   CONTINUE
        NTOT   = NTOT + ILASTW
        GO TO 60
      ENDIF
*
   40 CONTINUE
*
* *** Normal instructions
*
      CALL CDCHFI (KCODE, KONE)
      KLINE  = KONE//KTWO
      ILASTW = ICODE
      IF (ILASTW.LT.2) THEN
        NTOT   = NTOT+NWI
        LENGTH = 4
        GO TO 70
      ENDIF
      IFWORD = JBYT (IWORD, 17, 7)
*
      IF (ILASTW.GT.20 .OR. IFWORD.GT.ILASTW)     GO TO 100
*
      DO 50 IW = IFWORD, ILASTW
        JW     = NTOT + NWI + IW - IFWORD + 1
        LINE(IW) = IDATA(JW)
   50 CONTINUE
      NTOT   = NTOT + NWI + ILASTW - IFWORD + 1
   60 CONTINUE
      NC     = (IFWORD-1)*4 + 1
      LENGW  = ILASTW-IFWORD+1
      LENG   = 4*LENGW
      CALL CDITOC (LINE(IFWORD), KLINE(NC:), LENGW, LENG)
      IF (ICADRE.EQ.0) LENGTH = ILASTW*4
   70 I1     = LENGTH
      DO 80 I = I1, 1, -1
        IF (KLINE(I:I).NE.' ')                    GO TO 90
        LENGTH = LENGTH - 1
   80 CONTINUE
*
   90 CLINE  = KLINE(1:LENGTH)
      GO TO 900
*
  100 CALL CDPRNT (LPRTCD, '('' **** WARNING : Data in deck is corrup'//
     +     'ted ****'')', IARGCD, 0)
      CLINE  = ' '
      LENGTH = 0
  900 CONTINUE
*                                                             END CDLIND
  999 END
#endif

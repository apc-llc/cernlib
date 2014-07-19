*
* $Id: cdwrdp.F,v 1.1.1.1 1996/02/28 16:24:53 mclareni Exp $
*
* $Log: cdwrdp.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:53  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDWRDP (LUN, LAD)
*     ============================
*
************************************************************************
*                                                                      *
*        SUBR. CDWRDP (LUN, LAD)                                       *
*                                                                      *
*   Prepares the display the data on unit LUN and at address LAD       *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     LUN      Unit number of file for display                         *
*     LAD      Address of the data in CDSTOR                           *
*                                                                      *
*   Called by CDDISD, CDPEEK                                           *
*                                                                      *
*   Original Code : J. Zoll                                            *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "zebra/mzioc.inc"
#include "zebra/q_jbit.inc"
* Ignoring t=pass
*
*     ------------------------------------------------------------------
*
*  ** No data to display
*
      NDAT = IQ(KOFUCD+LAD-1)
      IF (NDAT.LE.0)        GO TO 999
*
*  ** Crack I/O Words into the character description
*
      LIO = LAD - IQ(KOFUCD+LAD-3) - JBYT(IQ(KOFUCD+LAD),19,4) - 1
      CALL MZIOCR (LQ(KOFUCD+LIO))
*
*  ** This part (upto the RETURN) is by courtesy of J. Zoll
*
      NDONE = 0
      JFOCUR = 0
   10 ITYPE = MFO(JFOCUR+1)
      IF (ITYPE.EQ.7)       GO TO 31
      NWSEC = MFO(JFOCUR+2)
      NDO   = NWSEC
      IT    = ITYPE
      IF (NDO.GT.0)         GO TO 41
*
      IF (NDO.LT.0)  THEN
*
*  *    Rest of the bank
*
        NDO = NDAT
      ELSE
*
*  *    Dynamic sector
*
        NDONE = NDONE + 1
        NDO   = IQ(KOFUCD+LAD+NDONE)
      ENDIF
      GO TO 34
*
*  ** Self-describing sector
*
   31 NDONE = NDONE + 1
      IWORD = IQ(KOFUCD+LAD+NDONE)
      IT    = MOD (IWORD,16)
      NDO   = IWORD/16
*
   34 IF (IT.GE.8)          GO TO 999
      IF (NDO.LT.0)         GO TO 999
      IF (NDO.EQ.0)         GO TO 999
*
   41 JS = NDONE + 1
      JE = MIN (NDONE+NDO, NDAT)
      DO 47 J = JS, JE
        IF (IT.EQ.1) THEN
          WRITE (LUN, 1001) J, Q(KOFUCD+LAD+J)
        ELSE IF (IT.EQ.2) THEN
          WRITE (LUN, 1002) J,IQ(KOFUCD+LAD+J)
        ELSE IF (IT.EQ.3) THEN
          WRITE (LUN, 1003) J, Q(KOFUCD+LAD+J)
        ELSE IF (IT.EQ.5) THEN
          WRITE (LUN, 1004) J,IQ(KOFUCD+LAD+J)
        ENDIF
   47 CONTINUE
      NDONE = JE
      IF (NDONE.GE.NDAT)    GO TO 999
*
      JFOCUR = JFOCUR + 2
      IF (JFOCUR.LT.JFOEND) GO TO 10
      JFOCUR = JFOREP
      GO TO 10
*
 1001 FORMAT (1X,'B',I5,4X,Z10)
 1002 FORMAT (1X,'I',I5,4X,I10)
 1003 FORMAT (1X,'F',I5,4X,E12.4)
 1004 FORMAT (1X,'H',I5,4X,A4)
*                                                             END CDWRDP
  999 END

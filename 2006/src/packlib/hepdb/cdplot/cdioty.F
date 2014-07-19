*
* $Id: cdioty.F,v 1.1.1.1 1996/02/28 16:24:22 mclareni Exp $
*
* $Log: cdioty.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:22  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDIOTY (LAD, NOBJ, KOBJ, IOTY)
*     =========================================
*
************************************************************************
*                                                                      *
*        SUBR. CDIOTY (LAD, NOBJ, KOBJ, IOTY*)                         *
*                                                                      *
*   Prepares the IO format of a list of elements in a bank from the    *
*   bank IO descriptor                                                 *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     LAD      Address of the data in CDSTOR                           *
*     NOBJ     Number of elements to be considered                     *
*     KOBJ     Indices of the elements                                 *
*     IOTY     Variable type (2 for integer, 3 for floating point,..)  *
*                                                                      *
*   Called by CDNTPL, CDPLOB, CDPLOV                                   *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "zebra/mzioc.inc"
      DIMENSION       KOBJ(9), IOTY(9)
#include "zebra/q_jbit.inc"
* Ignoring t=pass
*
*     ------------------------------------------------------------------
*
*  ** No data to display
*
      IF (NOBJ.LE.0)        GO TO 999
      DO 5 I = 1, NOBJ
    5 IOTY(I) = 3
      IF (LAD.LE.0)         GO TO 999
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
      IF (ITYPE.EQ.7)       GO TO 15
      NWSEC = MFO(JFOCUR+2)
      NDO   = NWSEC
      IT    = ITYPE
      IF (NDO.GT.0)         GO TO 30
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
      GO TO 20
*
*  ** Self-describing sector
*
   15 NDONE  = NDONE + 1
      IWORD  = IQ(KOFUCD+LAD+NDONE)
      IT     = MOD (IWORD,16)
      NDO    = IWORD/16
*
   20 IF (IT.GE.8)          GO TO 999
      IF (NDO.LT.0)         GO TO 999
      IF (NDO.EQ.0)         GO TO 999
*
   30 JS     = NDONE + 1
      JE     = MIN (NDONE+NDO, NDAT)
      DO 35 I = 1, NOBJ
        IF (KOBJ(I).GE.JS.AND.KOBJ(I).LE.JE) IOTY(I) = IT
   35 CONTINUE
      NDONE = JE
      IF (NDONE.GE.NDAT)    GO TO 999
*
      JFOCUR = JFOCUR + 2
      IF (JFOCUR.LT.JFOEND) GO TO 10
      JFOCUR = JFOREP
      GO TO 10
*                                                             END CDIOTY
  999 END

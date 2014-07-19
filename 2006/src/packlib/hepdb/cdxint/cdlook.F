*
* $Id: cdlook.F,v 1.1.1.1 1996/02/28 16:24:50 mclareni Exp $
*
* $Log: cdlook.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:50  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDLOOK (LAD, X, Y, PATH)
*     ===================================
*
************************************************************************
*                                                                      *
*        SUBR. CDLOOK (LAD, X, Y, PATH*)                               *
*                                                                      *
*   Returns the path name picked up by the cursor at x, y              *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     LAD      Address of the bank containing information about the    *
*              tree structure starting from the top level              *
*     X        X coordinate of the cursor position                     *
*     Y        Y coordinate of the cursor position                     *
*     PATH     Complete path name as picked up by the user             *
*              (Blank if no path is found)                             *
*                                                                      *
*   Called by CDACPL                                                   *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/ctpath.inc"
      DIMENSION       LAD(9)
      CHARACTER       PATH*(*)
*
*     ------------------------------------------------------------------
*
      PATH   = ' '
      IF (LAD(1).LE.0) GO TO 999
      NLEV   = IQ(KOFUCD+LAD(1)+MNLVCD)
      IF (NLEV.LE.0)   GO TO 999
*
* *** Find the level number from the x coordinate
*
      IPNL   = IQ(KOFUCD+LAD(1)+MPNLCD)
      HFRAM  =  Q(KOFUCD+LAD(1)+MHFMCD)
      KLEV   = 0
      DO 10 ILEV = 1, NLEV
        XDIST  = X - Q(KOFUCD+LAD(1)+IPNL+MXOFCD)
        DELX   = Q(KOFUCD+LAD(1)+IPNL+MXWDCD)
        IF (XDIST.GE.0.AND.XDIST.LE.DELX) THEN
          KLEV   = ILEV
        ENDIF
        IPNL   = IQ(KOFUCD+LAD(1)+IPNL+MPNLCD)
   10 CONTINUE
      IF (KLEV.LE.0)   GO TO 999
*
* *** Find the node number from the y coordinate
*
      IPNL   = IQ(KOFUCD+LAD(1)+MPNLCD)
      NODE   = 0
      IF (KLEV.GT.1) THEN
        DO 15 ILEV = 2, KLEV
          IPNL   = IQ(KOFUCD+LAD(1)+IPNL+MPNLCD)
   15   CONTINUE
      ENDIF
      NODES  = IQ(KOFUCD+LAD(1)+IPNL+MNODCD)
      IPNN   = IQ(KOFUCD+LAD(1)+IPNL+MPNNCD)
      DO 20 INOD = 1, NODES
        YDIST  = Y- Q(KOFUCD+LAD(1)+IPNN+MYFNCD)
        IF (YDIST.GE.0.AND.YDIST.LE.HFRAM) THEN
          NODE   = INOD
        ENDIF
        IPNN   = IQ(KOFUCD+LAD(1)+IPNN+MPNLCD)
   20 CONTINUE
      IF (NODE.LE.0)   GO TO 999
*
* *** Correct node is identified; now compute the Path
*
      IUPN   = NODE
      NCHRT  = 0
      PAT2CT = ' '
      DO 40 ILEV = 1, KLEV
        JLEV   = KLEV + 1 - ILEV
        IPNL   = IQ(KOFUCD+LAD(1)+MPNLCD)
        IF (JLEV.GT.1) THEN
          DO 25 IL = 2, JLEV
            IPNL   = IQ(KOFUCD+LAD(1)+IPNL+MPNLCD)
   25     CONTINUE
        ENDIF
        IPNN   = IQ(KOFUCD+LAD(1)+IPNL+MPNNCD)
        IF (IUPN.GT.1) THEN
          DO 30 INOD = 2, IUPN
            IPNN   = IQ(KOFUCD+LAD(1)+IPNN+MPNLCD)
   30     CONTINUE
        ENDIF
        NCHR   = IQ(KOFUCD+LAD(1)+IPNN+MNCHCD)
        IUPN   = IQ(KOFUCD+LAD(1)+IPNN+MNFNCD)
        CALL UHTOC (IQ(KOFUCD+LAD(1)+IPNN+MNAMCD), 4, TOP2CT, NCHR)
        PAT4CT = '/'//TOP2CT(1:NCHR)
        NCHR   = NCHR+1
        IF (NCHRT.GT.0) THEN
          PAT3CT = PAT4CT(1:NCHR)//PAT2CT(1:NCHRT)
        ELSE
          PAT3CT = PAT4CT
        ENDIF
        PAT2CT = PAT3CT
        NCHRT  = NCHRT + NCHR
   40 CONTINUE
      PATH  = '/'//PAT2CT
*                                                             END CDLOOK
  999 END

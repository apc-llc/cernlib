*
* $Id: pacsel.F,v 1.1.1.1 1996/03/01 11:38:38 mclareni Exp $
*
* $Log: pacsel.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:38  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.03/04 20/07/93  15.45.20  by  O.Couet
*-- Author :    Rene Brun   03/01/89
      SUBROUTINE PACSEL
*
*           To draw the current selection mechanism
*
#include "hbook/hcmail.inc"
#include "paw/pccsel.inc"
#include "paw/quest.inc"
*.______________________________________
*
      NCH = LENOCC(CHCSEL)
      IF(NCH.LE.0)RETURN
*
      CALL HPLSET('?GFON',CFON)
      ICFON  = INT(CFON/10.)
      CHMAIL = CHCSEL
      IF(ICFON.NE.0)THEN
         TFON = SIGN(1,ICFON)*(FLOAT(ABS(ICFON)*10)+1.)
         CALL IGSET('TXFP',TFON)
      ELSE
         CALL IGSET('TXFP',2.)
      ENDIF
      CALL IGTCCH(CHMAIL)
*
*          Get zone coordinates
*
      CALL HPLGIV(XLOW,YLOW,XHIGH,YHIGH)
      NTWIN = IQUEST(11)
      CALL ISELNT(1)
*
      IF(IOPTCB.NE.0)THEN
         YC = YHIGH-1.5*CSIZE
      ELSE
         YC = YHIGH+0.5*CSIZE
      ENDIF
      IF(IOPTCC.NE.0)THEN
         XC = 0.5*(XLOW+XHIGH)
         CALL IGSET('TXAL',20.)
      ELSEIF(IOPTCR.NE.0)THEN
         XC = XHIGH
         IF(IOPTCB.NE.0)XC=XC-0.5*CSIZE
         CALL IGSET('TXAL',30.)
      ELSE
         XC = XLOW
         IF(IOPTCB.NE.0)XC=XC+0.5*CSIZE
         CALL IGSET('TXAL',10.)
      ENDIF
*
      CALL IGSET('CHHE',CSIZE)
      CALL IGSET('TANG',0.)
      CALL ITX(XC,YC,CHMAIL)
      CALL ISELNT(NTWIN)
*
      END

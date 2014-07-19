*
* $Id: hpldat.F,v 1.4 1998/11/09 10:44:47 couet Exp $
*
* $Log: hpldat.F,v $
* Revision 1.4  1998/11/09 10:44:47  couet
* - Call IGDATE to be Y2K compliant
*
* Revision 1.3  1998/01/30 15:25:16  couet
* - APOLLO version removed
*
* Revision 1.2  1997/03/14 14:24:28  mclareni
* WNT mods
*
* Revision 1.1.1.1.2.1  1997/01/21 11:25:44  mclareni
* All mods for Winnt 96a on winnt branch
*
* Revision 1.1.1.1  1996/01/19 10:49:58  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.20/00 19/04/95  10.26.37  by  O.Couet
*-- Author :    O.Couet   13/01/89
      SUBROUTINE HPLDAT
*.==========>
*.
*.  Draw the current DATE and TIME on the current histogram.
*.
*..==========> (O.Couet)
#include "hplot/hpl1.inc"
#include "hplot/hpl6.inc"
#include "hplot/hpl2.inc"
#include "hplot/hpl4.inc"
#include "hplot/hpl13.inc"
      DIMENSION RTXFP(2)
      CHARACTER*32 CHDT
      SAVE IPRDAT
      DATA IPRDAT /0/
*._____________________________
*
      IF((.NOT.LODATE).OR.(IOPTU.NE.0).OR.(LOSAME))RETURN
      IF((IWIN.EQ.IWIN0).AND.(.NOT.LWSAME))IPRDAT=0
      IF(IPRDAT.EQ.1.AND.LOWIND)RETURN
      IPRDAT=1
*
      CALL IGQ('TXFP',RTXFP)
      TVCOM=TVSIZ(5)
*
*              Set date font and precision
*
      NTWOLD=NTWIN
      KFONT=IABS(IHFONT(5))
      ITXFON=KFONT/10
      ITXPRE=MOD(KFONT,10)
      IF(IHFONT(5).LT.0)ITXFON=-ITXFON
      CALL ISTXFP(ITXFON,ITXPRE)
*
*              Draw the date
*
      CALL IGDATE(CHDT)
      IF(TVSIZ(5).EQ.0.)TVSIZ(5)=0.28
      IF(IDATE.EQ.1)THEN
         KFLG=1
         XK=0.5
         YK=YSIZ-0.5-TVSIZ(5)
      ELSEIF(IDATE.EQ.2)THEN
         KFLG=3
         XK=XSIZ-0.5
         YK=YSIZ-0.5-TVSIZ(5)
      ELSEIF(IDATE.EQ.3)THEN
         KFLG=1
         XK=0.5
         YK=0.5
      ELSEIF(IDATE.EQ.4)THEN
         KFLG=3
         XK=XSIZ-0.5
         YK=0.5
      ENDIF
      CALL HPLTXT(XK,YK,CHDT,5,1,KFLG)
*
      TVSIZ(5)=TVCOM
*
*              Reset NT
*
      IF(NTWIN.NE.NTWOLD)THEN
         NTWIN=NTWOLD
         CALL ISELNT(NTWOLD)
      ENDIF
      IF(LOZFL)CALL IZPICT(' ','O')
*
      CALL ISTXFP(INT(RTXFP(1)),INT(RTXFP(2)))
      END

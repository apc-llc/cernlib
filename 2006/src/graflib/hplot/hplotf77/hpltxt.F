*
* $Id: hpltxt.F,v 1.1.1.1 1996/01/19 10:50:11 mclareni Exp $
*
* $Log: hpltxt.F,v $
* Revision 1.1.1.1  1996/01/19 10:50:11  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.20/00 19/04/95  10.27.47  by  O.Couet
*-- Author :
      SUBROUTINE HPLTXT(X,Y,CHTXT,KTYP,KORNT,KFLG)
*.==========>
*.            General text writing routine
*.            (Transformation no1 cm is selected)
*..=========>
#include "hplot/hpl2.inc"
#include "hplot/hpl6.inc"
      CHARACTER*(*) CHTXT
      DIMENSION RTXAL(2)
*._____________________________
*
*----
*     Cm are used
*----
      IF(NTWIN.NE.1)CALL ISELNT(1)
      NTWIN = 1
      NCHR  = LENOCC(CHTXT)
      IF (NCHR.EQ.0) RETURN
*
      CALL IGQ('CHHE',RCHHE)
      CALL IGQ('TXAL',RTXAL)
      CALL IGQ('TANG',RTANG)
*----
*       Character size
*----
      SZ=TVSIZ(KTYP)
      CALL ISCHH(SZ)
*----
*     Text adjustment :
*     test if text is to be centred/moved
*----
      IALV=0
*
*        Left adjusted
*
      IF(KFLG.EQ.1)THEN
         IALH=0
*
*        Centered
*
      ELSE IF(KFLG.EQ.2)THEN
         IALH=2
*
*        Right adjusted
*
      ELSE IF(KFLG.EQ.3)THEN
         IALH=3
      ENDIF
      CALL ISTXAL(IALH,IALV)
*----
*       Text orientation
*----
      IF(KORNT.EQ.1)THEN
        CALL ISCHUP(0.,1.)
      ELSEIF(KORNT.EQ.2)THEN
        CALL ISCHUP(-1.,0.)
      ELSEIF(KORNT.EQ.3)THEN
        CALL ISCHUP(0.,-1.)
      ELSEIF(KORNT.EQ.4)THEN
        CALL ISCHUP(1.,0.)
      ENDIF
*----
*     Verify text length
*     if too long text is truncated
*----
   80 GOTO (82,81,81,81,81,82,82,82,81),KTYP
   81 CALL HPLTX1(KTYP,KORNT)
   82 CONTINUE
*
      CALL ITX(X,Y,CHTXT(1:NCHR))
*
      CALL IGSET('CHHE',RCHHE)
      CALL ISTXAL(INT(RTXAL(1)),INT(RTXAL(2)))
      CALL IGSET('TANG',RTANG)
*
      END

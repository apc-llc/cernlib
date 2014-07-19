*
* $Id: hplpgn.F,v 1.1.1.1 1996/01/19 10:50:11 mclareni Exp $
*
* $Log: hplpgn.F,v $
* Revision 1.1.1.1  1996/01/19 10:50:11  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.18/07 14/01/94  15.33.32  by  O.Couet
*-- Author :
      SUBROUTINE HPLPGN
*.==========>
*.            Draws page number
*.            (Cm are used for call to HPLTXT)
*..=========>
#include "hplot/hpl1.inc"
#include "hplot/hpl4.inc"
      CHARACTER*8 CODED,CHPAGE
      LOGICAL LOLD
*._____________________________
*
      LOLD = LOSOFT
*----
*     Draw page number in form '- 123 -'
*     according to the option set in HPLOPT
*     (*P, **P, ***P for 1, 2, or 3 digits).
*     Page number must be .LT. 1000
*----
      IF(IOPAGE.GE.1000)THEN
         CALL HBUG('Page number must be .LT. 1000','HPLOPT',0)
         GOTO 30
      ELSE IF(IOPAGE.GE.100)THEN
         NDIGIT = 3
      ELSE IF(IOPAGE.GE.10)THEN
         NDIGIT = 2
      ELSE
         NDIGIT = 1
      ENDIF
      NDIGIT = MAX(NDIGIT,IPGSIZ)
      WRITE(CODED,'(2H(I,I1,1H.,I1,1H))')NDIGIT,NDIGIT
      CHPAGE = '- '
      WRITE(CHPAGE(3:),CODED)IOPAGE
      CHPAGE(LENOCC(CHPAGE)+1:) = ' -'
*----
*     Set up parameters for HPLTXT
*----
      KFLG  = 2
      IF(.NOT.LOVERT) GOTO 10
      KORNT = 1
*----
*     Define position for A4-vertical
*----
      XK = 0.5*XSIZ
      YK = YSIZ
      YK = YK-YNPAGE
      GOTO 20
*-----
*     Software characters should be used for horizontal plots
*     (metric paper sizes only)
*-----
   10 KORNT  = 4
      LOSOFT = .TRUE.
      XK     = XSIZ
      YK     = 0.5*YSIZ
      XK     = XK-0.5*XMRIT
*----
*     Plot page number
*----
   20 CALL HPLTXT(XK,YK,CHPAGE,6,KORNT,KFLG)
*-----
*     Increase page no.
*-----
      IOPAGE = IOPAGE+1
*
   30 LOSOFT = LOLD
*
      END

*
* $Id: hpli.F,v 1.1.1.1 1996/01/19 10:50:09 mclareni Exp $
*
* $Log: hpli.F,v $
* Revision 1.1.1.1  1996/01/19 10:50:09  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.14/09 26/11/92  13.42.22  by  O.Couet
*-- Author :
      SUBROUTINE HPLI
*.==========>
*.            starts new picture :
*.            draws picture surround
*.            and page number.
*..=========>
#include "hplot/hpl1.inc"
#include "hplot/hpl4.inc"
*._____________________________
*----
*     start new picture
*----
      IPLNUM=IPLNUM+1
      IF(LOZFL)IPLTNO=IPLTNO+1
*----
*       Call optional user routine HPLNXT and internal routine HPLNEW
*       before each new frame
*----
      CALL HPLNXT
      CALL HPLNEW
*----
*     (HPLCOL is called at the start of each frame to ensure the
*     correct colours are used even if individual frames are later
*     selected)
*----
*     draw surround
*----
      IF (LOBOX) THEN
         CALL ISLN(1)
         IBTOLD = IBTYP
         IBWOLD = IBWID
         IBCOLD = IBCOL
         IBBCOD = IBBCOL
         IBTYP  = IPTYP
         IBCOL  = IPCOL
         IBBCOL = IPBCOL
         IBWID  = IPWID
         CALL HPLBOX(0.,0.,XSIZ,YSIZ,'CM  ')
         CALL ISLN(ILTYP)
         IBTYP  = IBTOLD
         IBWID  = IBWOLD
         IBCOL  = IBCOLD
         IBBCOL = IBBCOD
      ENDIF
*----
*     are page numbers required?
*----
      IF(IPGSIZ.NE.0) CALL HPLPGN
*
      END


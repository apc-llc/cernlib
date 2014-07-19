*
* $Id: dzdlbx.F,v 1.1.1.1 1996/03/04 16:13:12 mclareni Exp $
*
* $Log: dzdlbx.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:12  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDLBX(X0,Y0,IFL,ILL,NSREAL)
*.
*...DZDLBX  draw a link box indicating a range of links
*.
*. INPUT     :  X0, Y0   position for the box
*.              IFL,ILL  first, last link number
*. SEQUENCE  : DZDCHV DZDPRM
*. CALLS     : DZDBOX DZDTXT
*. CALLED    : DZDRA2
*.
*. AUTHOR    : O.Schaile
*. VERSION   : 1.00
*. CREATED   : 11-Dec-87
*.********************************************************************
*.
#include "dzdprm.inc"
#include "dzdchv.inc"
*
      IF(IFL.GT.0 .AND. ILL.GT.0 .AND. ILL.LT.IFL)GOTO 999
      IF(ILL.GT.NSREAL)CALL ISLN(3)
      CALL DZDBOX(X0-4.*DXLK, X0, Y0, Y0+DYB,1000*IFLDIR+0)
      IF(ILL.GT.NSREAL)CALL ISLN(1)
      CLINE=' '
      WRITE(CLINE(73:80),'(I8)')IFL
      IS = INDEXC(CLINE,' ')
      CLINE(IS-2:IS-1) = '..'
      IS = IS - 2
      IF(ILL .LT. 0)THEN
        CLINE(IS-2:IS-1)= '**'
      ELSE
        WRITE(CLINE(IS-8:IS-1),'(I8)')ILL
      ENDIF
      IS = INDEXC(CLINE,' ')
      IF(IS .EQ. 0)IS = 1
*          left
*      XT= X0-4.*DXLK+0.1*CSIZE
*      YT= Y0 + 0.15*CSIZE
*      IALG=1
*          left
      XT= X0-2.*DXLK
      YT= Y0 + 0.5*DYB
*        centered
      IALG = 2
      CALL DZDTXT(CLINE(IS:),80-IS+1,XT,YT,CSIZE,0.,IALG)
999   END
**********************************************************************

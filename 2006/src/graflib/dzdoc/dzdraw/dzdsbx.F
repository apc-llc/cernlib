*
* $Id: dzdsbx.F,v 1.1.1.1 1996/03/04 16:13:12 mclareni Exp $
*
* $Log: dzdsbx.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:12  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDSBX(X0,Y0,IL,NSREAL)
*.
*...DZDSBX   draw a link box with one link
*.
*. INPUT     :  X0,X1,Y0,Y1  coordinates of the box
*.              IL           link number
*.              CS           character size
*.
*. SEQUENCE  : DZDCHV
*. CALLS     : DZDBOX DZDTXT
*. CALLED    : DZDRA2, DZDLNK
*.
*. AUTHOR    : O.Schaile
*. VERSION   : 1.00
*. CREATED   : 11-Dec-87
*. LAST MOD  : 18-Mar-88
*.
*. Modification Log.
*. 18-Mar-88:  include possible negative IL
*.**********************************************************************
*.
#include "dzdprm.inc"
#include "dzdchv.inc"
      IF(IL.GT.NSREAL)CALL ISLN(3)
      CALL DZDBOX(X0,X0+DXLK,Y0,Y0+DYB,1000*IFLDIR+0)
      IF(IL.GT.NSREAL)CALL ISLN(1)
      IF(IFLDIR .NE. 0)GOTO 999
      IF(IL .GT. 0)THEN
            IL1=MOD(IL,10)
            WRITE(CLINE,'(I8)')IL1
      ELSE
            IFC = 8
            CLINE(1:IFC)= '       *'
      ENDIF
      IFC = INDEXC(CLINE,' ')
*          left
*      XT = X0+0.05*CSIZE
*      YT = Y0+0.15*CSIZE
*      IALG=1
*          left
      XT = X0+DXLK*0.5
      YT = Y0+DYB*0.5
      IALG = 2
      CALL DZDTXT(CLINE(IFC:8),8-IFC+1
     &  ,XT,YT,CSIZE,0.,IALG)
999   END
**********************************************************************

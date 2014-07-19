*
* $Id: getatt.F,v 1.2 1996/08/30 10:04:01 lecointe Exp $
*
* $Log: getatt.F,v $
* Revision 1.2  1996/08/30 10:04:01  lecointe
* Restored Gouraud Shading in NT/PLOT
* 	Added padriso.F in Imakefile
* 	Added a subroutine (GETNBINS) in getatt.F
*
* Revision 1.1.1.1  1996/03/01 11:38:37  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.02/01 03/05/93  15.19.23  by  O.Couet
*-- Author :    O.Couet   03/09/92
      SUBROUTINE GETHPLOPT(C)
#include "paw/quest.inc"
      PARAMETER (NBOPT=23)
      CHARACTER*(*) C
      CHARACTER*4 KTRUE(NBOPT),KFALSE(NBOPT)
      DATA KTRUE/'VERT','EAH ','CHA ','AST '
     +,          'SOFT','SQR ','UTIT','TAB ','BOX '
     +,          'TIC ','STA ','FIT ','ZFL ','PTO '
     +,          'BAR ','DVXI','DVYI','GRID','DATE'
     +,          'FILE','LOGX','LOGY','LOGZ'/
      DATA KFALSE/'HORI','NEAH','NCHA','NAST'
     +,           'HARD','NSQR','HTIT','NTAB','NBOX'
     +,           'NTIC','NSTA','NFIT','NZFL','NPTO'
     +,           'NBAR','DVXR','DVYR','NGRI','NDAT'
     +,           'NFIL','LINX','LINY','LINZ'/
*
      N = -1
      CALL HPLOPT(C(1:4),N)
      IOPT = 0
      DO 10 I=1,NBOPT
         IF(C(1:4).EQ.KTRUE(I))THEN
            IOPT = I
            GOTO 20
         ENDIF
   10 CONTINUE
      RETURN
   20 IF(IQUEST(11).NE.0)THEN
         C(1:4) = KTRUE(IOPT)
      ELSE
         C(1:4) = KFALSE(IOPT)
      ENDIF
      END

*
      SUBROUTINE GETTP(THETA,PHI)
#include "paw/pawnpu.inc"
*
      THETA=ANGLE1
      PHI=ANGLE2
*
      END
*
***************************************************************************
*  Get the nb of points for a 3D plot
***************************************************************************
*
      SUBROUTINE GETNBINS(NX,NY,NZ)
*
*
      COMMON/PAWNPU/NPFUNC,NPFUNX,NPFUNY,NPFUNZ,ANGLE1,ANGLE2
     +,             RANGX1,RANGX2,RANGY1,RANGY2,RANGZ1,RANGZ2
     +,             VALHQF
*
      NX = NPFUNX
      NY = NPFUNY
      NZ = NPFUNZ
*
      END
*

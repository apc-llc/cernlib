*
* $Id: igphst.F,v 1.1.1.1 1996/02/14 13:10:37 mclareni Exp $
*
* $Log: igphst.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:37  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.14/00 20/02/92  17.03.21  by  O.Couet
*-- Author : G.Richards
      SUBROUTINE IGPHST(RMIN,RMAX,THETA,PHI,UP,CHOPT)
********************************************************************
*     Initalises (start) PHIGS and sets the structure id
*     sphere_id - structure id
*     NEW - to delelte or not to delete
********************************************************************
*
#if defined(CERNLIB_PHIGS)
#include "higz/hiatt.inc"
      CHARACTER*(*)  CHOPT
      REAL RMIN(3),RMAX(3)
      PARAMETER (PI=3.14159)
      INTEGER         NSNO
      REAL           V(4,4)
      COMMON /NAMESET/ NSNO
*.______________________________________
*
      IF (IDIM.EQ.3) THEN
         RTHETA=THETA/180.*PI
         RPHI=PHI/180.*PI
         RUP=UP/180.*PI
         IF (CHOPT.NE.'S') THEN
            CALL IGPARF(12,'D')
            CALL POPST (101)
            CALL PADS (1,1)
            CALL PSVWI (1)
            CALL PLB (0)
            CALL PSIS (1)
* Set label to locate the transformation matrix
            CALL PLB   (1)
* Set global matrix transformation
            SLX=(RMAX(1)-RMIN(1))
            SLY=(RMAX(2)-RMIN(2))
            SLZ=(RMAX(3)-RMIN(3))
            SLTX=-RMIN(1)/SLX
            SLTY=-RMIN(2)/SLY
            SLTZ=-RMIN(3)/SLZ
            MIDX=0.0
            MIDY=0.0
            MIDZ=0.0
            CALL PBLTM3
     +      (MIDX,MIDY,MIDZ,SLTX,SLTY,SLTZ,0.0,0.0,0.0,
     +      1/SLX,1/SLY,1/SLZ,IERR,V)
            CALL PSLMT3(V,0)
* Set default attributes
            CALL PSMKSC(.5)
            CALL PSMK(1)
            CALL PSPLCI(1)
* Set annotation text attributes
            CALL PSTXFN(-3)
            CALL PSTXCI(1)
            CALL PSATCH(.009)
            CALL PSCHXP(1.)
            CALL PSATCU(0.,1.)
            CALL PSATP(0)
* Set edge attributes
            CALL PLB (2)
            CALL PSEDFG (PON)
            CALL PCLST
            NSNO=3
         ELSE
            CALL IGPARF(12,'R')
         ENDIF
      ENDIF
#endif
      END

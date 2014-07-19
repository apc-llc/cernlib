*
* $Id: ihdfr2.F,v 1.1.1.1 1996/02/14 13:10:49 mclareni Exp $
*
* $Log: ihdfr2.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:49  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.19/05 26/08/93  09.34.08  by  O.Couet
*-- Author :
      SUBROUTINE IHDFR2(ICODES,XYZ,NP,IFACE,TT)
************************************************************************
*                                                                      *
*     IHDFR2                                     Date:    19.05.93     *
*     Author: E. Chernyaev (IHEP/Protvino)       Revised:              *
*                                                                      *
*     Function: Draw face - 2nd variant for "RASTER SCREEN" algorithm  *
*               (draw face for stacked lego plot)                      *
*                                                                      *
*     References: IHWTON, IHRLIN, IHRFIL                               *
*                                                                      *
*     Input: ICODES(*) - set of codes for the line (not used)          *
*              ICODES(1) - IX                                          *
*              ICODES(2) - IY                                          *
*              ICODES(3) - line code (N of lego)                       *
*            XYZ(3,*)  - coordinates of nodes                          *
*            NP        - number of nodes                               *
*            IFACE(NP) - face                                          *
*            TT(NP)    - additional function defined on this face      *
*                        (not used in this routine)                    *
*                                                                      *
************************************************************************
#include "higz/hcscrn.inc"
#include "higz/hihid.inc"
      REAL              XYZ(3,*),P(3),PP(2,12),TT(*),X(2),Y(2)
      INTEGER           ICODES(*),IFACE(*)
*-
**          C O P Y   P O I N T S   T O   A R R A Y
*
      DO 100 I=1,NP
        K      = IFACE(I)
        IF(K.LT.0)K = -K
        CALL IHWTON(XYZ(1,K),P)
        PP(1,I) = P(1)
        PP(2,I) = P(2)
  100   CONTINUE
*
**          D R A W   F A C E
*
      ICOL   = ICODES(3)
      IF(ICOL.NE.0)THEN
         CALL ISPLCI(ISTCOL(ICOL))
      ELSE
         CALL ISPLCI(1)
      ENDIF
      DO 320 I=1,NP
        IF(IFACE(I).LT.0)GOTO 320
        I1     = I
        I2     = I + 1
        IF (I .EQ. NP)           I2 = 1
        CALL IHRLIN(PP(1,I1),PP(1,I2),NTMAX,NT,T)
        XDEL   = PP(1,I2) - PP(1,I1)
        YDEL   = PP(2,I2) - PP(2,I1)
        DO 310 IT=1,NT
          X(1) = PP(1,I1) + XDEL*T(1,IT)
          Y(1) = PP(2,I1) + YDEL*T(1,IT)
          X(2) = PP(1,I1) + XDEL*T(2,IT)
          Y(2) = PP(2,I1) + YDEL*T(2,IT)
          CALL IPL(2,X,Y)
  310   CONTINUE
  320 CONTINUE
*
**          M O D I F Y    R A S T E R   S C R E E N
*
      CALL IHRFIL(NP,PP)
*
      END

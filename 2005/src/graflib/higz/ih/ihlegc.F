*
* $Id: ihlegc.F,v 1.1.1.1 1996/02/14 13:10:50 mclareni Exp $
*
* $Log: ihlegc.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:50  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.18/00 20/04/93  14.20.33  by  O.Couet
*-- Author :
      SUBROUTINE IHLEGC(ANG,NX,NY,FUN,DRFACE,CHOPT)
************************************************************************
*                                                                      *
*     IHLEGC                                     Date:    17.09.90     *
*     Author: E. Chernyaev (IHEP/Protvino)       Revised: 15.04.93     *
*                                                                      *
*     Function: Draw stack of lego-plots in cartesian coordinates      *
*                                                                      *
*     References: IHWZN                                                *
*                                                                      *
*     Input: ANG      - angle between X ang Y                          *
*            NX       - number of cells along X                        *
*            NY       - number of cells along Y                        *
*                                                                      *
*            FUN(IX,IY,NV,XY,V,T) - external routine                   *
*              IX     - X number of the cell                           *
*              IY     - Y number of the cell                           *
*              NV     - number of values for given cell                *
*              XY(2,4)- coordinates of the cell corners                *
*              V(NV)  - cell values                                    *
*              T(4,NV)- additional function (for example: temperature) *
*                                                                      *
*            DRFACE(ICODES,XYZ,NP,IFACE,T) - routine for face drawing  *
*              ICODES(*) - set of codes for this line                  *
*                ICODES(1) - IX                                        *
*                ICODES(2) - IY                                        *
*                ICODES(3) - IV                                        *
*                ICODES(4) - side: 1-face,2-right,3-back,4-left,       *
*                                  5-bottom, 6-top                     *
*                XYZ(3,*)  - coordinates of nodes                      *
*                NP        - number of nodes                           *
*                IFACE(NP) - face                                      *
*                T(4)   - additional function (here Z-coordinate)      *
*                                                                      *
*            CHOPT - options: 'BF' - from BACK to FRONT                *
*                             'FB' - from FRONT to BACK                *
*                                                                      *
************************************************************************
#include "higz/hcscrn.inc"
#include "higz/pnvmax.inc"
      REAL              XY(2,4),XYZ(3,8),V(NVMAX),TT(4,NVMAX),TFACE(4)
      INTEGER           ICODES(4),IFACE(4),IVIS(4)
      CHARACTER*(*)     CHOPT
      EXTERNAL          FUN,DRFACE
*-
      RAD    = ATAN(1.)*4./180.
      SINA   = SIN(ANG*RAD)
      COSA   = COS(ANG*RAD)
*
**          F I N D   T H E   M O S T   L E F T   P O I N T
*
      I1     = 1
      IF (TN(1,1) .LT. 0.)                      I1 = 2
      IF (TN(1,1)*COSA+TN(2,1)*SINA .LT. 0.)    I1 = 5 - I1
*
**          D E F I N E   O R D E R   O F   D R A W I N G
*
      IF (CHOPT(1:1).EQ.'B' .OR. CHOPT(1:1).EQ.'b') THEN
        INCRX  =-1
        INCRY  =-1
      ELSE
        INCRX  =+1
        INCRY  =+1
      END IF
      IF (I1.EQ.1 .OR. I1.EQ.2)                 INCRX =-INCRX
      IF (I1.EQ.2 .OR. I1.EQ.3)                 INCRY =-INCRY
      IX1    = 1
      IY1    = 1
      IF (INCRX .LT. 0)                         IX1 = NX
      IF (INCRY .LT. 0)                         IY1 = NY
      IX2    = NX - IX1 + 1
      IY2    = NY - IY1 + 1
*
**          F I N D   V I S I B I L I T Y   O F   S I D E S
*
      IVIS(1) = 0
      IVIS(2) = 0
      IVIS(3) = 0
      IVIS(4) = 0
      CALL IHWZN(0.,1.,0.,ZN)
      IF (ZN .LT. 0.)                           IVIS(1) = 1
      IF (ZN .GT. 0.)                           IVIS(3) = 1
      CALL IHWZN(SINA,COSA,0.,ZN)
      IF (ZN .GT. 0.)                           IVIS(2) = 1
      IF (ZN .LT. 0.)                           IVIS(4) = 1
*
**          D R A W   S T A C K   O F   L E G O - P L O T S
*
      DO 320 IY=IY1,IY2,INCRY
        DO 310 IX=IX1,IX2,INCRX
          CALL FUN(IX,IY,NV,XY,V,TT)
          IF (NV.LT.2 .OR. NV.GT.NVMAX) GOTO 310
          ICODES(1) = IX
          ICODES(2) = IY
          DO 110 I=1,4
            XYZ(1,I)   = XY(1,I) + XY(2,I)*COSA
            XYZ(2,I)   = XY(2,I)*SINA
            XYZ(1,I+4) = XYZ(1,I)
            XYZ(2,I+4) = XYZ(2,I)
  110     CONTINUE
*           D R A W   S T A C K
          DO 140 IV=1,NV-1
            DO 120 I=1,4
              XYZ(3,I)   = V(IV)
              XYZ(3,I+4) = V(IV+1)
  120       CONTINUE
            IF (V(IV) .EQ. V(IV+1))     GOTO 140
            ICODES(3) = IV
            DO 130 I=1,4
              IF (IVIS(I) .EQ. 0)       GOTO 130
              K1     = I
              K2     = I + 1
              IF (I .EQ. 4)             K2 = 1
              ICODES(4) = K1
              IFACE(1)  = K1
              IFACE(2)  = K2
              IFACE(3)  = K2 + 4
              IFACE(4)  = K1 + 4
              TFACE(1)  = TT(K1,IV)
              TFACE(2)  = TT(K2,IV)
              TFACE(3)  = TT(K2,IV+1)
              TFACE(4)  = TT(K1,IV+1)
              CALL DRFACE(ICODES,XYZ,4,IFACE,TFACE)
  130       CONTINUE
  140     CONTINUE
*           D R A W   B O T T O M   F A C E
          CALL IHWZN(0.,0.,1.,ZN)
          IF (ZN .LT. 0.) THEN
            ICODES(3) = 1
            ICODES(4) = 5
            DO 210 I=1,4
              XYZ(3,I) = V(1)
              IFACE(I) = 5 - I
              TFACE(I) = TT(5-I,1)
  210       CONTINUE
            CALL DRFACE(ICODES,XYZ,4,IFACE,TFACE)
          END IF
*           D R A W   T O P   F A C E
          IF (ZN .GT. 0.) THEN
            ICODES(3) = NV - 1
            ICODES(4) = 6
            DO 220 I=1,4
              IFACE(I) = I + 4
              TFACE(I) = TT(I,NV)
  220       CONTINUE
            CALL DRFACE(ICODES,XYZ,4,IFACE,TFACE)
          END IF
  310   CONTINUE
  320 CONTINUE
      RETURN
      END

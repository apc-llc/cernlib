*
* $Id: ihzdep.F,v 1.1.1.1 1996/02/14 13:10:54 mclareni Exp $
*
* $Log: ihzdep.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:54  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.19/06 27/08/93  14.16.47  by  O.Couet
*-- Author :
      SUBROUTINE IHZDEP(XYZ,NFACE,IFACE,DFACE,ABCD,IORDER)
************************************************************************
*                                                                      *
*     IHZDEP                                     Date:    26.07.93     *
*     Author: E. Chernyaev (IHEP/Protvino)       Revised:              *
*                                                                      *
*     Function: Z-depth algorithm for set of triangles                 *
*                                                                      *
*     Input: XYZ(3,*)   - nodes                                        *
*            NFACE      - number of triangular faces                   *
*            IFACE(3,*) - faces (triangles)                            *
*                                                                      *
*     Arrays: DFACE(6,*) - array for min-max scopes                    *
*             ABCD(4,*)  - array for face plane equations              *
*                                                                      *
*     Output: IORDER(*) - face order                                   *
*                                                                      *
************************************************************************
      PARAMETER         (DEL = 0.0001)
      INTEGER           IFACE(3,*),IORDER(*),NN(3),KK(3)
      REAL              XYZ(3,*),DFACE(6,*),ABCD(4,*)
      REAL              V(3,2),ABCDN(4),ABCDK(4)
      EQUIVALENCE       (N1,NN(1)),(N2,NN(2)),(N3,NN(3))
      EQUIVALENCE       (K1,KK(1)),(K2,KK(2)),(K3,KK(3))
*-
**          S E T   I N I T I A L   O R D E R
**          I G N O R E   V E R Y   S M A L L   F A C E S
**          S E T   M I N - M A X   S C O P E S
**          S E T   F A C E   P L A N E   E Q U A T I O N S
*
      NF     = 0
      DO 130 N=1,NFACE
        I1     = IABS(IFACE(1,N))
        I2     = IABS(IFACE(2,N))
        I3     = IABS(IFACE(3,N))
*           A R E A   T E S T
        IF (ABS(XYZ(1,I2)-XYZ(1,I1)).LE.DEL .AND.
     &      ABS(XYZ(2,I2)-XYZ(2,I1)).LE.DEL .AND.
     &      ABS(XYZ(3,I2)-XYZ(3,I1)).LE.DEL)    GOTO 130
        IF (ABS(XYZ(1,I3)-XYZ(1,I2)).LE.DEL .AND.
     &      ABS(XYZ(2,I3)-XYZ(2,I2)).LE.DEL .AND.
     &      ABS(XYZ(3,I3)-XYZ(3,I2)).LE.DEL)    GOTO 130
        IF (ABS(XYZ(1,I1)-XYZ(1,I3)).LE.DEL .AND.
     &      ABS(XYZ(2,I1)-XYZ(2,I3)).LE.DEL .AND.
     &      ABS(XYZ(3,I1)-XYZ(3,I3)).LE.DEL)    GOTO 130
*           P R O J E C T I O N   T E S T
        IF (ABS(XYZ(1,I2)-XYZ(1,I1)).LE.DEL .AND.
     &      ABS(XYZ(2,I2)-XYZ(2,I1)).LE.DEL .AND.
     &      ABS(XYZ(1,I3)-XYZ(1,I2)).LE.DEL .AND.
     &      ABS(XYZ(2,I3)-XYZ(2,I2)).LE.DEL .AND.
     &      ABS(XYZ(1,I1)-XYZ(1,I3)).LE.DEL .AND.
     &      ABS(XYZ(2,I1)-XYZ(2,I3)).LE.DEL)    GOTO 130
        NF     = NF + 1
        IORDER(NF) = N
*           F I N D   M I N - M A X
        DO 110 I=1,3
          WMIN = XYZ(I,I1)
          WMAX = XYZ(I,I1)
          IF (WMIN .GT. XYZ(I,I2))      WMIN = XYZ(I,I2)
          IF (WMAX .LT. XYZ(I,I2))      WMAX = XYZ(I,I2)
          IF (WMIN .GT. XYZ(I,I3))      WMIN = XYZ(I,I3)
          IF (WMAX .LT. XYZ(I,I3))      WMAX = XYZ(I,I3)
          DFACE(I,N)   = WMIN
          DFACE(I+3,N) = WMAX
  110   CONTINUE
*          F I N D   F A C E   E Q U A T I O N
        DO 120 I=1,3
          V(I,1) = XYZ(I,I2) - XYZ(I,I1)
          V(I,2) = XYZ(I,I3) - XYZ(I,I2)
  120   CONTINUE
        A      = (V(2,1)*V(3,2) - V(3,1)*V(2,2))
        B      = (V(3,1)*V(1,2) - V(1,1)*V(3,2))
        C      = (V(1,1)*V(2,2) - V(2,1)*V(1,2))
        Q      = SQRT(A*A+B*B+C*C)
        IF (C .LT. 0.)                  Q =-Q
        A      = A / Q
        B      = B / Q
        C      = C / Q
        ABCD(1,N) = A
        ABCD(2,N) = B
        ABCD(3,N) = C
        ABCD(4,N) =-(A*XYZ(1,I1) + B*XYZ(2,I1) + C*XYZ(3,I1))
  130 CONTINUE
      NFACE  = NF
      IF (NF .LE. 1)                    GOTO 999
*
**          S O R T   T R I A N G L E S   A L O N G   Z - M I N
*
      DO 220 ICUR=2,NFACE
        K      = IORDER(ICUR)
        ZCUR   = DFACE(3,K)
        DO 210 ITST=ICUR-1,1,-1
          K      = IORDER(ITST)
          IF (ZCUR .LT. DFACE(3,K))     GOTO 220
          K      = IORDER(ITST)
          IORDER(ITST) = IORDER(ITST+1)
          IORDER(ITST+1) = K
  210   CONTINUE
  220 CONTINUE
*
**          Z - D E P T H   A L G O R I T H M
*
      KFACE  = NFACE
  300 IF (KFACE .EQ. 1)                 GOTO 900
      NF     = IORDER(KFACE)
      IF (NF .LT. 0)                    NF =-NF
      ABCDN(1) = ABCD(1,NF)
      ABCDN(2) = ABCD(2,NF)
      ABCDN(3) = ABCD(3,NF)
      ABCDN(4) = ABCD(4,NF)
      N1       = IABS(IFACE(1,NF))
      N2       = IABS(IFACE(2,NF))
      N3       = IABS(IFACE(3,NF))
*
**          I N T E R N A L   L O O P
*
      DO 800 K=KFACE-1,1,-1
        KF     = IORDER(K)
        IF (KF .LT. 0)                  KF =-KF
        IF (DFACE(6,NF) .GT. DFACE(3,KF)+DEL)   GOTO 400
        IF (IORDER(K) .GT. 0)                   GOTO 900
        GOTO 800
*
**          M I N - M A X   T E S T
*
  400   IF (DFACE(1,KF) .GE. DFACE(4,NF)-DEL)   GOTO 800
        IF (DFACE(4,KF) .LE. DFACE(1,NF)+DEL)   GOTO 800
        IF (DFACE(2,KF) .GE. DFACE(5,NF)-DEL)   GOTO 800
        IF (DFACE(5,KF) .LE. DFACE(2,NF)+DEL)   GOTO 800
*
**          K F   B E F O R E   N F ?
*
        K1       = IABS(IFACE(1,KF))
        K2       = IABS(IFACE(2,KF))
        K3       = IABS(IFACE(3,KF))
        IF (ABCDN(1)*XYZ(1,K1)+ABCDN(2)*XYZ(2,K1)+
     &      ABCDN(3)*XYZ(3,K1)+ABCDN(4) .LT. -DEL)      GOTO 500
        IF (ABCDN(1)*XYZ(1,K2)+ABCDN(2)*XYZ(2,K2)+
     &      ABCDN(3)*XYZ(3,K2)+ABCDN(4) .LT. -DEL)      GOTO 500
        IF (ABCDN(1)*XYZ(1,K3)+ABCDN(2)*XYZ(2,K3)+
     &      ABCDN(3)*XYZ(3,K3)+ABCDN(4) .LT. -DEL)      GOTO 500
        GOTO 800
*
**          N F    A F T E R    K F ?
*
  500   ABCDK(1) = ABCD(1,KF)
        ABCDK(2) = ABCD(2,KF)
        ABCDK(3) = ABCD(3,KF)
        ABCDK(4) = ABCD(4,KF)
        IF (ABCDK(1)*XYZ(1,N1)+ABCDK(2)*XYZ(2,N1)+
     &      ABCDK(3)*XYZ(3,N1)+ABCDK(4) .GT. DEL)       GOTO 600
        IF (ABCDK(1)*XYZ(1,N2)+ABCDK(2)*XYZ(2,N2)+
     &      ABCDK(3)*XYZ(3,N2)+ABCDK(4) .GT. DEL)       GOTO 600
        IF (ABCDK(1)*XYZ(1,N3)+ABCDK(2)*XYZ(2,N3)+
     &      ABCDK(3)*XYZ(3,N3)+ABCDK(4) .GT. DEL)       GOTO 600
        GOTO 800
*
**          E D G E   B Y   E D G E   T E S T
**          K F - E D G E S   A G A I N S T   N F
*
  600   DO 610 I=1,3
          I1      = KK(I)
          I2      = KK(1)
          IF (I .NE. 3)                 I2 = KK(I+1)
          CALL IHZTST(DEL,XYZ,I1,I2,NN,ABCDN,IREP)
          IF (IREP) 700,610,800
  610   CONTINUE
**          N F - E D G E S   A G A I N S T   K F
        DO 620 I=1,3
          I1      = NN(I)
          I2      = NN(1)
          IF (I .NE. 3)                 I2 = NN(I+1)
          CALL IHZTST(DEL,XYZ,I1,I2,KK,ABCDK,IREP)
          IF (IREP) 800,620,700
  620   CONTINUE
        GOTO 800
*
**          C H A N G E   F A C E   O R D E R
*
  700   KF     = IORDER(K)
        DO 710 I=K+1,KFACE
          IORDER(I-1) = IORDER(I)
  710   CONTINUE
        IORDER(KFACE) =-KF
        IF (KF .GT. 0)                  GOTO 300
*        WRITE(*,*) 'IHZDEP: loop'
        GOTO 900
  800 CONTINUE
*
**          N E X T   F A C E
*
  900 IF (IORDER(KFACE) .LT. 0)         IORDER(KFACE) =-IORDER(KFACE)
      KFACE  = KFACE - 1
      IF (KFACE .GT. 0)                 GOTO 300
*
  999 RETURN
      END

*
* $Id: ihlegs.F,v 1.1.1.1 1996/02/14 13:10:50 mclareni Exp $
*
* $Log: ihlegs.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:50  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.18/01 04/05/93  14.11.01  by  O.Couet
*-- Author :
      SUBROUTINE IHLEGS(IPSDR,IORDR,NA,NB,FUN,DRFACE,CHOPT)
************************************************************************
*                                                                      *
*     IHLEGS                                     Date:    11.11.90     *
*     Author: E. Chernyaev (IHEP/Protvino)       Revised: 13.04.93     *
*                                                                      *
*     Function: Draw stack of lego-plots spheric coordinates           *
*                                                                      *
*     References: IHWPHI, IHWTH, IHWZN                                 *
*                                                                      *
*     Input: IPSDR - pseudo-rapidity flag                              *
*            IORDR - order of variables (0 - THETA,PHI; 1 - PHI,THETA) *
*            NA    - number of steps along 1st variable                *
*            NB    - number of steps along 2nd variable                *
*                                                                      *
*            FUN(IA,IB,NV,AB,V,TT) - external routine                  *
*              IA      - cell number for 1st variable                  *
*              IB      - cell number for 2nd variable                  *
*              NV      - number of values for given cell               *
*              AB(2,4) - coordinates of the cell corners               *
*              V(NV)   - cell values                                   *
*              TT(4,*) - additional function                           *
*                                                                      *
*            DRFACE(ICODES,XYZ,NP,IFACE,T) - routine for face drawing  *
*              ICODES(*) - set of codes for this face                  *
*                ICODES(1) - IA                                        *
*                ICODES(2) - IB                                        *
*                ICODES(3) - IV                                        *
*                ICODES(4) - side: 1,2,3,4 - ordinary sides            *
*                                  5-bottom,6-top                      *
*              XYZ(3,*)  - coordinates of nodes                        *
*              NP        - number of nodes in face                     *
*              IFACE(NP) - face                                        *
*              T(NP)     - additional function                         *
*                                                                      *
*            CHOPT       - options: 'BF' - from BACK to FRONT          *
*                                   'FB' - from FRONT to BACK          *
*                                                                      *
************************************************************************
#include "higz/hcphi.inc"
#include "higz/pnvmax.inc"
      REAL              AB(2,4),XYZ(3,8),V(NVMAX)
      REAL              TT(4,NVMAX),TFACE(4)
      REAL              COSTH(4),SINTH(4),COSPHI(4),SINPHI(4)
      INTEGER           ICODES(4),IFACE(4),IVIS(6)
      CHARACTER*(*)     CHOPT
      EXTERNAL          FUN,DRFACE
*-
      IF (IORDR .EQ. 0) THEN
        JTH    = 1
        JPHI   = 2
        NTH    = NA
        NPHI   = NB
      ELSE
        JTH    = 2
        JPHI   = 1
        NTH    = NB
        NPHI   = NA
      END IF
      IF (NTH  .GT. NPMAX)              GOTO 997
      IF (NPHI .GT. NPMAX)              GOTO 998
      RAD    = ATAN(1.)*4./180.
      IOPT   = 2
      IF (CHOPT(1:1).EQ.'B' .OR. CHOPT(1:1).EQ.'b')     IOPT = 1
*
**       P R E P A R E   P H I   A R R A Y
**       F I N D    C R I T I C A L   P H I   S E C T O R S
*
      KPHI   = NPHI
      MTH    = NTH/2
      IF (MTH .EQ. 0)                   MTH = 1
      IF (IORDR .EQ. 0)                 IA = MTH
      IF (IORDR .NE. 0)                 IB = MTH
      DO 10 I=1,NPHI
        IF (IORDR .EQ. 0)               IB = I
        IF (IORDR .NE. 0)               IA = I
        CALL FUN(IA,IB,NV,AB,V,TT)
        IF (I .EQ. 1)                   APHI(1)   = AB(JPHI,1)
        APHI(I)   = (APHI(I) + AB(JPHI,1))/2.
        APHI(I+1) = AB(JPHI,3)
   10 CONTINUE
      CALL IHWPHI(IOPT,KPHI,APHI,IPHI1,IPHI2)
*
**       P R E P A R E   T H E T A   A R R A Y
*
      IF (IORDR .EQ. 0)                 IB = 1
      IF (IORDR .NE. 0)                 IA = 1
      DO 20 I=1,NTH
        IF (IORDR .EQ. 0)               IA = I
        IF (IORDR .NE. 0)               IB = I
        CALL FUN(IA,IB,NV,AB,V,TT)
        IF (I .EQ. 1)                   APHI(1) = AB(JTH,1)
        APHI(I)   = (APHI(I) + AB(JTH,1))/2.
        APHI(I+1) = AB(JTH,3)
   20 CONTINUE
*
**       D R A W   S T A C K   O F   L E G O - P L O T S
*
      KTH    = NTH
      INCR   = 1
      IPHI   = IPHI1
  100 IF (IPHI .GT. NPHI)               GOTO 500
*        F I N D    C R I T I C A L   T H E T A   S E C T O R S
      IF (IORDR .EQ. 0) THEN
        IA     = MTH
        IB     = IPHI
      ELSE
        IA     = IPHI
        IB     = MTH
      END IF
      CALL FUN(IA,IB,NV,AB,V,TT)
      PHI    = (AB(JPHI,1) + AB(JPHI,3)) / 2.
      CALL IHWTH(IOPT,PHI,KTH,APHI,ITH1,ITH2)
      INCRTH = 1
      ITH    = ITH1
  200 IF (ITH .GT. NTH)                 GOTO 400
      IF (IORDR .EQ. 0)                 IA = ITH
      IF (IORDR .NE. 0)                 IB = ITH
      CALL FUN(IA,IB,NV,AB,V,TT)
      IF (NV.LT.2 .OR. NV.GT.NVMAX)     GOTO 400
*        D E F I N E   V I S I B I L I T Y   O F   S I D E S
      DO 300 I=1,6
        IVIS(I) = 0
  300 CONTINUE
      PHI1   = RAD * AB(JPHI,1)
      PHI2   = RAD * AB(JPHI,3)
      TH1    = RAD * AB(JTH,1)
      TH2    = RAD * AB(JTH,3)
      CALL IHWZN(SIN(PHI1),-COS(PHI1),0.,ZN)
      IF (ZN .GT. 0.)                   IVIS(2) = 1
      CALL IHWZN(-SIN(PHI2),COS(PHI2),0.,ZN)
      IF (ZN .GT. 0.)                   IVIS(4) = 1
      PHI    = (PHI1 + PHI2) / 2.
      CALL IHWZN(-COS(PHI)*COS(TH1),-SIN(PHI)*COS(TH1),SIN(TH1),ZN)
      IF (ZN .GT. 0.)                   IVIS(1) = 1
      CALL IHWZN(COS(PHI)*COS(TH2),SIN(PHI)*COS(TH2),-SIN(TH2),ZN)
      IF (ZN .GT. 0.)                   IVIS(3) = 1
      TH     = (TH1 + TH2) / 2.
      IF (IPSDR .EQ. 1)                               TH = RAD * 90.
      CALL IHWZN(COS(PHI)*SIN(TH),SIN(PHI)*SIN(TH),COS(TH),ZN)
      IF (ZN .LT. 0.)                   IVIS(5) = 1
      IF (ZN .GT. 0.)                   IVIS(6) = 1
*        D R A W   S T A C K
      ICODES(1) = IA
      ICODES(2) = IB
      DO 310 I=1,4
        J          = I
        IF (IORDR.NE.0 .AND. I.EQ.2)    J = 4
        IF (IORDR.NE.0 .AND. I.EQ.4)    J = 2
        COSTH(J)  = COS(RAD*AB(JTH,I))
        SINTH(J)  = SIN(RAD*AB(JTH,I))
        COSPHI(J) = COS(RAD*AB(JPHI,I))
        SINPHI(J) = SIN(RAD*AB(JPHI,I))
  310 CONTINUE
      DO 350 IV=1,NV-1
        IF (IPSDR .EQ. 1) THEN
          DO 320 I=1,4
            XYZ(1,I)   = V(IV)   * COSPHI(I)
            XYZ(2,I)   = V(IV)   * SINPHI(I)
            XYZ(3,I)   = V(IV)   * COSTH(I) / SINTH(I)
            XYZ(1,I+4) = V(IV+1) * COSPHI(I)
            XYZ(2,I+4) = V(IV+1) * SINPHI(I)
            XYZ(3,I+4) = V(IV+1) * COSTH(I) / SINTH(I)
  320     CONTINUE
        ELSE
          DO 330 I=1,4
            XYZ(1,I)   = V(IV)   * SINTH(I) * COSPHI(I)
            XYZ(2,I)   = V(IV)   * SINTH(I) * SINPHI(I)
            XYZ(3,I)   = V(IV)   * COSTH(I)
            XYZ(1,I+4) = V(IV+1) * SINTH(I) * COSPHI(I)
            XYZ(2,I+4) = V(IV+1) * SINTH(I) * SINPHI(I)
            XYZ(3,I+4) = V(IV+1) * COSTH(I)
  330     CONTINUE
        END IF
        IF (V(IV) .GE. V(IV+1))         GOTO 350
        ICODES(3) = IV
        DO 340 I=1,4
          IF (IVIS(I) .EQ. 0)           GOTO 340
          K1        = I - 1
          IF (I .EQ. 1)                 K1 = 4
          K2        = I
          IFACE(1)  = K1
          IFACE(2)  = K2
          IFACE(3)  = K2 + 4
          IFACE(4)  = K1 + 4
          TFACE(1)  = TT(K1,IV)
          TFACE(2)  = TT(K2,IV)
          TFACE(3)  = TT(K2,IV+1)
          TFACE(4)  = TT(K1,IV+1)
          ICODES(4) = I
          CALL DRFACE(ICODES,XYZ,4,IFACE,TFACE)
  340   CONTINUE
  350 CONTINUE
*        D R A W   B O T T O M   F A C E
      IF (IVIS(5).NE.0 .AND. V(1).GT.0.) THEN
        ICODES(3) = 1
        ICODES(4) = 5
        DO 360 I=1,4
          IF (IPSDR .EQ. 1) THEN
            XYZ(1,I) = V(1) * COSPHI(I)
            XYZ(2,I) = V(1) * SINPHI(I)
            XYZ(3,I) = V(1) * COSTH(I) / SINTH(I)
          ELSE
            XYZ(1,I) = V(1) * SINTH(I) * COSPHI(I)
            XYZ(2,I) = V(1) * SINTH(I) * SINPHI(I)
            XYZ(3,I) = V(1) * COSTH(I)
          ENDIF
          IFACE(I) = 5 - I
          TFACE(I) = TT(5-I,1)
  360   CONTINUE
        CALL DRFACE(ICODES,XYZ,4,IFACE,TFACE)
      END IF
*        D R A W   T O P   F A C E
      IF (IVIS(6).NE.0 .AND. V(NV).GT.0.) THEN
        ICODES(3) = NV - 1
        ICODES(4) = 6
        DO 370 I=1,4
          IFACE(I) = I+4
          TFACE(I) = TT(I+4,NV)
  370   CONTINUE
        CALL DRFACE(ICODES,XYZ,4,IFACE,TFACE)
      END IF
*        N E X T   T H E T A
  400 ITH    = ITH + INCRTH
      IF (ITH .EQ. 0)                   ITH = KTH
      IF (ITH .GT. KTH)                 ITH = 1
      IF (ITH .NE. ITH2)                GOTO 200
      IF (INCRTH) 410,500,420
  410 INCRTH = 0
      GOTO 200
  420 INCRTH =-1
      ITH    = ITH1
      GOTO 400
*        N E X T   P H I
  500 IPHI   = IPHI + INCR
      IF (IPHI .EQ. 0)                  IPHI = KPHI
      IF (IPHI .GT. KPHI)               IPHI = 1
      IF (IPHI .NE. IPHI2)              GOTO 100
      IF (INCR) 510,999,520
  510 INCR   = 0
      GOTO 100
  520 INCR   =-1
      IPHI   = IPHI1
      GOTO 500
*
  997 WRITE(*,*) 'IHLEGS: too many THETA sectors'
      GOTO 999
  998 WRITE(*,*) 'IHLEGS: too many PHI sectors'
      GOTO 999
  999 RETURN
      END

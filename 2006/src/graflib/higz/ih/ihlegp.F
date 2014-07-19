*
* $Id: ihlegp.F,v 1.1.1.1 1996/02/14 13:10:50 mclareni Exp $
*
* $Log: ihlegp.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:50  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.18/01 04/05/93  14.11.01  by  O.Couet
*-- Author :
      SUBROUTINE IHLEGP(IORDR,NA,NB,FUN,DRFACE,CHOPT)
************************************************************************
*                                                                      *
*     IHLEGP                                     Date:    04.11.90     *
*     Author: E. Chernyaev (IHEP/Protvino)       Revised: 03.03.93     *
*                                                                      *
*     Function: Draw stack of lego-plots in polar coordinates          *
*                                                                      *
*     References: IHWPHI, IHWVR1, IHWVR2, IHWZN                        *
*                                                                      *
*     Input: IORDR - order of variables (0 - R,PHI; 1 - PHI,R)         *
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
*                ICODES(4) - side: 1-internal,2-right,3-external,4-left*
*                                  5-bottom, 6-top                     *
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
      REAL              AB(2,4),XYZ(3,8),V(NVMAX),TT(4,NVMAX),TFACE(4)
      INTEGER           ICODES(4),IFACE(4),IVIS(6)
      CHARACTER*(*)     CHOPT
      EXTERNAL          FUN,DRFACE
*-
      IF (IORDR .EQ. 0) THEN
        JR     = 1
        JPHI   = 2
        NR     = NA
        NPHI   = NB
      ELSE
        JR     = 2
        JPHI   = 1
        NR     = NB
        NPHI   = NA
      END IF
      IF (NPHI .GT. NPMAX)                              GOTO 998
      RAD    = ATAN(1.)*4./180.
      IOPT   = 2
      IF (CHOPT(1:1).EQ.'B' .OR. CHOPT(1:1).EQ.'b')     IOPT = 1
*
**       P R E P A R E   P H I   A R R A Y
**       F I N D    C R I T I C A L   S E C T O R S
*
      KPHI   = NPHI
      IF (IORDR .EQ. 0)                 IA = NR
      IF (IORDR .NE. 0)                 IB = NR
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
**      E N C O D E   V I S I B I L I T Y   O F   S I D E S
**      A N D   O R D E R   A L O N G   R
*
      DO 20 I=1,NPHI
        IF (IORDR .EQ. 0)               IB = I
        IF (IORDR .NE. 0)               IA = I
        CALL FUN(IA,IB,NV,AB,V,TT)
        CALL IHWVR1(IOPT,AB(JPHI,1)*RAD,AB(JPHI,3)*RAD,APHI(I))
   20 CONTINUE
*
**       D R A W   S T A C K   O F   L E G O - P L O T S
*
      INCR   = 1
      IPHI   = IPHI1
  100 IF (IPHI .GT. NPHI)               GOTO 300
*       D E C O D E   V I S I B I L I T Y   O F   S I D E S
      CALL IHWVR2(APHI(IPHI),
     &            IVIS(1),IVIS(2),IVIS(3),IVIS(4),IVIS(5),IVIS(6),INCRR)
      IR1    = 1
      IF (INCRR .LT. 0)                 IR1 = NR
      IR2    = NR - IR1 + 1
*        D R A W   L E G O S   F O R   S E C T O R
      DO 290 IR=IR1,IR2,INCRR
        IF (IORDR .EQ. 0) THEN
          IA     = IR
          IB     = IPHI
        ELSE
          IA     = IPHI
          IB     = IR
        END IF
        CALL FUN(IA,IB,NV,AB,V,TT)
        IF (NV.LT.2 .OR. NV.GT.NVMAX)   GOTO 290
        ICODES(1) = IA
        ICODES(2) = IB
        DO 210 I=1,4
          J          = I
          IF (IORDR.NE.0 .AND. I.EQ.2)  J = 4
          IF (IORDR.NE.0 .AND. I.EQ.4)  J = 2
          XYZ(1,J)   = AB(JR,I)*COS(AB(JPHI,I)*RAD)
          XYZ(2,J)   = AB(JR,I)*SIN(AB(JPHI,I)*RAD)
          XYZ(1,J+4) = XYZ(1,J)
          XYZ(2,J+4) = XYZ(2,J)
  210   CONTINUE
*        D R A W   S T A C K
        DO 240 IV=1,NV-1
          DO 220 I=1,4
            XYZ(3,I)   = V(IV)
            XYZ(3,I+4) = V(IV+1)
  220     CONTINUE
          IF (V(IV) .GE. V(IV+1))       GOTO 240
          ICODES(3) = IV
          DO 230 I=1,4
            IF (IVIS(I) .EQ. 0)         GOTO 230
            K1     = I-1
            IF (I .EQ. 1)               K1 = 4
            K2     = I
            IF (XYZ(1,K1).EQ.XYZ(1,K2) .AND.
     &          XYZ(2,K1).EQ.XYZ(2,K2)) GOTO 230
            IFACE(1) = K1
            IFACE(2) = K2
            IFACE(3) = K2 + 4
            IFACE(4) = K1 + 4
            TFACE(1) = TT(K1,IV)
            TFACE(2) = TT(K2,IV)
            TFACE(3) = TT(K2,IV+1)
            TFACE(4) = TT(K1,IV+1)
            ICODES(4)= I
            CALL DRFACE(ICODES,XYZ,4,IFACE,TFACE)
  230     CONTINUE
  240   CONTINUE
*           D R A W   B O T T O M   F A C E
        IF (IVIS(5) .NE. 0) THEN
          ICODES(3) = 1
          ICODES(4) = 5
          DO 250 I=1,4
            XYZ(3,I) = V(1)
            IFACE(I) = 5 - I
            TFACE(I) = TT(5-I,1)
  250     CONTINUE
          CALL DRFACE(ICODES,XYZ,4,IFACE,TFACE)
        END IF
*           D R A W   T O P   F A C E
        IF (IVIS(6) .NE. 0) THEN
          ICODES(3) = NV - 1
          ICODES(4) = 6
          DO 260 I=1,4
            IFACE(I) = I + 4
            TFACE(I) = TT(I,NV)
  260     CONTINUE
          CALL DRFACE(ICODES,XYZ,4,IFACE,TFACE)
        END IF
  290 CONTINUE
*        N E X T   P H I
  300 IPHI   = IPHI + INCR
      IF (IPHI .EQ. 0)                  IPHI = KPHI
      IF (IPHI .GT. KPHI)               IPHI = 1
      IF (IPHI .NE. IPHI2)              GOTO 100
      IF (INCR) 310,999,320
  310 INCR   = 0
      GOTO 100
  320 INCR   =-1
      IPHI   = IPHI1
      GOTO 300
*
  998 WRITE(*,*) 'IHLEGP: too many PHI sectors'
  999 RETURN
      END

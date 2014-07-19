*
* $Id: ihsurs.F,v 1.1.1.1 1996/02/14 13:10:54 mclareni Exp $
*
* $Log: ihsurs.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:54  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.18/01 04/05/93  15.28.34  by  O.Couet
*-- Author :
      SUBROUTINE IHSURS(IPSDR,IORDR,NA,NB,FUN,DRFACE,CHOPT)
************************************************************************
*                                                                      *
*     IHSURS                                     Date:    07.11.90     *
*     Author: E. Chernyaev (IHEP/Protvino)       Revised: 12.04.93     *
*                                                                      *
*     Function: Draw surface in spheric coordinates                    *
*                                                                      *
*     References: IHWPHI, IHWTH                                        *
*                                                                      *
*     Input: IPSDR - pseudo-rapidity flag                              *
*            IORDR - order of variables (0 - THETA,PHI; 1 - PHI,THETA) *
*            NA    - number of steps along 1st variable                *
*            NB    - number of steps along 2nd variable                *
*                                                                      *
*            FUN(IA,IB,F,T) - external routine                         *
*              IA     - cell number for 1st variable                   *
*              IB     - cell number for 2nd variable                   *
*              F(3,4) - face which corresponds to the cell             *
*                F(1,*) - A                                            *
*                F(2,*) - B                                            *
*                F(3,*) - R                                            *
*              T(4)   - additional function (for example: temperature) *
*                                                                      *
*            DRFACE(ICODES,XYZ,NP,IFACE,T) - routine for face drawing  *
*              ICODES(*) - set of codes for this face                  *
*                ICODES(1) - IA                                        *
*                ICODES(2) - IB                                        *
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
      REAL              F(3,4),XYZ(3,4),TT(4),TTT(4)
      INTEGER           IFACE(4),ICODES(2)
      CHARACTER*(*)     CHOPT
      EXTERNAL          FUN,DRFACE
      DATA              IFACE/1,2,3,4/
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
      IF (NTH  .GT. NPMAX)                              GOTO 997
      IF (NPHI .GT. NPMAX)                              GOTO 998
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
        CALL FUN(IA,IB,F,TT)
        IF (I .EQ. 1)                   APHI(1)   = F(JPHI,1)
        APHI(I)   = (APHI(I) + F(JPHI,1))/2.
        APHI(I+1) = F(JPHI,3)
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
        CALL FUN(IA,IB,F,TT)
        IF (I .EQ. 1)                   APHI(1) = F(JTH,1)
        APHI(I)   = (APHI(I) + F(JTH,1))/2.
        APHI(I+1) = F(JTH,3)
   20 CONTINUE
*
**       D R A W   S U R F A C E
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
      CALL FUN(IA,IB,F,TT)
      PHI    = (F(JPHI,1) + F(JPHI,3)) / 2.
      CALL IHWTH(IOPT,PHI,KTH,APHI,ITH1,ITH2)
      INCRTH = 1
      ITH    = ITH1
  200 IF (ITH .GT. NTH)                 GOTO 400
      IF (IORDR .EQ. 0)                 IA = ITH
      IF (IORDR .NE. 0)                 IB = ITH
      CALL FUN(IA,IB,F,TT)
      IF (IPSDR .EQ. 1) THEN
        DO 310 I=1,4
          J          = I
          IF (IORDR.NE.0 .AND. I.EQ.2)  J = 4
          IF (IORDR.NE.0 .AND. I.EQ.4)  J = 2
          XYZ(1,J) = F(3,I) * COS(F(JPHI,I)*RAD)
          XYZ(2,J) = F(3,I) * SIN(F(JPHI,I)*RAD)
          XYZ(3,J) = F(3,I) * COS(F(JTH,I)*RAD) / SIN(F(JTH,I)*RAD)
          TTT(J)   = TT(I)
  310   CONTINUE
      ELSE
        DO 320 I=1,4
          J          = I
          IF (IORDR.NE.0 .AND. I.EQ.2)  J = 4
          IF (IORDR.NE.0 .AND. I.EQ.4)  J = 2
          XYZ(1,J) = F(3,I) * SIN(F(JTH,I)*RAD) * COS(F(JPHI,I)*RAD)
          XYZ(2,J) = F(3,I) * SIN(F(JTH,I)*RAD) * SIN(F(JPHI,I)*RAD)
          XYZ(3,J) = F(3,I) * COS(F(JTH,I)*RAD)
          TTT(J)   = TT(I)
  320   CONTINUE
      END IF
      ICODES(1) = IA
      ICODES(2) = IB
      CALL DRFACE(ICODES,XYZ,4,IFACE,TTT)
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
  997 WRITE(*,*) 'IHSURS: too many THETA sectors'
      GOTO 999
  998 WRITE(*,*) 'IHSURS: too many PHI sectors'
      GOTO 999
  999 RETURN
      END

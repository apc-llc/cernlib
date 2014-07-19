*
* $Id: ihsurp.F,v 1.1.1.1 1996/02/14 13:10:54 mclareni Exp $
*
* $Log: ihsurp.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:54  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.18/01 04/05/93  14.37.57  by  O.Couet
*-- Author :
      SUBROUTINE IHSURP(IORDR,NA,NB,FUN,DRFACE,CHOPT)
************************************************************************
*                                                                      *
*     IHSURP                                     Date:    06.10.90     *
*     Author: E. Chernyaev (IHEP/Protvino)       Revised: 26.03.93     *
*                                                                      *
*     Function: Draw surface in polar coordinates                      *
*                                                                      *
*     References: IHWPHI, IHWZN                                        *
*                                                                      *
*     Input: IORDR - order of variables (0 - R,PHI, 1 - PHI,R)         *
*            NA    - number of steps along 1st variable                *
*            NB    - number of steps along 2nd variable                *
*                                                                      *
*            FUN(IA,IB,F,T) - external routine                         *
*              IA     - cell number for 1st variable                   *
*              IB     - cell number for 2nd variable                   *
*              F(3,4) - face which corresponds to the cell             *
*                F(1,*) - A                                            *
*                F(2,*) - B                                            *
*                F(3,*) - Z                                            *
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
        CALL FUN(IA,IB,F,TT)
        IF (I .EQ. 1)                   APHI(1) = F(JPHI,1)
        APHI(I)   = (APHI(I) + F(JPHI,1))/2.
        APHI(I+1) = F(JPHI,3)
   10 CONTINUE
      CALL IHWPHI(IOPT,KPHI,APHI,IPHI1,IPHI2)
*
**       D R A W   S U R F A C E
*
      INCR   = 1
      IPHI   = IPHI1
  100 IF (IPHI .GT. NPHI)               GOTO 300
*        F I N D   O R D E R   A L O N G   R
      IF (IORDR .EQ. 0) THEN
        IA     = NR
        IB     = IPHI
      ELSE
        IA     = IPHI
        IB     = NR
      END IF
      CALL FUN(IA,IB,F,TT)
      PHI    = RAD * ((F(JPHI,1) + F(JPHI,3))/2.)
      CALL IHWZN(COS(PHI),SIN(PHI),0.,Z)
      INCRR  = 1
      IR1    = 1
      IF ((Z.LE.0. .AND. IOPT.EQ.1) .OR. (Z.GT.0. .AND. IOPT.EQ.2)) THEN
        INCRR  =-1
        IR1    = NR
      END IF
      IR2    = NR - IR1 + 1
*        D R A W   S U R F A C E   F O R   S E C T O R
      DO 200 IR=IR1,IR2,INCRR
        IF (IORDR .EQ. 0)               IA = IR
        IF (IORDR .NE. 0)               IB = IR
        CALL FUN(IA,IB,F,TT)
        DO 210 I=1,4
          J          = I
          IF (IORDR.NE.0 .AND. I.EQ.2)  J = 4
          IF (IORDR.NE.0 .AND. I.EQ.4)  J = 2
          XYZ(1,J) = F(JR,I)*COS(F(JPHI,I)*RAD)
          XYZ(2,J) = F(JR,I)*SIN(F(JPHI,I)*RAD)
          XYZ(3,J) = F(3,I)
          TTT(J)   = TT(I)
  210   CONTINUE
        ICODES(1) = IA
        ICODES(2) = IB
        CALL DRFACE(ICODES,XYZ,4,IFACE,TTT)
  200 CONTINUE
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
  998 WRITE(*,*) 'IHSURP: too many PHI sectors'
  999 RETURN
      END

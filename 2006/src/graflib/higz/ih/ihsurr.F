*
* $Id: ihsurr.F,v 1.1.1.1 1996/02/14 13:10:54 mclareni Exp $
*
* $Log: ihsurr.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:54  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.18/05 27/05/93  09.28.13  by  O.Couet
*-- Author :
      SUBROUTINE IHSURR(IORDR,NA,NB,FUN,DRFACE,CHOPT)
************************************************************************
*                                                                      *
*     IHSURR                                     Date:    04.11.90     *
*     Author: E. Chernyaev (IHEP/Protvino)       Revised: 31.03.93     *
*                                                                      *
*     Function: Draw surface in cylindrical coordinates                *
*                                                                      *
*     References: IHWPHI, IHWZN                                        *
*                                                                      *
*     Input: IORDR - order of variables (0 - Z,PHI, 1 - PHI,Z)         *
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
        JZ     = 1
        JPHI   = 2
        NZ     = NA
        NPHI   = NB
      ELSE
        JZ     = 2
        JPHI   = 1
        NZ     = NB
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
      IF (IORDR .EQ. 0)                 IA = NZ
      IF (IORDR .NE. 0)                 IB = NZ
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
**       F I N D   O R D E R   A L O N G   Z
*
      INCRZ  = 1
      IZ1    = 1
      CALL IHWZN(0.,0.,1.,Z)
      IF ((Z.LE.0. .AND. IOPT.EQ.1) .OR. (Z.GT.0. .AND. IOPT.EQ.2)) THEN
        INCRZ  =-1
        IZ1    = NZ
      END IF
      IZ2    = NZ - IZ1 + 1
*
**       D R A W   S U R F A C E
*
      INCR   = 1
      IPHI   = IPHI1
  100 IF (IPHI .GT. NPHI)               GOTO 400
      DO 300 IZ=IZ1,IZ2,INCRZ
        IF (IORDR .EQ. 0) THEN
          IA     = IZ
          IB     = IPHI
        ELSE
          IA     = IPHI
          IB     = IZ
        END IF
        CALL FUN(IA,IB,F,TT)
        DO 200 I=1,4
          J          = I
          IF (IORDR.EQ.0 .AND. I.EQ.2)  J = 4
          IF (IORDR.EQ.0 .AND. I.EQ.4)  J = 2
          XYZ(1,J) = F(3,I)*COS(F(JPHI,I)*RAD)
          XYZ(2,J) = F(3,I)*SIN(F(JPHI,I)*RAD)
          XYZ(3,J) = F(JZ,I)
          TTT(J)   = TT(I)
  200   CONTINUE
        ICODES(1) = IA
        ICODES(2) = IB
        CALL DRFACE(ICODES,XYZ,4,IFACE,TTT)
  300 CONTINUE
*        N E X T   P H I
  400 IPHI   = IPHI + INCR
      IF (IPHI .EQ. 0)                  IPHI = KPHI
      IF (IPHI .GT. KPHI)               IPHI = 1
      IF (IPHI .NE. IPHI2)              GOTO 100
      IF (INCR) 410,999,420
  410 INCR   = 0
      GOTO 100
  420 INCR   =-1
      IPHI   = IPHI1
      GOTO 400
*
  998 WRITE(*,*) 'IHSURR: too many PHI sectors'
  999 RETURN
      END

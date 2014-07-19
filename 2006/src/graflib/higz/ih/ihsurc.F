*
* $Id: ihsurc.F,v 1.1.1.1 1996/02/14 13:10:54 mclareni Exp $
*
* $Log: ihsurc.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:54  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.18/00 20/04/93  14.20.34  by  O.Couet
*-- Author :
      SUBROUTINE IHSURC(ANG,NX,NY,FUN,DRFACE,CHOPT)
************************************************************************
*                                                                      *
*     IHSURC                                     Date:    10.01.92     *
*     Author: E. Chernyaev (IHEP/Protvino)       Revised:              *
*                                                                      *
*     Function: Draw surface in cartesian coordinate system            *
*                                                                      *
*     Input: ANG      - angle between X ang Y                          *
*            NX       - number of steps along X                        *
*            NY       - number of steps along Y                        *
*                                                                      *
*            FUN(IX,IY,F,T) - external routine                         *
*              IX     - X number of the cell                           *
*              IY     - Y number of the cell                           *
*              F(3,4) - face which corresponds to the cell             *
*              T(4)   - additional function (for example: temperature) *
*                                                                      *
*            DRFACE(ICODES,XYZ,NP,IFACE,T) - routine for face drawing  *
*              ICODES(*) - set of codes for this face                  *
*                ICODES(1) - IX                                        *
*                ICODES(2) - IY                                        *
*              NP        - number of nodes in face                     *
*              IFACE(NP) - face                                        *
*              T(NP)     - additional function                         *
*                                                                      *
*            CHOPT - options: 'BF' - from BACK to FRONT                *
*                             'FB' - from FRONT to BACK                *
*                                                                      *
************************************************************************
#include "higz/hcscrn.inc"
      REAL              F(3,4),XYZ(3,4),TT(4)
      INTEGER           IFACE(4),ICODES(2)
      CHARACTER*(*)     CHOPT
      EXTERNAL          FUN,DRFACE
      DATA              IFACE/1,2,3,4/
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
      IF (I1.EQ.1 .OR. I1.EQ.2)         INCRX =-INCRX
      IF (I1.EQ.2 .OR. I1.EQ.3)         INCRY =-INCRY
      IX1    = 1
      IY1    = 1
      IF (INCRX .LT. 0)                 IX1 = NX
      IF (INCRY .LT. 0)                 IY1 = NY
      IX2    = NX - IX1 + 1
      IY2    = NY - IY1 + 1
*
**          D R A W   S U R F A C E
*
      DO 120 IY=IY1,IY2,INCRY
        DO 110 IX=IX1,IX2,INCRX
          CALL FUN(IX,IY,F,TT)
          DO 100 I=1,4
            XYZ(1,I) = F(1,I) + F(2,I)*COSA
            XYZ(2,I) = F(2,I)*SINA
            XYZ(3,I) = F(3,I)
  100     CONTINUE
          ICODES(1)= IX
          ICODES(2)= IY
          CALL DRFACE(ICODES,XYZ,4,IFACE,TT)
  110   CONTINUE
  120 CONTINUE
      RETURN
      END

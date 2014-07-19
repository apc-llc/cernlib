*
* $Id: mnfixp.F,v 1.1.1.1 1996/03/07 14:31:29 mclareni Exp $
*
* $Log: mnfixp.F,v $
* Revision 1.1.1.1  1996/03/07 14:31:29  mclareni
* Minuit
*
*
#include "minuit/pilot.h"
      SUBROUTINE MNFIXP(IINT,IERR)
#include "minuit/d506dp.inc"
CC        removes parameter IINT from the internal (variable) parameter
CC        list, and arranges the rest of the list to fill the hole.
CC
#include "minuit/d506cm.inc"
      DIMENSION YY(MNI)
C                           first see if it can be done
      IERR = 0
      IF (IINT.GT.NPAR .OR. IINT.LE.0)  THEN
         IERR = 1
         WRITE (ISYSWR,'(A,I4)')
     +       ' MINUIT ERROR.  ARGUMENT TO MNFIXP=',IINT
         GO TO 300
      ENDIF
      IEXT = NEXOFI(IINT)
      IF (NPFIX .GE. MNI) THEN
         IERR = 1
         WRITE (ISYSWR,'(A,I4,A,I4)') ' MINUIT CANNOT FIX PARAMETER',
     +   IEXT,' MAXIMUM NUMBER THAT CAN BE FIXED IS',MNI
         GO TO 300
      ENDIF
C                           reduce number of variable parameters by one
      NIOFEX(IEXT) = 0
      NOLD = NPAR
      NPAR = NPAR - 1
C                       save values in case parameter is later restored
      NPFIX = NPFIX + 1
      IPFIX(NPFIX) = IEXT
      LC = IINT
      XS(NPFIX) = X(LC)
      XTS(NPFIX) = XT(LC)
      DIRINS(NPFIX) = WERR(LC)
      GRDS(NPFIX) = GRD(LC)
      G2S(NPFIX) = G2(LC)
      GSTEPS(NPFIX) = GSTEP(LC)
C                        shift values for other parameters to fill hole
      DO 100  IK= IEXT+1, NU
         IF  (NIOFEX(IK) .GT. 0)  THEN
         LC = NIOFEX(IK) - 1
         NIOFEX(IK) = LC
         NEXOFI(LC) = IK
         X(LC)     = X(LC+1)
         XT(LC)    = XT(LC+1)
         DIRIN(LC) = DIRIN(LC+1)
         WERR(LC)  = WERR(LC+1)
         GRD(LC)   = GRD(LC+1)
         G2(LC)    = G2(LC+1)
         GSTEP(LC) = GSTEP(LC+1)
         ENDIF
  100 CONTINUE
      IF (ISW(2) .LE. 0)  GO TO 300
C                    remove one row and one column from variance matrix
      IF (NPAR .LE. 0)  GO TO 300
      DO 260 I= 1, NOLD
      M = MAX(I,IINT)
      N = MIN(I,IINT)
      NDEX = M*(M-1)/2 + N
  260 YY(I)=VHMAT(NDEX)
      YYOVER = 1.0/YY(IINT)
      KNEW = 0
      KOLD = 0
      DO 294 I= 1, NOLD
      DO 292 J= 1, I
      KOLD = KOLD + 1
      IF (J.EQ.IINT .OR. I.EQ.IINT)  GO TO 292
      KNEW = KNEW + 1
      VHMAT(KNEW) = VHMAT(KOLD) - YY(J)*YY(I)*YYOVER
  292 CONTINUE
  294 CONTINUE
  300 RETURN
      END

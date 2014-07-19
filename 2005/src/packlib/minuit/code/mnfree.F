*
* $Id: mnfree.F,v 1.1.1.1 1996/03/07 14:31:29 mclareni Exp $
*
* $Log: mnfree.F,v $
* Revision 1.1.1.1  1996/03/07 14:31:29  mclareni
* Minuit
*
*
#include "minuit/pilot.h"
      SUBROUTINE MNFREE(K)
#include "minuit/d506dp.inc"
CC        Restores one or more fixed parameter(s) to variable status
CC        by inserting it into the internal parameter list at the
CC        appropriate place.
CC
#include "minuit/d506cm.inc"
C--       K = 0 means restore all parameters
C--       K = 1 means restore the last parameter fixed
C--       K = -I means restore external parameter I (if possible)
C--       IQ = fix-location where internal parameters were stored
C--       IR = external number of parameter being restored
C--       IS = internal number of parameter being restored
      IF (K .GT. 1)  WRITE (ISYSWR,510)
      IF (NPFIX .LT. 1)  WRITE (ISYSWR,500)
      IF (K.EQ.1 .OR. K.EQ.0)  GO TO 40
C                   release parameter with specified external number
      KA = IABS(K)
      IF (NIOFEX(KA) .EQ. 0)  GO TO 15
      WRITE (ISYSWR,540)
  540 FORMAT (' IGNORED.  PARAMETER SPECIFIED IS ALREADY VARIABLE.')
      RETURN
   15 IF (NPFIX .LT. 1)  GO TO 21
      DO 20 IK= 1, NPFIX
      IF (IPFIX(IK) .EQ. KA)  GO TO 24
   20 CONTINUE
   21 WRITE (ISYSWR,530) KA
  530 FORMAT (' PARAMETER',I4,' NOT FIXED.  CANNOT BE RELEASED.')
      RETURN
   24 IF (IK .EQ. NPFIX)  GO TO 40
C                   move specified parameter to end of list
      IPSAV = KA
      XV = XS(IK)
      XTV = XTS(IK)
      DIRINV = DIRINS(IK)
      GRDV = GRDS(IK)
      G2V = G2S(IK)
      GSTEPV = GSTEPS(IK)
         DO 30 I= IK+1,NPFIX
         IPFIX(I-1) = IPFIX(I)
         XS(I-1) = XS(I)
         XTS(I-1) = XTS(I)
         DIRINS(I-1) = DIRINS(I)
         GRDS(I-1) = GRDS(I)
         G2S(I-1) = G2S(I)
         GSTEPS(I-1) = GSTEPS(I)
   30    CONTINUE
      IPFIX(NPFIX) = IPSAV
      XS(NPFIX) = XV
      XTS(NPFIX) = XTV
      DIRINS(NPFIX) = DIRINV
      GRDS(NPFIX) = GRDV
      G2S(NPFIX) = G2V
      GSTEPS(NPFIX) = GSTEPV
C                restore last parameter in fixed list  -- IPFIX(NPFIX)
   40 CONTINUE
      IF (NPFIX .LT. 1)  GO TO 300
      IR = IPFIX(NPFIX)
      IS = 0
      DO 100 IK= NU, IR, -1
        IF (NIOFEX(IK) .GT. 0) THEN
         LC = NIOFEX(IK) + 1
         IS = LC - 1
         NIOFEX(IK) = LC
         NEXOFI(LC) = IK
         X(LC)     = X(LC-1)
         XT(LC)    = XT(LC-1)
         DIRIN(LC) = DIRIN(LC-1)
         WERR(LC)  = WERR(LC-1)
         GRD(LC)   = GRD(LC-1)
         G2(LC)    = G2(LC-1)
         GSTEP(LC) = GSTEP(LC-1)
        ENDIF
  100 CONTINUE
      NPAR = NPAR + 1
      IF (IS .EQ. 0)   IS = NPAR
      NIOFEX(IR) = IS
      NEXOFI(IS) = IR
      IQ = NPFIX
      X(IS) = XS(IQ)
      XT(IS) = XTS(IQ)
      DIRIN(IS) = DIRINS(IQ)
      WERR(IS)  = DIRINS(IQ)
      GRD(IS) = GRDS(IQ)
      G2(IS) = G2S(IQ)
      GSTEP(IS) = GSTEPS(IQ)
      NPFIX = NPFIX - 1
      ISW(2) = 0
      DCOVAR = 1.
      IF (ISW(5)-ITAUR .GE. 1)  WRITE(ISYSWR,520) IR,CPNAM(IR)
      IF (K.EQ.0)  GO TO 40
  300 CONTINUE
C         if different from internal, external values are taken
      CALL MNEXIN(X)
  400 RETURN
  500 FORMAT (' CALL TO MNFREE IGNORED.  THERE ARE NO FIXED PA',
     + 'RAMETERS'/)
  510 FORMAT (' CALL TO MNFREE IGNORED.  ARGUMENT GREATER THAN ONE'/)
  520 FORMAT (20X, 9HPARAMETER,I4,2H, ,A10,' RESTORED TO VARIABLE.')
      END

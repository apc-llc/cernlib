*
* $Id: dmpfit.F,v 1.1.1.1 1996/02/15 17:51:29 mclareni Exp $
*
* $Log: dmpfit.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:29  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE DMPFIT (LUNP,NWFIT,COMMENT)
 
C CERN PROGLIB#         DMPFIT          .VERSION KERNCDC  1.16  820129
 
C-    DEBUG ROUTINE TO PRINT  FIT CONTENTS WITH MNEMONICS
C-         LUNP    LOGICAL UNIT
C-         NWFIT   NUMBER OF FIT-WORDS TO BE PRINTED
C-         COMMENT TEXT-STRING TERMINATED BY .  FOR PRINTING
 
C-    EXAMPLE:  CALL DMPFIT (3LPAM, 100,"PAM OPENED.")
C-                           ALL FIT-WORDS
 
      COMMON /COMFIT/  LUN,LFIT,MFIT(40),MWK(40)
      COMMON /IDFIT/ IDI(200)
      DIMENSION    COMMENT (9)
      DIMENSION    MCM(80)
      EQUIVALENCE (MCM,LUN)
      DIMENSION    MFMT(20), KFMT(15)
 
      DATA  MFMT / 10H(5X3HL +  ,10HI2,3X,     , 18*0     /
      DATA  MFMTE/ 3H1X) /
      DATA  KFMT / 10HO1,1X,A5,    ,10HO2,1X,A5,   ,10HO3,1X,A5,
     +,            10HO4,1X,A5,    ,10HO5,1X,A5,   ,10HO6,1X,A5,
     +,            10HO7,1X,A5,    ,10HO8,1X,A5,   ,10HO9,1X,A5,
     +,            10HO10,1X,A5,   ,10HO11,1X,A5,  ,10HO12,1X,A5,
     +,            10HO13,1X,A5,   ,10HO14,1X,A5,  ,10HO15,1X,A5,     /
      DATA  IDOT / 1H. /
      DATA  IFALL /4HO20) /
 
      DATA IDI /0, 12,3HDVT,  1,3H-  ,  1,3HRDR,  9,3H-  ,  1,3HFF ,      1
     +    6,3H-  ,  6,3HDC ,  6,3HLGF, 18,3HFWB,        0,0,0,0,0,0,0,0,  1+
     +0, 24,3HLBL,  1,3HLCR,  1,3H-  ,  7,3HFP ,  3,3HULP,  2,3HLT ,      10
     +   22,3HLA ,                                                        10+
     +0, 24,3HRL ,  1,3HCM ,  2,3HOF ,  3,3HCF ,  2,3HVF ,  4,3HRT ,      11
     +    3,3HBT ,  3,3HFO , 18,3HLX ,                                    11+
     +0, 24,3HMRL, 18,3H-  , 18,3HDX ,                                    12
     +0,  2,3H-  ,  2,3HDFC,  2,3HEFC,  9,3HECT,  9,3HERL,  1,3H-  ,      13
     +    1,3HPEF,  3,3H-  ,  4,3HSES,  9,3HES , 18,3HEX ,            0,  13+
     +0, 24,3HHL ,  3,3H-  ,  3,3HEO ,  1,3H-  ,  1,3HBAL,  1,3HSTF,      15
     +    1,3HPDF,  1,3HSBF,  1,3HSPR,  1,3H-  ,  1,3HORG, 22,3HWSA,      15+
     +0, 24,3HTL ,  6,3HCL ,  6,3HPC ,  6,3HMUL, 18,3HHRL,                16
     +0,  1,3HFNF,  2,3HOC ,  3,3HPD ,  6,3H-  ,  1,3HB8F,  1,3HC1 ,      17
     +    1,3HSB , 24,3HCP ,  1,3H-  ,  1,3HCNF,  1,3HBBH, 18,3HBFS,      17+
     +0, 24,3HPTL,  6,3HLOP, 30,3HRC ,                                    18
     +0, 24,3HMBL,  6,3HVNO, 30,3HBN ,                                    19
     +             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0            /
 
 
      CALL VBLANK (MFMT(3),18)
      CALL UBLOW (COMMENT,MCM,80)
      NCM = IUFIND (IDOT,MCM,1,79)
      PRINT 9001, (MCM(J),J=1,NCM)
 
      LUN = LUNP
      CALL LOAFIT (LUN)
      PRINT 9004, LUN,LFIT
      JX  = 1
      JW  = 0
      NWPR= MIN0 (NWFIT,35)
 
C----              START FIT
      MWK(1) = 0
 
C--                NEXT WORD
 
   21 JW   = JW + 1
      MWD  = MFIT(JW)
      JB   = 61
      JFMT = 3
      JPUT = 1
      IF (IDI(JX).EQ.0)      GO TO 26
 
C--                NEXT ITEM
 
   24 NB   = IDI(JX)
      IF (NB.EQ.0)           GO TO 27
      JB   = JB - NB
      JPUT = JPUT + 2
      MWK(JPUT-1) = JBYT (MWD,JB,NB)
      MWK(JPUT)   = IDI(JX+1)
      J = (NB-1)/3
      MFMT(JFMT) = KFMT(J+1)
      JFMT = JFMT + 1
      JX   = JX + 2
      GO TO 24
 
C--                PRINT 1 WORD
 
   26 JPUT      = JPUT+1
      MWK(JPUT) = MWD
      MFMT(JFMT)= IFALL
      JFMT      = JFMT+1
 
   27 MFMT(JFMT) = MFMTE
      PRINT MFMT, (MWK(J),J=1,JPUT)
      MWK(1) = MWK(1) + 1
      JX = JX + 1
      IF (MWK(1).LT.NWPR)    GO TO 21
      PRINT 9099
      RETURN
 
 9001 FORMAT ("0DMPFIT.  ",80A1)
 9004 FORMAT ("0FIT FOR FILE ",A7," AT ",O6/1X)
 9099 FORMAT (1X)
      END

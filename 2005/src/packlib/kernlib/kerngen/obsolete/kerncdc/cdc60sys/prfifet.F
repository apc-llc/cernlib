*
* $Id: prfifet.F,v 1.1.1.1 1996/02/15 17:51:32 mclareni Exp $
*
* $Log: prfifet.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:32  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE PRFIFET (LUNP,NWFIT,NWFET,COMMENT)
 
C CERN PROGLIB#         PRFIFET         .VERSION KERNCDC  1.03  771219
 
C-    DEBUG ROUTINE TO PRINT  FIT/FET CONTENTS WITH MNEMONICS
C-         LUNP    LOGICAL UNIT
C-         NWFIT   NUMBER OF FIT-WORDS TO BE PRINTED,  MAY BE ZERO
C-         NWFET   NUMBER OF FET-WORDS TO BE PRINTED,  MAY BE ZERO
C-         COMMENT TEXT-STRING TERMINATED BY .  FOR PRINTING
 
C-    EXAMPLE:  CALL PRFIFET (3LPAM,20,0,"PAM OPENED.")
C-                           ALL FIT-WORDS, NOT FET
 
      COMMON /FITFET/  LUN,LFIT,MFIT(16),LFET,MFET(10),MWK(53)
      COMMON /IDFIFET/ IDI(164),IDE(90)
      DIMENSION    COMMENT (9)
      DIMENSION    MCM(80)
      EQUIVALENCE (MCM,LUN)
      DIMENSION    MFMT(15), KFMT(15)
 
      DATA  MFMT / 10H(5X3HL +  ,10HI2,3X,     , 13*(1H ) /
      DATA  MFMTE/ 3H1X) /
      DATA  KFMT / 10HO1,A5,       ,10HO2,A5,      ,10HO3,A5,
     +,            10HO4,A5,       ,10HO5,A5,      ,10HO6,A5,
     +,            10HO7,A5,       ,10HO8,A5,      ,10HO9,A5,
     +,            10HO10,A5,      ,10HO11,A5,     ,10HO12,A5,
     +,            10HO13,A5,      ,10HO14,A5,     ,10HO15,A5,        /
      DATA  IDOT / 1H. /
 
      DATA  LGIDI/ 164 /
      DATA  IDI  /        42,3HLFN,18,3HR  ,  0
     +, 24,3HRL , 1,3HPM , 3,3HFO , 1,3HNDX, 3,3HBT , 1,3HBCK, 1,3HDT
     +,                    4,3HRT , 1,3HDKI, 3,3HPD ,18,3HFET,  0
     +, 24,3HPTL, 2,3HOF , 2,3HVF , 2,3HCF , 2,3HLT , 3,3HULP, 7,3HFP
     +,                   18,3HLX ,  0
     +, 24,3HHL ,18,3HBFS,18,3HDX ,  0
     +, 24,3HTL , 1,3HFNF, 1,3HPEF, 3,3HR  , 5,3HSES, 8,3HIRS,18,3HEX ,0
     +,  6,3HVNO, 9,3HECT, 9,3HERL, 3,3HF/S, 2,3HE/S, 2,3HOC , 3,3HF/L
     +,                    2,3HK/H, 6,3HLVL,18,3HFWB,  0
     +, 24,3HMRL,10,3HR  , 1,3HCM , 3,3HEO ,22,3HWSA,  0
     +,  4,3HKP , 9,3HKL , 9,3HMKL, 4,3HRKP,12,3HRKW,22,3HPNA,  0
     +, 24,3HMNB, 8,3HRMK, 6,3HPC ,22,3HLA ,  0
     +, 24,3HLP ,14,3HRB ,22,3HPAR,  0
     +,  1,3HPDF, 1,3HSBF,13,3HR  , 1,3HCNF, 1,3HSB , 1,3HC1 , 6,3HLL
     +,                    6,3HLOP,30,3HRC ,  0
     +, 24,3HLBL, 6,3HMUL,30,3HBN ,  0
     +, 24,3HMBL, 6,3HNL , 2,3HOVF,28,3HDL ,  0,  0   /
 
      DATA  IDE  /        42,3HLFN, 4,3HLV , 5,3HERC, 9,3HCOD,  0
     +, 12,3HDT , 3,3HR/U, 3,3HE/M, 3,3HX/E, 3,3HN/S,12,3HDSP, 6,3HFLG
     +,                   18,3HFIR,  0
     +, 12,3HZ  ,15,3HZ  ,15,3HZ  ,18,3HIN ,  0
     +, 12,3HZ  ,15,3HZ  ,15,3HZ  ,18,3HOUT,  0
     +, 12,3HFNT,15,3HRBS,15,3HPRU,18,3HLIM,  0
     +, 12,3HR  ,18,3HFWA,12,3HR  ,18,3HLWA,  0
     +, 12,3HDEC,18,3HEXT, 6,3HUBC,24,4HMLRS,  0
     +, 18,3HRNO, 6,3HR  ,18,3HIXL,18,3HIX ,  0
     +, 12,3HR  ,18,3HEOI,12,3HR  ,18,3HEEA,  0,  0   /
 
 
      CALL UBLOW (COMMENT,MCM,80)
      NCM = IUFIND (IDOT,MCM,1,79)
      PRINT 9001, (MCM(J),J=1,NCM)
 
      LUN = LUNP
      CALL LDFIFET (LUN)
 
      IF (NWFIT.LE.0)        GO TO 31
      PRINT 9004, LUN,LFIT
      JX  = 1
      JW  = 0
      NWPR= NWFIT
 
C----              START FIT OR FET
 
   20 MWK(1) = 0
 
C--                NEXT WORD
 
   21 JW   = JW + 1
      MWD  = MFIT(JW)
      JB   = 61
      JFMT = 3
      JPUT = 1
 
C--                NEXT ITEM
 
   24 NB   = IDI(JX)
      IF (NB.EQ.0)           GO TO 27
      JB   = JB - NB
      JPUT = JPUT + 2
      MWK(JPUT-1) = JBYT (MWD,JB,NB)
      MWK(JPUT)   = IDI(JX+1)
      CALL SHFTZF (MWK(JPUT),18)
      J = (NB-1)/3
      MFMT(JFMT) = KFMT(J+1)
      JFMT = JFMT + 1
      JX   = JX + 2
      GO TO 24
 
C--                PRINT 1 WORD
 
   27 MFMT(JFMT) = MFMTE
      PRINT MFMT, (MWK(J),J=1,JPUT)
      MWK(1) = MWK(1) + 1
      IF (MWK(1).GE.NWPR)    GO TO 28
      JX = JX + 1
      IF (IDI(JX).NE.0)      GO TO 21
 
   28 IF   (JW.GE.18)        RETURN
 
C------            START PRINTING OF  FET
 
   31 IF (NWFET.LE.0)        RETURN
      JW   = 17
      JX   = LGIDI + 1
      NWPR = NWFET
      PRINT 9031, LUN,LFET
      GO TO 20
 
 9001 FORMAT (1H0,80A1)
 9004 FORMAT ("0FIT FOR FILE "A7," AT "O6/1X)
 9031 FORMAT ("0     FET FOR "A7," AT "O6/1X )
      END

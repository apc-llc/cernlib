*
* $Id: cqhead.F,v 1.1.1.1 1996/03/06 10:46:59 mclareni Exp $
*
* $Log: cqhead.F,v $
* Revision 1.1.1.1  1996/03/06 10:46:59  mclareni
* Zebra
*
*
#include "test_include/pilot.h"
      SUBROUTINE CQHEAD (IFLAGH,IHEAD,IHEAD2,INFORM,NAMES)

C-    Starting a new lot of check-data,  user called

#include "zebra/zbcd.inc"
#include "zebra/zunit.inc"
#include "test_include/cqc.inc"
#include "test_include/cqc1.inc"
C--------------    End CDE                             --------------
#if !defined(CERNLIB_F77TRARG)
      DIMENSION    IHEAD(9), IHEAD2(9), INFORM(9), NAMES(9)
#endif
#if defined(CERNLIB_F77TRARG)
      CHARACTER*(*) IHEAD, IHEAD2, INFORM, NAMES
#endif
      DIMENSION    MLP(100),      MPU(80)
      EQUIVALENCE (MLP(1),M(1)), (MPU(1),M(111))

      DIMENSION    ISYMB(9)
      SAVE         ISYMB
#if defined(CERNLIB_QHOLL)
      DATA  ISYMB  /4HA   , 4HF   , 4HI   , 4HO   , 4HH   ,
     +              4HL   , 4HB   , 4HJ   , 4HP   /
#endif

C-    IQCOPT(1)      Execution
C-    IQCOPT(2)      Listing
C-    IQCOPT(3)      Punching
C-    IQCOPT(4)      Comparing
C-    IQCOPT(5)      Stop on comparison failure
C-    IQCOPT(6)      Maximum output
C-    IQCOPT(7)      Check machine-dependent parameters also
C-    IQCOPT(8)      Tempor. inhibition of checking,
C-                   CQ is used for printing only

C-------           Digest header

C-    IFLAGH = -1  cancel
C-              0  same lot continued, change format or naming
C-              1  same lot cont., new sub-title, or format or naming
C-              2  next lot, same name
C-              3  new  lot, new  name

      IF (IQCOPT(1).EQ.0)    RETURN
      IF   (IFLAGH)          81, 19, 11
   11 IF   (IFLAGH-2)        12, 14, 13
   12 IF (IQCOPT(2).EQ.0)          GO TO 19
      GO TO 15

   13 CALL UCTOH1 (IHEAD,M(11),4)
      CALL UBUNCH (M(11),M(1), 4)
#if !defined(CERNLIB_QHOLL)
      CALL UCTOH ('A   F   I   O   H   L   B   J   P   ',ISYMB,4,36)
#endif
      IF (M(1).EQ.NAME)            GO TO 14
      NAME = M(1)
      NUM  = 0
   14 NUM  = NUM + 1
      M(1) = NAME
      M(2) = NUM

   15 CALL UCTOH1 (IHEAD2,M(3),50)
      N = IUFIND (IQDOT,M,3,51)
      IF (IFLAGH.GE.2)             GO TO 16
      WRITE (IQPRNT,9014) (M(J), J=3,N)
      GO TO 19

   16 WRITE (IQPRNT,9015) (M(J), J=1,N)
      WRITE (IQPRNT,9016)
      IF (IQTYPE.NE.0 .AND. IQTYPE.NE.IQPRNT)
     +                     WRITE (IQTYPE,9015) (M(J),J=1,N)
      IF (IQCOPT(3).NE.0)  WRITE (IQPNCH,9017) (M(J),J=1,N)

C------            Position check-cards

      IQCHEK= IQCOPT(4)
      IF (IQCOPT(4).EQ.0)          GO TO 18
      CALL CQPOS
      IF (IQCHEK.EQ.0)             GO TO 18
      WRITE (IQPRNT,9018)

C-------           Digest format

   18 JCRD = 1
   19 CALL UCTOH1  (INFORM,MUST,7)
      IF (MUST(1).EQ.IQDOT)        GO TO 41
      IF (MUST(1).EQ.IQMINS)       GO TO 41
      NENTR= 0
      CALL UCTOH1  ('(5H CQD.,I7,3H-  ', MLP(1), 17)
      CALL UCTOH1  ('(A4,2I3'          ,MPU(1), 7)
      JLP = 18
      JPU =  8

      DO 38  J=1,7
      IT = IUCOMP (MUST(J),ISYMB,9)
      IF (IT.EQ.0)                 GO TO 39
      NENTR = NENTR + 1
      KODEFM(NENTR)   = IT
      KODEFM(NENTR+1) = IT
      GO TO (21,22,23,24,25,26,27,28,29), IT

   21 CALL UCTOH1  (',4X,A4,6X'   ,MLP(JLP), 9)
      CALL UCTOH1  (',6X,A4'      ,MPU(JPU), 6)
      JLP = JLP  + 9
      JPU = JPU  + 6
      GO TO 38

   22 CALL UCTOH1  (',E14.3'      ,MLP(JLP), 6)
      CALL UCTOH1  (',E10.3'      ,MPU(JPU), 6)
      JLP = JLP  + 6
      JPU = JPU  + 6
      GO TO 38

   23 CALL UCTOH1  (',4X,I10'     ,MLP(JLP), 7)
      CALL UCTOH1  (',I10'        ,MPU(JPU), 4)
      JLP = JLP  + 7
      JPU = JPU  + 4
      GO TO 38

   24 CALL UCTOH1  (',4X,10I1'    ,MLP(JLP), 8)
      CALL UCTOH1  (',I10'        ,MPU(JPU), 4)
      JLP = JLP  + 8
      JPU = JPU  + 4
      GO TO 38

   25 CALL UCTOH1  (',4X,2A4,2X'  ,MLP(JLP),10)
      CALL UCTOH1  (',2X,2A4'     ,MPU(JPU), 7)
      JLP = JLP  + 10
      JPU = JPU  + 7
      GO TO 37

   26 CALL UCTOH1  (',I7,1X,A4,2X',MLP(JLP),12)
      CALL UCTOH1  (',I5,A4,1X'   ,MPU(JPU), 9)
      JLP = JLP  + 12
      JPU = JPU  + 9
      KODEFM(NENTR)   = 8
      GO TO 37

   27 CALL UCTOH1  (',2(3X,A4)'   ,MLP(JLP), 9)
      CALL UCTOH1  (',2(A4,1X)'   ,MPU(JPU), 9)
      JLP = JLP  + 9
      JPU = JPU  + 9
      GO TO 37

   28 CALL UCTOH1  (',2I7'        ,MLP(JLP), 4)
      CALL UCTOH1  (',2I5'        ,MPU(JPU), 4)
      JLP = JLP  + 4
      JPU = JPU  + 4
      GO TO 37

   29 CALL UCTOH1  (',2(2X,5I1)'  ,MLP(JLP), 10)
      CALL UCTOH1  (',2I5'        ,MPU(JPU), 4)
      JLP = JLP  + 10
      JPU = JPU  + 4

   37 NENTR = NENTR + 1
   38 CONTINUE

   39 MLP(JLP) = IQCLOS
      MPU(JPU) = IQCLOS
      CALL UH1TOC (MLP,MFLP,JLP)
      CALL UH1TOC (MPU,MFPU,JPU)
      IF (NENTR.EQ.0)  CALL ZFATAM ('NENTR=0 IN CQHEAD.')

C-------           Digest naming

   41 CALL UCTOH1 (NAMES,M(1),120)
      IF (M(1).EQ.IQDOT)           GO TO 89
      IF (M(1).EQ.IQMINS)          GO TO 89
      NAMEPR= 0
      CALL VZERO  (MACHFL,14)
      CALL VBLANK (NAMEVC,104)
      LENM = IUFIND (IQDOT,M,1,120) - 1
      LPUT = 7
      LK   = 0
      NH   = 0

   44 IF (NH.GE.NENTR)             GO TO 89
      LT   = LK
   45 IF (LT.GE.LENM)              GO TO 89
      LT   = LT + 1
      IF (M(LT).EQ.IQBLAN)         GO TO 45
      LK   = IUFIND (IQCOMA,M,LT,LENM)
      NP   = LK - LT

      NH   = NH + 1
      JH   = NH
      KOFM = KODEFM(NH)
      IF (KOFM-5)            52, 54, 53
   52 IF (KOFM.EQ.1)               GO TO 55
      LPUT = LPUT + 7
   53 LPUT = LPUT + 7
      JP   = MAX (1,LPUT-NP)
      NP   = LPUT - JP
      GO TO 58

   54 NH   = NH + 1
   55 JP   = LPUT + 4
      LPUT = LPUT + 14

   58 IF (NP.EQ.0)                 GO TO 44
      CALL UCOPY (M(LT),NAMEVC(JP),NP)
      IF (M(LT).EQ.IQSLAS)         GO TO 62
      IF (M(LT).NE.IQSTAR)         GO TO 44
      IFLM = 7
      GO TO 63

   62 IFLM = -7
   63 MACHFL(JH) = IFLM
      MACHFL(NH) = IFLM
      GO TO 44

   81 NAME = 0
   89 RETURN


 9014 FORMAT (1X/10H CQHEAD.  ,8X,4H--- ,60A1)
 9015 FORMAT (1X/10H CQHEAD.  ,A4,I3,5X,60A1)
 9016 FORMAT (10X,7(10H----------))
 9017 FORMAT (A4,I3,3H  0,70A1)
 9018 FORMAT (' CQHEAD.  Checked execution initiated.')
      END
*      ==================================================
#include "zebra/qcardl.inc"

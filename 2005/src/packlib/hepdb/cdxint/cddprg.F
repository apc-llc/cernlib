*
* $Id: cddprg.F,v 1.1.1.1 1996/02/28 16:24:49 mclareni Exp $
*
* $Log: cddprg.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:49  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDDPRG (KDISP, NK, CHOPT)
*     ====================================
*
************************************************************************
*                                                                      *
*        SUBR. CDDPRG (KDISP*, NK, CHOPT)                              *
*                                                                      *
*   Sets flag to keys to be displayed (requested by the user) in the   *
*   Horizontal Mode                                                    *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     KDISP    Key Flag = 0 for No Display, = 1 for Key to be displayed*
*     NK       Number of Keys                                          *
*     CHOPT    Character string specifying following options           *
*        TERM  Range of keys requested from terminal                   *
*                                                                      *
*   Called by CDDISH                                                   *
*                                                                      *
************************************************************************
*
#include "hepdb/cbconc.inc"
#include "hepdb/ckkeys.inc"
      CHARACTER       CRANG*80, CUSE*2, CHOPT*(*)
      DIMENSION       KDISP (NK)
*
*     ------------------------------------------------------------------
*
      CALL VZERO (KDISP, NK)
*
      IF (CHOPT.EQ.'TERM') THEN
        CALL KUPROC ('Range of Keys to display: K1-K2,K3-K4,..',
     +              CRANG, LCRNG)
      ELSE IF (CHOPT.EQ.'EDIT') THEN
        WRITE (CUSE, '(I2)') IUSECB(NK)
        CRANG = '3-5,8-'//CUSE
        LCRNG = 8
      ELSE
        WRITE (CUSE, '(I2)') IUSECB(NK)
        CRANG = '3-5,8-'//CUSE
        LCRNG = 8
      ENDIF
      IF ((CRANG.EQ.' ') .OR. (LCRNG.EQ.0)) GO TO 20
      NUMB = 0
      NUMB1 = 0
      NUMB2 = 0
      I = 0
    5 CONTINUE
      I = I + 1
      IF ((I.LE.LCRNG) .AND. (CRANG(I:I).EQ.','.OR.CRANG.EQ.' ')
     +  .OR. (I.GT.LCRNG)) THEN
        IF (NUMB.NE.0) THEN
          IF (NUMB1.EQ.0) THEN
            NUMB1 = NUMB
          ELSE
            NUMB2 = NUMB
          ENDIF
          IF (NUMB2.LT.NUMB1) NUMB2 = NUMB1
          NUMB2 = MIN0 (NUMB2, NK)
          NUMBF = ISYSCB(NUMB1)
          NUMBL = ISYSCB(NUMB2)
          NUMBL = NUMBL + ICONCK(NUMBL)
          DO 10 J = NUMBF, NUMBL
            KDISP(J) = 1
   10     CONTINUE
          NUMB = 0
          NUMB1 = 0
          NUMB2 = 0
        ENDIF
      ELSE IF (CRANG(I:I).EQ.'-') THEN
        NUMB1 = NUMB
        NUMB = 0
      ELSE
        READ (CRANG(I:I), '(I1)') IMANT
        IF (IMANT.GE.0.AND.IMANT.LE.9) THEN
          NUMB = NUMB*10 + IMANT
        ENDIF
      ENDIF
      IF (I.LE.LCRNG)                       GO TO 5
      GO TO 999
   20 CONTINUE
      DO 30 I = 1, NK
        KDISP(I) = 1
   30 CONTINUE
*                                                             END CDDPRG
  999 END

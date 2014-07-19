*
* $Id: cd.F,v 1.1.1.1 1996/02/15 17:54:18 mclareni Exp $
*
* $Log: cd.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:18  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      PROGRAM CD
C
C CERN PROGLIB#         CD              .VERSION KERNVAX  2.45  940228
C ORIG.  31/07/90, Julius Zoll
C MODIF. 24/02/94, JZ,  to try ./ ../ ~/ name
C
C-    Program to set the current directory using Unix style

      COMMON /SLATE/ NDSLAT,NESLAT,NFSLAT,NGSLAT,DUMMY(36)
      CHARACTER    CWD*256, TEXT*256, CHINQ*256
      CHARACTER    LINE*256, COLM(256)*1
      EQUIVALENCE (LINE,COLM)
      INTEGER*2    LGPAR2
      LOGICAL      THERE


C!      CHARACTER    ORG*256
C!      CALL GETWDF (ORG)
C!      NORG = NDSLAT
C!      CALL FFRVAX (ORG,NORG)
C!
C!   11 CALL TMPRO  (' dir > ')
C!      CALL TMREAD (80, LINE, NNL, ISTAT)
C!      IF (ISTAT.NE.0)  THEN
C!          CALL FTOVAX (ORG,NORG)
C!          CALL CHDIRF (ORG(1:NORG))
C!          STOP
C!        ENDIF

      IRC = 1
      CALL LIB$GET_FOREIGN  (LINE,,LGPAR2)
      NNL = LGPAR2
      IF (NNL.LE.0)                GO TO 67

      CALL GETWDF (CWD)
      NCWD = NDSLAT
      CALL FFRVAX (CWD,NCWD)

      JX = ICNEXT (LINE,1,NNL)
      IF (NDSLAT.EQ.0)             GO TO 67
      NNL = NESLAT - 1
      CALL CLEFT (LINE,1,NNL)
      NNL = NDSLAT
      IF (NNL.EQ.1)  THEN
          IF (COLM(1).EQ.'?')      GO TO 81
          IF (COLM(1).EQ.'.')      GO TO 67
        ENDIF

      CALL FFRVAX (LINE,NNL)
      IF (NDSLAT.LT.0)             GO TO 61
      IF (COLM(NNL).NE.'/')  THEN
          NNL = NNL + 1
          COLM(NNL) = '/'
        ENDIF

      CALL FTOVAX (LINE,NNL)
      IF (NDSLAT.LT.0)             GO TO 61
      CALL FFRVAX (LINE,NNL)
      IF (NDSLAT.LT.0)             GO TO 61
C     PRINT 9014, LINE(1:NNL)
C9014 FORMAT (' FFRVAX returns ',A)

      IF (LINE(1:2).EQ.'./')  THEN
          IF (NNL.EQ.2)            GO TO 67
        ENDIF

      IF (COLM(1).EQ.'/')          GO TO 51
      IF (COLM(1).EQ.'.')          GO TO 51

C----              check existence

      TEXT = LINE
      NTX  = NNL

      NNL = NTX + NCWD
      LINE(1:NNL) = CWD(1:NCWD) // TEXT(1:NTX)

C--           try ./name

      JSLA = 0
   42 JSLA = ICFIND ('/', LINE,JSLA+1,NNL)
      IF (LINE(JSLA:JSLA+1).EQ.'//')  GO TO 42
      IF (LINE(JSLA:JSLA+1).EQ.'/=')  GO TO 42

      NINQ = NNL + 3
      CHINQ(1:NINQ) = LINE(1:NNL-1) // '.DIR'

      CALL FTOVAX (CHINQ,NINQ)
      INQUIRE (FILE=CHINQ(1:NINQ), EXIST=THERE)
C     PRINT *, ' Inquire for file ',CHINQ(1:NINQ),' is', THERE

      IF (THERE)                   GO TO 51

C--           try ../name

      JSLP = ICFILA ('/', LINE,1,NCWD-1)
      IF (JSLP.EQ.JSLA)            GO TO 46
      LINE(JSLP+1:JSLP+NTX) = TEXT(1:NTX)
      NNL  = JSLP + NTX
      NINQ = NNL + 3
      CHINQ(1:NINQ) = LINE(1:NNL-1) // '.DIR'

      CALL FTOVAX (CHINQ,NINQ)
      INQUIRE (FILE=CHINQ(1:NINQ), EXIST=THERE)
C     PRINT *, ' Inquire for file ',CHINQ(1:NINQ),' is', THERE

      IF (THERE)                   GO TO 51

C--           try ~/name

   46 NNL = NTX + 2
      LINE(1:NNL) = '~/' // TEXT(1:NTX)
      NINQ = NNL + 3
      CHINQ(1:NINQ) = LINE(1:NNL-1) // '.DIR'

      CALL FTOVAX (CHINQ,NINQ)
      INQUIRE (FILE=CHINQ(1:NINQ), EXIST=THERE)
C     PRINT *, ' Inquire for file ',CHINQ(1:NINQ),' is', THERE

      IF (THERE)                   GO TO 51

      PRINT 9049, TEXT(1:NTX-1)
 9049 FORMAT (' !! None of ./ ../ ~/ ',A,' found !!')
      GO TO 62


C----             Set the directory

   51 CALL FTOVAX (LINE,NNL)
      IF (NDSLAT.LT.0)             GO TO 61
C     PRINT 9054, LINE(1:NNL)
C9054 FORMAT (' FTOVAX returns ',A)

      CALL CHDIRF (LINE(1:NNL))
C     PRINT 9801, LINE(1:NNL)
C9801 FORMAT (' CHDIR entered with >',A,'<')
      GO TO 67

   61 PRINT *, ' !! Faulty syntax !!'
   62 IRC = 4
   67 CALL GETWDF (LINE)
      NNL = NDSLAT
      PRINT 9067, LINE(1:NNL)
 9067 FORMAT (' c.d. = ',A)
      GO TO 99

   81 PRINT 9081
 9081 FORMAT (/' You may give for example :'/
     F/' Go down:        cd  patchy/install'
     F/' Go up + over:   cd  ../log'
     F/'                 cd  .../kern/install'
     F/' Go home + down: cd  ~/pam/patchy'
     F/' Go elsewhere:   cd                   /lib/patchy/install'
     F/'                 cd          /=disk$cr/lib/patchy/install'
     F/'                 cd  //vxcern/=disk$cr/lib/patchy/install'/
     F/' As on Unix, if you give:'/
     F/'                 cd  patchy/install'/
     F/'    we will try:   ./patchy/install'
     F/'                  ../patchy/install'
     F/'                   ~/patchy/install'
     F/' Good luck.'/)
   99 CONTINUE
      CALL SYS$EXIT (%VAL(IRC))
C!      GO TO 11
      END

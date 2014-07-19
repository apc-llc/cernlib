*
* $Id: hrdirm.F,v 1.1.1.1 1996/01/16 17:08:10 mclareni Exp $
*
* $Log: hrdirm.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:10  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.21/07 20/12/93  10.24.13  by  Rene Brun
*-- Author :    Rene Brun   20/12/93
      SUBROUTINE HRDIRM(IPAWD,MAXDIR,CHDIR,NDIR)
*.==========>
*.           To return list of subdirectories in shared memory IPAWD
*..=========> ( R.Brun)
#include "hbook/hcdire.inc"
      CHARACTER*(*) CHDIR(*)
      DIMENSION IHDIR(4)
      DIMENSION IPAWD(100)
*.___________________________________________
      NW=IPAWD(1)
      KOF=IPAWD(2)
      ILAST=IPAWD(NW)
      JR1=ILAST-KOF
*
*          Search levels down
*
      IF(NLPAT.GT.1)THEN
         DO 50 IL=2,NLPAT
            CALL UCTOH(CHPAT(IL),IHDIR,4,16)
            JR1=IPAWD(JR1-1)-KOF
  30        IF(JR1.EQ.0)GO TO 99
            DO 40 I=1,4
               IF(IHDIR(I).NE.IPAWD(JR1+I+8))THEN
                  JR1=IPAWD(JR1)-KOF
                  GO TO 30
               ENDIF
  40        CONTINUE
  50     CONTINUE
      ENDIF
*
      JCDIR = JR1
      NDIR=0
      LF=IPAWD(JCDIR-1)
      IF(LF.NE.0)THEN
   60    IF(NDIR.GE.MAXDIR)GO TO 99
         NDIR=NDIR+1
         CHDIR(NDIR)=' '
         LF=LF-KOF
         CALL UHTOC(IPAWD(LF+9),4,CHDIR(NDIR),16)
         LF=IPAWD(LF)
         IF(LF.NE.0)GO TO 60
      ENDIF
  99  END

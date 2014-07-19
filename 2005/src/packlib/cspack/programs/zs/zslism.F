*
* $Id: zslism.F,v 1.1.1.1 1996/03/08 15:44:21 mclareni Exp $
*
* $Log: zslism.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:21  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
#if defined(CERNLIB_MMAP)
      SUBROUTINE ZSLISM(IPAWD)
*.==========>
*.           To list  histograms in shared memory IPAWD
*..=========> ( R.Brun)
#include "cspack/hcdire.inc"
*
      DIMENSION IHDIR(4)
      DIMENSION IPAWD(100)
*.___________________________________________
      NW=IPAWD(1)
      KOF=IPAWD(2)
      ILAST=IPAWD(NW)
      JR1=ILAST-KOF
*
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
      JTAB  = IPAWD(JCDIR-3)-KOF
*
      NTOT =IPAWD(JTAB+7)
      ISTAT=0
      DO 60 I=1,NTOT
         IF(IPAWD(JTAB-I).EQ.0)GO TO 60
         ID1=IPAWD(JTAB+I+8)
         CALL ZSLISM1(IPAWD(9),IPAWD(1),ID1,JTAB,KOF,ISTAT)
         IF(ISTAT.NE.0)GO TO 99
  60  CONTINUE
*
  99  RETURN
      END
#endif

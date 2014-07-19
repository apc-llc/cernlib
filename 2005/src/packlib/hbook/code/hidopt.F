*
* $Id: hidopt.F,v 1.1.1.1 1996/01/16 17:07:39 mclareni Exp $
*
* $Log: hidopt.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:39  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/04 02/06/94  14.13.27  by  Rene Brun
*-- Author :
      SUBROUTINE HIDOPT(ID1,KKOPT)
*.==========>
*.           To set options
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcbits.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcunit.inc"
      CHARACTER*(*)KKOPT
      CHARACTER*4 KOPT
*
      DIMENSION IOPT(31)
      EQUIVALENCE (IOPT(1),I1)
      CHARACTER*4 CHOPT(31),LCHOPT(21)
      SAVE CHOPT
      DATA CHOPT/'    ','    ','TABL','    ','    ','    ','STAT','    '
     +          ,'    ','    ','ERRO','    ','ROTA','PFUN','PLOW','PHIS'
     +          ,'    ','    ','    ','MAXI','MINI','INTE','2PAG','1EVL'
     +          ,'PSTA','LOGY','BLAC','STAR','PCHA','PCON','PERR'/
*.___________________________________________
      KOPT=KKOPT
      CALL CLTOU(KOPT)
      IRET=1
  10  CALL HLOOP(ID1,'HIDOPT',IRET)
      IF(IRET.EQ.0)RETURN
      CALL HDCOFL
*
      IF    (KOPT.EQ.'BLAC')THEN
         CALL HSIFLA(28,0)
         CALL HSIFLA(27,1)
         GO TO 90
      ENDIF
      IF(KOPT.EQ.'CONT')THEN
         CALL HSIFLA(27,0)
         CALL HSIFLA(28,0)
         GO TO 90
      ENDIF
      IF(KOPT.EQ.'STAR')THEN
         CALL HSIFLA(27,0)
         CALL HSIFLA(28,1)
         GO TO 90
      ENDIF
      IF(KOPT.EQ.'STAT')THEN
         CALL HSIFLA(7,1)
         GO TO 90
      ENDIF
      IF(KOPT.EQ.'NSTA')THEN
         CALL HSIFLA(7,0)
         GO TO 90
      ENDIF
      IF(KOPT.EQ.'TABL')THEN
         IF(I230.NE.0)THEN
            CALL HSIFLA(3,1)
            CALL HSIFLA(2,0)
         ENDIF
         GO TO 90
      ENDIF
      IF(KOPT.EQ.'SCAT')THEN
         IF(I230.NE.0)THEN
            CALL HSIFLA(3,0)
            CALL HSIFLA(2,1)
         ENDIF
         GO TO 90
      ENDIF
      IF(KOPT.EQ.'ERRO')THEN
         IF(I9+I10.EQ.0)CALL HSIFLA(11,1)
         GO TO 90
      ENDIF
      IF(KOPT.EQ.'NERR')THEN
         CALL HSIFLA(11,0)
         GO TO 90
      ENDIF
      IF(KOPT.EQ.'INTE')THEN
         CALL HSIFLA(22,1)
         GO TO 90
      ENDIF
      IF(KOPT.EQ.'NINT')THEN
         CALL HSIFLA(22,0)
         GO TO 90
      ENDIF
      IF(KOPT.EQ.'LOGY')THEN
         CALL HSIFLA(26,1)
         GO TO 90
      ENDIF
      IF(KOPT.EQ.'LINY')THEN
         CALL HSIFLA(26,0)
         GO TO 90
      ENDIF
      IF(KOPT.EQ.'PCHA')THEN
         CALL HSIFLA(29,0)
         GO TO 90
      ENDIF
      IF(KOPT.EQ.'NPCH')THEN
         CALL HSIFLA(29,1)
         GO TO 90
      ENDIF
      IF(KOPT.EQ.'PERR')THEN
         CALL HSIFLA(31,1)
         GO TO 90
      ENDIF
      IF(KOPT.EQ.'NPER')THEN
         CALL HSIFLA(31,0)
         GO TO 90
      ENDIF
      IF(KOPT.EQ.'PFUN')THEN
         CALL HSIFLA(14,1)
         GO TO 90
      ENDIF
      IF(KOPT.EQ.'NPFU')THEN
         CALL HSIFLA(14,0)
         GO TO 90
      ENDIF
      IF(KOPT.EQ.'PLOW')THEN
         CALL HSIFLA(15,0)
         GO TO 90
      ENDIF
      IF(KOPT.EQ.'NPLO')THEN
         CALL HSIFLA(15,1)
         GO TO 90
      ENDIF
      IF(KOPT.EQ.'PHIS')THEN
         CALL HSIFLA(16,0)
         GO TO 90
      ENDIF
      IF(KOPT.EQ.'NPHI')THEN
         CALL HSIFLA(16,1)
         GO TO 90
      ENDIF
      IF(KOPT.EQ.'PSTA')THEN
         CALL HSIFLA(25,0)
         GO TO 90
      ENDIF
      IF(KOPT.EQ.'NPST')THEN
         CALL HSIFLA(25,1)
         GO TO 90
      ENDIF
      IF(KOPT.EQ.'PCON')THEN
         CALL HSIFLA(30,0)
         GO TO 90
      ENDIF
      IF(KOPT.EQ.'NPCO')THEN
         CALL HSIFLA(30,1)
         GO TO 90
      ENDIF
      IF(KOPT.EQ.'ROTA')THEN
         CALL HSIFLA(13,1)
         GO TO 90
      ENDIF
      IF(KOPT.EQ.'NROT')THEN
         CALL HSIFLA(13,0)
         GO TO 90
      ENDIF
      IF(KOPT.EQ.'1EVL')THEN
         CALL HSIFLA(24,1)
         GO TO 90
      ENDIF
      IF(KOPT.EQ.'AEVL')THEN
         CALL HSIFLA(24,0)
         GO TO 90
      ENDIF
      IF(KOPT.EQ.'2PAG')THEN
         CALL HSIFLA(23,1)
         GO TO 90
      ENDIF
      IF(KOPT.EQ.'1PAG')THEN
         CALL HSIFLA(23,0)
         GO TO 90
      ENDIF
      IF(KOPT.EQ.'AUTO')THEN
         CALL HSIFLA(20,0)
         CALL HSIFLA(21,0)
         GO TO 90
      ENDIF
      IF(KOPT.EQ.'PROE')THEN
         LCONT=LQ(LCID-1)
         LW=LQ(LCONT)
         IF(LW.NE.0)CALL SBYT(0,IQ(LW),1,2)
         GO TO 90
      ENDIF
      IF(KOPT.EQ.'PROS')THEN
         LCONT=LQ(LCID-1)
         LW=LQ(LCONT)
         IF(LW.NE.0)CALL SBYT(1,IQ(LW),1,2)
         GO TO 90
      ENDIF
      IF(KOPT.EQ.'PROI')THEN
         LCONT=LQ(LCID-1)
         LW=LQ(LCONT)
         IF(LW.NE.0)CALL SBYT(2,IQ(LW),1,2)
         GO TO 90
      ENDIF
      IF(KOPT.EQ.'SETD')THEN
         DO 20 I=11,16
            CALL HSIFLA(I,0)
  20     CONTINUE
         DO 30 I=22,31
            CALL HSIFLA(I,0)
  30     CONTINUE
         GO TO 90
      ENDIF
      IF(KOPT.EQ.'SHOW')THEN
         NCH=0
         DO 40 I=3,31
            IF(CHOPT(I).EQ.'    ')GO TO 40
            IF(IOPT(I).EQ.0)GO TO 40
            IF(NCH.GT.20)GO TO 40
            NCH=NCH+1
            LCHOPT(NCH)=CHOPT(I)
  40     CONTINUE
         IF(I1.NE.0.AND.I8.NE.0)THEN
            LCONT=LQ(LCID-1)
            LW=LQ(LCONT)
            IF(LW.NE.0)THEN
               NCH=NCH+1
               IOPTS=JBYT(IQ(LW),1,2)
               IF(IOPTS.EQ.0)THEN
                  LCHOPT(NCH)='PROE'
               ELSEIF(IOPTS.EQ.1)THEN
                  LCHOPT(NCH)='PROS'
               ELSE
                  LCHOPT(NCH)='PROI'
               ENDIF
            ENDIF
         ENDIF
         IF(NCH.EQ.0)THEN
            WRITE(LOUT,1000)ID
         ELSE
            WRITE(LOUT,1100)ID,(LCHOPT(I),I=1,NCH)
         ENDIF
      ENDIF
  90  CALL HDCOFL
*
      IRET=2
      GO TO 10
 1000 FORMAT(' ID=',I8,' Options : NONE')
 1100 FORMAT(' ID=',I8,' Options :',20(1X,A4))
      END

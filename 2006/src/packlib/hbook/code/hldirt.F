*
* $Id: hldirt.F,v 1.1.1.1 1996/01/16 17:07:42 mclareni Exp $
*
* $Log: hldirt.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:42  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.23/01 15/11/94  18.29.06  by  Fons Rademakers
*-- Author :    Rene Brun   17/07/91
      SUBROUTINE HLDIRT(CHDIR)
*.==========>
*.           To list the contents of a RZ directory
*.           in format ID  Title
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcunit.inc"
#include "hbook/hcntpar.inc"
      CHARACTER*(*) CHDIR
      COMMON/QUEST/IQUEST(100)
      CHARACTER*1 HTYPE
      INTEGER     KEYS(2)
*.___________________________________________
*
*             Write name of current directory
*
      NCH=LENOCC(CHDIR)
      WRITE(LOUT,1000)CHDIR(1:NCH)
*
*--        Sort directory if IOPTS set
      IOPTS=IQUEST(88)
      IOPTN=IQUEST(89)
      IF(IOPTS.NE.0)CALL HRSORT('S')
*
*
*          Find first ID in the RZ directory
*
      KEYNUM  = 1
      KEYS(1) = KEYNUM
      KEYS(2) = 0
      CALL HRZIN(IHWORK,0,0,KEYS,9999,'SC')
      IDN=IQUEST(21)
      IQ42=IQUEST(22)
*
*             Enough space left ?
*
  10  IF (IDN .EQ. 0) GOTO 90
      KEYS(1) = KEYNUM
      CALL HRZIN(IHWORK,0,0,KEYS,9999,'SNC')
      IF(IQUEST(1).NE.0)GO TO 90
      IDN =IQUEST(21)
      IQ40=IQUEST(40)
      IQ41=IQUEST(41)
      IQ42=IQUEST(42)
      IF(IQ40.EQ.0) IQ41=0
      NWORDS=IQUEST(12)
      IOPTA=JBIT(IQUEST(14),4)
      IF(IOPTA.NE.0)GO TO 40
      CALL HSPACE(NWORDS+1000,'HLDIR ',IDN)
      IF(IERR.NE.0)                    GO TO 90
*
*             Read histogram data structure
*
      CALL HRZIN(IHWORK,LHWORK,1,KEYS,9999,'SND')
      IF(IQUEST(1).NE.0)THEN
         CALL HBUG('Bad sequence for RZ','HLDIR',IDN)
         GO TO 90
      ENDIF
*
      IF(IQ(LHWORK-2).EQ.0)THEN
         WRITE(LOUT,2100)IDN
      ELSEIF(JBIT(IQ(LHWORK+KBITS),1).NE.0)THEN
         IF(IOPTN.EQ.0)THEN
            HTYPE='1'
            NWTITL=IQ(LHWORK-1)-KTIT1+1
            WRITE(LOUT,2000)IDN,HTYPE,(IQ(LHWORK+KTIT1+I-1),I=1,NWTITL)
         ENDIF
      ELSEIF(JBYT(IQ(LHWORK+KBITS),2,2).NE.0)THEN
         IF(IOPTN.EQ.0)THEN
            HTYPE='2'
            NWTITL=IQ(LHWORK-1)-KTIT2+1
            WRITE(LOUT,2000)IDN,HTYPE,(IQ(LHWORK+KTIT2+I-1),I=1,NWTITL)
         ENDIF
      ELSEIF(JBIT(IQ(LHWORK+KBITS),4).NE.0)THEN
         HTYPE='N'
         IF (IQ(LHWORK-2) .EQ. 2) THEN
            ITIT1=IQ(LHWORK+9)
            NWTITL=IQ(LHWORK+8)
         ELSE
            ITIT1=IQ(LHWORK+ZITIT1)
            NWTITL=IQ(LHWORK+ZNWTIT)
         ENDIF
         WRITE(LOUT,2000)IDN,HTYPE,(IQ(LHWORK+ITIT1+I-1),I=1,NWTITL)
      ENDIF
*
      CALL MZDROP(IHWORK,LHWORK,' ')
  40  LHWORK=0
*
      IF(IQ40.EQ.0)THEN
         CALL MZWIPE(IHWORK)
         GO TO 99
      ENDIF
      KEYNUM=KEYNUM+1
      IDN=IQ41
      GO TO 10
*
  90  CONTINUE
*
 1000 FORMAT(//,' ===> Directory : ',A)
 2000 FORMAT(1X,I10,1X,'(',A,')',3X,20A4)
 2100 FORMAT(1X,I10,1X,'(A)   Unnamed array')
  99  RETURN
      END
 

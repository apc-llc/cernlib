*
* $Id: hldir1.F,v 1.1.1.1 1996/01/16 17:07:42 mclareni Exp $
*
* $Log: hldir1.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:42  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/09 08/02/93  12.59.16  by  Rene Brun
*-- Author :
      SUBROUTINE HLDIR1(IOPTI,IOPTN,IOPTT)
*.==========>
*.      To list the contents of directory pointed by LCDIR.
*.      If IOPTI=1 lists with HINDEX format
*.      If IOPTN=1 lists only Ntuples
*.      If IOPTT=1 lists subdirectories
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcunit.inc"
#include "hbook/hcbits.inc"
#include "hbook/hcmail.inc"
#include "hbook/hcdire.inc"
#include "hbook/hcntpar.inc"
      CHARACTER*1 HTYPE
*.___________________________________________
*
*             Write name of directory pointed by LCDIR
*
      CHMAIL=' '
      CALL HPAFF(CHCDIR,NLPAT,CHMAIL)
      NCH=LENOCC(CHMAIL)
      WRITE(LOUT,1000)CHMAIL(1:NCH)
      IF(IOPTT.EQ.0)GO TO 99
*
      IF(IOPTI.NE.0)THEN
         CALL HINDEX
         GO TO 99
      ENDIF
*
      IDD=0
      IRET=3
  10  CALL HLOOP(IDD,'HLDIR ',IRET)
      IF(IRET.EQ.0)GO TO 99
*
      CALL HDCOFL
      IF(I1.NE.0)THEN
         IF(IOPTN.EQ.0)THEN
            HTYPE='1'
            NWTITL=IQ(LCID-1)-KTIT1+1
            WRITE(LOUT,2000)ID,HTYPE,(IQ(LCID+KTIT1+I-1),I=1,NWTITL)
         ENDIF
      ELSEIF(I230.NE.0)THEN
         IF(IOPTN.EQ.0)THEN
            HTYPE='2'
            NWTITL=IQ(LCID-1)-KTIT2+1
            WRITE(LOUT,2000)ID,HTYPE,(IQ(LCID+KTIT2+I-1),I=1,NWTITL)
         ENDIF
      ELSEIF(I4.NE.0)THEN
         HTYPE='N'
         IF (IQ(LCID-2) .EQ. 2) THEN
            ITIT1=IQ(LCID+9)
            NWTITL=IQ(LCID+8)
         ELSE
            ITIT1=IQ(LCID+ZITIT1)
            NWTITL=IQ(LCID+ZNWTIT)
         ENDIF
         WRITE(LOUT,2000)ID,HTYPE,(IQ(LCID+ITIT1+I-1),I=1,NWTITL)
      ELSE
         WRITE(LOUT,2100)ID
      ENDIF
      IRET=2
      GO TO 10
*
 1000 FORMAT(//,' ===> Directory : ',A)
 2000 FORMAT(1X,I10,1X,'(',A,')',3X,20A4)
 2100 FORMAT(1X,I10,1X,'(A)   Unnamed array')
  99  RETURN
      END

*
* $Id: pahope.F,v 1.3 2001/06/13 08:46:53 couet Exp $
*
* $Log: pahope.F,v $
* Revision 1.3  2001/06/13 08:46:53  couet
* - The bug fixe we did to avoid deleting 8-digit histogram IDs after
*   an histogram operation had a side effect which prevent the following
*   macro to work as in the past:
*
*   h/file 45 behist_test.rz
*   h/copy 11 991
*   h/copy 12 992
*   opera/add 991 992 993
*   h/plot 991
*
*   The histogram 991 was not seen.
*
* Revision 1.2  2001/02/20 14:37:31  couet
* - If one used a 8-digit histogramm-identifier [hid1] in:
*     h/op/div [hid1] [hid2] [hid3]
*   the result [hid3] was created but [hid1] was deleted afterwards.
*
* Revision 1.1.1.1  1996/03/01 11:38:40  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.05/10 27/06/94  19.27.54  by  Rene Brun
*-- Author :    Rene Brun   03/01/89
      SUBROUTINE PAHOPE
*
*           /HISTOGRAM/OPERATIONS
*
#include "hbook/hcbook.inc"
#include "paw/pawcom.inc"
#include "paw/pcpatl.inc"
#include "paw/pcchar.inc"
      CHARACTER*8 CHOPT
      CHARACTER*1 OPTION,CHOPT1
*
      CALL KUPATL(CHPATL,NPAR)
*
      CHOPT1=' '
      IF(CHPATL.EQ.'ADD')THEN
         CHOPT1='+'
      ENDIF
*
      IF(CHPATL.EQ.'SUBTRACT')THEN
         CHOPT1='-'
      ENDIF
*
      IF(CHPATL.EQ.'MULTIPLY')THEN
         CHOPT1='*'
      ENDIF
*
      IF(CHPATL.EQ.'DIVIDE')THEN
         CHOPT1='/'
      ENDIF
*
      IF(CHOPT1.NE.' ')THEN
         CALL HCDIR(CHTITL,'R')
         CALL KUGETC(CHID,N)
         JOFSET=10000000
         CALL HGETID(CHID)
         IF(LCID.LE.0)GO TO 99
         ID1=ID
         KHRIN1=KHRIN
         CALL HCDIR(CHTITL,' ')
         CALL KUGETC(CHID,N)
         JOFSET=20000000
         CALL HGETID(CHID)
         IF(LCID.LE.0)GO TO 99
         ID2=ID
         KHRIN2=KHRIN
         CALL HCDIR(CHTITL,' ')
         CALL PAGETI(ID3)
         CALL KUGETR(C1)
         CALL KUGETR(C2)
         CALL KUGETC(OPTION,NCH)
         CHOPT=CHOPT1//OPTION
         CALL HOPERA(ID1,CHOPT,ID2,ID3,C1,C2)
         IF(ID2.GT.20000000.AND.KHRIN2.NE.0)CALL HDELET(ID2)
         IF(ID1.GT.10000000.AND.KHRIN1.NE.0)CALL HDELET(ID1)
         GO TO 99
      ENDIF
*
      IF(CHPATL.EQ.'RESET')THEN
         CALL KUGETC(CHID,N)
         IDOLD=1
         CALL HGETID(CHID)
         IDOLD=0
         IF(ID.NE.0.AND.LCID.LE.0)GO TO 99
         CALL KUGETS(CHTITL,NCH)
         CALL HRESET(ID,CHTITL)
         CALL HSETCD
         GO TO 99
      ENDIF
*
      IF(CHPATL.EQ.'SORT')THEN
         CALL KUGETC(CHID,N)
         CALL HGETID(CHID)
         IF(ID.NE.0.AND.LCID.LE.0)GO TO 99
         CALL KUGETS(CHTITL,NCH)
         CHTEMP(1:1) = 'S'
         CHTEMP(2:)  = CHTITL(1:LENOCC(CHTITL))
         CALL HLABEL(ID,0,' ',CHTEMP)
         CALL HSETCD
         GO TO 99
      ENDIF
*
      IF(CHPATL.EQ.'DIFF')THEN
         CALL HCDIR(CHTITL,'R')
         CALL KUGETC(CHID,N)
         JOFSET=10000000
         CALL HGETID(CHID)
         IF(LCID.LE.0)GOTO 99
         ID1=ID
         KHRIN1=KHRIN
         CALL KUGETC(CHID,N)
         JOFSET=20000000
         CALL HGETID(CHID)
         IF(LCID.LE.0)GOTO 99
         ID2=ID
         KHRIN2=KHRIN
         CALL KUGETC(CHOPT,NCH)
         CALL HDIFF(ID1,ID2,PRB,CHOPT)
         CALL HCDIR(CHTITL,' ')
         IF(ID2.GT.20000000.AND.KHRIN2.NE.0)CALL HDELET(ID2)
         IF(ID1.GT.10000000.AND.KHRIN1.NE.0)CALL HDELET(ID1)
      ENDIF
*
  99  JOFSET = 0
*
      END

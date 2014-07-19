*
* $Id: zepex2.F,v 1.1.1.1 1996/03/08 12:01:14 mclareni Exp $
*
* $Log: zepex2.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:14  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
      SUBROUTINE ZEPEX2
C
C    EXAMPLE 11:  - USAGE OF ZEPNXT, ZEPCAT, ZEPIN TO READ THE
C                   FILE WRITTEN IN EXAMPLE 10
C
      COMMON/ZEPCMM/Z(1),ID1,ID2(3),ID3,IDF,WS(1)
      DIMENSION IZ(10000),IWS(1)
      EQUIVALENCE (IZ(1),Z(1)), (IWS(1),WS(1))
C
      DIMENSION KEYI(3)
C
      DATA KEYI,LUNI,LREC/1000, 2000, 3000,    2,    0/
C
      CALL ZINIT (IZ,WS,10000)
C
      CALL ZEPKEY (IZ,ID1,1000,  1, 0,IERR)
      CALL ZEPKEY (IZ,ID2,2000,  3, 0,IERR)
      CALL ZEPKEY (IZ,ID3,3000,  1, 0,IERR)
C
      CALL ZEPDEF (IZ,LUNI,LREC,IERR)
      IF (IERR.NE.0)                             GO TO 999
C
C    LOOP ON EVENTS
C
      DO  40 IEVENT = 1,999
C
      CALL ZBOOK (IZ,IDF,0)
C
   10 CALL ZEPNXT (IZ,LUNI,LABEL,IERR)
      IF (IERR.NE.0)                             GO TO 999
C
      WRITE(6,1000) LABEL
      IF (IEVENT.GT.1)                           GO TO 20
C
      CALL ZEPCAT (IZ,LUNI,JCAT,LABEL,IERR)
      IF (IERR.NE.0)                             GO TO 999
C
      CALL ZPTREE (IZ,JCAT,8HCATALOG$,1HI)
C
   20 IF (LABEL.NE.123)                          GO TO 10
C
      CALL ZEPIN (IZ,LUNI,KEYI(1),3,LABEL,IERR)
      IF (IERR.NE.0)                             GO TO 999
C
      CALL ZPTREE (IZ,ID1,'ID1$',1HI)
      CALL ZPTREE (IZ,ID3,'ID3$',1HF)
C
      DO  30 I = 1,3
   30 CALL ZPTREE (IZ,ID2(I),'ID2$',1HI)
C
   40 CALL ZAGAIN (IZ,IDF)
C
  999 RETURN
 1000 FORMAT ('0',20('*'),10X,'LABEL',9X,I3,13X,20('*'))
      END

*
* $Id: hcopy.F,v 1.1.1.1 1996/01/16 17:07:34 mclareni Exp $
*
* $Log: hcopy.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:34  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.20/08 12/09/93  18.56.12  by  Rene Brun
*-- Author :
      SUBROUTINE HCOPY(ID1,ID2,CHTITL)
*.==========>
*.           COPY ID1 ON ID2
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcbits.inc"
      CHARACTER*(*) CHTITL
*.___________________________________________
      IF(ID2.EQ.ID1)GO TO 90
      IF(ID2.EQ.0)GO TO 90
      NRHIST=IQ(LCDIR+KNRH)
      CALL HFIND(ID1,'HCOPY ')
      IF(LCID.EQ.0)GO TO 99
      IDPOS=LOCATI(IQ(LTAB+1),NRHIST,ID2)
      IF(IDPOS.GT.0)THEN
         CALL HBUG('+Already existing histogram replaced','HCOPY',ID2)
         CALL HDELET(ID2)
         NRHIST=IQ(LCDIR+KNRH)
         IDPOS=-IDPOS+1
      ENDIF
      CALL HFIND(ID1,'HCOPY ')
*
      CALL HDCOFL
      ID=ID2
      NARG=3
      CALL NOARG(NARG)
*
*             Get new title (if any)
*
      IF(I1.NE.0)THEN
         NWOLD=IQ(LCID-1)-KTIT1+1
      ELSEIF(I230.NE.0)THEN
         NWOLD=IQ(LCID-1)-KTIT2+1
      ELSE
         NWOLD=0
      ENDIF
      NWTITL=NWOLD
      IF(NARG.EQ.3)THEN
         CALL HBTIT(CHTITL,NWTITL,NCHT)
      ENDIF
*
      NMORE=NWTITL-NWOLD
      IF(NWTITL.EQ.0)NMORE=0
      N2=IQ(LCID+KNTOT)+NMORE
      CALL HSPACE(N2+1000,'HCOPY ',ID1)
      IF(IERR.NE.0)GO TO 99
*
*             Enter ID in the list of ordered IDs
*
      IDPOS=-IDPOS+1
      IF(NRHIST.GE.IQ(LTAB-1))THEN
         CALL MZPUSH(IHDIV,LTAB,500,500,' ')
      ENDIF
      DO 10 I=NRHIST,IDPOS,-1
         IQ(LTAB+I+1)=IQ(LTAB+I)
         LQ(LTAB-I-1)=LQ(LTAB-I)
  10  CONTINUE
*
*             Copy ID1 data structure
*
      LLID=LQ(LCDIR-9)
      CALL MZCOPY(IHDIV,LCID,IHDIV,LLID,0,' ')
      LCID=LQ(LLID)
      LLID=LCID
      CALL SBIT0(IQ(LCID),5)
      IQ(LCID+KNTOT)=N2
      LQ(LCDIR-9)=LLID
      IQ(LCID-5)=ID2
      IQ(LTAB+IDPOS)=ID2
      LQ(LTAB-IDPOS)=LCID
      NRHIST=NRHIST+1
      IQ(LCDIR+KNRH)=NRHIST
*
*             Adjust size of top bank for new title
*
      IF(NMORE.NE.0)THEN
         CALL MZPUSH(IHDIV,LCID,0,NMORE,' ')
      ENDIF
*
*             Enter new title
*
      IF(NWTITL.NE.0)THEN
         IF(I1.NE.0)THEN
            J1=KTIT1
         ELSE
            J1=KTIT2
         ENDIF
         CALL UCTOH(CHTITL,IQ(LCID+J1),4,NCHT)
      ENDIF
      GO TO 99
*
  90  CALL HBUG('Wrong parameter','HCOPY ',ID1)
*
  99  RETURN
      END

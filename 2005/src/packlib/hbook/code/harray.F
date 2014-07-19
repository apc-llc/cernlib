*
* $Id: harray.F,v 1.1.1.1 1996/01/16 17:07:31 mclareni Exp $
*
* $Log: harray.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:31  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.20/08 12/09/93  18.54.40  by  Rene Brun
*-- Author :
      SUBROUTINE HARRAY(IDD,N,ILOC)
*.==========>
*.           reserve n consecutive words in /PAWC/ for user
*.           the 1st free address is ILOC+1
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
*.___________________________________________
*
*             Check if ID already in the table
*
      ILOC=0
      NRHIST=IQ(LCDIR+KNRH)
      IDPOS=LOCATI(IQ(LTAB+1),NRHIST,IDD)
      IF(IDPOS.GT.0)THEN
         CALL HBUG('+Already existing array','HARRAY',IDD)
         CALL HDELET(IDD)
         NRHIST=IQ(LCDIR+KNRH)
         IDPOS=-IDPOS+1
      ENDIF
      ID=IDD
*
      CALL HSPACE(N+1000,'HARRAY',IDD)
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
*             Build array bank
*
      IF(LIDS.EQ.0)THEN
         CALL MZBOOK(IHDIV,LIDS,LCDIR,-2,'HIDA',0,0,N,3,0)
         LCID=LIDS
      ELSE
         LLID=LQ(LCDIR-9)
         CALL MZBOOK(IHDIV,LCID,LLID,  0,'HIDA',0,0,N,3,0)
      ENDIF
      LQ(LCDIR-9)=LCID
*
      IQ(LCID-5)=ID
      IQ(LTAB+IDPOS)=ID
      LQ(LTAB-IDPOS)=LCID
      ILOC=LCID
      NRHIST=NRHIST+1
      IQ(LCDIR+KNRH)=NRHIST
*
  99  RETURN
      END

*
* $Id: hcopyt.F,v 1.1.1.1 1996/01/16 17:07:34 mclareni Exp $
*
* $Log: hcopyt.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:34  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.21/00 23/10/93  16.55.46  by  Rene Brun
*-- Author :
      SUBROUTINE HCOPYT(IB,LB,ID1,IOFSET,JCID,KOF)
*.==========>
*.           Auxiliary for HCOPYM/HCOPYN
*.           Copy one Ntuple from mapped /PAWC/ to current /PAWC/
*..=========> ( R.Brun)
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcform.inc"
      DIMENSION IB(1),LB(1)
*.___________________________________________
      NDIM  = IB(JCID+2)
      JCONT = LB(JCID-1)-KOF
      JLIMS = LB(JCID-2)-KOF
      NWID  = IB(JCID-1)
      NTOT  = NWID+IB(JCONT-1)+IB(JLIMS-1)+100
*
*             Check if ID already in the table
*
      ID=ID1+IOFSET
      NRHIST=IQ(LCDIR+KNRH)
      IDPOS=LOCATI(IQ(LTAB+1),NRHIST,ID)
      IF(IDPOS.GT.0)THEN
         CALL HBUG('+Already existing Ntuple replaced','HCOPYM',ID)
         CALL HDELET(ID)
         NRHIST=IQ(LCDIR+KNRH)
         IDPOS=-IDPOS+1
      ENDIF
*
*             Enough space left to copy ID ?
*
      CALL HSPACE(NTOT+1000,'HCOPYM',ID1)
      IF(IERR.NE.0)                    GO TO 99
*
*             Enter ID in the list of ordered IDs
*
      IDPOS=-IDPOS+1
      IF(NRHIST.GE.IQ(LTAB-1))THEN
         CALL MZPUSH(IHDIV,LTAB,500,500,' ')
      ENDIF
      DO 30 I=NRHIST,IDPOS,-1
         IQ(LTAB+I+1)=IQ(LTAB+I)
         LQ(LTAB-I-1)=LQ(LTAB-I)
  30  CONTINUE
*
*             Build top level bank
*
      IF(LIDS.EQ.0)THEN
         CALL MZBOOK(IHDIV,LIDS,LCDIR,-2,'HIDN',3,2,NWID,IOHN,0)
         LCID=LIDS
      ELSE
         LLID=LQ(LCDIR-9)
         CALL MZBOOK(IHDIV,LCID,LLID,  0,'HIDN',3,2,NWID,IOHN,0)
      ENDIF
      CALL UCOPY(IB(JCID+1),IQ(LCID+1),NWID)
      LQ(LCDIR-9)=LCID
      IQ(LCID-5)=ID
      IQ(LTAB+IDPOS)=ID
      LQ(LTAB-IDPOS)=LCID
      NRHIST=NRHIST+1
      IQ(LCDIR+KNRH)=NRHIST
*
      NW=IB(JCONT-1)
      CALL MZBOOK(IHDIV,LCONT,LCID,-1,'HCON',0,0,NW,3,0)
      CALL MZBOOK(IHDIV,LLIMS,LCID,-2,'HLIM',0,0,2*NDIM,3,0)
      CALL UCOPY(IB(JCONT+1),IQ(LCONT+1),NW)
      CALL UCOPY(IB(JLIMS+1),IQ(LLIMS+1),2*NDIM)
      JR1=LB(JCONT)-KOF
      LR1=LCONT
      IF(LB(JCONT).NE.0)THEN
  40     NW=IB(JR1-1)
         LR2=LR1
         CALL MZBOOK(IHDIV,LR1,LR2,0,'HCON',0,0,NW,3,0)
         CALL UCOPY(IB(JR1+1),IQ(LR1+1),NW)
         IF(LB(JR1).NE.0)THEN
            JR1=LB(JR1)-KOF
            GO TO 40
         ENDIF
      ENDIF
      LQ(LCID-3)=LR1
*
  99  RETURN
      END

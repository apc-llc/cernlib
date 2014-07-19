*
* $Id: hcopyn.F,v 1.1.1.1 1996/01/16 17:07:34 mclareni Exp $
*
* $Log: hcopyn.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:34  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/11 23/08/94  14.17.45  by  Rene Brun
*-- Author :
      SUBROUTINE HCOPYN(IB,LB,ID1,IOFSET,JTAB,KOF)
*.==========>
*.           Auxiliary for HCOPYM
*.           Copy one histogram from mapped /PAWC/ to current /PAWC/
*..=========> ( R.Brun)
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcform.inc"
#include "hbook/hcbits.inc"
      CHARACTER*4 BNAME
      DIMENSION IB(1),LB(1)
*.___________________________________________
      NH=IB(JTAB-1)
      DO 10 I=1,NH
         IF(IB(JTAB+I).EQ.ID1)GO TO 20
   10 CONTINUE
      GO TO 99
*
   20 CONTINUE
      ID=ID1+IOFSET
      JCID=LB(JTAB-I)-KOF
      I4=JBIT(IB(JCID+KBITS),4)
      IF(I4.NE.0)THEN
         CALL HCOPYT(IB,LB,ID1,IOFSET,JCID,KOF)
         GO TO 99
      ENDIF
      I123=JBYT(IB(JCID+KBITS),1,3)
      NTOT=IB(JCID+KNTOT)
      IF(I123.EQ.0)THEN
         CALL HBUG('Object is not an histogram','HCOPYM',ID1)
         GO TO 99
      ENDIF
      JCONT=LB(JCID-1)-KOF
      NWID=IB(JCID-1)
      NBPROX=IB(JCONT+KNBIT)
      I1=JBIT(I123,1)
*
*             Check if ID already in the table
*
      NRHIST=IQ(LCDIR+KNRH)
      IDPOS=LOCATI(IQ(LTAB+1),NRHIST,ID)
      IF(IDPOS.GT.0)THEN
         CALL HBUG('+Already existing histogram replaced','HCOPYM',ID)
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
   30 CONTINUE
*
*             Build top level bank
*
      IF(I1.NE.0)THEN
         IODES=IOH1
         BNAME='HID1'
         NL=1
      ELSE
         IODES=IOH2
         BNAME='HID2'
         NL=7
      ENDIF
*
      IF(LIDS.EQ.0)THEN
         CALL MZBOOK(IHDIV,LIDS,LCDIR,-2,BNAME,NL,NL,NWID,IODES,0)
         LCID=LIDS
      ELSE
         LLID=LQ(LCDIR-9)
         CALL MZBOOK(IHDIV,LCID,LLID,  0,BNAME,NL,NL,NWID,IODES,0)
      ENDIF
      CALL UCOPY(IB(JCID+1),IQ(LCID+1),NWID)
      LQ(LCDIR-9)=LCID
      IQ(LCID-5)=ID
      IQ(LTAB+IDPOS)=ID
      LQ(LTAB-IDPOS)=LCID
      NRHIST=NRHIST+1
      IQ(LCDIR+KNRH)=NRHIST
*
*           1-DIM case
*
      IF(I1.NE.0)THEN
         IF(NBPROX.GE.32)THEN
            IODES=IOCF1
         ELSE
            IODES=IOCB1
         ENDIF
         NW=IB(JCONT-1)
         CALL MZBOOK(IHDIV,LCONT,LCID,-1,'HCO1',2,2,NW,IODES,0)
         CALL UCOPY(IB(JCONT+1),IQ(LCONT+1),NW)
         IF(LB(JCONT).NE.0)THEN
            JR1=LB(JCONT)-KOF
            NW=IB(JR1-1)
            CALL MZBOOK(IHDIV,LR1,LCONT,0,'HI1E',0,0,NW,3,0)
            CALL UCOPY(IB(JR1+1),IQ(LR1+1),NW)
            IF(JBIT(IB(JR1),1).NE.0)CALL SBIT1(IQ(LR1),1)
            IF(LB(JR1).NE.0)THEN
               JR2=LB(JR1)-KOF
               NW=IB(JR2-1)
               CALL MZBOOK(IHDIV,LR2,LR1,0,'HI1N',0,0,NW,3,0)
               CALL UCOPY(IB(JR2+1),IQ(LR2+1),NW)
            ENDIF
         ENDIF
         IF(LB(JCONT-1).NE.0)THEN
            JFUNC=LB(JCONT-1) -KOF
            NW=IB(JFUNC-1)
            CALL MZBOOK(IHDIV,LFUNC,LCONT,-1,'HFUN',0,0,NW,IOCF2,0)
            CALL UCOPY(IB(JFUNC+1),IQ(LFUNC+1),NW)
         ENDIF
*????????????????? Should copy LHFIT structure here.
         GO TO 99
      ENDIF
*
*           2-DIM case
*
      IF(NBPROX.GE.32)THEN
         IODES=IOCF2
      ELSE
         IODES=IOCB2
      ENDIF
      NW=IB(JCONT-1)
      CALL MZBOOK(IHDIV,LCONT,LCID,-1,'HCO2',2,2,NW,IODES,0)
      CALL UCOPY(IB(JCONT+1),IQ(LCONT+1),NW)
* New (30/07/92) 2-D contents bank has 2 structural links.  If old histogram
*   was old style, NTOT should should be increased by 2!
      IF(IB(JCONT-2).EQ.0)IQ(LCID+KNTOT)=IQ(LCID+KNTOT)+2
*
*????????????? Should copy LFUNC and LHFIT structures here.
*
*           PROX
*
      IF(LB(JCID-2).NE.0)THEN
         JCONT=LB(JCID-2)-KOF
         NBPROX=IB(JCONT+KNBIT)
         IF(NBPROX.GE.32)THEN
            IODES=IOCF1
         ELSE
            IODES=IOCB1
         ENDIF
         NW=IB(JCONT-1)
         CALL MZBOOK(IHDIV,LCONT,LCID,-2,'PROX',2,2,NW,IODES,0)
         CALL UCOPY(IB(JCONT+1),IQ(LCONT+1),NW)
         IF(LB(JCONT).NE.0)THEN
            JW=LB(JCONT)-KOF
            NW=IB(JW-1)
            CALL MZBOOK(IHDIV,LW,LCONT,0,'PRXE',0,0,NW,3,0)
            CALL UCOPY(IB(JW+1),IQ(LW+1),NW)
         ENDIF
         IF(LB(JCONT-1).NE.0)THEN
            JFUNC=LB(JCONT-1)-KOF
            NW=IB(JFUNC-1)
            CALL MZBOOK(IHDIV,LFUNC,LCONT,-1,'HFUN',0,0,NW,IOCF2,0)
            CALL UCOPY(IB(JFUNC+1),IQ(LFUNC+1),NW)
         ENDIF
      ENDIF
*
*           PROY
*
      IF(LB(JCID-3).NE.0)THEN
         JCONT=LB(JCID-3)-KOF
         NBPROX=IB(JCONT+KNBIT)
         IF(NBPROX.GE.32)THEN
            IODES=IOCF1
         ELSE
            IODES=IOCB1
         ENDIF
         NW=IB(JCONT-1)
         CALL MZBOOK(IHDIV,LCONT,LCID,-3,'PROY',2,2,NW,IODES,0)
         CALL UCOPY(IB(JCONT+1),IQ(LCONT+1),NW)
         IF(LB(JCONT).NE.0)THEN
            JW=LB(JCONT)-KOF
            NW=IB(JW-1)
            CALL MZBOOK(IHDIV,LW,LCONT,0,'PRYE',0,0,NW,3,0)
            CALL UCOPY(IB(JW+1),IQ(LW+1),NW)
         ENDIF
         IF(LB(JCONT-1).NE.0)THEN
            JFUNC=LB(JCONT-1)-KOF
            NW=IB(JFUNC-1)
            CALL MZBOOK(IHDIV,LFUNC,LCONT,-1,'HFUN',0,0,NW,IOCF2,0)
            CALL UCOPY(IB(JFUNC+1),IQ(LFUNC+1),NW)
         ENDIF
      ENDIF
*
*           SLIX
*
      IF(LB(JCID-4).NE.0)THEN
         JSLIX=LB(JCID-4)
         DO 40 I=1,IB(JSLIX-2)
**               CALL HREZ0(LQ(LSLIX-I))
   40    CONTINUE
      ENDIF
*
*           SLIY
*
      IF(LB(JCID-5).NE.0)THEN
         JSLIY=LB(JCID-5)
         DO 50 I=1,IB(JSLIY-2)
**               CALL HREZ0(LQ(LSLIY-I))
   50    CONTINUE
      ENDIF
*
*           BANX
*
      JBANX=LB(JCID-6)
   60 IF(JBANX.NE.0)THEN
**            CALL HREZ0(LQ(LBANX-1))
         JBANX=LB(JBANX)
         IF(JBANX.NE.0)GO TO 60
      ENDIF
*
*           BANY
*
      JBANY=LB(JCID-7)
   70 IF(JBANY.NE.0)THEN
**            CALL HREZ0(LQ(LBANY-1))
         JBANY=LB(JBANY)
         IF(JBANY.NE.0)GO TO 70
      ENDIF
 
   99 RETURN
      END

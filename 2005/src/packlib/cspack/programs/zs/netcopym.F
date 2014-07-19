*
* $Id: netcopym.F,v 1.1.1.1 1996/03/08 15:44:21 mclareni Exp $
*
* $Log: netcopym.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:21  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
#if defined(CERNLIB_OS9)
      SUBROUTINE Net_HCOPYM(ID1,istat)
*.==========>
*.           To copy histogram ID from (cpu,module) to /PAWC/
*..=========> ( R.Brun)
*KEEP,HCBOOK
      PARAMETER(KNCX=3,KXMIN=4,KXMAX=5,KBWIDX=6,KMIN=7,KMAX=8,KNORM=9,
     +          KTIT1=10,KNCY=7,KYMIN=8,KYMAX=9,KBWIDY=10,KSCAL2=11,
     +          KTIT2=12,KNBIT=1,KNOENT=2,KSTAT1=3,KNSDIR=5,KNRH=6,
     +          KCON1=9,KCON2=3,KBITS=1,KNTOT=2)
*
#include "cspack/pawc.inc"
      DIMENSION IQ(2),Q(2),LQ(8000)
      EQUIVALENCE (LQ(1),LMAIN),(IQ(1),LQ(9)),(Q(1),IQ(1))
      COMMON/HCBOOK/HVERSN,IHWORK,LHBOOK,LHPLOT,LGTIT,LHWORK,
     +LCDIR,LSDIR,LIDS,LTAB,LCID,LCONT,LSCAT,LPROX,LPROY,LSLIX,
     +LSLIY,LBANX,LBANY,LPRX,LPRY,LFIX,LLID,LR1,LR2,LCIDN
*
*KEEP,HCFLAG
      COMMON/HCFLAG/ID    ,IDBADD,LID   ,IDLAST,IDHOLD,NBIT  ,NBITCH,
     +       NCHAR ,NRHIST,IERR  ,NV
*
*KEEP,HCFORM
      COMMON/HCFORM/IODIR,IOH1,IOH2,IOHN,IOCF1,IOCF2,IOCB1,IOCB2,IFRM(2)
*
*KEEP,HCBITS
      COMMON / HCBITS  / I1,   I2,   I3,   I4,   I5,   I6,   I7,   I8,
     +                  I9,   I10,  I11,  I12,  I13,  I14,  I15,  I16,
     +I17,  I18,  I19,  I20,  I21,  I22,  I23,  I24,  I25,  I26,  I27,
     +I28,  I29,  I30,  I31,  I32,  I33,  I34,  I35,  I123, I230
*
#include "cspack/hos9c.inc"
      parameter (klq =-8)
*.___________________________________________
      istat=0
      DO 10 ihist=1,nhist
         if(ihead(kid,ihist).eq.id1)go to 20
  10  CONTINUE
      GO TO 90
*
  20  CONTINUE
      ID=ID1
      ntot  = ihead(kntot  ,ihist)
      jcont = ihead(kjcont ,ihist)
      nwid  = ihead(knwid  ,ihist)
      nw    = ihead(knw    ,ihist)
      nbprox= ihead(knbprox,ihist)
      jr1   = ihead(kjr1   ,ihist)
      i123  = jbyt(ihead(kbits,ihist),1,3)
      IF(I123.EQ.0)THEN
         CALL HBUG('Object is not histogram','HCOPYM',ID1)
         GO TO 90
      ENDIF
      I1=JBIT(I123,1)
*
*             Check if ID already in the table
*
      NRHIST=IQ(LCDIR+KNRH)
      IDPOS=LOCATI(IQ(LTAB+1),NRHIST,ID)
      IF(IDPOS.GT.0)THEN
         CALL HBUG('Already existing histogram replaced','HCOPYM',ID)
         CALL HDELET(ID)
         NRHIST=IQ(LCDIR+KNRH)
         IDPOS=-IDPOS+1
      ENDIF
*
*             Enough space left to copy ID
*
      CALL HSPACE(NTOT+105,'HCOPYM',ID1)
      IF(IERR.NE.0)                    GO TO 90
*
*             Enter ID in the list of ordered IDs
*
      IDPOS=-IDPOS+1
      IF(NRHIST.GE.IQ(LTAB-1))THEN
         CALL MZPUSH(IHDIV,LTAB,50,50,'I')
      ENDIF
      DO 30 I=NRHIST,IDPOS,-1
         IQ(LTAB+I+1)=IQ(LTAB+I)
         LQ(LTAB-I-1)=LQ(LTAB-I)
  30  CONTINUE
*
*             Build top level bank
*
      IF(I1.NE.0)THEN
         IODES=IOH1
         NL=1
      ELSE
         IODES=IOH2
         NL=7
      ENDIF
*
      IF(LIDS.EQ.0)THEN
         CALL MZBOOK(IHDIV,LIDS,LCDIR,-2,'HIDE',NL,NL,NWID,IODES,0)
         LCID=LIDS
      ELSE
         LLID=LQ(LCDIR-5)
         CALL MZBOOK(IHDIV,LCID,LLID,  0,'HIDE',NL,NL,NWID,IODES,0)
      ENDIF
      DO 40 I=1,NWID
         IQ(LCID+I)=IHEAD(I,IHIST)
  40  CONTINUE
      LQ(LCDIR-5)=LCID
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
         CALL MZBOOK(IHDIV,LCONT,LCID,-1,'HCON',2,2,NW,IODES,0)
         call Net_Copy(jcont+1,nw,iq(lcont+1),istat)
         if(istat.ne.0)return
         IF(JR1.NE.0)THEN
            call Net_Copy(jr1-1,1,nw,istat)
            if(istat.ne.0)return
            CALL MZBOOK(IHDIV,LR1,LCONT,0,'HI1E',0,0,NW,3,0)
            call Net_Copy(jr1+1,nw,iq(lr1+1),istat)
            if(istat.ne.0)return
            call Net_Copy(klq+jr1,1,jr2,istat)
            if(istat.ne.0)return
            IF(JR2.NE.0)THEN
               call Net_Copy(jr2-1,1,nw,istat)
               if(istat.ne.0)return
               CALL MZBOOK(IHDIV,LR2,LR1,0,'HI1N',0,0,NW,3,0)
               call Net_Copy(jr2+1,nw,iq(lr2+1),istat)
               if(istat.ne.0)return
            ENDIF
         ENDIF
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
      CALL MZBOOK(IHDIV,LCONT,LCID,-1,'HCON',0,0,NW,IODES,0)
      call Net_Copy(jcont+1,nw,iq(lcont+1),istat)
      go to 99
*
*          Error
*
  90  istat=1
  99  RETURN
      END
 
#endif

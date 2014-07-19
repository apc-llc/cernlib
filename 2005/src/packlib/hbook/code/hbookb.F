*
* $Id: hbookb.F,v 1.1.1.1 1996/01/16 17:07:32 mclareni Exp $
*
* $Log: hbookb.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:32  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.20/08 12/09/93  18.56.12  by  Rene Brun
*-- Author :
      SUBROUTINE HBOOKB(IDD,CHTITL,NX,XBINS,VALMAX)
*.==========>
*.           booking of a 1-dim histogram
*.           with non-equidistant bins
*..=========> ( R.Brun )
#include "hbook/hcflag.inc"
#include "hbook/hcbook.inc"
#include "hbook/hcform.inc"
#include "hbook/hcprin.inc"
      DIMENSION XBINS(2)
      CHARACTER*(*) CHTITL
*.___________________________________________
      IERR=0
      IF(IDD.EQ.0)THEN
         CALL HBUG('ID=0 is an illegal identifier','HBOOKB',IDD)
         RETURN
      ENDIF
*
      NARG=5
      CALL NOARG(NARG)
      XVALMA=0.
      IF(NARG.EQ.5)XVALMA=FLOARG(VALMAX)
*
*             Check consistency of array XBINS
      DO 5 I=2,NX+1
         IF(XBINS(I).LE.XBINS(I-1))THEN
            CALL HBUG('Bins are not in increasing order','HBOOKB',IDD)
            GO TO 99
         ENDIF
   5  CONTINUE
      X0=XBINS(1)
      X1=XBINS(NX+1)
      ID=IDD
*
*             Check if ID already in the table
*
      NRHIST=IQ(LCDIR+KNRH)
      IDPOS=LOCATI(IQ(LTAB+1),NRHIST,ID)
      IF(IDPOS.GT.0)THEN
         CALL HBUG('+Already existing histogram','HBOOKB',IDD)
         CALL HDELET(IDD)
         NRHIST=IQ(LCDIR+KNRH)
         IDPOS=-IDPOS+1
      ENDIF
*
*            Compute packing factor
*
      NBPROX=32
      IF(NARG.GE.5)THEN
         IF(XVALMA.GE.1.)THEN
            NBPROX=LOG(XVALMA)/LOG(2.)+1.
            IF(NBPROX.GT.16)NBPROX=32
         ENDIF
      ENDIF
      IF(NBPROX.EQ.32)THEN
         IODES=IOCF1
      ELSE
         IODES=IOCB1
      ENDIF
*
*             Get title length
*
      CALL HBTIT(CHTITL,NWTITL,NCHT)
      NB=32/NBPROX
      NWH=(NX+1)/NB + KCON1
      NWID=NWTITL+KTIT1-1
      NTOT=NWH+NWID+35+NX
*
*             Enough space left ?
*
      CALL HSPACE(NTOT+1000,'HBOOKB',IDD)
      IF(IERR.NE.0)                    GO TO 99
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
*             Build histogram data structure
*
      IF(LIDS.EQ.0)THEN
         CALL MZBOOK(IHDIV,LIDS,LCDIR,-2,'HID1',2,2,NWID,IOH1,0)
         LCID=LIDS
      ELSE
         LLID=LQ(LCDIR-9)
         CALL MZBOOK(IHDIV,LCID,LLID,  0,'HID1',2,2,NWID,IOH1,0)
      ENDIF
      LQ(LCDIR-9)=LCID
      CALL MZBOOK(IHDIV,LCONT,LCID,-1,'HCO1',2,2,NWH,IODES,0)
*
      IQ(LCID-5)=ID
      IQ(LTAB+IDPOS)=ID
      LQ(LTAB-IDPOS)=LCID
      CALL SBIT1(IQ(LCID+KBITS),1)
*
*             Store bin values
*
      CALL SBIT1(IQ(LCID+KBITS),6)
      CALL MZBOOK(IHDIV,LBINS,LCID,-2,'HBIN',0,0,NX+1,3,-1)
      DO 30 I=1,NX+1
         Q(LBINS+I)=XBINS(I)
  30  CONTINUE
*
      IF(NWTITL.NE.0)THEN
         CALL UCTOH(CHTITL,IQ(LCID+KTIT1),4,NCHT)
      ENDIF
*
      IQ(LCID+KNTOT)=NTOT
      IQ(LCID+KNCX)=NX
      Q (LCID+KXMIN)=X0
      Q (LCID+KXMAX)=X1
*-* The following statement should be removed in 1994
      Q (LCID+KXMAX+1)=FLOAT(NX)/(X1-X0)
      LCONT=LQ(LCID-1)
      IQ(LCONT+KNBIT)=NBPROX
      NRHIST=NRHIST+1
      IQ(LCDIR+KNRH)=NRHIST
*
  99  RETURN
      END

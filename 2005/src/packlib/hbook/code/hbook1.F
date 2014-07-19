*
* $Id: hbook1.F,v 1.1.1.1 1996/01/16 17:07:32 mclareni Exp $
*
* $Log: hbook1.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:32  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.21/04 27/11/93  10.56.03  by  Rene Brun
*-- Author :
      SUBROUTINE HBOOK1(IDD,CHTITL,NNNX,XXX0,XXX1,VALMAX)
*.==========>
*.           booking of a 1-dim histogram
*.
*B..HID1   1-D histogram header TITLE@DATA(10:11)
*B.AU      m.g. 05/02/93
*B.VE      1.00
*B.ST      /PAWC/
*B.DV      QDIV2
*B.NL             7
*B.NS             7
*B.ND            15
*B.NX      HID2
*B.UP      HDIR
*B.IO      1B 2I 3F 1I 4F -H
*B.LINK
*B.REP                    7
*B.1               HCO1
*B/REP
*B/LINK
*B.DATA
*B.1       KBITS    Status word
*B.2       NW       Total number of words in histogram data structure
*B.3       NCX      Number of channels in X
*B.4       XMIN     Lower limit in X
*B.5       XMAX     Upper limit in X
*B.6       XIBW     Inverse of binwidth in X
*B.7       HMIN     Minimum for histogram (HMINIM)
*B.8       HMAX     Maximum for histogram (HMAXIM)
*B.9       HNOR     Normalization factor (HNORMA)
*B.REP                    2
*B.1               TITL     Title of histogram
*B/REP
*B/DATA
*..=========> ( R.Brun )
#include "hbook/hcflag.inc"
#include "hbook/hcbook.inc"
#include "hbook/hcform.inc"
#include "hbook/hcprin.inc"
#include "hbook/hcopt.inc"
      CHARACTER*(*) CHTITL
*.___________________________________________
      IERR=0
      IF(IDD.EQ.0)THEN
         CALL HBUG('ID=0 is an illegal identifier','HBOOK1',IDD)
         RETURN
      ENDIF
*
      NOENT=0
      NARG=6
      CALL NOARG(NARG)
      XVALMA=0.
      IF(NARG.EQ.6)XVALMA=FLOARG(VALMAX)
      X1=FLOARG(XXX1)
      X0=FLOARG(XXX0)
      NX=INTARG(NNNX)
      IF(NX.EQ.0)NX=100
      IF(X1.LE.X0)NOENT=1
*
*             Automatic boundaries adjustment
      ID=IDD
      IF(KBINSZ.NE.0)THEN
         CALL HBIN(X0,X1,NX,X0,X1,NXX,BWID)
         X1=X0+BWID*FLOAT(NX)
      ENDIF
*
*             Check if ID already in the table
*
      NRHIST=IQ(LCDIR+KNRH)
      IDPOS=LOCATI(IQ(LTAB+1),NRHIST,ID)
      IF(IDPOS.GT.0)THEN
         CALL HBUG('+Already existing histogram replaced','HBOOK1',IDD)
         CALL HDELET(IDD)
         NRHIST=IQ(LCDIR+KNRH)
         IDPOS=-IDPOS+1
      ENDIF
*
*            Compute packing factor
*
      NBPROX=32
      IF(NARG.GE.6)THEN
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
      NTOT=NWH+NWID+23
*
*             Enough space left ?
*
      CALL HSPACE(NTOT+1000,'HBOOK1',IDD)
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
         CALL MZBOOK(IHDIV,LIDS,LCDIR,-2,'HID1',1,1,NWID,IOH1,0)
         LCID=LIDS
      ELSE
         LLID=LQ(LCDIR-9)
         CALL MZBOOK(IHDIV,LCID,LLID,  0,'HID1',1,1,NWID,IOH1,0)
      ENDIF
      LQ(LCDIR-9)=LCID
      CALL MZBOOK(IHDIV,LCONT,LCID,-1,'HCO1',2,2,NWH,IODES,0)
*
      IQ(LCID-5)=ID
      IQ(LTAB+IDPOS)=ID
      LQ(LTAB-IDPOS)=LCID
      CALL SBIT1(IQ(LCID+KBITS),1)
*
*             Automatic binning
*
      IF(NOENT.NE.0)THEN
         CALL SBIT1(IQ(LCID+KBITS),5)
         CALL MZBOOK(IHDIV,LAUTO,LCONT,-2,'HAUT',0,0,200,3,0)
      ENDIF
*
*             Automatic filling of statistics
*
      IF(ISTAF.NE.0)THEN
         CALL SBIT1(IQ(LCID+KBITS),7)
      ENDIF
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
      IF(NOENT.NE.1)Q (LCID+KXMAX+1)=FLOAT(NX)/(X1-X0)
      LCONT=LQ(LCID-1)
      IQ(LCONT+KNBIT)=NBPROX
      NRHIST=NRHIST+1
      IQ(LCDIR+KNRH)=NRHIST
*
  99  RETURN
      END

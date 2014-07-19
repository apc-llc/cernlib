*
* $Id: hbook2.F,v 1.1.1.1 1996/01/16 17:07:32 mclareni Exp $
*
* $Log: hbook2.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:32  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.21/04 27/11/93  10.54.09  by  Rene Brun
*-- Author :
      SUBROUTINE HBOOK2(IDD,CHTITL,NNX,XX0,XX1,NNY,YY0,YY1,VALMAX)
*.==========>
*.           booking of a 2-dim histogram
*.
*B..HID2   2-D histogram header TITLE@DATA(12:99)
*B.AU      m.g. 7 feb 1993
*B.VE      1.00
*B.ST      /PAWC/
*B.DV      QDIV2
*B.NL             7
*B.NS             7
*B.ND      variable
*B.NX      HIDT
*B.UP      HDIR     -2
*B.IO      1B 2I 3F 1I 4F -H
*B.LINK
*B.1       HCO2
*B.2       PROX
*B.3       PROY
*B.4       SLMX
*B.5       SLMY
*B.6       BAMX
*B.7       BAMY
*B/LINK
*B.DATA
*B.1       KBITS    Status word
*B.2       NW       Total number of words in histogram data structure
*B.3       NCX      Number of channels in X
*B.4       XMIN     Lower limit in X
*B.5       XMAX     Upper limit in X
*B.6       XIBW     Inverse of binwidth in X
*B.7       NCY      Number of channels in Y
*B.8       YMIN     Lower limit in Y
*B.9       YMAX     Upper limit in Y
*B.10      YIBW     Inverse of binwidth in Y
*B.11      SCAL     Scale Factor (HSCALE)
*B.REP             NNNN
*B.12        TITL     Title of histogram
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
         CALL HBUG('ID=0 is an illegal identifier','HBOOK2',IDD)
         RETURN
      ENDIF
*
      NX=INTARG(NNX)
      X0=FLOARG(XX0)
      X1=FLOARG(XX1)
      NY=INTARG(NNY)
      Y0=FLOARG(YY0)
      Y1=FLOARG(YY1)
*
      NARG=9
      CALL NOARG(NARG)
      IF(X0.GE.X1)THEN
         CALL HBUG('XMIN.GE.XMAX','HBOOK2',IDD)
         GO TO 20
      ENDIF
      IF(Y0.GE.Y1)THEN
         CALL HBUG('YMIN.GE.YMAX','HBOOK2',IDD)
         GO TO 20
      ENDIF
      IF(NX.LE.0.OR.NY.LE.0)THEN
         CALL HBUG('NX.LE.0.or.NY.LE.0','HBOOK2',IDD)
         GO TO 20
      ENDIF
*
*             Automatic boundaries adjustment
      ID=IDD
      IF(KBINSZ.NE.0)THEN
         CALL HBIN(X0,X1,NX,X0,X1,NX,BWIDX)
         CALL HBIN(Y0,Y1,NY,Y0,Y1,NY,BWIDY)
      ENDIF
      NX=INTARG(NNX)
      NY=INTARG(NNY)
*
*             Check if ID already in the table
*
      NRHIST=IQ(LCDIR+KNRH)
      IDPOS=LOCATI(IQ(LTAB+1),NRHIST,ID)
      IF(IDPOS.GT.0)THEN
         CALL HBUG('+Already existing histogram replaced','HBOOK2',IDD)
         CALL HDELET(IDD)
         NRHIST=IQ(LCDIR+KNRH)
         IDPOS=-IDPOS+1
      ENDIF
*
*            Compute packing factor
*
      NBSCAT=10/NV
      IF(NARG.GE.9)THEN
         XVALMA=FLOARG(VALMAX)
         IF(XVALMA.LT.1.)THEN
            NBSCAT=32
         ELSE
            NBSCAT=LOG(XVALMA)/LOG(2.)+1.
            IF(NBSCAT.GT.16)NBSCAT=32
         ENDIF
      ENDIF
      IF(NBSCAT.EQ.32)THEN
         IODES=IOCF2
      ELSE
         IODES=IOCB2
      ENDIF
*
*             Title transform from various accepted formats
*
      CALL HBTIT(CHTITL,NWTITL,NCHT)
      NB=32/NBSCAT
      NWH=((NX+2)*(NY+2)-1)/NB+KCON2
      NWID=NWTITL+KTIT2-1
      NTOT=NWH+NWID+29
*
*             Enough space left ?
*
      CALL HSPACE(NTOT+1000,'HBOOK2',IDD)
      IF(IERR.NE.0)GO TO 20
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
   10 CONTINUE
*
*             Build histogram data structure
*
      IF(LIDS.EQ.0)THEN
         CALL MZBOOK(IHDIV,LIDS,LCDIR,-2,'HID2',7,7,NWID,IOH2,0)
         LCID=LIDS
      ELSE
         LLID=LQ(LCDIR-9)
         CALL MZBOOK(IHDIV,LCID,LLID,  0,'HID2',7,7,NWID,IOH2,0)
      ENDIF
      LQ(LCDIR-9)=LCID
      CALL MZBOOK(IHDIV,LCONT,LCID,-1,'HCO2',2,2,NWH,IODES,0)
*
      IQ(LCID-5)=ID
      IQ(LTAB+IDPOS)=ID
      LQ(LTAB-IDPOS)=LCID
      IF(NV.EQ.2)THEN
         CALL SBIT1(IQ(LCID+KBITS),2)
      ELSE
         CALL SBIT1(IQ(LCID+KBITS),3)
      ENDIF
*
*             Automatic filling of statistics
*
      IF(ISTAF.NE.0)THEN
         CALL SBIT1(IQ(LCID+KBITS),7)
      ENDIF
*
      IF(NWTITL.NE.0)THEN
         CALL UCTOH(CHTITL,IQ(LCID+KTIT2),4,NCHT)
      ENDIF
*
      IQ(LCID+KNTOT)=NTOT
      IQ(LCID+KNCX)=NX
      Q(LCID+KXMIN)=X0
      Q(LCID+KXMAX)=X1
*
      IQ(LCID+KNCY)=NY
      Q(LCID+KYMIN)=Y0
      Q(LCID+KYMAX)=Y1
*-* The following 2 statements should be removed in 1994
      Q (LCID+KXMAX+1)=FLOAT(NX)/(X1-X0)
      Q (LCID+KYMAX+1)=FLOAT(NY)/(Y1-Y0)
*
      IQ(LCONT+KNBIT)=NBSCAT
      NRHIST=NRHIST+1
      IQ(LCDIR+KNRH)=NRHIST
*
   20 RETURN
      END

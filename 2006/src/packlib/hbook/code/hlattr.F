*
* $Id: hlattr.F,v 1.1.1.1 1996/01/16 17:07:41 mclareni Exp $
*
* $Log: hlattr.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:41  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :          27/07/95  17.19.33  by  Julian Bunn
*-- Author :    Fons Rademakers   06/07/92
      SUBROUTINE HLATTR(IDH,CHTYPE,CHTITL,NX,XMIN,XMAX,NY,YMIN,YMAX,
     +                  ISW,IER)
*.==========>
*.      To get the attributes of the object IDH.
*.
*.
*. Input:
*.   IDH      identifier of object (1d, 2d or N-tuple)
*.   CHTYPE*  returns histogram type:
*.            '1'   1-dimensional
*.            '2'   2-dimensional
*.            'N'   N-tuple
*.   CHTITL*  returns histogram title or subdirectory name
*.   NX*      returns number of X channels (# of columns for Ntuple)
*.   XMIN*    returns X min
*.   XMAX*
*.   NY*      returns number of Y channels (# of entries for Ntuple)
*.   YMIN*
*.   YMAX*
*.   ISW*     returns objects status word
*.   IER*     returns -1 when object not found
*.
*..=========> ( Fons Rademakers )
*
#include "hbook/hcbits.inc"
#include "hbook/hcbook.inc"
#include "hbook/hcdire.inc"
#include "hbook/hcntpar.inc"
      DIMENSION IPAWC(99)
      EQUIVALENCE (NWPAW,IPAWC(1))
*
      CHARACTER*(*) CHTYPE,CHTITL
      COMMON/QUEST/IQUEST(100)
      INTEGER   KEYS(2)
*.___________________________________________
*
      IER  = 0
      NX   = 0
      XMIN = 0.0
      XMAX = 0.0
      NY   = 0
      YMIN = 0.0
      YMAX = 0.0
      ISW  = 0
      CHTYPE = ' '
      CHTITL = ' '
*
      IF(ICHTOP(ICDIR).EQ.0)GO TO 20
*
*             Return histograms in RZ file
*
      KOF=0
      IF(ICHTOP(ICDIR).GT.0)THEN
         KEYS(1) = IDH
         KEYS(2) = 0
         CALL HRZIN(IHWORK,LHWORK,1,KEYS,9999,' ')
         IF(IQUEST(1).NE.0) GOTO 30
         IF(LHWORK.EQ.0)GO TO 30
         IF(IQ(LHWORK-2).EQ.0)THEN
            CHTYPE = '?'
            CHTITL = '??? '
            GO TO 10
         ENDIF
      ENDIF
*
*-*  Shared memory or global section
#if defined(CERNLIB_VAX)
      IF(ICHTOP(ICDIR).LT.0)THEN
         LOCQ=1-LOCF(IPAWC(1))-ICHTOP(ICDIR)
         CALL HRZING(IPAWC(LOCQ),IDH,JCID)
         LHWORK=JCID+LOCQ-1
         KOF=1-LOCQ
      ENDIF
#endif
#if defined(CERNLIB_HMMAP)
      IF(ICHTOP(ICDIR).LT.0)THEN
         IGOFF=-LOCF(LQ(1))-ICHTOP(ICDIR)
         KOF=LQ(IGOFF+2)
         CALL HRZINM(LQ(IGOFF+1),IDH,JCID)
         LHWORK=JCID+IGOFF
      ENDIF
#endif
*
      IF (JBIT(IQ(LHWORK+KBITS),1).NE.0) THEN
         CHTYPE = '1'
         CHTITL = ' '
         NWTITL = IQ(LHWORK-1)-KTIT1+1
         CALL UHTOC(IQ(LHWORK+KTIT1),4,CHTITL,NWTITL*4)
         NX = IQ(LHWORK+KNCX)
         XMIN = Q(LHWORK+KXMIN)
         XMAX = Q(LHWORK+KXMAX)
         ISW = IQ(LHWORK+KBITS)
      ELSEIF (JBYT(IQ(LHWORK+KBITS),2,2).NE.0) THEN
         CHTYPE = '2'
         CHTITL = ' '
         NWTITL = IQ(LHWORK-1)-KTIT2+1
         CALL UHTOC(IQ(LHWORK+KTIT2),4,CHTITL,NWTITL*4)
         NX = IQ(LHWORK+KNCX)
         XMIN = Q(LHWORK+KXMIN)
         XMAX = Q(LHWORK+KXMAX)
         NY = IQ(LHWORK+KNCY)
         YMIN = Q(LHWORK+KYMIN)
         YMAX = Q(LHWORK+KYMAX)
         ISW = IQ(LHWORK+KBITS)
      ELSE
         CHTYPE = 'N'
         CHTITL = ' '
         IF (IQ(LHWORK-2) .EQ. 2) THEN
            ITIT1 = IQ(LHWORK+9)
            NWTITL = IQ(LHWORK+8)
            ISW = IQ(LHWORK+KBITS)
            NX = IQ(LHWORK+2)
            NY = IQ(LHWORK+3)
            LL = LQ(LHWORK-2)-KOF
            XMIN = Q(LL+1)
            XMAX = Q(LL+2)
         ELSE
            ITIT1 = IQ(LHWORK+ZITIT1)
            NWTITL = IQ(LHWORK+ZNWTIT)
            ISW = IQ(LHWORK+ZBITS)
            NX = IQ(LHWORK+ZNDIM)
            NY = IQ(LHWORK+ZNOENT)
            XMIN = 1
            XMAX = 100
         ENDIF
         CALL UHTOC(IQ(LHWORK+ITIT1),4,CHTITL,NWTITL*4)
      ENDIF
   10 IF(ICHTOP(ICDIR).GT.0)CALL MZDROP(IHWORK,LHWORK,' ')
      LHWORK=0
      GO TO 999
*
*             Return histograms in memory
*
   20 CALL HFIND(IDH,'HLATTR')
      IF(LCID.EQ.0) GOTO 30
      CALL HDCOFL
*
      IF(IQ(LCID-2).EQ.0)THEN
         CHTYPE = '?'
         CHTITL = '??? '
         GO TO 999
      ENDIF
      IF (I1.NE.0) THEN
         CHTYPE = '1'
         CHTITL = ' '
         NWTITL = IQ(LCID-1)-KTIT1+1
         CALL UHTOC(IQ(LCID+KTIT1),4,CHTITL,NWTITL*4)
         NX = IQ(LCID+KNCX)
         XMIN = Q(LCID+KXMIN)
         XMAX = Q(LCID+KXMAX)
         ISW = IQ(LCID+KBITS)
      ELSEIF (I230.NE.0) THEN
         CHTYPE = '2'
         CHTITL = ' '
         NWTITL = IQ(LCID-1)-KTIT2+1
         CALL UHTOC(IQ(LCID+KTIT2),4,CHTITL,NWTITL*4)
         NX = IQ(LCID+KNCX)
         XMIN = Q(LCID+KXMIN)
         XMAX = Q(LCID+KXMAX)
         NY = IQ(LCID+KNCY)
         YMIN = Q(LCID+KYMIN)
         YMAX = Q(LCID+KYMAX)
         ISW = IQ(LCID+KBITS)
      ELSEIF (I4.NE.0) THEN
         CHTYPE = 'N'
         CHTITL = ' '
         IF (IQ(LCID-2) .EQ. 2) THEN
            ITIT1 = IQ(LCID+9)
            NWTITL = IQ(LCID+8)
            ISW = IQ(LCID+KBITS)
            NX = IQ(LCID+2)
            NY = IQ(LCID+3)
            LL = LQ(LCID-2)
            XMIN = Q(LL+1)
            XMAX = Q(LL+2)
         ELSE
            ITIT1 = IQ(LCID+ZITIT1)
            NWTITL = IQ(LCID+ZNWTIT)
            ISW = IQ(LCID+ZBITS)
            NX = IQ(LCID+ZNDIM)
            NY = IQ(LCID+ZNOENT)
            XMIN = 1
            XMAX = 100
         ENDIF
         CALL UHTOC(IQ(LCID+ITIT1),4,CHTITL,NWTITL*4)
      ELSE
         CHTYPE = '?'
         CHTITL = '??? '
      ENDIF
      GO TO 999
*
   30 IER = -1
*
  999 END

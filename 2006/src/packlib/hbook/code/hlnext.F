*
* $Id: hlnext.F,v 1.1.1.1 1996/01/16 17:07:42 mclareni Exp $
*
* $Log: hlnext.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:42  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :          27/07/95  17.19.58  by  Julian Bunn
*-- Author :    Alfred Nathaniel   06/07/92
      SUBROUTINE HLNEXT(IDH,CHTYPE,CHTITL,CHOPT)
*.==========>
*.      To scan the content of the current directory in memory or RZ file:
*.
*.      IDH=0
*.  1   CONTINUE
*.      CALL HLNEXT(IDH,CHTYPE,CHTITL,CHOPT)
*.      IF(IDH.NE.0) THEN
*.         ... process
*.         GOTO 1
*.      ENDIF
*.
*. Input:
*.   *IDH*    must be zero at first call
*.            returns identifier of next histogram
*.            returns zero if all histograms have been processed
*.   CHTYPE*  returns histogram type:
*.            '1'   1-dimensional
*.            '2'   2-dimensional
*.            'N'   N-tuple
*.            'D'   subdirectory
*.            '?'   unknown
*.   CHTITL*  returns histogram title or subdirectory name
*.   CHOPT    Character variable specifying the items to be scanned:
*.            '1'   include 1d histograms
*.            '2'   include 2d histograms
*.            'N'   include N-tuples
*.            'D'   include subdirectories
*.            ' '   include everything, i.e. equivalent to CHOPT='12ND?'
*..=========> ( A.Nathaniel )
#include "hbook/hcbits.inc"
#include "hbook/hcbook.inc"
#include "hbook/hcdire.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcntpar.inc"
#include "hbook/hcpiaf.inc"
#include "hbook/czsock.inc"
      DIMENSION IPAWC(99)
      EQUIVALENCE (NWPAW,IPAWC(1))
      CHARACTER*(*) CHTYPE,CHTITL,CHOPT
      COMMON/QUEST/IQUEST(100)
      PARAMETER(MAXDIR=100)
      CHARACTER CHDIR(MAXDIR)*16,CHOPTL*5
      INTEGER   IOPT(5), KEYS(2)
      EQUIVALENCE (IOPT1,IOPT(1)),(IOPT2,IOPT(2)),(IOPTN,IOPT(3))
      EQUIVALENCE (IOPTD,IOPT(4)),(IOPTX,IOPT(5))
      LOGICAL HISTOS,INFILE,FOUND,REMOTE
      SAVE    HISTOS,INFILE,REMOTE
      SAVE    CHDIR
      SAVE    IOPT,KEYS,KEYNUM,NDIR,IDN,IQ40,IQ41,IQ42,IDD,IRET
*.___________________________________________
      IF(IDH.EQ.0) THEN
*
*             Initialize on first call
*
         KEYNUM  = 1
         CHOPTL = CHOPT
         IF(CHOPTL.EQ.' ') CHOPTL = '12ND?'
         CALL HUOPTC (CHOPTL,'12ND?',IOPT)
*
         REMOTE = .FALSE.
         IF (ICHTOP(ICDIR) .GT. 1000) THEN
            REMOTE = .TRUE.
            GOTO 5
         ENDIF
*
         HISTOS = IOPT1.NE.0 .OR. IOPT2.NE.0
     +       .OR. IOPTN.NE.0 .OR. IOPTX.NE.0
         IF (HISTOS) THEN
*
*          Find first ID in the RZ directory / Setup HLOOP
*
            INFILE = ICHTOP(ICDIR).GT.0.AND.ICHTOP(ICDIR).LT.1000
            IF(INFILE) THEN
               KEYS(1) = KEYNUM
               KEYS(2) = 0
               CALL HRSORT('S')
               CALL HRZIN(IHWORK,0,0,KEYS,9999,'SC')
               IDN=IQUEST(21)
               IQ42=IQUEST(22)
            ELSEIF(ICHTOP(ICDIR).EQ.0)THEN
               CALL ZSORTI(IHDIV,LIDS,-5)
               IDD=0
               IRET=3
            ENDIF
         ENDIF
*
*             Inquire number of subdirectories
*
         NDIR=0
         IF(IOPTD.NE.0) THEN
            CALL HRDIR(MAXDIR,CHDIR,NDIR)
            IF(NDIR.GT.MAXDIR) THEN
               CALL HBUG('too many subdirectories','HLNEXT',NDIR)
               NDIR=MAXDIR
            ENDIF
            IOPTD=0
         ENDIF
      ENDIF
*
 5    IF (REMOTE) THEN
#if defined(CERNLIB_CZ)
         IF(ICHTOP(ICDIR).GT.1000 .AND.
     +      ICHLUN(ICDIR).EQ.0.AND.ICHTYP(ICDIR).EQ.0)THEN
*
* remote file on PAWSERV (global section or shared memory)
*
            if(idh.eq.0)then
               ISKIN =MOD(ICHTOP(ICDIR),10000)
               ISKOUT=ICHTOP(ICDIR)/10000
               CHSMPF='MESS :LI'
               CALL CZPUTA(CHSMPF,ISTAT)
               IF (ISTAT .NE. 0) RETURN
            endif
            CALL CZGETA(CHSMPF,ISTAT)
            IF (ISTAT .NE. 0) RETURN
            IF(CHSMPF(1:1).EQ.'0')then
               idh=0
               RETURN
            endif
            READ(CHSMPF,'(1X,I10,2X,A1,4X,A60)')IDH,CHTYPE,CHTITL
            RETURN
         ENDIF
*
*--- remote file on Piaf server
*
         ISKIN  = MOD(ICHTOP(ICDIR),10000)
         ISKOUT = ICHTOP(ICDIR)/10000
         CHSMPF = 'HLNEXT:'
         WRITE(CHSMPF(8:),'(I8,A)') IDH,CHOPT
         CALL CZPUTA(CHSMPF,IRC)
         IF (IRC .NE. 0) THEN
            IDH = 0
            RETURN
         ENDIF
         CALL HLOGPF(' ',IRC)
         IDH = IQUEST(1)
         CALL CZGETA(CHSMPF,IRC)
         CHTYPE = CHSMPF(1:1)
         CHTITL = CHSMPF(2:)
#endif
#if !defined(CERNLIB_CZ)
         CALL HBUG('CZ option not active','HLNEXT',0)
         IQUEST(1)=1
#endif
         RETURN
      ENDIF
*
*             Return subdirectories
*
      IOPTD=IOPTD+1
      IF(IOPTD.LE.NDIR) THEN
         IDH=IOPTD
         CHTYPE='D'
         CHTITL=CHDIR(IOPTD)
         RETURN
      ENDIF
*
      IF(.NOT.HISTOS) RETURN
*
*-*  Shared memory or global section
#if defined(CERNLIB_VAX)
      IF(ICHTOP(ICDIR).LT.0)THEN
         LOCQ=1-LOCF(IPAWC(1))-ICHTOP(ICDIR)
         IF(KEYNUM.EQ.1)IDH=0
         CALL HLNXTG(IPAWC(LOCQ),IDH,CHTYPE,CHTITL,CHOPTL)
         KEYNUM=KEYNUM+1
         RETURN
      ENDIF
#endif
#if defined(CERNLIB_HMMAP)
      IF(ICHTOP(ICDIR).LT.0)THEN
         IGOFF=-LOCF(LQ(1))-ICHTOP(ICDIR)
         IF(KEYNUM.EQ.1)IDH=0
         CALL HLNXTM(LQ(IGOFF+1),IDH,CHTYPE,CHTITL,CHOPTL)
         KEYNUM=KEYNUM+1
         RETURN
      ENDIF
#endif
*
      IDH=0
      FOUND=.FALSE.
 10   CONTINUE
      IF(INFILE) THEN
*
*             Return histograms in RZ file
*
         IF(IDN.EQ.0) RETURN
         KEYS(1) = KEYNUM
         CALL HRZIN(IHWORK,0,0,KEYS,9999,'SNC')
         IF(IQUEST(1).NE.0) RETURN
         IDN =IQUEST(21)
         IQ40=IQUEST(40)
         IQ41=IQUEST(41)
         IQ42=IQUEST(42)
         IF(IQ40.EQ.0) IQ41=0
         NWORDS=IQUEST(12)
         IF(JBIT(IQUEST(14),4).EQ.0) THEN
            CALL HSPACE(NWORDS+1000,'HLNEXT',IDN)
            IF(IERR.NE.0) RETURN
*
*             Read histogram data structure
*
**            KEYS(1)=IDN
**            KEYS(2)=0
            CALL HRZIN(IHWORK,LHWORK,1,KEYS,9999,'SND')
            IF(IQUEST(1).NE.0) THEN
               CALL HBUG('Bad sequence for RZ','HLNEXT',IDN)
               RETURN
            ENDIF
*
            IF(IQ(LHWORK-2).EQ.0) THEN
               IF(IOPTX.NE.0) THEN
                  FOUND=.TRUE.
                  IDH=IDN
                  CHTYPE='?'
                  CHTITL='??? '
               ENDIF
            ELSEIF(JBIT(IQ(LHWORK+KBITS),1).NE.0) THEN
               IF(IOPT1.NE.0) THEN
                  FOUND=.TRUE.
                  IDH=IDN
                  CHTYPE='1'
                  CHTITL=' '
                  NWTITL=IQ(LHWORK-1)-KTIT1+1
                  CALL UHTOC(IQ(LHWORK+KTIT1),4,CHTITL,NWTITL*4)
               ENDIF
            ELSEIF(JBYT(IQ(LHWORK+KBITS),2,2).NE.0)THEN
               IF(IOPT2.NE.0) THEN
                  FOUND=.TRUE.
                  IDH=IDN
                  CHTYPE='2'
                  CHTITL=' '
                  NWTITL=IQ(LHWORK-1)-KTIT2+1
                  CALL UHTOC(IQ(LHWORK+KTIT2),4,CHTITL,NWTITL*4)
               ENDIF
            ELSEIF(JBIT(IQ(LHWORK+KBITS),4).NE.0)THEN
               IF(IOPTN.NE.0) THEN
                  FOUND=.TRUE.
                  IDH=IDN
                  CHTYPE='N'
                  CHTITL=' '
                  IF (IQ(LHWORK-2) .EQ. 2) THEN
                     ITIT1=IQ(LHWORK+9)
                     NWTITL=IQ(LHWORK+8)
                  ELSE
                     ITIT1=IQ(LHWORK+ZITIT1)
                     NWTITL=IQ(LHWORK+ZNWTIT)
                  ENDIF
                  CALL UHTOC(IQ(LHWORK+ITIT1),4,CHTITL,NWTITL*4)
               ENDIF
            ENDIF
*
            CALL MZDROP(IHWORK,LHWORK,' ')
         ENDIF
         LHWORK=0
*
         IF(IQ40.EQ.0) CALL MZWIPE(IHWORK)
         KEYNUM=KEYNUM+1
         IDN=IQ41
         IF(.NOT.FOUND) GOTO 10
      ELSE
*
*             Return histograms in memory
*
 20      CONTINUE
         CALL HLOOP(IDD,'HLNEXT',IRET)
         IF(IRET.EQ.0) RETURN
         IRET=2
*
         CALL HDCOFL
         IF(I1.NE.0) THEN
            IF(IOPT1.NE.0) THEN
               IDH=ID
               CHTYPE='1'
               CHTITL=' '
               NWTITL=IQ(LCID-1)-KTIT1+1
               CALL UHTOC(IQ(LCID+KTIT1),4,CHTITL,NWTITL*4)
               RETURN
            ENDIF
         ELSEIF(I230.NE.0) THEN
            IF(IOPT2.NE.0) THEN
               IDH=ID
               CHTYPE='2'
               CHTITL=' '
               NWTITL=IQ(LCID-1)-KTIT2+1
               CALL UHTOC(IQ(LCID+KTIT2),4,CHTITL,NWTITL*4)
               RETURN
            ENDIF
         ELSEIF(I4.NE.0) THEN
            IF(IOPTN.NE.0) THEN
               IDH=ID
               CHTYPE='N'
               CHTITL=' '
               IF (IQ(LCID-2) .EQ. 2) THEN
                  ITIT1=IQ(LCID+9)
                  NWTITL=IQ(LCID+8)
               ELSE
                  ITIT1=IQ(LCID+ZITIT1)
                  NWTITL=IQ(LCID+ZNWTIT)
               ENDIF
               CALL UHTOC(IQ(LCID+ITIT1),4,CHTITL,NWTITL*4)
               RETURN
            ENDIF
         ELSE
            IF(IOPTX.NE.0) THEN
               IDH=-1
               CHTYPE='?'
               CHTITL='??? '
               RETURN
            ENDIF
         ENDIF
         GOTO 20
      ENDIF
*
      END

*
* $Id: cdldir.F,v 1.1.1.1 1996/02/28 16:24:44 mclareni Exp $
*
* $Log: cdldir.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:44  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDLDIR(PATH,LWRITE,NLEVEL,CHOPT,IRC)
*
************************************************************************
*
*        To traverse the HEPDB directory tree starting from
*        the specified pathname.
*        Modified from the RZ routine RZSTAT / RZSCAN
*
* Input:
*   CHPATH  The pathname of the starting directory
*   LWRITE  The logical unit on which the information is written
*   NLEVEL  Number of levels  below CHPATH to descend
*
* Output:
*           IQUEST(11) = number of directories found
*           IQUEST(12) = number of directories that match
*
* Called by CDLDC
*           <user>
*
*
************************************************************************
#include "zebra/rzcl.inc"
#include "zebra/rzdir.inc"
#include "zebra/rzch.inc"
#include "zebra/rzk.inc"
      CHARACTER*(*) PATH
      CHARACTER*255 CHPATH
      CHARACTER*255 OLDDIR
      CHARACTER*132 CHLINE
      PARAMETER     (MAXLEV=20)
      CHARACTER*16  DIRNAM(MAXLEV),CHDIR
      DIMENSION     ISD(MAXLEV),NSD(MAXLEV),IHDIR(4)
      DIMENSION     IHTAG(2)
      CHARACTER*8   CHTAG(KNMAX)
#include "hepdb/slate.inc"
#include "hepdb/cdunit.inc"
#include "hepdb/cdrnge.inc"
#include "hepdb/hdbopts.inc"
*
*-----------------------------------------------------------------------
*
*
      IRC=0
      IQUEST(1)  = 0
      IQUEST(11) = 0
      IWIDTH     = 0
      JWIDTH     = 78
      IF(IOPTV.NE.0) JWIDTH = 132
      CALL CDLDDI (PATH, CHPATH, LPATH)
      LPATHI     = LPATH
*
*     Split input path name into its component pieces
*     (this is the opposite of RZPAFF which glues them together)
*
      CALL CDPAFF(CHPATH(1:LPATH),DIRNAM,MAXLEV,IRET)
*
*     Find first wild card in generic name
*
      IF(LLOGCD.GE.3)
     +WRITE(LPRTCD,*) 'CDLDIR. enter for PATH = ',CHPATH(1:LPATH),
     +        ' nlevel = ',NLEVEL,' chopt = ',CHOPT
      IWILD  = ICFMUL('*%(<>[]',CHPATH,1,LPATH)
      IF(IWILD.LE.LPATH) THEN
         LPATH = INDEXB(CHPATH(1:IWILD),'/') - 1
      ENDIF
*
      ITIME=0
      NLEV=NLEVEL
      IF(NLEV.LE.0)NLEV=99
      NFOUND = 0
      NMATCH = 0
      ITEMP  = 0
      CALL RZCDIR(CHWOLD,'R')
      OLDDIR = ' '
      LOLD   = 1
*
*     Set CWD to the current level
*
   10 CONTINUE
      IF(ITIME.NE.0)THEN
         CALL RZPAFF(CHPAT,NLPAT,CHL)
         CALL RZCDIR(CHL,' ')
      ELSE
         CALL RZCDIR(CHPATH(1:LPATH),' ')
         IF(IQUEST(1).NE.0)GO TO 80
         IF(IQ(KQSP+LCDIR+KNSD).GT.0) WRITE(LWRITE,*)
     +      'List of subdirectories...'
         NLPAT0=NLPAT
         CHL = CHPATH(1:LPATH)
      ENDIF
      IF(IQUEST(1).NE.0)GO TO 70
*
*     Set IQUEST
*
      IQUEST(12) = IQ(KQSP+LCDIR+KNSD)
      IQUEST(13) = NLPAT
      IQUEST(14) = IQ(KQSP+LCDIR+KNKEYS)
      IQUEST(15) = IQ(KQSP+LCDIR+KNWKEY)
*
*     Ensure that entire path name matches before printing
*
      NFOUND = NFOUND + 1
      CALL CDMTCH(CHL,CHPATH(1:LPATHI),IRC)
      IF(IRC.EQ.0) THEN
         NMATCH = NMATCH + 1
         LCHL = LENOCC(CHL)
         ISLASH = INDEXB(CHL(1:LCHL),'/')
 
         IF(IOPTV+IOPTW.EQ.0) THEN
*
*     Dates and times
*
            WRITE(LWRITE,*) CHL(1:LCHL)
 
            IF(IOPTC.NE.0) THEN
               CALL RZDATE(IQ(KQSP+LCDIR+KDATEC),IDC,ITC,1)
               WRITE(LWRITE,*) ' Created        :',IDC,ITC
            ENDIF
            IF(IOPTM.NE.0) THEN
               CALL RZDATE(IQ(KQSP+LCDIR+KDATEM),IDM,ITM,1)
               WRITE(LWRITE,*) ' Modified       :',IDM,ITM
            ENDIF
 
            IF(IOPTO.NE.0) THEN
               WRITE(LWRITE,*) ' Objects        :',IQ(KQSP+LCDIR+KNKEYS)
            ENDIF
 
            IF(IOPTS.NE.0) THEN
               WRITE(LWRITE,*) ' Subdirectories :',IQ(KQSP+LCDIR+KNSD)
            ENDIF
 
            IF(IOPTT.NE.0) THEN
               NWKEY=IQ(KQSP+LCDIR+KNWKEY)
               KTAGS=KKDES+(NWKEY-1)/10+1
               DO 20 J=1,NWKEY
                  CALL ZITOH(IQ(KQSP+LCDIR+KTAGS+2*J-2),IHTAG,2)
                  CALL UHTOC(IHTAG,4,CHTAG(J),8)
   20          CONTINUE
               WRITE(LWRITE,*) ' Tags           :'
               WRITE(LWRITE,9001) (CHTAG(J),J=1,NWKEY)
 9001          FORMAT(8(2X,A8))
            ENDIF
 
            IF(IOPTC+IOPTM+IOPTS+IOPTO+IOPTT.NE.0) WRITE(LWRITE,*)
 
         ELSE
            IF(CHL(1:ISLASH-1).NE.OLDDIR(1:LOLD)) THEN
*     flush current buffer
               IF(IWIDTH.NE.0) THEN
                  WRITE(LWRITE,'(1X,A)') CHLINE(1:IWIDTH)
                  IWIDTH = 0
               ENDIF
 
               WRITE(LWRITE,*)
               WRITE(LWRITE,'(1X,A,A)') 'Directory: ',CHL(1:ISLASH-1)
               WRITE(LWRITE,*)
               LOLD = ISLASH - 1
               OLDDIR = CHL(1:LOLD)
 
            ENDIF
 
            LF     = LCHL - ISLASH
            IF(IWIDTH+LF.GE.JWIDTH) THEN
*     flush current buffer
               WRITE(LWRITE,'(1X,A)') CHLINE(1:IWIDTH)
               IWIDTH = 0
            ENDIF
            IF(IWIDTH.EQ.0) THEN
               CHLINE = CHL(ISLASH+1:LCHL) // ' '
            ELSE
               CHLINE = CHLINE(1:IWIDTH) // CHL(ISLASH+1:LCHL) // ' '
            ENDIF
            IWIDTH = IWIDTH + LF + 1
         ENDIF
      ENDIF
 
      ISD(NLPAT)=0
      NSD(NLPAT)=IQ(KQSP+LCDIR+KNSD)
*
*     If there is a <> at this level, perform matching
*     and follow only that path
*
      ILT = INDEX(DIRNAM(NLPAT+1),'<')
      IGT = INDEX(DIRNAM(NLPAT+1),'>')
      NCRNGE(NLPAT+1) = 0
 
      IF((ILT.NE.0).AND.(IGT.NE.0)) THEN
         IF(LLOGCD.GE.-3) WRITE(LWRITE,9002) NLPAT,DIRNAM(NLPAT+1)
 9002 FORMAT(' CDLDIR. invalid wild-carding at level ',I6,
     +       ' in pathname - ',A)
         IF(LLOGCD.GE.-3) WRITE(LWRITE,9003)
 9003 FORMAT(' CDLDIR. only one of < or > may be specified')
         IQUEST(1) = -1
         GOTO 70
      ENDIF
 
      IF(ILT+IGT.NE.0) THEN
*
*     Is there a [mm:nn] syle range?
*
         JR  = LENOCC(DIRNAM(NLPAT+1))
         ISQ = INDEX(DIRNAM(NLPAT+1)(1:JR),'[')
         IF(ISQ.NE.0) THEN
            ILOW   = ICDECI(DIRNAM(NLPAT+1),ISQ+1,JR)
            IF(IS(1).EQ.0) THEN
               IRC = -1
               IF(LLOGCD.GE.-3) WRITE(LWRITE,9004)
     +            DIRNAM(NLPAT+1)(ISQ:JR)
 9004 FORMAT(' CDLDIR. error reading decimal value from ',A/
     +       '         Only integer ranges are supported',
     +       ' between [] characters, e.g. [10:20]')
               GOTO 80
            ENDIF
            IHIGH  = ICDECI(DIRNAM(NLPAT+1),
     +               INDEX(DIRNAM(NLPAT+1),':') + 1,JR)
            IF(IS(1).EQ.0) THEN
               IRC = -1
               IF(LLOGCD.GE.-3) WRITE(LWRITE,9004)
     +            DIRNAM(NLPAT+1)(ISQ:JR)
               GOTO 80
            ENDIF
            IF(LLOGCD.GE.3) WRITE(LWRITE,9005) ILOW,IHIGH
 9005 FORMAT(' CDLDIR. ilow/ihigh = ',I6,'/',I6)
            NCRNGE(NLPAT+1) = 0
            IF(IHIGH-ILOW+1.GT.100) THEN
               IF(LLOGCD.GE.-3) WRITE(LWRITE,9006)
 9006 FORMAT(' CDLDIR. maximum range is 100',
     +            ' - excess elements will be ignored')
               IHIGH = ILOW + 99
            ENDIF
            DO 30 JJ=ILOW,IHIGH
               NCRNGE(NLPAT+1) = NCRNGE(NLPAT+1) + 1
               ICRNGE(NCRNGE(NLPAT+1),NLPAT+1) = JJ
               ICELEM(NCRNGE(NLPAT+1),NLPAT+1) = -1
               IF(ILT.NE.0) THEN
                  ICVAL(NCRNGE(NLPAT+1),NLPAT+1) = 999999
               ELSE
                  ICVAL(NCRNGE(NLPAT+1),NLPAT+1) = -1
               ENDIF
   30       CONTINUE
         ENDIF
*
*     Loop over all subdirectories at this level
*
         DO 40 JJ=1,NSD(NLPAT)
            LS=IQ(KQSP+LCDIR+KLS)
            IH=LS+7*(JJ-1)
            CALL ZITOH(IQ(KQSP+LCDIR+IH),IHDIR,4)
            CALL UHTOC(IHDIR,4,CHDIR,16)
            CALL CDSELP(CHDIR,DIRNAM(NLPAT+1),JJ,NSD(NLPAT),JINDEX,IRC)
   40    CONTINUE
         IF(JINDEX.GT.0) THEN
            IF(NCRNGE(NLPAT+1).EQ.0) THEN
               IF(LLOGCD.GE.3) THEN
                  LS=IQ(KQSP+LCDIR+KLS)
                  IH=LS+7*(JINDEX-1)
                  CALL ZITOH(IQ(KQSP+LCDIR+IH),IHDIR,4)
                  CALL UHTOC(IHDIR,4,CHDIR,16)
                  WRITE(LWRITE,9007) CHDIR
 9007 FORMAT(' CDLDIR. selected subdirectory ',A)
               ENDIF
               NSD(NLPAT) = JINDEX
               ISD(NLPAT) = JINDEX - 1
            ELSE
               LS=IQ(KQSP+LCDIR+KLS)
               IF(LLOGCD.GE.3) WRITE(LWRITE,9008)
 9008 FORMAT(' CDLDIR. selected following subdirectories...')
               JMIN = 999999
               JMAX = 0
               DO 50 II=1,NCRNGE(NLPAT+1)
                  JJ=ICELEM(II,NLPAT+1)
                  IF(JJ.LE.0) GOTO 50
                  IF(JJ.LT.JMIN) JMIN = JJ
                  IF(JJ.GT.JMAX) JMAX = JJ
                  IF(LLOGCD.GE.3) THEN
                     IH=LS+7*(JJ-1)
                     CALL ZITOH(IQ(KQSP+LCDIR+IH),IHDIR,4)
                     CALL UHTOC(IHDIR,4,CHDIR,16)
                     WRITE(LWRITE,9009) CHDIR
 9009 FORMAT(1X,A)
                  ENDIF
   50          CONTINUE
               NSD(NLPAT) = JMAX
               ISD(NLPAT) = JMIN - 1
            ENDIF
         ELSE
            IF(LLOGCD.GE.2) WRITE(LWRITE,9010)
 9010 FORMAT(' CDLDIR. no subdirectory matches')
            NSD(NLPAT) = 0
         ENDIF
      ELSE
         JINDEX = -1
      ENDIF
*
*            Process possible down directories
*
   60 ISD(NLPAT)=ISD(NLPAT)+1
 
      IF((ISD(NLPAT).LE.NSD(NLPAT)).AND.
     +   (NLPAT.LT.(NLPAT0+NLEV))) THEN
         IF(NCRNGE(NLPAT+1).NE.0
     +      .AND.IUFIND(ISD(NLPAT),
     +         ICELEM(1,NLPAT+1),1,NCRNGE(NLPAT+1)).GT.
     +         NCRNGE(NLPAT+1)) GOTO 60
         NLPAT=NLPAT+1
         LS=IQ(KQSP+LCDIR+KLS)
         IH=LS+7*(ISD(NLPAT-1)-1)
         CALL ZITOH(IQ(KQSP+LCDIR+IH),IHDIR,4)
         CALL UHTOC(IHDIR,4,CHPAT(NLPAT),16)
         ITIME=ITIME+1
         GO TO 10
      ELSE
*
*            Write information on current directory
*
         CALL RZPAFF(CHPAT,NLPAT,CHL)
*        IF(NLPAT.LE.(NLPAT0+NLEV))THEN
*        ENDIF
         NLPAT=NLPAT-1
         IF(NLPAT.GE.NLPAT0)THEN
            LUP=LQ(KQSP+LCDIR+1)
            CALL MZDROP(JQPDVS,LCDIR,' ')
            LCDIR=LUP
            GO TO 60
         ENDIF
      ENDIF
*
*            Reset CWD
*
   70 CALL RZCDIR(CHWOLD,' ')
 
      IF((IWIDTH.NE.0).AND.(IOPTV+IOPTW.NE.0)) THEN
         WRITE(LWRITE,'(1X,A)') CHLINE(1:IWIDTH)
      ENDIF
*
   80 CONTINUE
      IRC = IQUEST(1)
      IQUEST(11) = MAX(NFOUND -1,0)
      IQUEST(12) = NMATCH
      END
 

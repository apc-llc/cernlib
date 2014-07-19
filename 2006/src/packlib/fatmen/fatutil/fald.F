*
* $Id: fald.F,v 1.1.1.1 1996/03/07 15:18:06 mclareni Exp $
*
* $Log: fald.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:06  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FALD(PATH,NLEVEL,LWRITE,CHOPT,IRC)
*
************************************************************************
*
*        To traverse the FATMEN directory tree starting from
*        the specified pathname.
*        Modified from the RZ routine RZSTAT / RZSCAN
*
* ==> To be modified to support wild-cards in path name, including
* ==> <>
*
* Input:
*   CHPATH  The pathname of the starting directory
*   NLEVEL  Number of levels  below CHPATH to descend
*
* Output:
*           IQUEST(11) = number of directories found
*           IQUEST(12) = number of directories that match
*
* Called by FMLD
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
      DIMENSION ISD(MAXLEV),NSD(MAXLEV),IHDIR(4)
#include "fatmen/slate.inc"
#include "fatmen/fatbug.inc"
#include "fatmen/farnge.inc"
      SAVE DIRNAM
#include "fatmen/fatopts.inc"
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
      LPATH      = LENOCC(PATH)
      LPATHI     = LPATH
      CHPATH     = PATH(1:LPATH)
      CALL CLTOU(CHPATH(1:LPATH))
*
*     Split input path name into its component pieces
*     (this is the opposite of RZPAFF which glues them together)
*
      CALL FMPAFF(CHPATH(1:LPATH),DIRNAM,MAXLEV,IRET)
*
*     Find first wild card in generic name
*
      IF(IDEBFA.GE.3)
     +PRINT *,'FALD. enter for PATH = ',CHPATH(1:LPATH),' nlevel = ',
     +        NLEVEL,' chopt = ',CHOPT
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
      CALL FACDIR(CHWOLD,'R')
      OLDDIR = ' '
      LOLD   = 1
*
*     Set CWD to the current level
*
   10 CONTINUE
      IF(ITIME.NE.0)THEN
         CALL RZPAFF(CHPAT,NLPAT,CHL)
         IF(IQUEST(1).NE.0) THEN
            NLPAT = NLPAT - 1
            GOTO 60
         ENDIF
         CALL FACDIR(CHL,' ')
      ELSE
         CALL FACDIR(CHPATH(1:LPATH),' ')
         IF(IQUEST(1).NE.0)GO TO 80
         NLPAT0=NLPAT
         CHL = CHPATH(1:LPATH)
      ENDIF
      IF(IQUEST(1).NE.0) THEN
         NLPAT = NLPAT - 1
         GOTO 60
      ENDIF
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
      IRC    = 0
 
      IF(ITIME.NE.0) THEN
         NFOUND = NFOUND + 1
         CALL FMATCH(CHL,CHPATH(1:LPATHI),IRC)
         IF(IRC.EQ.0) NMATCH = NMATCH + 1
      ENDIF
 
      IF(IRC.EQ.0) THEN
      LCHL   = LENOCC(CHL)
      ISLASH = INDEXB(CHL(1:LCHL),'/')
 
         IF(IOPTV+IOPTW.EQ.0) THEN
            WRITE(LWRITE,*) CHL(1:LCHL)
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
            LOLD   = ISLASH - 1
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
      NFRNGE(NLPAT+1) = 0
 
      IF((ILT.NE.0).AND.(IGT.NE.0)) THEN
         IF(IDEBFA.GE.-3) PRINT *,'FALD. invalid wild-carding ',
     +      'at level ',NLPAT,' in pathname - ',DIRNAM(NLPAT+1)
         IF(IDEBFA.GE.-3) PRINT *,'FALD. only one of < or > ',
     +      'may be specified'
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
               IF(IDEBFA.GE.-3) PRINT *,'FALD. error reading ',
     +            'decimal value from ',DIRNAM(NLPAT+1)(ISQ:JR)
               IF(IDEBFA.GE.-3) PRINT *,'      Only integer ranges ',
     +            'are supported between [] characters, e.g. [10:20]'
               GOTO 80
             ENDIF
            IHIGH  = ICDECI(DIRNAM(NLPAT+1),
     +               INDEX(DIRNAM(NLPAT+1),':') + 1,JR)
            IF(IS(1).EQ.0) THEN
               IRC = -1
               IF(IDEBFA.GE.-3) PRINT *,'FALD. error reading ',
     +            'decimal value from ',DIRNAM(NLPAT+1)(ISQ:JR)
               IF(IDEBFA.GE.-3) PRINT *,'      Only integer ranges ',
     +            'are supported between [] characters, e.g. [10:20]'
               GOTO 80
             ENDIF
            IF(IDEBFA.GE.3) PRINT *,'FALD. ilow/ihigh = ',ILOW,IHIGH
            NFRNGE(NLPAT+1) = 0
            IF(IHIGH-ILOW+1.GT.100) THEN
               IF(IDEBFA.GE.-3) PRINT *,'FALD. maximum range is 100',
     +            ' - excess elements will be ignored'
               IHIGH = ILOW + 99
            ENDIF
            DO 20 JJ=ILOW,IHIGH
            NFRNGE(NLPAT+1) = NFRNGE(NLPAT+1) + 1
            IFRNGE(NFRNGE(NLPAT+1),NLPAT+1) = JJ
            IFELEM(NFRNGE(NLPAT+1),NLPAT+1) = -1
            IF(ILT.NE.0) THEN
               IFVAL(NFRNGE(NLPAT+1),NLPAT+1) = 999999
            ELSE
               IFVAL(NFRNGE(NLPAT+1),NLPAT+1) = -1
            ENDIF
   20       CONTINUE
         ENDIF
*
*     Loop over all subdirectories at this level
*
         DO 40 JJ=1,NSD(NLPAT)
            LS=IQ(KQSP+LCDIR+KLS)
            IH=LS+7*(JJ-1)
            CALL ZITOH(IQ(KQSP+LCDIR+IH),IHDIR,4)
            CALL UHTOC(IHDIR,4,CHDIR,16)
            CALL FASELP(CHDIR,DIRNAM(NLPAT+1),JJ,NSD(NLPAT),JINDEX,IRC)
   40       CONTINUE
         IF(JINDEX.GT.0) THEN
            IF(NFRNGE(NLPAT+1).EQ.0) THEN
               IF(IDEBFA.GE.3) THEN
                  LS=IQ(KQSP+LCDIR+KLS)
                  IH=LS+7*(JINDEX-1)
                  CALL ZITOH(IQ(KQSP+LCDIR+IH),IHDIR,4)
                  CALL UHTOC(IHDIR,4,CHDIR,16)
                  PRINT *,'FALD. selected subdirectory ',CHDIR
               ENDIF
               NSD(NLPAT) = JINDEX
               ISD(NLPAT) = JINDEX - 1
            ELSE
               LS=IQ(KQSP+LCDIR+KLS)
               IF(IDEBFA.GE.3)
     +            PRINT *,'FALD. selected following subdirectories...'
               JMIN = 999999
               JMAX = 0
               DO 50 II=1,NFRNGE(NLPAT+1)
                  JJ=IFELEM(II,NLPAT+1)
                  IF(JJ.LE.0) GOTO 50
                  IF(JJ.LT.JMIN) JMIN = JJ
                  IF(JJ.GT.JMAX) JMAX = JJ
                  IF(IDEBFA.GE.3) THEN
                     IH=LS+7*(JJ-1)
                     CALL ZITOH(IQ(KQSP+LCDIR+IH),IHDIR,4)
                     CALL UHTOC(IHDIR,4,CHDIR,16)
                     PRINT *,CHDIR
                  ENDIF
   50          CONTINUE
               NSD(NLPAT) = JMAX
               ISD(NLPAT) = JMIN - 1
            ENDIF
         ELSE
               IF(IDEBFA.GE.2)
     +         PRINT *,'FALD. no subdirectory matches'
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
         IF(NFRNGE(NLPAT+1).NE.0
     +      .AND.IUFIND(ISD(NLPAT),
     +         IFELEM(1,NLPAT+1),1,NFRNGE(NLPAT+1)).GT.
     +         NFRNGE(NLPAT+1)) GOTO 60
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
   70 CALL FACDIR(CHWOLD,' ')
 
      IF((IWIDTH.NE.0).AND.(IOPTV+IOPTW.NE.0)) THEN
         WRITE(LWRITE,'(1X,A)') CHLINE(1:IWIDTH)
      ENDIF
*
   80 CONTINUE
      IRC = IQUEST(1)
      IQUEST(11) = NFOUND
      IQUEST(12) = NMATCH
      RETURN
      END

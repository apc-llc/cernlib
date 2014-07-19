*
* $Id: fmloop.F,v 1.1.1.1 1996/03/07 15:18:06 mclareni Exp $
*
* $Log: fmloop.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:06  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMLOOP(PATH,NLEVEL,UROUT,IRC)
*
************************************************************************
*
*        To traverse the FATMEN directory tree starting from
*        the specified pathname.
*        Modified from the RZ routine RZSTAT / RZSCAN
*
*        This routine behaves like FMSCAN, but for files, rather than
*        directories.
*
* Input:
*   CHPATH  The pathname of the starting directory
*   NLEVEL  Number of levels  below CHPATH about which  space information
*           has to be accumulated.
*   UROUT   External user routine to be called for each pathname
*
* Output:
*           IQUEST(10) = number levels in initial path
*           IQUEST(11) = number of directories found
*
*           IQUEST(12) = number of subdirectories at this level
*           IQUEST(13) = number of elements in path name
*           IQUEST(14) = number of keys
*           IQUEST(15) = number of words per key
*           IQUEST(16) = incremental count of #keys selected
*           IQUEST(17) = number of this key vector
*
* Called by <USER>
*
*
************************************************************************
#include "fatmen/faust.inc"
#include "fatmen/slate.inc"
#include "zebra/rzcl.inc"
#include "zebra/rzdir.inc"
#include "zebra/rzch.inc"
#include "zebra/rzk.inc"
      CHARACTER*(*) PATH
      CHARACTER*255 CHPATH
      CHARACTER*20  CHFILE,FNAME
      PARAMETER     (MAXLEV=20)
      CHARACTER*16  DIRNAM(MAXLEV),CHDIR
      DIMENSION ISD(15),NSD(15),IHDIR(4)
      DIMENSION     IOPTV(2)
      EQUIVALENCE   (IOPTQ,IOPTV(1)), (IOPTX,IOPTV(2))
      EXTERNAL      UROUT
#include "fatmen/fmnkeys.inc"
      DIMENSION     KEYS(LKEYFA)
#include "fatmen/fatpara.inc"
#include "fatmen/fatloc.inc"
#include "fatmen/fatmtp.inc"
#include "fatmen/fatcpl.inc"
#include "fatmen/farnge.inc"
#include "fatmen/fatbug.inc"
      SAVE           DIRNAM
*
*-----------------------------------------------------------------------
*
*
 
      NFLOOP     = NFLOOP + 1
      IRC        = 0
      IQUEST(1)  = 0
      IQUEST(11) = 0
      LPATH      = LENOCC(PATH)
      CHPATH     = PATH(1:LPATH)
      CALL CLTOU(CHPATH(1:LPATH))
*
*     Find file name
*
      LSLASH     = INDEXB(CHPATH(1:LPATH),'/')
      CHFILE     = CHPATH(LSLASH+1:LPATH)
      LFILE      = LPATH - LSLASH
      LPATH      = LSLASH - 1
      LPATHI     = LPATH
*
*     Get number of levels in initial path
*
      CALL FMNWRD('/',CHPATH(3:LPATHI),NLEV0)
*
*     Split input path name into its component pieces
*     (this is the opposite of RZPAFF which glues them together)
*
      CALL FMPAFF(CHPATH(1:LPATH),DIRNAM,MAXLEV,IRET)
*
*     Find first wild card in generic name
*
      IF(IDEBFA.GE.3)
     +PRINT *,'FMLOOP. enter for PATH = ',CHPATH(1:LPATH),
     +        ' file = ',CHFILE(1:LFILE)
      IWILD  = ICFMUL('*%(<>',CHPATH,1,LPATH)
      IF(IWILD.LE.LPATH) THEN
         LPATH = INDEXB(CHPATH(1:IWILD),'/') - 1
      ENDIF
*
      ITIME=0
      NLEV=NLEVEL
      IF(NLEV.LE.0)NLEV=99
      NFOUND = 0
      ITEMP  = 0
      CALL FACDIR(CHWOLD,'R')
*
*            Set CWD to the current level
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
      IQUEST(10) = NLEV0
      IQUEST(12) = IQ(KQSP+LCDIR+KNSD)
      IQUEST(13) = NLPAT
      IQUEST(14) = IQ(KQSP+LCDIR+KNKEYS)
      IQUEST(15) = IQ(KQSP+LCDIR+KNWKEY)
      NWK        = IQ(KQSP+LCDIR+KNWKEY)
      NKEYS      = IQ(KQSP+LCDIR+KNKEYS)
      LK         = IQ(KQSP+LCDIR+KLK)
*
*     Ensure that entire path name matches before calling UROUT
*
      CALL FMATCH(CHL,CHPATH(1:LPATHI),IRC)
*
*     Trailing unmatched characters in CHPATH
*     Accept if first character is a / and total length
*     of pathname is ok
*
      IF(IRC.EQ.2.AND.CHL(IQUEST(1):IQUEST(1)).EQ.'/'
     +   .AND.NLPAT.LE.NLEV0+NLEV) IRC = 0
 
      IF(IRC.EQ.0) THEN
*
*     Get the keys
*
      NDONE = 0
      DO 30 I=1,NKEYS
*
*     Number of this key vector
*
         IQUEST(17) = I
         K=LK+(NWK+1)*(I-1)
         DO 20 J=1,NWK
            IKDES=(J-1)/10
            IKBIT1=3*J-30*IKDES-2
            IF(JBYT(IQ(KQSP+LCDIR+KKDES+IKDES),IKBIT1,3).LT.3)THEN
               KEYS(J)=IQ(KQSP+LCDIR+K+J)
            ELSE
               CALL ZITOH(IQ(KQSP+LCDIR+K+J),KEYS(J),1)
            ENDIF
   20    CONTINUE
         CALL UHTOC(KEYS(MKFNFA),4,FNAME,20)
         LF = LENOCC(FNAME)
*
*     Check that keys match those selected
*     Location code:
*
         IF(NUMLOC.GT.0) THEN
            IF(IUCOMP(KEYS(MKLCFA),MFMLOC,NUMLOC).EQ.0) THEN
               IF(IDEBFA.GE.3) PRINT *,'FMLOOP. candidate # ',I,
     +            ' fails location code check'
               GOTO 30
            ENDIF
         ENDIF
*
*     Copy level:
*
         IF(NUMCPL.GT.0) THEN
            IF(IUCOMP(KEYS(MKCLFA),MFMCPL,NUMCPL).EQ.0) THEN
               IF(IDEBFA.GE.3) PRINT *,'FMLOOP. candidate # ',I,
     +            ' fails copy level check'
               GOTO 30
            ENDIF
         ENDIF
*
*     Media type:
*
         IF(NUMMTP.GT.0) THEN
            IF(IUCOMP(KEYS(MKMTFA),MFMMTP,NUMMTP).EQ.0) THEN
               IF(IDEBFA.GE.3) PRINT *,'FMLOOP. candidate # ',I,
     +            ' fails media type check'
               GOTO 30
            ENDIF
         ENDIF
*
*     File name:
*
         LF     = LENOCC(FNAME)
*
*     Fast check: CHFILE = '*'
*
         IF(CHFILE(1:LFILE).EQ.'*.') THEN
            IMAT = 0
         ELSE
            CALL FMATCH(FNAME(1:LF),CHFILE(1:LFILE),IMAT)
         ENDIF
 
         IF(IMAT.NE.0) THEN
               IF(IDEBFA.GE.3) PRINT *,'FMLOOP. candidate # ',I,
     +            ' fails filename check'
               GOTO 30
         ENDIF
*
*     Now call user routine
*
         NDONE = NDONE + 1
         IQUEST(16) = NDONE
         CALL UROUT(CHL(1:LENOCC(CHL))//'/'
     +              //FNAME(1:LF),KEYS,IURC)
*
*     Skip rest of this directory?
*
         IF(IURC.LT.0) GOTO 40
*
*     Finish now?
*
         IF(IURC.GT.0) RETURN
   30    CONTINUE
   40    CONTINUE
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
         IF(IDEBFA.GE.-3) PRINT *,'FMLOOP. invalid wild-carding ',
     +      'at level ',NLPAT,' in pathname - ',DIRNAM(NLPAT+1)
         IF(IDEBFA.GE.-3) PRINT *,'FMLOOP. only one of < or > ',
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
               IF(IDEBFA.GE.-3) PRINT *,'FMLOOP. error reading ',
     +            'decimal value from ',DIRNAM(NLPAT+1)(ISQ:JR)
               IF(IDEBFA.GE.-3) PRINT *,'        Only integer ranges ',
     +            'are supported between [] characters, e.g. [10:20]'
               GOTO 80
             ENDIF
            IHIGH  = ICDECI(DIRNAM(NLPAT+1),
     +               INDEX(DIRNAM(NLPAT+1),':') + 1,JR)
            IF(IS(1).EQ.0) THEN
               IRC = -1
               IF(IDEBFA.GE.-3) PRINT *,'FMLOOP. error reading ',
     +            'decimal value from ',DIRNAM(NLPAT+1)(ISQ:JR)
               IF(IDEBFA.GE.-3) PRINT *,'        Only integer ranges ',
     +            'are supported between [] characters, e.g. [10:20]'
               GOTO 80
             ENDIF
            IF(IDEBFA.GE.3) PRINT *,'FMLOOP. ilow/ihigh = ',ILOW,IHIGH
            NFRNGE(NLPAT+1) = 0
            IF(IHIGH-ILOW+1.GT.100) THEN
               IF(IDEBFA.GE.-3) PRINT *,'FMLOOP. maximum range is 100',
     +            ' - excess elements will be ignored'
               IHIGH = ILOW + 99
            ENDIF
            DO 22 JJ=ILOW,IHIGH
            NFRNGE(NLPAT+1) = NFRNGE(NLPAT+1) + 1
            IFRNGE(NFRNGE(NLPAT+1),NLPAT+1) = JJ
            IFELEM(NFRNGE(NLPAT+1),NLPAT+1) = -1
            IF(ILT.NE.0) THEN
               IFVAL(NFRNGE(NLPAT+1),NLPAT+1) = 999999
            ELSE
               IFVAL(NFRNGE(NLPAT+1),NLPAT+1) = -1
            ENDIF
   22       CONTINUE
         ENDIF
*
*     Loop over all subdirectories at this level
*
         DO 44 JJ=1,NSD(NLPAT)
            LS=IQ(KQSP+LCDIR+KLS)
            IH=LS+7*(JJ-1)
            CALL ZITOH(IQ(KQSP+LCDIR+IH),IHDIR,4)
            CALL UHTOC(IHDIR,4,CHDIR,16)
            CALL FASELP(CHDIR,DIRNAM(NLPAT+1),JJ,NSD(NLPAT),JINDEX,IRC)
   44       CONTINUE
         IF(JINDEX.GT.0) THEN
            IF(NFRNGE(NLPAT+1).EQ.0) THEN
               IF(IDEBFA.GE.3) THEN
                  LS=IQ(KQSP+LCDIR+KLS)
                  IH=LS+7*(JINDEX-1)
                  CALL ZITOH(IQ(KQSP+LCDIR+IH),IHDIR,4)
                  CALL UHTOC(IHDIR,4,CHDIR,16)
                  PRINT *,'FMLOOP. selected subdirectory ',CHDIR
               ENDIF
               NSD(NLPAT) = JINDEX
               ISD(NLPAT) = JINDEX - 1
            ELSE
               LS=IQ(KQSP+LCDIR+KLS)
               IF(IDEBFA.GE.3)
     +            PRINT *,'FMLOOP. selected following subdirectories...'
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
     +         PRINT *,'FMLOOP. no subdirectory matches'
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
         IF(NLPAT.LE.(NLPAT0+NLEV))THEN
            NFOUND = NFOUND + 1
         ENDIF
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
*
   80 CONTINUE
      IRC = IQUEST(1)
      IQUEST(11) = NFOUND
      RETURN
      END

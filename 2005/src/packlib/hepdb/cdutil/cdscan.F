*
* $Id: cdscan.F,v 1.1.1.1 1996/02/28 16:24:38 mclareni Exp $
*
* $Log: cdscan.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:38  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDSCAN(PATH,NLEVEL,UROUT,IRC)
*
************************************************************************
*
*        To traverse the HEPDB directory tree starting from
*        the specified pathname.
*        Modified from the RZ routine RZSTAT / RZSCAN
*
*
* Input:
*   CHPATH  The pathname of the starting directory
*   NLEVEL  Number of levels below CHPATH to descend
*   UROUT   External user routine to be called for each pathname
*
* Output:
*           IQUEST(10) = number of elements in initial path
*           IQUEST(11) = number of directories found
*
*           IQUEST(12) = number of subdirectories at this level
*           IQUEST(13) = number of elements in path name
*           IQUEST(14) = number of keys
*           IQUEST(15) = number of words per key
*
* Called by <USER>
*
*
************************************************************************
#include "hepdb/slate.inc"
#include "zebra/rzcl.inc"
#include "zebra/rzdir.inc"
#include "zebra/rzch.inc"
#include "zebra/rzk.inc"
      CHARACTER*(*) PATH
      CHARACTER*255 CHPATH
      PARAMETER     (MAXLEV=20)
      CHARACTER*16  DIRNAM(MAXLEV),CHDIR
      DIMENSION ISD(MAXLEV),NSD(MAXLEV),IHDIR(4)
      DIMENSION    IOPTV(2)
      EQUIVALENCE (IOPTQ,IOPTV(1)), (IOPTX,IOPTV(2))
      EXTERNAL    UROUT
#include "hepdb/cdrnge.inc"
#include "hepdb/cdunit.inc"
*
*-----------------------------------------------------------------------
*
*
      IRC=0
      IQUEST(1)  = 0
      IQUEST(11) = 0
      LPATH      = LENOCC(PATH)
      LPATHI     = LPATH
      CHPATH     = PATH(1:LPATH)
      CALL CLTOU(CHPATH(1:LPATH))
*
      ITIME  = 0
      NLEV   = NLEVEL
      IF(NLEV.LE.0) NLEV=99
      NFOUND = 0
      ITEMP  = 0
      CALL RZCDIR(CHWOLD,'R')
      LOLD   = LENOCC(CHWOLD)
*
*     Split input path name into its component pieces
*     (this is the opposite of RZPAFF which glues them together)
*
      CALL CDPAFF(CHPATH(1:LPATH),DIRNAM,MAXLEV,IRET)
*
*     Find first wild card in generic name
*
      IF(LLOGCD.GE.3)
     +PRINT *,'CDSCAN. enter for PATH = ',CHPATH(1:LPATH)
      CALL CDNWRD('/',CHPATH(3:LPATHI),NLEV0)
      IWILD  = ICFMUL('*%(<>',CHPATH,1,LPATH)
      IF(IWILD.LE.LPATH) THEN
         LPATH = INDEXB(CHPATH(1:IWILD),'/') - 1
         CALL CDNWRD('/',CHPATH(LPATH+2:LPATHI),NLEV1)
         NLEV  = MIN(99,NLEV+NLEV1)
*     ELSE
*        LPATHI = LPATHI + 2
*        CHPATH(LPATHI-1:LPATHI) = '/*'
      ENDIF
 
      IF(LLOGCD.GE.3) PRINT *,'CDSCAN. show directories ',
     +   'below ',CHPATH(1:LPATH),' down ',NLEV,' levels'
*
*            Set CWD to the current level
*
   10 CONTINUE
      IF(ITIME.NE.0)THEN
         CALL RZPAFF(CHPAT,NLPAT,CHL)
         CALL RZCDIR(CHL,' ')
      ELSE
         CALL RZCDIR(CHPATH(1:LPATH),' ')
         IF(IQUEST(1).NE.0)GO TO 70
         NLPAT0=NLPAT
         CHL = CHPATH(1:LPATH)
      ENDIF
      IF(IQUEST(1).NE.0)GO TO 60
*
*     Set IQUEST
*
      IQUEST(10) = NLEV0
      IQUEST(12) = IQ(KQSP+LCDIR+KNSD)
      IQUEST(13) = NLPAT
      IQUEST(14) = IQ(KQSP+LCDIR+KNKEYS)
      IQUEST(15) = IQ(KQSP+LCDIR+KNWKEY)
*
*     Ensure that entire path name matches before calling UROUT
*
      CALL CDMTCH(CHL,CHPATH(1:LPATHI),IRC)
*
*     Trailing unmatched characters in CHPATH
*     Accept if first character is a / and total length
*     of pathname is ok
*
      IF(IRC.EQ.2.AND.CHL(IQUEST(1):IQUEST(1)).EQ.'/'
     +   .AND.NLPAT.LE.NLEV0+NLEV) IRC = 0
 
      IF(IRC.EQ.0) THEN
         CALL UROUT(CHL,IURC)
         IF(IURC.NE.0) RETURN
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
         IF(LLOGCD.GE.-3) PRINT *,'CDSCAN. invalid wild-carding ',
     +      'at level ',NLPAT,' in pathname - ',DIRNAM(NLPAT+1)
         IF(LLOGCD.GE.-3) PRINT *,'CDSCAN. only one of < or > ',
     +      'may be specified'
         IQUEST(1) = -1
         GOTO 60
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
               IF(LLOGCD.GE.-3) PRINT *,'CDSCAN. error reading ',
     +            'decimal value from ',DIRNAM(NLPAT+1)(ISQ:JR)
               IF(LLOGCD.GE.-3) PRINT *,'        Only integer ranges ',
     +            'are supported between [] characters, e.g. [10:20]'
               GOTO 70
             ENDIF
            IHIGH  = ICDECI(DIRNAM(NLPAT+1),
     +               INDEX(DIRNAM(NLPAT+1),':') + 1,JR)
            IF(IS(1).EQ.0) THEN
               IRC = -1
               IF(LLOGCD.GE.-3) PRINT *,'CDSCAN. error reading ',
     +            'decimal value from ',DIRNAM(NLPAT+1)(ISQ:JR)
               IF(LLOGCD.GE.-3) PRINT *,'        Only integer ranges ',
     +            'are supported between [] characters, e.g. [10:20]'
               GOTO 70
             ENDIF
            IF(LLOGCD.GE.3) PRINT *,'CDSCAN. ilow/ihigh = ',ILOW,IHIGH
            NCRNGE(NLPAT+1) = 0
            IF(IHIGH-ILOW+1.GT.100) THEN
               IF(LLOGCD.GE.-3) PRINT *,'CDSCAN. maximum range is 100',
     +            ' - excess elements will be ignored'
               IHIGH = ILOW + 99
            ENDIF
            DO 20 JJ=ILOW,IHIGH
            NCRNGE(NLPAT+1) = NCRNGE(NLPAT+1) + 1
            ICRNGE(NCRNGE(NLPAT+1),NLPAT+1) = JJ
            ICELEM(NCRNGE(NLPAT+1),NLPAT+1) = -1
            IF(ILT.NE.0) THEN
               ICVAL(NCRNGE(NLPAT+1),NLPAT+1) = 999999
            ELSE
               ICVAL(NCRNGE(NLPAT+1),NLPAT+1) = -1
            ENDIF
   20       CONTINUE
         ENDIF
*
*     Loop over all subdirectories at this level
*
         DO 30 JJ=1,NSD(NLPAT)
            LS=IQ(KQSP+LCDIR+KLS)
            IH=LS+7*(JJ-1)
            CALL ZITOH(IQ(KQSP+LCDIR+IH),IHDIR,4)
            CALL UHTOC(IHDIR,4,CHDIR,16)
            CALL CDSELP(CHDIR,DIRNAM(NLPAT+1),JJ,NSD(NLPAT),JINDEX,IRC)
   30       CONTINUE
         IF(JINDEX.GT.0) THEN
            IF(NCRNGE(NLPAT+1).EQ.0) THEN
               IF(LLOGCD.GE.3) THEN
                  LS=IQ(KQSP+LCDIR+KLS)
                  IH=LS+7*(JINDEX-1)
                  CALL ZITOH(IQ(KQSP+LCDIR+IH),IHDIR,4)
                  CALL UHTOC(IHDIR,4,CHDIR,16)
                  PRINT *,'CDSCAN. selected subdirectory ',CHDIR
               ENDIF
               NSD(NLPAT) = JINDEX
               ISD(NLPAT) = JINDEX - 1
            ELSE
               LS=IQ(KQSP+LCDIR+KLS)
               IF(LLOGCD.GE.3)
     +            PRINT *,'CDSCAN. selected following subdirectories...'
               JMIN = 999999
               JMAX = 0
               DO 40 II=1,NCRNGE(NLPAT+1)
                  JJ=ICELEM(II,NLPAT+1)
                  IF(JJ.LE.0) GOTO 40
                  IF(JJ.LT.JMIN) JMIN = JJ
                  IF(JJ.GT.JMAX) JMAX = JJ
                  IF(LLOGCD.GE.3) THEN
                     IH=LS+7*(JJ-1)
                     CALL ZITOH(IQ(KQSP+LCDIR+IH),IHDIR,4)
                     CALL UHTOC(IHDIR,4,CHDIR,16)
                     PRINT *,CHDIR
                  ENDIF
   40          CONTINUE
               NSD(NLPAT) = JMAX
               ISD(NLPAT) = JMIN - 1
            ENDIF
         ELSE
               IF(LLOGCD.GE.2)
     +         PRINT *,'CDSCAN. no subdirectory matches'
            NSD(NLPAT) = 0
         ENDIF
      ELSE
         JINDEX = -1
      ENDIF
*
*            Process possible down directories
*
   50 ISD(NLPAT)=ISD(NLPAT)+1
      IF((ISD(NLPAT).LE.NSD(NLPAT)).AND.
     +   (NLPAT.LT.(NLPAT0+NLEV))) THEN
         IF(NCRNGE(NLPAT+1).NE.0
     +      .AND.IUFIND(ISD(NLPAT),
     +         ICELEM(1,NLPAT+1),1,NCRNGE(NLPAT+1)).GT.
     +         NCRNGE(NLPAT+1)) GOTO 50
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
            GO TO 50
         ENDIF
      ENDIF
*
*            Reset CWD
*
   60 CALL RZCDIR(CHWOLD(1:LOLD),' ')
*
   70 CONTINUE
      IRC = IQUEST(1)
      IQUEST(11) = NFOUND
      RETURN
      END

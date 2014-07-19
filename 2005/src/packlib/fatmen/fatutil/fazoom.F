*
* $Id: fazoom.F,v 1.1.1.1 1996/03/07 15:18:07 mclareni Exp $
*
* $Log: fazoom.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:07  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FAZOOM(PATH,IRC)
*
*        To zoom down the FATMEN directory tree starting from
*        the specified pathname.
*
#include "zebra/rzcl.inc"
#include "zebra/rzdir.inc"
#include "zebra/rzch.inc"
#include "zebra/rzk.inc"
      CHARACTER*(*) PATH
      CHARACTER*255 CHPATH
      PARAMETER     (MAXLEV=20)
      CHARACTER*16  DIRNAM(MAXLEV),CHDIR
      DIMENSION ISD(15),NSD(15),IHDIR(4)
      DIMENSION    IOPTV(2)
      EQUIVALENCE (IOPTQ,IOPTV(1)), (IOPTX,IOPTV(2))
#include "fatmen/fatbug.inc"
      SAVE         DIRNAM
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
*     NLEV   = NLEVEL
*     IF(NLEV.LE.0) NLEV=99
      NLEV   = 99
      NFOUND = 0
      ITEMP  = 0
      CALL FACDIR(CHWOLD,'R')
      LOLD   = LENOCC(CHWOLD)
*
*     Split input path name into its component pieces
*     (this is the opposite of RZPAFF which glues them together)
*
      CALL FMPAFF(CHPATH(1:LPATH),DIRNAM,MAXLEV,IRET)
*
*     Find first wild card in generic name
*
      IF(IDEBFA.GE.3)
     +PRINT *,'FAZOOM. enter for PATH = ',CHPATH(1:LPATH)
      IWILD  = ICFMUL('*%(<>',CHPATH,1,LPATH)
      IF(IWILD.LE.LPATH) THEN
         LPATH = INDEXB(CHPATH(1:IWILD),'/') - 1
         CALL FMNWRD('/',CHPATH(LPATH+2:LPATHI),NLEV0)
         NLEV  = MIN(99,NLEV+NLEV0)
*     ELSE
*        LPATHI = LPATHI + 2
*        CHPATH(LPATHI-1:LPATHI) = '/*'
      ENDIF
 
      IF(IDEBFA.GE.3) PRINT *,'FAZOOM. zoom down ',
     +   CHPATH(1:LPATH)
*
*            Set CWD to the current level
*
   10 CONTINUE
      IF(ITIME.NE.0)THEN
         CALL RZPAFF(CHPAT,NLPAT,CHL)
         IF(IQUEST(1).NE.0) THEN
            NLPAT = NLPAT - 1
            GOTO 20
         ENDIF
         CALL FACDIR(CHL,' ')
      ELSE
         CALL FACDIR(CHPATH(1:LPATH),' ')
         IF(IQUEST(1).NE.0)GO TO 99
         NLPAT0=NLPAT
         CHL = CHPATH(1:LPATH)
      ENDIF
      IF(IQUEST(1).NE.0) THEN
         NLPAT = NLPAT - 1
         GOTO 20
      ENDIF
*
*     Set IQUEST
*
      IQUEST(12) = IQ(KQSP+LCDIR+KNSD)
      IQUEST(13) = NLPAT
      IQUEST(14) = IQ(KQSP+LCDIR+KNKEYS)
      IQUEST(15) = IQ(KQSP+LCDIR+KNWKEY)
 
      IF(IDEBFA.GE.3) PRINT *,'FAZOOM. directory ',CHL,
     +   ' subdirectories: ',IQUEST(12),' files ',IQUEST(14)
*
*     Ensure that entire path name matches ...
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
*     If we have found a lowest level directory with 1 or more
*     files, then return
*
         IF(IQUEST(12).EQ.0.AND.IQUEST(14).GT.0) RETURN
      ENDIF
 
      ISD(NLPAT)=0
      NSD(NLPAT)=IQ(KQSP+LCDIR+KNSD)
*
*     If there is a <> at this level, perform matching
*     and follow only that path
*
      ILT = INDEX(DIRNAM(NLPAT+1),'<')
      IGT = INDEX(DIRNAM(NLPAT+1),'>')
 
      IF((ILT.NE.0).AND.(IGT.NE.0)) THEN
         IF(IDEBFA.GE.-3) PRINT *,'FAZOOM. invalid wild-carding ',
     +      'at level ',NLPAT,' in pathname - ',DIRNAM(NLPAT+1)
         IF(IDEBFA.GE.-3) PRINT *,'FAZOOM. only one of < or > ',
     +      'may be specified'
         IQUEST(1) = -1
         GOTO 90
      ENDIF
 
      IF(ILT+IGT.NE.0) THEN
*
*     Loop over all subdirectories at this level
*
         DO 5 JJ=1,NSD(NLPAT)
            LS=IQ(KQSP+LCDIR+KLS)
            IH=LS+7*(JJ-1)
            CALL ZITOH(IQ(KQSP+LCDIR+IH),IHDIR,4)
            CALL UHTOC(IHDIR,4,CHDIR,16)
            CALL FASELP(CHDIR,DIRNAM(NLPAT+1),JJ,NSD(NLPAT),JINDEX,IRC)
    5    CONTINUE
         IF(JINDEX.GT.0) THEN
            IF(IDEBFA.GE.3) THEN
               LS=IQ(KQSP+LCDIR+KLS)
               IH=LS+7*(JINDEX-1)
               CALL ZITOH(IQ(KQSP+LCDIR+IH),IHDIR,4)
               CALL UHTOC(IHDIR,4,CHDIR,16)
               IF(IDEBFA.GE.2)
     +         PRINT *,'FAZOOM. selected subdirectory ',CHDIR
            ENDIF
            NSD(NLPAT) = JINDEX
            ISD(NLPAT) = JINDEX - 1
         ELSE
            IF(IDEBFA.GE.2) PRINT *,'FAZOOM. no subdirectory matches'
            NSD(NLPAT) = 0
         ENDIF
      ELSE
         JINDEX = -1
      ENDIF
*
*            Process possible down directories
*
   20 ISD(NLPAT)=ISD(NLPAT)+1
      IF((ISD(NLPAT).LE.NSD(NLPAT)).AND.
     +   (NLPAT.LT.(NLPAT0+NLEV))) THEN
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
   30    CONTINUE
         NLPAT=NLPAT-1
         IF(NLPAT.GE.NLPAT0)THEN
            LUP=LQ(KQSP+LCDIR+1)
            CALL MZDROP(JQPDVS,LCDIR,' ')
            LCDIR=LUP
            GO TO 20
         ENDIF
      ENDIF
*
*            Reset CWD
*
   90 CALL FACDIR(CHWOLD(1:LOLD),' ')
*
   99 CONTINUE
*
*     No match
*
      IRC = 1
      RETURN
      END

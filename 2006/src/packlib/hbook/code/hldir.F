*
* $Id: hldir.F,v 1.1.1.1 1996/01/16 17:07:42 mclareni Exp $
*
* $Log: hldir.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:42  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.23/01 15/11/94  18.27.42  by  Fons Rademakers
*-- Author :
      SUBROUTINE HLDIR(CHPATH,CHOPT)
*.==========>
*.      To list histogram titles and IDs of directory CHPATH.
*. Input:
*.   *CHPATH* Character  variable  specifying  the  pathname
*.            (if CHPATH=' ') same as Current directory
*.   CHOPT    Character variable specifying the option
*.            ' '   list only Current directory
*.            'T'   list subdirectories tree as well
*.            'I'   option HINDEX selected instead of simple list
*.            'N'   only Ntuples are listed
*.            'R'   List using RZ format
*.            'S'   List directory in increasing order of IDs
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcunit.inc"
#include "hbook/hcdire.inc"
#include "hbook/hcmail.inc"
#include "hbook/hcpiaf.inc"
      DIMENSION IPAWC(99)
      EQUIVALENCE (NWPAW,IPAWC(1))
      COMMON/QUEST/IQUEST(100)
      CHARACTER*(*) CHPATH,CHOPT
      CHARACTER*128 CHWOLD
      DIMENSION LCUR(15),IOPT(5)
      EQUIVALENCE (IOPTT,IOPT(1)),(IOPTR,IOPT(2)),(IOPTN,IOPT(3))
      EQUIVALENCE (IOPTI,IOPT(4)),(IOPTS,IOPT(5))
      EXTERNAL HLDIRT
*.___________________________________________
      IF(CHPATH.EQ.'//')THEN
         DO 10 I=1,NCHTOP
            CHMAIL=CHTOP(I)//HFNAME(I)
            NCH=LENOCC(CHMAIL)
            WRITE(LOUT,1000)CHMAIL(1:NCH)
  10     CONTINUE
 1000    FORMAT(' //',A)
         GO TO 99
      ENDIF
      IF(LHBOOK.EQ.0)GO TO 99
      CALL HUOPTC (CHOPT,'TRNIS',IOPT)
*
*             Save Current directory
*
      CALL HPAFF(CHCDIR,NLCDIR,CHWOLD)
      LR2=LCDIR
*
*             Set Current Working directory to CHPATH
*
      CALL HCDIR(CHPATH,' ')
      IF (IQUEST(1) .NE. 0) GOTO 40
*
      IF(ICHTOP(ICDIR).NE.0)THEN
#if defined(CERNLIB_CZ)
         IF(ICHTOP(ICDIR).GT.1000 .AND.
     +      ICHLUN(ICDIR).NE.0)THEN
*
* remote file on Piaf server
*
            ISKIN =MOD(ICHTOP(ICDIR),10000)
            ISKOUT=ICHTOP(ICDIR)/10000
            CHSMPF='HLDIR:'//CHOPT
            CHSMPF(21:)=CHPATH
            CALL CZPUTA(CHSMPF,ISTAT)
            IF (ISTAT .NE. 0) GOTO 40
            CALL HLOGPF(' ',ISTAT)
            GOTO 40
         ENDIF
         IF(ICHTOP(ICDIR).GT.1000 .AND.
     +      ICHLUN(ICDIR).EQ.0)THEN
*
* remote file on PAWSERV (global section or shared memory)
*
            ISKIN =MOD(ICHTOP(ICDIR),10000)
            ISKOUT=ICHTOP(ICDIR)/10000
            CHSMPF='MESS :LI'
            CALL CZPUTA(CHSMPF,ISTAT)
            IF (ISTAT .NE. 0) GOTO 40
  15        CALL CZGETA(CHMAIL,ISTAT)
            IF (ISTAT .NE. 0) GOTO 40
            IF(CHMAIL(1:1).EQ.'0')GO TO 40
            NCH=LENOCC(CHMAIL)
            IF(NCH.GT.1)THEN
               WRITE(LOUT,'(A)')CHMAIL(2:NCH)
            ENDIF
            IF(CHMAIL(1:1).EQ.'2')GO TO 15
            GOTO 40
         ENDIF
#endif
#if defined(CERNLIB_HMMAP)
         IF(ICHTOP(ICDIR).LT.0)THEN
            IGOFF=-LOCF(LQ(1))-ICHTOP(ICDIR)
            CALL HPLISM(LQ(IGOFF+1))
            GO TO 99
         ENDIF
#endif
#if defined(CERNLIB_VAX)
         IF(ICHTOP(ICDIR).LT.0)THEN
            LOCQ=1-LOCF(IPAWC(1))-ICHTOP(ICDIR)
            CALL HPLISG(IPAWC(LOCQ))
            GO TO 99
         ENDIF
#endif
         IF(IOPTR.NE.0)THEN
            CALL HRZLD(' ',CHOPT)
         ELSE
            IQUEST(88)=IOPTS
            IQUEST(89)=IOPTN
            IF(IOPTT.EQ.0)THEN
               CALL HLDIRT(CHPATH)
            ELSE
               CALL RZSCAN(' ',HLDIRT)
            ENDIF
         ENDIF
         GO TO 40
      ENDIF
*
      NLPAT0=NLPAT
      LCUR(NLPAT)=LCDIR
*
*            Lists current level
*
      IF(IOPTS.NE.0)CALL ZSORTI(IHDIV,LIDS,-5)
      CALL HLDIR1(IOPTI,IOPTN,1)
*
*            Lists subdirectories
*
  20  NLPAT=NLPAT+1
      LCDIR=LQ(LCDIR-1)
  30  LCUR(NLPAT)=LCDIR
      IF(LCDIR.EQ.0)THEN
         NLPAT=NLPAT-1
         LCDIR=LCUR(NLPAT)
         IF(NLPAT.LE.NLPAT0)GO TO 40
         LCDIR=LQ(LCDIR)
         GO TO 30
      ENDIF
      CALL UHTOC(IQ(LCDIR+1),4,CHCDIR(NLPAT),16)
      LIDS=LQ(LCDIR-2)
      LTAB=LQ(LCDIR-3)
      IF(IOPTS.NE.0)CALL ZSORTI(IHDIV,LIDS,-5)
      CALL HLDIR1(IOPTI,IOPTN,IOPTT)
      GO TO 20
*
*          Restore Current Directory
*
  40  CALL HCDIR(CHWOLD,' ')
      LCDIR = LR2
      LIDS  = LQ(LCDIR-2)
      LTAB  = LQ(LCDIR-3)
      LBUFM = LQ(LCDIR-4)
      LTMPM = LQ(LCDIR-5)
*
  99  RETURN
      END

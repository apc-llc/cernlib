*
* $Id: hcdir.F,v 1.1.1.1 1996/01/16 17:07:33 mclareni Exp $
*
* $Log: hcdir.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:33  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/03 19/05/94  14.31.24  by  Rene Brun
*-- Author :
      SUBROUTINE HCDIR(CHPATH,CHOPT)
*.==========>
*.      To set,read or print the Current Working Directory
*. Input:
*.   *CHPATH* Character  variable  specifying  the  pathname  of  the  CWD
*.            (default option).
*.            The top level directory is //PAWC.  When  the pathname does
*.            not start with a '/', the pathname is prefixed with the path
*.            of the CWD.
*.   CHOPT    Character variable specifying the option
*.            'R'   Read the CWD pathname into CHPATH
*.            'P'   Print the CWD
*.            ' '   Set the CWD
*. Output:
*.   *CHPATH* Character variable  containing the complete pathname  of the
*.            current working directory (with 'R' option only).
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcunit.inc"
#include "hbook/hcdire.inc"
#include "hbook/hcmail.inc"
#include "hbook/hcpiaf.inc"
#include "hbook/czsock.inc"
*
      COMMON/QUEST/IQUEST(100)
*
      CHARACTER*2   NODIR
      PARAMETER    (NODIR = '@#')
      CHARACTER*128 CHAIN, CACHE
      DIMENSION     IOPTV(2),IHDIR(4)
      EQUIVALENCE  (IOPTR,IOPTV(1)), (IOPTP,IOPTV(2))
      CHARACTER*(*) CHPATH,CHOPT
*
      SAVE  CACHE
      DATA  CACHE  /NODIR/
*.___________________________________________
      IF(LHBOOK.EQ.0)GO TO 99
      CALL HUOPTC (CHOPT,'RP',IOPTV)
*
*             Read Working directory
*
      IF(IOPTR.NE.0)THEN
         CALL HPAFF(CHCDIR,NLCDIR,CHPATH)
         GO TO 99
      ENDIF
*
*             Print Working directory
*
      IF(IOPTP.NE.0)THEN
         CALL HPAFF(CHCDIR,NLCDIR,CHMAIL)
         WRITE(LOUT,1000)CHMAIL(1:90)
 1000 FORMAT(' Current Working Directory = ',A)
         GO TO 99
      ENDIF
*
*             Set Working directory
*
      IQUEST(1)=0
      IF(CHPATH(1:1).EQ.'.')THEN
         CALL HPATH(' ')
      ELSE
         CALL HPATH(CHPATH)
      ENDIF
*
      IF(NLPAT.LE.0)GO TO 99
*
      ICDOLD=ICDIR
      ICDIR=1
      DO 10 I=1,NCHTOP
         IF(CHPAT(1).EQ.CHTOP(I))THEN
            ICDIR=I
            IF(ICHTOP(I).GT.0)THEN
*
*-- 200<ICHTOP<300 for a chain referencing an already open unit
*
               IF (ICHTOP(I).GT.200 .AND. ICHTOP(I).LT.300) THEN
                  CALL HPAFF(CHPAT,NLPAT,CHMAIL)
                  CALL HCHNAM(CHMAIL, ICHTOP(I), CHAIN)
                  CALL HRZCD(CHAIN, CHOPT)
               ELSEIF (ICHTOP(ICDIR).GT.1000 .AND. CONNPF) THEN
*-- don't allow CD to server connection point when connected to Piaf
                  IF (ICHLUN(ICDIR) .EQ. 0) GOTO 10
#if defined(CERNLIB_CZ)
                  IF (CHPATH.EQ.CACHE.AND.INDEX(CHOPT,'R').EQ.0) GOTO 60
                  IF (CACHE.NE.NODIR .AND.INDEX(CHOPT,'R').NE.0) THEN
                     CHPATH = CACHE
                     GOTO 60
                  ENDIF
*
*--- remote file on Piaf server
*
                  ISKIN =MOD(ICHTOP(ICDIR),10000)
                  ISKOUT=ICHTOP(ICDIR)/10000
                  CHSMPF='HCDIR:'//CHOPT
                  CHSMPF(21:)=CHPATH
                  CALL CZPUTA(CHSMPF,IRC)
                  IF (IRC .NE. 0) THEN
                     CACHE = NODIR
                     GOTO 90
                  ENDIF
                  CALL HLOGPF(' ',IRC)
                  IF(IQUEST(1).EQ.0 .AND. INDEX(CHOPT,'R').NE.0) THEN
                     CALL CZGETA(CHPATH,IRC)
                  ENDIF
                  IF (IQUEST(1).EQ.0) THEN
                     CACHE = CHPATH
                  ELSE
                     CACHE = NODIR
                  ENDIF
#endif
#if !defined(CERNLIB_CZ)
                  CALL HBUG('CZ option not active','HCDIR',0)
                  IQUEST(1)=1
#endif
               ELSE
                  IF(CHPATH(1:1).EQ.'.')THEN
                     CALL HRZCD(' ',CHOPT)
                  ELSE
                     CALL HRZCD(CHPATH,CHOPT)
                  ENDIF
               ENDIF
               IF(IQUEST(1).NE.0)THEN
                  ICDIR=ICDOLD
                  GO TO 99
               ENDIF
               GO TO 60
            ELSEIF(ICHTOP(I).LT.0)THEN
               GO TO 60
            ENDIF
            GO TO 20
         ENDIF
  10  CONTINUE
      ICDIR=ICDOLD
      GO TO 90
*
  20  LR1 = LHBOOK
*
*          Search levels down
*
      IF(NLPAT.GT.1)THEN
         DO 50 IL=2,NLPAT
            CALL UCTOH(CHPAT(IL),IHDIR,4,16)
            LR1=LQ(LR1-1)
  30        IF(LR1.EQ.0)GO TO 90
            DO 40 I=1,4
               IF(IHDIR(I).NE.IQ(LR1+I))THEN
                  LR1=LQ(LR1)
                  GO TO 30
               ENDIF
  40        CONTINUE
  50     CONTINUE
      ENDIF
*
  60  NLCDIR= NLPAT
      DO 70 I=1,NLPAT
         CHCDIR(I)=CHPAT(I)
  70  CONTINUE
      IF(ICHTOP(ICDIR).EQ.0)THEN
         LCDIR = LR1
         LID   = 0
      ENDIF
      IDLAST= 0
      IDHOLD= 0
      LIDS  = LQ(LCDIR-2)
      LTAB  = LQ(LCDIR-3)
      LBUFM = LQ(LCDIR-4)
      LTMPM = LQ(LCDIR-5)
      IQUEST(1)=0
      GO TO 99
*
*          Error
*
  90  CALL HPAFF(CHPAT,NLPAT,CHMAIL)
      IQUEST(1)=1
      WRITE(LOUT,2000)CHMAIL(1:90)
 2000 FORMAT(' HCDIR. UNKNOWN DIRECTORY ',A)
*
  99  RETURN
      END

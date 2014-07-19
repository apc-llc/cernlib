*
* $Id: fmlfil.F,v 1.2 1996/04/02 22:42:57 thakulin Exp $
*
* $Log: fmlfil.F,v $
* Revision 1.2  1996/04/02 22:42:57  thakulin
* Workaround for an Apogee Fortran compiler bug.
*
* Revision 1.1.1.1  1996/03/07 15:18:12  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMLFIL(CHPATH,CHFILE,KEYS,NFOUND,MAXKEY,JCONT,IRC)
#include "fatmen/faust.inc"
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fmnkeys.inc"
      PARAMETER     (MAXDIR=500)
      PARAMETER     (NMAX=100)
      CHARACTER*20  FNAME
      DIMENSION     MYKEYS(LKEYFA,NMAX)
      DIMENSION     KEYS(LKEYFA,MAXKEY)
      CHARACTER*255 CHDIR(MAXDIR),PATHO
      CHARACTER*(*) CHFILE(MAXKEY)
      CHARACTER*20  CHPATT
      CHARACTER*(*) CHPATH
      SAVE          CHDIR,NDIRS,IFIRST,ILAST,NRET,ISTART

#if defined(CERNLIB_QFAPOGEE)
* workaround for a compiler bug in APOGEE F77 3.0/3.1 (12 Mar 96)
      INEG1  = -1
#endif 
      NFLFIL = NFLFIL + 1
 
      LCH    = LENOCC(CHPATH)
      LPA    = INDEXB(CHPATH(1:LCH),'/') - 1
      CHPATT = CHPATH(LPA+2:LCH)
      LCHP   = LENOCC(CHPATT)
      IRC    = 0
      NFOUND = 0
      NFILES = 0
      IF(IDEBFA.GE.2) PRINT *,'FMLFIL. enter for path/file = ',
     +   CHPATH(1:LPA),',',CHPATT(1:LCHP),' JCONT = ',JCONT
      CALL FACDIR(PATHO,'R')
      IF(JCONT.NE.0) GOTO 20
      ICONT  = 0
*
*     Are there any wild-cards in directory name?
*
      IWILD = ICFMUL('*%()<>',CHPATH,1,LPA)
      IF(IWILD.LE.LPA) THEN
         GOTO 10
      ELSE
         ISTART = 1
         NDIRS  = 1
         IFIRST = 1
         ILAST  = NMAX
         CHDIR(1) = CHPATH(1:LPA)
         GOTO 20
      ENDIF
*
*     Get list of file names
*
   10 CONTINUE
      CALL FMLDIR(CHPATH(1:LPA),
     +CHDIR,NDIRS,MAXDIR,ICONT,IRC)
      IF(IDEBFA.GE.2) PRINT *,'FMLFIL. ',NDIRS,' directories found'
      IF((IRC.NE.0).AND.(IRC.NE.-1)) PRINT *,'FMLFIL. return code ',
     +   IRC,' from FMLDIR'
      IF(IRC.EQ.-1) THEN
         ICONT = 1
      ELSE
         ICONT = 0
      ENDIF
 
      ISTART = 1
      IFIRST = 1
      ILAST  = NMAX
*
*     Branch here on re-entry on file names
*
   20 CONTINUE
      IF((IDEBFA.GE.2).AND.(JCONT.NE.0))
     +   PRINT *,'FMLFIL. - reenter for directory ',
     +   CHDIR(ISTART)
      JCONT  = 0
      DO 60 I=ISTART,NDIRS
*
*     Process next directory
*
         LEND = LENOCC(CHDIR(I))
         CALL FACDIR(CHDIR(I)(1:LEND),' ')
         IF(IQUEST(1).NE.0) THEN
            IF(IDEBFA.GE.-3) PRINT *,'FMLFIL. error ',IQUEST(1),
     +         ' setting directory ',CHDIR(I)(1:LEND)
            IRC = IQUEST(1)
            GOTO 70
         ENDIF
         IF(IDEBFA.GE.3) PRINT *,'FMLFIL. processing directory ',
     +      CHDIR(I)(1:LEND)
 
   30    CONTINUE
         CALL FMKEYS(LKEYFA,NMAX,IFIRST,ILAST,MYKEYS,NFILES,IRET)
         NKEYS = IQUEST(11)
         IF(IDEBFA.GE.2) PRINT *,'FMLFIL. ',NKEYS,' files found in ',
     +      CHDIR(I)(1:LEND)
         IF(IQUEST(1) .NE. 0) THEN
            IF(IDEBFA.GE.2) PRINT *,'FMLFIL. More than ',NMAX,' files '
     +      //'in ',CHDIR(I)(1:LEND)
            IF(IDEBFA.GE.2) PRINT *,'FMLFIL. IQUEST(11-12) = ',
     +      IQUEST(11),IQUEST(12)
         ENDIF
*
*     Process all keys returned and move those that match to KEYS
*
         NRET = IQUEST(13)
         IF(NRET.EQ.0) GOTO 60
   40    CONTINUE
         DO 50 J=1,NRET
            CALL UHTOC(MYKEYS(2,J),4,FNAME,(MKCLFA-MKFNFA)*4)
            LF = LENOCC(FNAME)
            CALL FMATCH(FNAME(1:LF),CHPATT(1:LCHP),IMAT)
            IF(IMAT.EQ.0) THEN
               IF(NFOUND.GE.MAXKEY) THEN
#if defined(CERNLIB_QFAPOGEE)
                  IRC    = INEG1
#else
                  IRC    = -1
#endif
                  JCONT  = 1
                  IFIRST = IFIRST + J -1
                  ISTART = I
                  IF(IDEBFA.GE.3) THEN
                     PRINT *,'FMLFIL. cannot accept any more files. ',
     +                       'Last file accepted:'
                     PRINT *,CHFILE(NFOUND)(1:LENOCC(CHFILE(NFOUND)))
                     PRINT *,'Current file/directory:'
                     PRINT *,FNAME(1:LF),' - ',CHDIR(I)(1:LEND)
                     PRINT *,'First file to be retrieved in next ',
     +                       'batch = ',IFIRST,' start directory = ',
     +                       ISTART
                  ENDIF
                  ILAST = MIN(NKEYS,IFIRST+NMAX-1)
                  GOTO 70
               ELSE
                  NFOUND = NFOUND + 1
                  CHFILE(NFOUND) = CHDIR(I)(1:LEND)//'/'//FNAME
                  CALL UCOPY(MYKEYS(1,J),KEYS(1,NFOUND),LKEYFA)
               ENDIF
            ENDIF
 
   50       CONTINUE
            IF(ILAST.LT.NKEYS) THEN
               IFIRST = IFIRST + NMAX
               ILAST  = MIN(NKEYS,ILAST+NMAX)
               GOTO 30
            ENDIF
*-
         IFIRST = 1
         ILAST  = NMAX
   60    CONTINUE
         IF(ICONT.NE.0) GOTO 10
 
   70    CONTINUE
         LPATHO = LENOCC(PATHO)
         CALL FACDIR(PATHO(1:LPATHO),' ')
         IF(IQUEST(1).NE.0) THEN
            IF(IDEBFA.GE.-3) PRINT *,'FMLFIL. error ',
     +         'resetting directory to ',PATHO(1:LPATHO)
         ENDIF
      END

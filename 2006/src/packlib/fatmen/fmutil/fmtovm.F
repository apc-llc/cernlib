*
* $Id: fmtovm.F,v 1.1.1.1 1996/03/07 15:18:18 mclareni Exp $
*
* $Log: fmtovm.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:18  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMTOVM(LUN,FNAME,IRC)
#include "fatmen/fatsys.inc"
#if defined(CERNLIB_VAXVMS)
*
*     Copy file on unit 'lun' to CERNVM
*
#include "fatmen/fatbank.inc"
      CHARACTER*(*) FNAME
      CHARACTER*80  CARD
      CHARACTER*128 FILEN
      INTEGER       SYSTEMF
      IF(TOPDIR(1:LENOCC(TOPDIR)).NE.'//CERN') RETURN
 
      LSN   = LENOCC(SERNAM)
      LFN   = LENOCC(FNAME)
      IRC   = 0
 
      IF(INDEX(FNAME(1:LFN),SERNAM(1:LSN)//'_'//'CERNVM').NE.0) THEN
         PRINT *,'FMTOVM. file ',FNAME(1:LFN),' originated at CERNVM'
         PRINT *,'        will not be sent back'
         RETURN
      ENDIF
 
      FILEN =
     +'CERNVM::"RDR:<'//SERNAM(1:LSN)//'>FATMEN.RDRFILE/CARD/CR"'
      IF(IDEBFA.GE.3) PRINT *,'FMTOVM. sending file ',
     +   ' to ',FILEN
      OPEN(LUN,FILE=FNAME(1:LFN),STATUS='OLD',FORM='FORMATTED',
     +     ACCESS='SEQUENTIAL')
      ISTAT = LIB$GET_LUN(LUNVM)
#include "fatmen/fatvaxrc.inc"
      OPEN(UNIT=LUNVM,NAME=FILEN(1:LENOCC(FILEN)),STATUS='NEW',ERR=98)
1     CONTINUE
      READ(LUN,'(A)',END=99) CARD
      WRITE(LUNVM,'(A)') CARD
      GOTO 1
98    IRC = 1
99    CLOSE (LUN)
      CLOSE (LUNVM)
      ISTAT = LIB$FREE_LUN(LUNVM)
#include "fatmen/fatvaxrc.inc"
      IF ((IRC.EQ.0) .AND.(IDEBFA.GE.-3))
     +   WRITE(LPRTFA,*) 'Sent file to ',SERNAM,' at CERNVM'
#endif
#if defined(CERNLIB_IBMVM)
      CHARACTER*(*) FNAME
      LSN   = LENOCC(SERNAM)
      CALL VMCMS('EXEC FAT2CERN '//SERNAM(1:LSN),IRC)
#endif
#if (defined(CERNLIB_UNIX))&&(defined(CERNLIB_CRAY))
      CHARACTER*255 COMAND
      CHARACTER*255 FNAME
      INTEGER       SYSTEMF
      COMAND = '/usr/bin/dispose '//FNAME(1:LENOCC(FNAME))//
     +         ' -dPU -mVA -t"fn=fatcray,ft=fzfile,tid='//
     +         SERNAM(1:LENOCC(SERNAM))//'"'
      ISTAT = SYSTEMF(COMAND(1:LENOCC(COMAND)))
      PRINT *,COMAND
*dispose <fz-file> -d PU -t'fn=fatcray,ft=fzfile,tid=<fmxxx>' -mVA
#endif
#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB_CRAY))
*
*     Just move the file to /tovm...
*
#include "fatmen/fatbug.inc"
      CHARACTER*(*) FNAME
      CHARACTER*255 FNIN,FNOUT
      CHARACTER*255 STR1,STR2
      INTEGER       SYSTEMF
 
      IRC    = 0
      CALL FMJOUR(FNOUT)
      LFNIN  = LENOCC(FNAME)
      LFNOUT = LENOCC(FNOUT)
      FNIN   = FNAME(1:LFNIN)
      LDEF   = LENOCC(DEFAULT)
      LSN    = LENOCC(SERNAM)
 
      STR1   = SERNAM(1:LSN)//'_CERNVM'
      STR2   = FNAME
      LSTR1  = LSN + 7
      LSTR2  = LFNIN
 
      CALL CUTOL(STR1(1:LSTR1))
      CALL CUTOL(STR2(1:LSTR2))
 
      IF(INDEX(STR2(1:LSTR2),STR1(1:LSTR1)).NE.0) THEN
         PRINT *,'FMTOVM. file ',FNAME(1:LFNIN),' originated at CERNVM'
         PRINT *,'        will not be sent back'
         RETURN
      ENDIF
*
*     Copy the file to the /tovm directory
*
      IF(IDEBFA.GE.0) PRINT *,'FMTOVM. executing ',
     +   'cp '//FNIN(1:LFNIN)//' '//DEFAULT(1:LDEF)//
     +       '/tovm/'//FNOUT(1:LFNOUT)
 
      IRC = SYSTEMF(
     +   'cp '//FNIN(1:LFNIN)//' '//DEFAULT(1:LDEF)//
     +       '/tovm/'//FNOUT(1:LFNOUT))
*
*     Now rename it
*
      IF(IDEBFA.GE.0) PRINT *,'FMTOVM. executing ',
     +              'mv '//DEFAULT(1:LDEF)//'/tovm/'//FNOUT(1:LFNOUT)
     +              //' '//DEFAULT(1:LDEF)//'/tovm/aa'//FNOUT(3:LFNOUT)
 
      IRC = SYSTEMF('mv '//DEFAULT(1:LDEF)//'/tovm/'//FNOUT(1:LFNOUT)
     +              //' '//DEFAULT(1:LDEF)//'/tovm/aa'//FNOUT(3:LFNOUT))
 
#endif
      END

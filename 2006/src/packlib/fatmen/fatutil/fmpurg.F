*
* $Id: fmpurg.F,v 1.1.1.1 1996/03/07 15:18:05 mclareni Exp $
*
* $Log: fmpurg.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:05  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMPURG(PATH,KEYSEL,MINSIZ,MINACC,MAXDYS,
     +                  MINCPS,LUNPUR,CHOPT,IRC)
*CMZ :          25/01/91  14.15.46  by  Jamie Shiers
*-- Author :    Jamie Shiers   25/01/91
*     Purge entries in PATH according to key selection in KEYSEL.
*     Files above escape deletion if their size is <= MINSIZ,
*     have been accessed at least MINACC times,
*     the last access being <= MAXDYS ago
*     and for which at least MINCPS copies exist.
*     Any check can be bypassed by coding a -1.
*
*     CHOPT: P - simply print the usage statistics on these files
*            K - write a FATMEN KUMAC on LUNPUR for these files
*            R - remove these entries
*
#include "fatmen/fmnkeys.inc"
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
      DIMENSION     KEYS(LKEYFA),KEYSEL(LKEYFA)
      DIMENSION     NDAYS(3)
      CHARACTER*4   CHOPP
      CHARACTER*3   CHSTAT
      CHARACTER*12  KUMAC
      LOGICAL       IEXIST
      CHARACTER*(*) PATH
*
*     Maximum # of directories that can be processed per pass
*
      PARAMETER     (NMAX=100)
*
*     Maximum # of files that can be processed per directory
*
      PARAMETER     (MAXFIL=1000)
      COMMON/FAPURG/PATHS,FILES
      CHARACTER*255 PATHS(NMAX),FILES(MAXFIL)
#include "fatmen/fatopts.inc"
      IRC   = 0
      ICONT = 0
      NPURG = 0
      LCH   = LENOCC(CHOPT)
      CALL FMCHOP('FMPURG',CHOPT(1:LCH),'KPR',IC)
*
*     Get list of directory names using FMLDIR...
*
    1 CONTINUE
      CALL FMLDIR(PATH,PATHS,NFOUND,NMAX,ICONT,IC)
      IF(IC.EQ.-1) THEN
         ICONT = 1
         IC    = 0
      ELSE
         ICONT = 0
      ENDIF
      IF(IC.NE.0) THEN
         IF(IDEBFA.GE.0) PRINT *,'FMPURG. return code ',IC,
     +   ' from FMLDIR'
         GOTO 99
      ENDIF
      DO 10 I=1,NFOUND
         LP = LENOCC(PATHS(I))
         CALL FMUNIQ(PATHS(I)(1:LP),KEYSEL,FILES,MAXFIL,NMATCH,' ',IC)
         IF(IC.NE.0) THEN
            IF(IDEBFA.GE.0) PRINT *,'FMPURG. return code ',IC, ' from '
     +      //'FMUNIQ'
            GOTO 10
         ENDIF
*
*     Process files in this directory
*
         CHOPP = 'H'
         DO 20 J=1,NMATCH
            LF = LENOCC(FILES(J))
            CALL FMGET(FILES(J)(1:LF),LBANK,KEYS,IRC)
            IF(IRC.NE.0) THEN
               PRINT *,'FMPURG. error in FMGET for ',FILES(J)(1:LF)
               GOTO 99
            ENDIF
            IF(IOPTP.NE.0) THEN
               CALL FMPUSE(FILES(J)(1:LF),LBANK,KEYS,CHOPP,IC)
               CHOPP = ' '
            ENDIF
            ISIZE = IQ(LBANK+KOFUFA+MFSZFA)
            IUSE = IQ(LBANK+KOFUFA+MUSCFA)
            IF((ISIZE.LE.MINSIZ).OR.(MINSIZ.EQ.-1)) THEN
               IF(IDEBFA.GE.3) PRINT *,'FMPURG. File ',FILES(J)(1:LF),
     +         ' passes file size cut. SIZE/CUT = ',ISIZE,'/',MINSIZ
               GOTO 30
            ELSE
               IF(IDEBFA.GE.3) PRINT *,'FMPURG. File ',FILES(J)(1:LF),
     +         ' fails file size cut. SIZE/CUT = ',ISIZE,'/',MINSIZ
            ENDIF
 
            IF((IUSE .GE.MINACC).OR.(MINACC.EQ.-1)) THEN
               IF(IDEBFA.GE.3) PRINT *,'FMPURG. File ',FILES(J)(1:LF),
     +         ' passes use count cut. NUSE/CUT = ',IUSE,'/',MINACC
               GOTO 30
            ELSE
               IF(IDEBFA.GE.3) PRINT *,'FMPURG. File ',FILES(J)(1:LF),
     +         ' fails use count cut. NUSE/CUT = ',IUSE,'/',MINACC
            ENDIF
            CALL FMDAYS(FILES(J)(1:LF),LBANK,KEYS,NDAYS,' ',IRC)
            IF(IRC.NE.0) THEN
               PRINT *,'FMPURG. error in FMDAYS for ',FILES(J)(1:LF)
               GOTO 99
            ENDIF
*     Has file ever been accessed?
            IF(NDAYS(3).LT.0) THEN
               IF(IDEBFA.GE.3) PRINT *,'FMPURG. File ',FILES(J)(1:LF),
     +         ' never used or last access date invalid - ',NDAYS(3)
               NDAYS(3) = 999
            ENDIF
            IF((NDAYS(3).LE.MAXDYS).OR.(MAXDYS.EQ.-1)) THEN
               IF(IDEBFA.GE.3) PRINT *,'FMPURG. File ',FILES(J)(1:LF),
     +         ' passes last access cut. NDAYS/CUT = ',NDAYS(3),'/',
     +         MAXDYS
               GOTO 30
            ELSE
               IF(IDEBFA.GE.3) PRINT *,'FMPURG. File ',FILES(J)(1:LF),
     +         ' fails last access cut. NDAYS/CUT = ',NDAYS(3),'/',
     +         MAXDYS
            ENDIF
            CALL FMEXST(FILES(J)(1:LF),NCOP)
            IF(NCOP.LE.MINCPS) THEN
               IF(IDEBFA.GE.3) PRINT *,'FMPURG. File ',FILES(J)(1:LF),
     +         ' passes min. # copies cut. NCOP/CUT = ',NCOP,'/',
     +         MINCPS
               GOTO 30
            ELSE
               IF(IDEBFA.GE.3) PRINT *,'FMPURG. File ',FILES(J)(1:LF),
     +         ' fails min. # copies cut. NCOP/CUT = ',NCOP,'/',MINCPS
            ENDIF
*
*     Candidate for removal...
*
            IF(IOPTK.NE.0) THEN
               IF(NPURG.EQ.0) THEN
                  KUMAC = 'FATPUR.KUMAC'
                  LMAC  = 12
#if defined(CERNLIB_UNIX)
         CALL CUTOL(KUMAC)
#endif
         INQUIRE(FILE=KUMAC(1:LMAC),EXIST=IEXIST)
         IF(IEXIST) THEN
            CHSTAT  = 'OLD'
         ELSE
            CHSTAT  = 'NEW'
         ENDIF
#if defined(CERNLIB_IBMVM)
         CALL CTRANS('.',' ',KUMAC,1,LMAC)
         OPEN(LUNPUR,FILE='/'//KUMAC(1:LMAC),ACCESS='SEQUENTIAL',
#endif
#if !defined(CERNLIB_IBMVM)
         OPEN(LUNPUR,FILE=KUMAC(1:LMAC),ACCESS='SEQUENTIAL',
#endif
     +      STATUS=CHSTAT,FORM='FORMATTED',IOSTAT=IRC)
                  IF(IRC.NE.0) THEN
                     PRINT *,'FMPURG. return code ',IRC,' from OPEN'
                     CALL MZDROP(IDIVFA,LBANK,' ')
                     LBANK = 0
                     GOTO 99
                  ENDIF
               ENDIF
               WRITE(LUNPUR,'(1X,A,A,I6)') 'rm ',FILES(J)(1:LF),
     +         KEYS(MKSRFA)
            ENDIF
            IF(IOPTR.NE.0) THEN
               CALL FMRM(FILES(J)(1:LF),LBANK,KEYS,IRC)
               IF(IRC.NE.0) THEN
                  PRINT *,'FMPURG. return code ',IRC,' from FMRM'
               ENDIF
            ENDIF
            NPURG = NPURG + 1
 
   30       CONTINUE
            CALL MZDROP(IDIVFA,LBANK,' ')
            LBANK = 0
   20    CONTINUE
   10 CONTINUE
      IF(ICONT.NE.0) GOTO 1
   99 CONTINUE
      IF(IOPTK.NE.0) CLOSE(LUNPUR)
      END

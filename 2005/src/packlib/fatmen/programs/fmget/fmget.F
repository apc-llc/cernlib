*
* $Id: fmget.F,v 1.1.1.1 1996/03/07 15:17:42 mclareni Exp $
*
* $Log: fmget.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:42  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      PROGRAM FATGET
*
*     Taken command lines arguments and issue FMGET
*
      CHARACTER*255 GENAM,CHFILE
      CHARACTER*36  CHOPT
      CHARACTER*8   CHKSN,CHCODE
      CHARACTER*8   CHHOST,CHFSEQ
      CHARACTER*6   CHVSN,CHVID
      CHARACTER*255 CHFQNA
      CHARACTER*8   CHLOGL
#include "fatmen/slate.inc"
#include "fatmen/fmnkeys.inc"
      DIMENSION     KEYS(LKEYFA)
#include "fatmen/fatout.inc"
#include "fatmen/tmsdef0.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fatbank.inc"
#include "fatmen/fatoptd.inc"
 
      NARGS = IARGC(DUMMY)
 
      IF(NARGS.EQ.0) THEN
         PRINT 9001
 9001 FORMAT(' FMGET. usage: FMGET GENAM [KSN] [CHOPT] [CHFILE]')
         GOTO 99
      ENDIF
 
      CALL GETARG(1,GENAM)
      LGN = LENOCC(GENAM)
      CALL CLTOU(GENAM(1:LGN))
 
      IF(NARGS.GE.2) THEN
         CALL GETARG(2,CHKSN)
         KSN = ICDECI(CHKSN,1,LENOCC(CHKSN))
      ELSE
         KSN = 0
      ENDIF
 
      IF(NARGS.GE.3) THEN
         CALL GETARG(3,CHOPT)
         CALL FMOPTC(CHOPT,ALFNUM,IOPT)
         LOPT  = LENOCC(CHOPT)
      ELSE
         CHOPT = ' '
         CALL VZERO(IOPT,36)
         LOPT  = 0
      ENDIF
 
      IF(LOPT.GT.0) CALL CLTOU(CHOPT(1:LOPT))
 
      IF(LOPT.EQ.1.AND.
     +   INDEX('ABCDEFGHIJKLMNOPQRSTUVWXYZ',CHOPT(1:LOPT)).EQ.0) THEN
         CHOPT = ' '
         CALL VZERO(IOPT,36)
         LOPT  = 0
      ENDIF
 
      IF(NARGS.GE.4) THEN
         CALL GETARG(4,CHFILE)
         LFILE = LENOCC(CHFILE)
      ELSE
         CHFILE = ' '
         LFILE  = 0
      ENDIF
*
*     Get FATMEN root and initialise
*
      LUNRZ = 1
      LUNFZ = 2
 
      LEND  = INDEX(GENAM(3:LGN),'/') + 3
      LEND  = LEND + INDEX(GENAM(LEND:LGN),'/') - 2
*
*     Get debug level
*
      CALL GETENVF('FMLOGL',CHLOGL)
      IF(IS(1).NE.0) THEN
         IDEBUG = ICDECI(CHLOGL,1,IS(1))
      ELSE
         IDEBUG = -3
      ENDIF
 
 
      CALL FMLOGL(IDEBUG)
      CALL FMSTRT(LUNRZ,LUNFZ,GENAM(1:LEND),IRC)
 
      IF(LFILE.GT.0) THEN
         CALL FAFILE(3,CHFILE(1:LFILE),IRC)
         OUTPUT = CHFILE(1:LFILE)
         LWRITE = 3
      ELSE
         OUTPUT = 'TTY'
         LWRITE = 6
      ENDIF
 
      LFAT  = 0
 
      KEYS(1) = KSN
 
      CALL FMGETA(GENAM(1:LGN),LFAT,KEYS,IRC)
 
      IF(IRC.EQ.0) THEN
         IF(LOPT.EQ.0) THEN
            IF(IRC.EQ.0) THEN
               IF(KEYS(MKMTFA).EQ.1) THEN
                  CHFQNA = ' '
                  CHHOST = ' '
                  CALL UHTOC(IQ(LFAT+KOFUFA+MFQNFA),4,CHFQNA,NFQNFA)
                  CALL UHTOC(IQ(LFAT+KOFUFA+MHSNFA),4,CHHOST,NHSNFA)
                  WRITE(LWRITE,9002) CHHOST(1:LENOCC(CHHOST)), CHFQNA(1
     +            :LENOCC(CHFQNA))
 9002 FORMAT(A,':',A)
               ELSE
                  CALL UHTOC(IQ(LFAT+KOFUFA+MVSNFA),4,CHVSN,NVSNFA)
                  CALL UHTOC(IQ(LFAT+KOFUFA+MVIDFA),4,CHVID,NVIDFA)
                  CALL FMITOC(IQ(LFAT+KOFUFA+MFSQFA),CHFSEQ,LFSEQ)
                  IQUEST(11) = KEYS(MKMTFA)
                  CALL FMQTMS(CHVID,LIB,MODEL,DENS,MNTTYP,LABTYP,IRC)
                  WRITE(LWRITE,9003) CHVSN(1:LENOCC(CHVSN)), CHFSEQ(1:
     +            LFSEQ), LABTYP(1:LENOCC(LABTYP)), CHVID(1:
     +            LENOCC(CHVID))
 9003 FORMAT(A,'.',A,'.',A,'.',A)
               ENDIF
            ENDIF
         ELSE
            CALL FMSHOW(GENAM(1:LGN),LFAT,KEYS,CHOPT,IRC)
         ENDIF
      ENDIF
 
      CALL FMEND(ISTAT)
 
      CALL FMITOC(IRC,CHCODE,LCODE)
#if defined(CERNLIB_UNIX)
      CALL FAEXIT(IRC)
#endif
#if defined(CERNLIB_IBMVM)
      CALL VMREXX('S','FATCODE',CHCODE(1:LCODE),ISTAT)
      IF(IRC.EQ.0) THEN
         STOP
      ELSE
         STOP 16
      ENDIF
#endif
#if defined(CERNLIB_VAXVMS)
      ISTAT = LIB$SET_SYMBOL('FATCODE',CHCODE(1:LCODE),
     +                       LIB$K_CLI_GLOBAL_SYM)
      IF(IRC.EQ.0) THEN
         STOP
         CALL SYS$EXIT(%VAL(1))
      ELSE
         CALL SYS$EXIT(%VAL(44))
      ENDIF
#endif
 
 
   99 END

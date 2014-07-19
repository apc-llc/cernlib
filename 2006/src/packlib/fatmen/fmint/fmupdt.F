*
* $Id: fmupdt.F,v 1.2 1996/10/29 10:08:22 jamie Exp $
*
* $Log: fmupdt.F,v $
* Revision 1.2  1996/10/29 10:08:22  jamie
* Protect against unit 0
*
* Revision 1.1.1.1  1996/03/07 15:18:11  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMUPDT(MAX,NBATCH,IFLAG,IRC)
*
*     Subroutine to control how updates are sent.
*     Defaults are MAX=999, NGROUP=0
*     IFLAG=-1 = reset to system defaults
*            0 = reset as per MAX and NGROUP sending outstanding updates
*           +1 = reset as per MAX and NGROUP purging outstanding updates
*
#include "fatmen/fatsys.inc"
#include "fatmen/fatupd.inc"
#include "fatmen/fatbank.inc"
      CHARACTER*255 CHFILE
      LOGICAL IOPEN
 
      MAXDEF = 999
      NGRDEF = 0
      IRC    = 0
 
      LUNFZ  = IABS(LUFZFA)
      IF(LUNFZ.EQ.0) THEN
         IOPEN = .FALSE.
      ELSE
         INQUIRE(LUNFZ,OPENED=IOPEN,NAME=CHFILE)
      ENDIF

      IF(IOPEN) LFILE = LENOCC(CHFILE)
#if defined(CERNLIB_IBM)
      ISTART = 2
#endif
#if !defined(CERNLIB_IBM)
      ISTART = 1
#endif
*
*     Branch according to IFLAG
*
      IF(IFLAG.LT.0) THEN
*
         MAXUPD = MAXDEF
         NGROUP = NGRDEF
#if defined(CERNLIB_IBMVM)
         IF(LUFZFA.GT.0.AND.FATNOD.EQ.' ') THEN
            IF(IOPEN) CLOSE(LUNFZ)
            CALL VMCMS('CP SPOOL PUNCH NOCONT CLOSE',IRC)
         ELSE
#endif
            IF(IOPEN) THEN
               CLOSE(LUNFZ)
               CALL FMSEND(CHFILE(ISTART:LFILE),IRC)
            ENDIF
#if defined(CERNLIB_IBMVM)
         ENDIF
#endif
         IF(IDEBFA .GE. 2) WRITE(LPRTFA,9001) MAXUPD,NGROUP
 9001 FORMAT(' FMUPDT. FATMEN updating parameters reset to defaults',/,
     +       ' FMUPDT. MAX = ',I10,' NGROUP = ',I10)
*
      ELSEIF(IFLAG.EQ.0) THEN
         MAXUPD = MAX
         NGROUP = NBATCH
#if defined(CERNLIB_IBMVM)
         IF(LUFZFA.GT.0.AND.FATNOD.EQ.' ') THEN
            IF(IOPEN) CLOSE(LUNFZ)
            CALL VMCMS('CP SPOOL PUNCH NOCONT CLOSE',IRC)
            IF(NGROUP.GT.0) CALL VMCMS('CP SPOOL PUNCH CONT',IRC)
         ELSE
#endif
            IF(IOPEN) THEN
               CLOSE(LUNFZ)
               CALL FMSEND(CHFILE(ISTART:LFILE),IRC)
            ENDIF
#if defined(CERNLIB_IBMVM)
         ENDIF
#endif
         IF(IDEBFA .GE. 2) WRITE(LPRTFA,9002) MAXUPD,NGROUP
 9002 FORMAT(' FMUPDT. FATMEN updating parameters reset as follows',/,
     +       ' FMUPDT. MAX = ',I10,' NGROUP = ',I10,/,
     +       ' FMUPDT. Any outstanding updates have been sent')
      ELSE
         MAXUPD = MAX
         NGROUP = NBATCH
#if defined(CERNLIB_IBMVM)
         IF(IOPEN) CLOSE(LUNFZ)
         IF(LUFZFA.GT.0.AND.FATNOD.EQ.' ') THEN
            CALL VMCMS('CP SPOOL PUNCH NOCONT PURGE',IRC)
            IF(NGROUP.GT.0) CALL VMCMS('CP SPOOL PUNCH CONT',IRC)
         ELSE
#endif
            IF(IOPEN) THEN
               CLOSE(LUNFZ,STATUS='DELETE')
               IF(IDEBFA.GE.1) PRINT *,'FMUPDT. purging update file ',
     +         CHFILE(ISTART:LFILE)
            ENDIF
#if defined(CERNLIB_IBMVM)
         ENDIF
#endif
         IF(IDEBFA .GE. 2) WRITE(LPRTFA,9003) MAXUPD,NGROUP
 9003 FORMAT(' FMUPDT. FATMEN updating parameters reset as follows',/,
     +       ' FMUPDT. MAX = ',I10,' NGROUP = ',I10,/,
     +       ' FMUPDT. Any outstanding updates have been purged')
 
      ENDIF
*
 
      END

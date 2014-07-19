*
* $Id: fmsend.F,v 1.1.1.1 1996/03/07 15:18:07 mclareni Exp $
*
* $Log: fmsend.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:07  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMSEND(FILEN,IRC)
      CHARACTER*(*) FILEN
      CHARACTER*255 CHFILE,CHDEST,CHTEMP
      CHARACTER*80  CHLINE
      INTEGER       SYSTEMF
#include "zebra/quest.inc"
#include "fatmen/fatsys.inc"
#include "fatmen/fatbug.inc"
*
*     Send file 'CHFILE' to server
*
      IRC    = 0
      LFILE  = LENOCC(FILEN)
      CHFILE = FILEN(1:LFILE)
      CALL FMJOUR(CHTEMP)
      LTEMP  = LENOCC(CHTEMP)
      LDEF   = LENOCC(DEFAULT)
 
      IF(FATNOD.EQ.' ') THEN
*
*     'Local' mode
*
#if defined(CERNLIB_VAXVMS)
*
*     Now rename the file
*
         CHDEST = DEFAULT(1:LDEF-1)//'.TODO'//DEFAULT(LDEF:LDEF)//
     +      'AA'//CHTEMP(3:LTEMP)
         LDEST  = LDEF + LTEMP + 5
         IF(IDEBFA.GE.1) THEN
            WRITE(LPRTFA,9005)
            WRITE(LPRTFA,9006) CHFILE(1:LFILE)
            WRITE(LPRTFA,9007) CHDEST(1:LDEST)
         ENDIF
 
         ISTAT = LIB$RENAME_FILE(CHFILE(1:LFILE),
     +                           CHDEST(1:LDEST),,,,,,,,,,)
#include "fatmen/fatvaxrc.inc"
 
#endif
#if defined(CERNLIB_UNIX)
*
*     Now rename the file
*
         CHDEST = DEFAULT(1:LDEF)//'/todo/aa'//CHTEMP(3:LTEMP)
         LDEST  = LDEF + LTEMP + 6
         IF(IDEBFA.GE.1) THEN
            WRITE(LPRTFA,9005)
            WRITE(LPRTFA,9006) CHFILE(1:LFILE)
            WRITE(LPRTFA,9007) CHDEST(1:LDEST)
         ENDIF
         IRC = SYSTEMF('/bin/mv '//CHFILE(1:LFILE)
     +      //' '//CHDEST(1:LDEST))
#endif
#if defined(CERNLIB_IBMVM)
         IF(LUFZFA.LT.0) THEN
*
*     Send temporary file
*
            IF(IDEBFA.GE.2) WRITE(LPRTFA,9001) CHFILE(1:LFILE),SERNAM
 9001 FORMAT(' FMSEND. issuing SENDFILE ',A,' to ',A)
            CALL VMCMS('EXEC SENDFILE '//CHFILE(1:LFILE)//
     +         ' TO '//SERNAM,IRC)
         ELSE
*
*     Spool file to server
*
            IF(IDEBFA.GE.2) WRITE(LPRTFA,9002)
 9002 FORMAT(' FMSEND. issuing CP SPOOL PUNCH CLOSE')
            CALL VMCMS('CP SPOOL PUNCH CLOSE',IRC)
 
         ENDIF
#endif
#if defined(CERNLIB_IBMMVS)
#endif
      ELSE
*
*     'CSPACK' mode
*
#if !defined(CERNLIB_CSPACK)
         IF(IDEBFA.GE.2) WRITE(LPRTFA,9003)
 9003 FORMAT(' FMSEND. error - CSPACK option has not been selected',
     +       ' at installation time.')
         IRC = 99
         GOTO 30
#endif
#if defined(CERNLIB_CSPACK)
*
*     Transfer update file
*
         CHDEST = DEFAULT(1:LDEF)//'/todo/'//CHTEMP(1:LTEMP)
         LDEST  = LDEF + LTEMP + 6
         IF(IDEBFA.GE.1) WRITE(LPRTFA,9004) CHFILE(1:LFILE),
     +      CHDEST(1:LDEST)
 9004 FORMAT(' FMSEND. transferring ',A,' to ',A)
*        CALL XZPUTA(CHFILE(1:LFILE),CHDEST(1:LDEST),' ',IRC)
*        IF(IRC.NE.0) GOTO 99
         LRECL  = 80
         CALL SZOPEN(LUFZFA,CHFILE(1:LFILE),LRECL,'F',IRC)
         CALL XZOPEN(LUFZFA,CHDEST(1:LDEST),FATNOD,LRECL,'NOF',IRC)
   10    CONTINUE
         READ(LUFZFA,'(A)',END=20) CHLINE
         LLINE = LENOCC(CHLINE)
         CALL XZPUTL(LUFZFA,CHLINE(1:LLINE),'(A)',' ',IRC)
         GOTO 10
   20    CALL XZCLOS(LUFZFA,' ',IRC)
         CLOSE(LUFZFA)
*
*     Rename update on remote node
*
         IF(IDEBFA.GE.1) THEN
            WRITE(LPRTFA,9005)
            WRITE(LPRTFA,9006) CHDEST(1:LDEST)
            WRITE(LPRTFA,9007) CHTEMP(1:LDEST)
         ENDIF
         CHFILE = DEFAULT(1:LDEF)//'/todo/aa'//CHTEMP(3:LTEMP)
         CALL XZMV(CHDEST(1:LDEST),CHFILE(1:LDEST),' ',IRC)
         IF(IRC.NE.0) GOTO 30
*
*     Remove local file
*
         CHFILE = FILEN(1:LFILE)
#endif
#if (defined(CERNLIB_IBMVM))&&(defined(CERNLIB_CSPACK))
         CHTEMP = CHFILE(1:LFILE)
         CALL CTRANS('.',' ',CHTEMP,1,LFILE)
         CALL VMCMS('ERASE '//CHTEMP(1:LFILE),IRC)
#endif
#if (defined(CERNLIB_IBMMVS))&&(defined(CERNLIB_CSPACK))
         OPEN(LUFZFA,FILE='/'//CHFILE(1:LFILE),ACTION='READWRITE',
     +        STATUS='OLD',FORM='FORMATTED')
         CLOSE(LUFZFA,STATUS='DELETE')
#endif
#if (defined(CERNLIB_VAXVMS))&&(defined(CERNLIB_CSPACK))
         ISTAT = LIB$DELETE_FILE(CHFILE(1:LFILE),,,,,,,,)
#include "fatmen/fatvaxrc.inc"
#endif
#if (defined(CERNLIB_UNIX))&&(defined(CERNLIB_CSPACK))
         IRC = SYSTEMF('rm '//CHFILE(1:LFILE))
#endif
      ENDIF
 
 9005 FORMAT(' FMSEND. renaming update file')
 9006 FORMAT(' FMSEND. old name = ',A)
 9007 FORMAT(' FMSEND. new name = ',A)
   30 CONTINUE
      IQUEST(1) = 0
      END

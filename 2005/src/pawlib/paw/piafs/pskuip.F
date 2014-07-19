*
* $Id: pskuip.F,v 1.3 1996/12/03 10:26:20 dinofm Exp $
*
* $Log: pskuip.F,v $
* Revision 1.3  1996/12/03 10:26:20  dinofm
* Remove a side effect when dealing with V 2.06 PAW client. The nt/plot
* command has one of the default parameter enclosed in quotes ('!').
* If the client is an old one the quotes are removed.
*
* Revision 1.2  1996/11/27 08:49:50  dinofm
* Whenever a NTUPLE command is received on the master, the subroutine
* TRIMFN is applied to strip path from filenames enclosed in double quotes.
*
* Revision 1.1.1.1  1996/03/01 11:39:27  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.04/13 26/02/94  18.57.38  by  Fons Rademakers
*-- Author :    Fons Rademakers   16/08/93
      SUBROUTINE PSKUIP(CHCMD,ISTAT)
************************************************************************
*                                                                      *
* Execute a Kuip command locally and/or send a kuip command, when      *
* on the master server, to the slave servers.                          *
* This is the server equivalent of the client PFKUIP routine.          *
*                                                                      *
************************************************************************
*
#include "hbook/hcdire.inc"
#include "hbook/hcpiaf.inc"
#include "paw/quest.inc"
#include "paw/pchost.inc"
*
      CHARACTER*(*) CHCMD
      CHARACTER*32  CHS
      INTEGER       IQSAVE(100)
      LOGICAL       NTCMD
      COMMON /PCCMD/NTCMD
*
      ISTAT = 0
      CHS   = CHCMD(:LEN(CHS))
*
      NTCMD = .FALSE.
      IF (CHS(:7).EQ.'nt/plot' .OR. CHS(:7).EQ.'nt/proj' .OR.
     +    CHS(:7).EQ.'nt/loop' .OR. CHS(:7).EQ.'nt/scan') NTCMD = .TRUE.
*
      IF (NTCMD) THEN
         CALL PSTIME(0, 0, 0, 0., 0.)
         CALL PNTLOG(5, 0, 0.)
*
*------- Remove path enclosed in double quotes
*
         IF ( MASTPF ) THEN
            CALL TRIMFN (CHCMD)
*---------- If the command comes from an old PAW client
            IF ((CHS(:7).EQ.'nt/plot').AND.ISPROTO.LT.2) THEN
*------------- If the command contains '!' remove the quotes
               CALL RMQUOTE (CHCMD)
            ENDIF
         ENDIF

      ENDIF
*
*-- On master in sequential mode or on slave
*
      IF ((MASTPF .AND. NSLAVE.EQ.0) .OR. SLAVPF) THEN
*
         IF (MASTPF .AND. NTCMD) CALL PSBLOG(CHCMD)
         CALL KUEXEC(CHCMD)
      ELSE
*
*-- Commands that should be executed on the slaves only
*
         IF (NTCMD) THEN
*
            CALL PSBLOG(CHCMD)
            CALL PSKUPN(CHCMD, NSLAVE, ISTAT)
*
*-- Commands that should be executed on both master and slave
*
         ELSEIF (CHS(:10) .EQ. 'histo/file'    .OR.
     +           CHS(:13) .EQ. 'fortran/close' .OR.
     +           CHS(:12) .EQ. 'histo/delete'  .OR.
     +           CHS(:7)  .EQ. 'nt/cuts'       .OR.
     +           CHS(:8)  .EQ. 'nt/chain'      .OR.
     +           CHS(:10) .EQ. 'nt/cselect'    .OR.
     +           CHS(:7)  .EQ. 'nt/vmem'       .OR.
     +           CHS(:3)  .EQ. 'cd ')              THEN
*
            CALL KUEXEC(CHCMD)
*
*-- Execute only on active slaves
*
            IF (CHS(:12) .EQ. 'histo/delete'  .OR.
     +          CHS(:7)  .EQ. 'nt/cuts')          THEN
               CALL PSKUPN(CHCMD, NSLAVE, ISTAT)
            ELSE
               CALL PSKUPN(CHCMD, NSLVPF, ISTAT)
            ENDIF
*
*-- Commands that should be executed on the master only
*
         ELSE
            CALL KUEXEC(CHCMD)
         ENDIF
      ENDIF
*
*-- Get Ntuple command statistics and save IQUEST
*
      IF (SLAVPF .AND. NTCMD) THEN
         CALL PNTLOG(2, 0, 0.)
         CALL PSTIME(2, 0, 0, RQUEST(97), RQUEST(98))
      ENDIF
      IF (ISTAT .NE. 0) IQUEST(1) = 1
      CALL UCOPY(IQUEST(1),IQSAVE(1),100)
*
*-- Send back to the client the created histograms and the log file
*
      CALL PSPUSH
*
      IF (NTCMD) THEN
         IF (LOGLPF .GE. 1) CALL PSTIME(1,NSLAVE,MYSID,0.,0.)
         IF (MASTPF)        CALL PSLOG
      ENDIF
*
      CALL PSPLOG(IQSAVE(1),ISTAT)
*
      END

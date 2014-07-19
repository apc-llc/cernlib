*
* $Id: pfcsex.F,v 1.1.1.1 1996/03/01 11:38:45 mclareni Exp $
*
* $Log: pfcsex.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:45  mclareni
* Paw
*
*
#include "paw/pilot.h"
#if (defined(CERNLIB_COMIS))&&(defined(CERNLIB_CZ))
*CMZ :  2.04/12 04/02/94  20.15.30  by  Fons Rademakers
*-- Author :    Alfred Nathaniel   14/04/93
      SUBROUTINE PFCSEX(LUNIN,CHFILE,IER)
      CHARACTER*(*) CHFILE
*
* Send the Fortran file to the Piaf server and Comis compile it there
*
#include "hbook/hcpiaf.inc"
#include "paw/pawlun.inc"
#include "paw/quest.inc"

      CHARACTER CHREMF*80,CHLOCF*16,CHLINE*80
      LOGICAL   ISF77

      IF (.NOT.CONNPF .OR. SERVPF) RETURN

      IER   = 0
      ISF77 = .FALSE.

      IF(CHFILE.EQ.' ') THEN
         IF(LUNIN.EQ.5) RETURN
*
* APPLication COMIS: copy the file from unit LUNIN to CHLOCF
*
         CHLOCF='APPCOMIS.TMP'
         CALL PALUNF(1,3,LUN)
         CALL KUOPEN(LUN,CHLOCF,'UNKNOWN',ISTAT)
         REWIND(LUNIN)
 1       CONTINUE
         CALL KUREAD(LUNIN,CHLINE,NCH)
         IF(NCH.GE.0) THEN
            CALL KUWRIT(LUN,CHLINE)
            GOTO 1
         ENDIF
         CALL KICLOS(LUN,' ',ISTAT)
         CHREMF=' '
         CALL PZPUTA(CHLOCF,CHREMF,0,ISTA)
         CALL KUOPEN(LUN,CHLOCF,'UNKNOWN',ISTAT)
         CALL KICLOS(LUN,'DELETE',ISTAT)
         LUNIT(LUN)=0
         IF (ISTA .NE. 0) THEN
            IER = 1
            RETURN
         ENDIF
      ELSE
         CHREMF = ' '
         LF = LENOCC(CHFILE)
         IF (CHFILE(LF-1:LF) .EQ. '77') THEN
            ISF77   = .TRUE.
            LF = LF - 2
         ENDIF
         CALL PZPUTA(CHFILE(:LF),CHREMF,0,ISTAT)
         IF (ISTAT .NE. 0) THEN
            IER = 1
            RETURN
         ENDIF
      ENDIF

      IF (ISF77) CHREMF = CHREMF(1:LENOCC(CHREMF))//'77'
      CALL CZPUTA('CSEXEC'//CHREMF,ISTAT)
      CALL PFLOOP(ISTAT)
      IF (IQUEST(1) .NE. 0) THEN
         IER = 2
         RETURN
      ENDIF

      END
#endif

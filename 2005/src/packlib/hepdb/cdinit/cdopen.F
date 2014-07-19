*
* $Id: cdopen.F,v 1.4 1996/03/11 09:53:25 jamie Exp $
*
* $Log: cdopen.F,v $
* Revision 1.4  1996/03/11 09:53:25  jamie
* opt R
*
* Revision 1.3  1996/03/11 09:09:01  jamie
* option R bug
*
* Revision 1.2  1996/03/04 10:16:11  jamie
* suppress r/o warning if opt R is specified
*
* Revision 1.1.1.1  1996/02/28 16:24:14  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDOPEN(LUNDB,LUNFZ,CHTOP,CHFILE,LRECL,IDIV,CHOPT,IRC)
 
      CHARACTER*(*) CHTOP,CHFILE,CHOPT
#if defined(CERNLIB_VAXVMS)
      CHARACTER*16  RECTYP
      LOGICAL       CDCIO
#endif
      CHARACTER*20  CHRZOP,CHCDOP
      CHARACTER*10  RZOPTS
      CHARACTER*15  CDOPTS
      PARAMETER     (NRZOPT=10)
      PARAMETER     (NCDOPT=15)
      DIMENSION     IOPTRZ(NRZOPT)
      DIMENSION     IOPTCD(NCDOPT)
#include "hepdb/cduscm.inc"
#include "hepdb/quest.inc"
 
      DATA RZOPTS/'LNSU1WYCXP'/
      DATA CDOPTS/'SU1LDCXMZQEPRTA'/
 
      IRC = 0
*
*     Build options for RZOPEN: X default, N means native, not NEW
*
      CALL UOPTC(CHOPT,RZOPTS,IOPTRZ)
 
      IOPTN = IOPTRZ(2)
      IF(IOPTN.EQ.0) IOPTRZ(9) = 1
*
*     Disallowed options, New, Update, 1=single user
*
      IOPTRZ(2) = 0
      IOPTRZ(4) = 0
      IOPTRZ(5) = 0
*
*     Compulsory options: Shared
*
      IOPTRZ(3) = 1
*
*     Check access rights
*
      CALL CDACL(LUNDB,CHTOP(3:4),IOPTRR,IOPTWW,' ',IRC)
      IF(IOPTRR+IOPTWW.EQ.0) THEN
         IF(IDEBCD.GE.0) WRITE(LPRTCD,9001) CHTOP
 9001 FORMAT(' CDOPEN. access to database ',A,' denied.')
         IRC = -11
         GOTO 999
      ENDIF
*
*     Read only?
*
      IF(IOPTWW.EQ.0) THEN
         IOPTCD(13) = 1
         IF(INDEX(CHOPT,'R').EQ.0.AND.IDEBCD.GE.0)
     +      WRITE(LPRTCD,9002) CHTOP
 9002 FORMAT(' CDOPEN. access to database ',A,' will be READONLY')
      ENDIF
 
      CHRZOP = ' '
      J      = 0
 
      DO 10 I=1,NRZOPT
         IF(IOPTRZ(I).NE.0) THEN
            J = J + 1
            CHRZOP(J:J) = RZOPTS(I:I)
         ENDIF
   10 CONTINUE
#if defined(CERNLIB_VAXVMS)
*
*     If the database file has record type STREAM_LF
*     use C I/O
*
      CDCIO = .FALSE.
      INQUIRE(FILE=CHFILE,RECORDTYPE=RECTYP)
      IF(INDEX(RECTYP,'STREAM_LF').NE.0) THEN
         J = J + 1
         CHRZOP(J:J) = 'C'
         CDCIO = .TRUE.
      ENDIF
#endif
*
*     Open the database file
*
      CALL RZOPEN(LUNDB,CHTOP,CHFILE,CHRZOP,LRECL,IRC)
      IF(IRC.NE.0) GOTO 999
*
*     Save IQUEST(10-11)
*
      IQ10 = IQUEST(10)
      IQ11 = IQUEST(11)
*
*     Build options for CDINIT
*
      CALL UOPTC(CHOPT,CDOPTS,IOPTCD)
      IF(IOPTN.EQ.0) IOPTCD(7) = 1
*
*     Public mode is compulsory
*
      IOPTCD(12) = 1
#if defined(CERNLIB_VAXVMS)
*
*     Signal C I/O
*
      IF(CDCIO) IOPTCD(6) = 1
#endif
 
      CHCDOP = ' '
      J      = 0
 
      DO 20 I=1,NCDOPT
         IF(IOPTCD(I).NE.0) THEN
            J = J + 1
            CHCDOP(J:J) = CDOPTS(I:I)
         ENDIF
   20 CONTINUE
*
*     Initialise HEPDB for this file
*
      IQUEST(10) = IQ10
      IQUEST(11) = IQ11
      CALL CDINIT(IDIV,LUNDB,LUNFZ,CHTOP,NPAIR,NRECS,NTOP,CHCDOP,IRC)
 
  999 END

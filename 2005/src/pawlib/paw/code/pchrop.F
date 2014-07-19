*
* $Id: pchrop.F,v 1.7 2004/08/24 08:18:27 couet Exp $
*
* $Log: pchrop.F,v $
* Revision 1.7  2004/08/24 08:18:27  couet
* - undo previous change. LENHFN is initialized in hbook/hcdire.inc
*
* Revision 1.5  2003/02/07 12:19:49  couet
* - When a file name is too long in a CHAIN a warning is printed. In the
*   previous version the length limit was 80. Now it is LENHFN.
*
* Revision 1.4  2000/01/13 10:09:41  couet
* - Print a warning when the file names lenght in a chain are too long.
*
* Revision 1.3  1998/03/13 15:46:59  couet
* - On VMS FORTRAN IO is used for files in a chain.
*
* Revision 1.2  1997/05/13 15:22:44  couet
* - use C/IO always
*
* Revision 1.1.1.1  1996/03/01 11:38:44  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.07/00 13/04/95  11.28.43  by  O.Couet
*-- Author :    Fons Rademakers   23/03/93
      SUBROUTINE PCHROP(CHAIN1, MEMBER, IER)
******************************************************************
*                                                                *
*  Open an HBOOK file MEMBER belonging to CHAIN1.                *
*  The top directory will be called //CHAIN1.                    *
*  If MEMBER=' ' then just reserve a LUN for a CHAIN but do not  *
*  connect any file.                                             *
*  In case of no free luns or too many open file IER=1, in case  *
*  member does not exist or can not be opened correctly IER=2    *
*  else IER=0.                                                   *
*                                                                *
******************************************************************
*
#include "hbook/hcdire.inc"
#include "hbook/hcmail.inc"
#include "hbook/czbuff.inc"
#include "hbook/hcpiaf.inc"
#include "paw/pawlun.inc"
#include "paw/pawchn.inc"
#include "paw/quest.inc"
*
      CHARACTER*(*) CHAIN1, MEMBER
      CHARACTER*80  FNAME, UCHAIN
      CHARACTER*8   CHOPT, TOPDIR
      LOGICAL       NOMEM, CHPIAF
*
      IER    = 0
      LRECL  = 0
#if defined(CERNLIB_VAXVMS)
      CHOPT = ' '
#endif
#if !defined(CERNLIB_VAXVMS)
      CHOPT = 'C'
#endif
*
      UCHAIN = MEMBER(:7)
      CALL CUTOL(UCHAIN)
      IF (UCHAIN .EQ. '//piaf/') THEN
         CHPIAF = .TRUE.
      ELSE
         CHPIAF = .FALSE.
      ENDIF
*
      LM = LENOCC(MEMBER)
      IF (LM .EQ. 0) THEN
         FNAME = 'Chain'
         NOMEM = .TRUE.
      ELSE
         IF (LM+10+LENOCC(CHAIN1).GT.LENHFN)THEN
            PRINT*, 
     +'*** WARNING: Long file names (>',LENHFN-10-LENOCC(CHAIN1),
     +' characters) in a chain may be misunderstood'
         ENDIF
         FNAME = 'Chain '//CHAIN1(1:LENOCC(CHAIN1))//' -- '//
     +           MEMBER(1:LM)
         NOMEM = .FALSE.
      ENDIF
*
*-- find if a LUN is already used for chains and use that LUN again
*
      DO 10 I = 1, NCHTOP
*
*-- if chain is already open just CD to it
*
         IF (FNAME .EQ. HFNAME(I)) THEN
            IF (NOMEM) GOTO 999
            IF (CHPIAF .AND. CHNXMT) GOTO 20
            CHMAIL='//'//CHTOP(I)
            CALL HCDIR(CHMAIL,' ')
            GOTO 999
         ENDIF
*
         IF (HFNAME(I)(1:5) .EQ. 'Chain') THEN
*
*-- Close the Hbook file and close the unit.
*-- Don't close when file was already open (in case of //LUN).
*
            IF (ICHTOP(I).GT.0 .AND. ICHTOP(I).LT.200) THEN
               CALL RZCLOS(CHTOP(I),' ')
            ENDIF
            GOTO 20
         ENDIF
  10  CONTINUE
*
      CALL PALUNF(1,3,LUNCHN)
      IF (LUNCHN .EQ. 0) GOTO 901
*
      IF (NCHTOP .GE. MXFILES) GOTO 901
      NCHTOP = NCHTOP + 1
      I = NCHTOP
      LUNIT(LUNCHN) = 3
*
  20  IF (NOMEM) THEN
         IF (CHTOP(I) .NE. ' ') THEN
            CHMAIL='//'//CHTOP(I)
            CALL HBFREEC(CHMAIL, LENOCC(CHMAIL))
         ENDIF
         CHTOP(I)  = ' '
         ICHTYP(I) = 0
         ICHTOP(I) = 0
         ICHLUN(I) = 0
         HFNAME(I) = FNAME
      ELSE
         UCHAIN = CHAIN1
         CALL CLTOU(UCHAIN)
#if defined(CERNLIB_CZ)
*
*-- RPC in case chain on PIAF.
*
         IF (CHPIAF) THEN
            IF (CONNPF) THEN
               L = 80+128
               CHBUF(1:L) = ' '
               WRITE(CHSMPF,'(A,I6)') 'PCHROP', L
               WRITE(CHBUF(1:L),'(A,A)') CHAIN1(1:80), MEMBER(8:LM)
               CALL CZPUTA(CHSMPF, ISTAT)
               IF (ISTAT .NE. 0) GOTO 900
               CALL CZPUTC(L, ISTAT)
               IF (ISTAT .NE. 0) GOTO 900
*
               CALL PFLOOP(ISTAT)
               IF (ISTAT .NE. 0) GOTO 900
               IF (IQUEST(1) .NE. 0) GOTO 900
*
               ICHTYP(I) = 0
               ICHTOP(I) = CLINPF+10000*CLUTPF
               ICHLUN(I) = LUNCHN
               GOTO 40
            ELSE
               PRINT *,'No connection to Piaf server'
               GOTO 900
            ENDIF
         ENDIF
#endif
*
*-- is the chain member a file or a LUN?
*
         IF (MEMBER(1:2) .EQ. '//') THEN
            TOPDIR = MEMBER(3:)
            CALL CLTOU(TOPDIR)
*
*-- if it is a LUN find it
*
            DO 30 J = 1, NCHTOP
               IF (CHTOP(J) .EQ. TOPDIR) THEN
                  IF (CHTOP(J)(1:3) .EQ. 'LUN') THEN
                     IF (ICHTOP(J) .GT. 1000) THEN
*-- remote file on PIAF
                        ICHTOP(I) = ICHLUN(J) + 200
                     ELSE
                        ICHTOP(I) = ICHTOP(J) + 200
                     ENDIF
                  ENDIF
                  ICHTYP(I) = ICHTYP(J)
                  GOTO 40
               ENDIF
   30       CONTINUE
            GOTO 900
         ELSE
*
*-- if it is a file open it
*
            CHMAIL = MEMBER
            CALL KUHOME(CHMAIL,LM)
            CALL RZOPEN(LUNCHN,TOPDIR,CHMAIL,'PX'//CHOPT,LRECL,ISTAT)
            IF (ISTAT.NE.0) GOTO 900
*
            CALL RZFILE(LUNCHN,UCHAIN,CHOPT)
            IF (IQUEST(1) .EQ. 2) IQUEST(1) = 0
            IF (IQUEST(1) .NE. 0) GOTO 902
            ICHTOP(I) = LUNCHN
            ICHLUN(I) = 0
            ICHTYP(I) = IQUEST(8)
         ENDIF
*
  40     CHTOP(I)  = UCHAIN
         HFNAME(I) = FNAME
         CHMAIL='//'//CHTOP(I)
         CALL HCDIR(CHMAIL,' ')
      ENDIF
*
      GOTO 999
*
  900 CHTOP(I)  = ' '
      ICHTYP(I) = 0
      ICHTOP(I) = 0
      ICHLUN(I) = 0
      HFNAME(I) = 'Chain'
      CALL HCDIR('//PAWC',' ')
      IQUEST(1) = 1
      GOTO 902
*
  901 IER = 1
      RETURN
*
  902 IER = 2
      RETURN
*
  999 END

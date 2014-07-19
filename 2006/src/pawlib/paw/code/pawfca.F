*
* $Id: pawfca.F,v 1.5 1997/05/20 09:37:58 couet Exp $
*
* $Log: pawfca.F,v $
* Revision 1.5  1997/05/20 09:37:58  couet
* *** empty log message ***
*
* Revision 1.4  1996/09/09 14:51:48  couet
* - old qp code removed
*
* Revision 1.3  1996/05/28 09:19:40  couet
* - Disable the last fix (some side effects)
*
* Revision 1.2  1996/05/24 12:38:23  couet
* - Dealocate the adress of the previous comis routine used
*
* Revision 1.1.1.1  1996/03/01 11:38:43  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.06/21 03/04/95  14.16.29  by  O.Couet
*-- Author :    Rene Brun   03/01/89
      SUBROUTINE PAWFCA(CHFUNC1,NCH1,JAD,IER)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *        To analize character string CHFUNC of length NCH        *
C.    *        CHFUNC may be the name of a COMIS function              *
C.    *        or a file name. A file cannot contain one of            *
C.    *        the following characters: +*=-()<>                      *
C.    *                                                                *
C.    ******************************************************************
#include "paw/pawcfu.inc"
#include "hbook/hcbits.inc"
#include "paw/quest.inc"
#include "paw/pcslas.inc"
#include "hbook/hcpiaf.inc"
      CHARACTER*(*) CHFUNC1
      CHARACTER*12 CHPID
      CHARACTER*128 CHFILE, CHFUNC, TFILE
      INTEGER CSADDR
C.
C.    ------------------------------------------------------------------
C.
#if defined(CERNLIB_COMIS)
*      IF(JADF.NE.0)CALL CSDPRO(JADF)
#endif
      JADF=0
      IQUEST(1)=0
#if defined(CERNLIB_COMIS)
      CALL PAWCS
      CHFUNC=CHFUNC1
* If CHFUNC is real or integer it is not a file name and JADF=0 is returned
      CALL KICTON(CHFUNC,IVAL,RVAL)
      IF (IQUEST(1).EQ.0) GOTO 99
*
      NCH=NCH1
      IF(NCH.LE.1.AND.CHFUNC(1:1).EQ.'0')GO TO 50
      IF(NCH.LE.0)GO TO 99
      IFILE=0
      TFILE=' '
      IF(INDEX(CHFUNC,']').EQ.0.AND.INDEX(CHFUNC,':').EQ.0)THEN
         IF(INDEX(CHFUNC,'+').NE.0.OR.
     +      INDEX(CHFUNC,'*').NE.0.OR.
     +      INDEX(CHFUNC,'=').NE.0.OR.
     +      INDEX(CHFUNC,'-').NE.0.OR.
     +      INDEX(CHFUNC,'(').NE.0.OR.
     +      INDEX(CHFUNC,')').NE.0.OR.
     +      INDEX(CHFUNC,'<').NE.0.OR.
     +      INDEX(CHFUNC,'>').NE.0)GOTO 20
         ENDIF
      DO 10 I=1,NCH-1
         IF(CHFUNC(I:I).EQ.'.')THEN
#endif
#if (defined(CERNLIB_COMIS))&&(defined(CERNLIB_IBMMVS))
            IF(IFILE.EQ.0)IFILE=I-1
#endif
#if (defined(CERNLIB_COMIS))&&(defined(CERNLIB_IBM))&&(!defined(CERNLIB_IBMMVS))
            CHFUNC(I:I)=' '
            IF(IFILE.EQ.0)IFILE=I-1
#endif
#if (defined(CERNLIB_COMIS))&&(!defined(CERNLIB_IBM))
            IFILE=I-1
#endif
#if defined(CERNLIB_COMIS)
         ENDIF
  10  CONTINUE
  20  IF(IFILE.NE.0)THEN
* Filename must be lower case, to change this remove next statement, correct
* the check for .f below, fix CSEXEC to not case convert and fix PZXFER
* (called via PFCSEX) to not case convert.
CCC         CALL CUTOL(CHFUNC)
         OLDNCH = NCH
         CALL KUHOME(CHFUNC,NCH)
         IFILE = IFILE + NCH-OLDNCH
         IF (SERVPF) THEN
            IF (CHFUNC(NCH-1:NCH) .EQ. '77') THEN
               CALL HITOC(MASPID, CHPID, LP, IERROR)
               I = INDEXB(CHFUNC(:NCH), '.f')
               TFILE = CHFUNC(:I-1)//'_'//CHPID(1:LP)//'.sl'
            ELSEIF (CHFUNC(NCH-1:NCH) .EQ. '.c') THEN
               CALL HITOC(MASPID, CHPID, LP, IERROR)
               I = INDEXB(CHFUNC(:NCH), '.c')
               TFILE = CHFUNC(:I-1)//'_'//CHPID(1:LP)//'.csl'
            ELSE
               TFILE = CHFUNC(1:NCH)
            ENDIF
         ELSE
            TFILE = CHFUNC(1:NCH)
         ENDIF
         CHFILE='!FILE '//TFILE
         CALL CSEXEC(CHFILE,IRET)
#endif
#if (defined(CERNLIB_COMIS))&&(defined(CERNLIB_CZ))
*
* Send the file to the Piaf server and compile it there as well
*
         IF (IRET.EQ.0) THEN
            CALL PFCSEX(0,CHFUNC(1:NCH),IRET)
         ENDIF
#endif
#if defined(CERNLIB_COMIS)
      ENDIF
*
      DO 30 I=IFILE,1,-1
         IF(CHFUNC(I:I).EQ.'/'.OR.
     +      CHFUNC(I:I).EQ.BSLASH.OR.
     +      CHFUNC(I:I).EQ.']'.OR.
     +      CHFUNC(I:I).EQ.':'.OR.
     +      CHFUNC(I:I).EQ.'~')THEN
               J1=I+1
               GO TO 40
         ENDIF
  30  CONTINUE
      J1=1
  40  IF (IFILE .EQ. 0) THEN
         CHFILE = CHFUNC
      ELSE
         CHFILE = CHFUNC(J1:IFILE)
         CHFUNC = CHFILE
         NCH = IFILE-J1+1
      ENDIF
*
* csaddr takes UPPERcase letters
*
      CALL CLTOU(CHFILE)
      JADF=CSADDR(CHFILE)
*
*-- Find the Ntuple variable used in the COMIS routine. These variables
*-- need to be read by the Ntuple read routines.
*
      IF (JADF.NE.0 .AND. I4.NE.0) THEN
         CALL PFINDV(TFILE, JADF, IERROR)
         IF (IERROR .NE. 0) THEN
            IF (IFILE .NE. 0) THEN
               PRINT *, 'PAWFCA: Error analyzing variables in file ',
     +                  TFILE(1:LENOCC(TFILE))
            ELSE
               PRINT *, 'PAWFCA: Error analyzing variables in file ',
     +                  CHFILE(1:LENOCC(CHFILE))
            ENDIF
         ENDIF
      ENDIF
#endif
  50  JAD=JADF
      IF(JADF.EQ.0.AND.IER.NE.0)THEN
         IQUEST(1)=1
         CALL KUALFA
         LCH=LENOCC(CHFUNC)
         PRINT 1000,CHFUNC(1:LCH)
 1000    FORMAT(' *** Unknown routine or function ---> ',A)
      ENDIF
*
  99  END

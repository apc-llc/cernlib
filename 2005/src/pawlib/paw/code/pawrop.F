*
* $Id: pawrop.F,v 1.5 1998/03/18 11:15:51 couet Exp $
*
* $Log: pawrop.F,v $
* Revision 1.5  1998/03/18 11:15:51  couet
* - fortran i/o used for VAX/VMS
*
* Revision 1.4  1997/05/13 15:22:12  couet
* - use C/IO always
*
* Revision 1.3  1997/01/24 17:01:22  gunter
* allow to use C I/O on OSF, and pass on eXchange flag, if detected by rzopen
*
* Revision 1.2  1996/08/28 09:58:55  couet
* - Mods in the way C option is appended in the RZOPEN CHOPT
*
* Revision 1.1.1.1  1996/03/01 11:38:43  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.05/19 23/09/94  17.44.03  by  Fons Rademakers
*-- Author :    Rene Brun   03/01/89
      SUBROUTINE PAWROP(CASE)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *        OPEN A RZ FILE INTERACTIVELY                            *
C.    *        IF(CASE='HIGZ' ) Open a HIGZ file                       *
C.    *        IF(CASE='HBOOK') Open a HBOOK file                      *
C.    *                        a RZ file otherwise                     *
C.    *                                                                *
C.    ******************************************************************
*
#include "hbook/hcdire.inc"
#include "hbook/hcmail.inc"
#include "paw/pawlun.inc"
#include "paw/pcchar.inc"
#include "hbook/hcpiaf.inc"
#include "paw/quest.inc"
*
      CHARACTER*(*) CASE
      CHARACTER*8   CHOPT,TOPDIR
      CHARACTER*80  CHTITP
C.
C.    ------------------------------------------------------------------
C.
      IQUEST(1)=0
      NCHT=NCHTOP
      CALL KUGETI(LUN)
      IF (LUN .EQ. 0) THEN
         CALL PALUNF(1,3,LUN)
         IF (LUN .EQ. 0) GOTO 999
      ELSE
         CALL PALUNF(LUN,1,IFREE)
         IF(IFREE.NE.0)GO TO 999
      ENDIF
      CALL KUGETF(CHTITL,NCHN)
      CHTITP = CHTITL
      CALL KUHOME(CHTITL,NCHN)
      CALL KUGETI(LRECL)
      IF(CASE.NE.'RZMAKE')THEN
         CALL KUGETC(CHOPT,NCH)
      ELSE
         CHOPT='N'
      ENDIF
*
      IOPTC = INDEX(CHOPT,'C')
*
      DO 10 I=1,NCHTOP
         IF(CHTITL.EQ.HFNAME(I) .OR. CHTITP.EQ.HFNAME(I))THEN
            CALL HBUG('File already connected','PAWROP',0)
            IQUEST(1)=1
            GO TO 999
         ENDIF
  10  CONTINUE
*
#if defined(CERNLIB_CZ)
      CHSMPF=CHTITP(:7)
      CALL CUTOL(CHSMPF)
*
*-- allow absolute pathnames in PIAF. Since KUIP removes the // (concatenation
*-- sequence), we have to put the // back. The original
*--     //piaf//nfs/piaf1/data3/...
*-- becomes:
*--     //piafnfs/piaf1/data3/...
*
      IF (CHSMPF(1:6).EQ.'//piaf' .AND. CHSMPF(7:7).NE.'/') THEN
         CHSMPF = CHTITP(1:6)//'//'//CHTITP(7:)
         CHTITP = CHSMPF
         CHSMPF = CHTITP(:7)
         CALL CUTOL(CHSMPF)
      ENDIF
      IF(CASE.EQ.'HBOOK' .AND. CHSMPF.EQ.'//piaf/') THEN
         IF (INDEX(CHOPT,'U').NE.0 .OR. INDEX(CHOPT,'N').NE.0) THEN
            CALL HBUG('Piaf server allows only read access','PAWROP',0)
            IQUEST(1)=1
            GOTO 999
         ENDIF
*
*-- check if Piaf connection is already established, if not try to connect
*
         IF (.NOT.CONNPF) THEN
            CALL KUEXEC('connect piaf')
            IF (.NOT. CONNPF) THEN
               CALL HBUG('Could not connect to Piaf server','PAWROP',0)
               IQUEST(1)=1
               GOTO 999
            ENDIF
         ENDIF
*--- send command removing //piaf/ from file name
         WRITE(CHLMPF,'(A,I4,1X,A,1X,I8,1X,A)') 'histo/file',
     +             LUN,CHTITP(8:LENOCC(CHTITP)),LRECL,'-X'
         CALL PFKUIP(CHLMPF,ISTAT)
         IF(ISTAT.NE.0) THEN
            CALL HBUG('Cannot open file on Piaf server','PAWROP',0)
            IQUEST(1)=1
            GOTO 999
         ENDIF
*--- make it look like a local //LUNnn
         IF(LUN.LT.10) THEN
            WRITE(TOPDIR,'(A,I1)') 'LUN',LUN
         ELSE
            WRITE(TOPDIR,'(A,I2)') 'LUN',LUN
         ENDIF
*--- put file information into Hbook tables
         NCHTOP=NCHTOP+1
         CHTOP(NCHTOP)=TOPDIR
         ICHTOP(NCHTOP)=CLINPF+10000*CLUTPF
         ICHLUN(NCHTOP)=LUN
         HFNAME(NCHTOP)=CHTITP
         LUNIT(LUN)=3
         CHMAIL='//'//TOPDIR
         CALL HCDIR(CHMAIL,' ')
         GOTO 999
      ENDIF
#endif
* C/IO is the default. Option F set FORTRAN IO.
#if !defined(CERNLIB_VAXVMS)
      IC = MIN(LENOCC(CHOPT)+1,8)
      CHOPT(IC:IC) = 'C'
      IOPTC = 1
#endif
#if defined(CERNLIB_VAXVMS)
      IOPTC = 0
#endif
*
      CALL RZOPEN(LUN,TOPDIR,CHTITL,'WP'//CHOPT,LRECL,ISTAT)
*
      IF (IQUEST(12).NE.0 ) THEN
*          file is in exchange mode, this to hrfile/rzfile
         IC = LENOCC(CHOPT)+1
         CHOPT(IC:IC) = 'X'
      ENDIF
*
*-- bug in RZOPEN with C I/O, TOPDIR is not returned correctly
*
      IF (IOPTC .NE. 0) THEN
         IF (LUN .LT. 10) WRITE(TOPDIR,3000) LUN
         IF (LUN .GE. 10) WRITE(TOPDIR,4000) LUN
 3000    FORMAT('LUN', I1)
 4000    FORMAT('LUN', I2)
      ENDIF
*
  20  IF(ISTAT.NE.0)THEN
         CALL HBUG('Cannot open file','PAWROP',0)
         IQUEST(1)=1
         GO TO 999
      ENDIF
*
      IF(CASE.EQ.'HIGZ')THEN
         IF(NCHTOP.GE.MXFILES)THEN
            CALL HBUG('Too many open files','PAWROP',LUN)
            GOTO 999
         ENDIF
         NCHTOP=NCHTOP+1
         CHTOP(NCHTOP)=TOPDIR
         ICHTOP(NCHTOP)=LUN
         ICHLUN(NCHTOP)=0
         CALL IZFILE(LUN,TOPDIR,CHOPT)
         CHMAIL='//'//CHTOP(NCHTOP)
         CALL HCDIR(CHMAIL,' ')
         LUNIT(LUN)=4
         HFNAME(NCHTOP)=CHTITL
      ELSEIF(CASE.EQ.'HBOOK')THEN
         IF (IOPTC.NE.0) IQUEST(99) = IQUEST(10)
         CALL HRFILE(LUN,TOPDIR,CHOPT)
         IF(IQUEST(1).NE.0)THEN
            ISTAT=IQUEST(1)
            CALL RZCLOS(TOPDIR,' ')
            GO TO 20
         ENDIF
         IF (NCHT.NE.NCHTOP) THEN
            LUNIT(LUN)=3
            IF (SERVPF) THEN
               HFNAME(NCHTOP) = CHTITP
            ELSE
               HFNAME(NCHTOP) = CHTITL
            ENDIF
         ENDIF
      ELSE
         IF(NCHTOP.GE.MXFILES)THEN
            CALL HBUG('Too many open files','PAWROP',LUN)
            GOTO 999
         ENDIF
         NCHTOP=NCHTOP+1
         CHTOP(NCHTOP)=TOPDIR
         ICHTOP(NCHTOP)=LUN
         ICHLUN(NCHTOP)=0
         IF(CASE.NE.'RZMAKE')CALL RZFILE(LUN,TOPDIR,CHOPT)
         LUNIT(LUN)=5
         HFNAME(NCHTOP)=CHTITL
      ENDIF
*
  999 END

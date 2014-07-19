*
* $Id: pahio.F,v 1.3 2003/06/04 15:00:11 couet Exp $
*
* $Log: pahio.F,v $
* Revision 1.3  2003/06/04 15:00:11  couet
* - New COMMON block MMPSHR in order to share the variable IGOFF with the
*   HBOOK routine HLIMAP. Without this, the reset of the histograms (via
*   GRESET) in shared memory, does not work.
*
* Revision 1.2  2001/10/02 15:01:56  couet
* - HLIMAP code was duplicated in this routine. HLIMAP is now call instead.
*
* Revision 1.1.1.1  1996/03/01 11:38:40  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.07/04 28/06/95  10.17.43  by  O.Couet
*-- Author :    Rene Brun   03/01/89
      SUBROUTINE PAHIO
*
*            /HISTOGRAM/IO
*           I/O Operations
*
#include "hbook/hcbook.inc"
#include "hbook/hcdire.inc"
#include "paw/pawcom.inc"
#include "paw/pawlun.inc"
#include "paw/pcpatl.inc"
#include "paw/pcchar.inc"
#if defined(CERNLIB_VAX)
      CHARACTER*64 CHGLOB
      DIMENSION IPAWC(99)
      EQUIVALENCE (NWPAW,IPAWC(1))
#endif
#if defined(CERNLIB_MMAP)
      CHARACTER*64 CHGLOB
      SAVE CHGLOB
#endif
      CHARACTER*8 CHOPT
#if defined(CERNLIB_IBM)
      CHARACTER*3  CHLUN
#endif
#if defined(CERNLIB_IBMMVS)
      CHARACTER*20   PREFIX
      CHARACTER*80   MVSFIL
      CHARACTER*4    IUNIT, ITRK, IRECFM
      CHARACTER*9    IACTIO
      LOGICAL*4      EXS, OPN

      INTEGER*4      ISPACE(3), IDCB(2)

      DATA           ISPACE / 10, 10, 0 /
      DATA           IDCB   / 0, 3600 /
      DATA           ITRK   / 'TRK' /, IRECFM / 'VBS' /
#endif
#if (defined(CERNLIB_IBMMVS))&&(!defined(CERNLIB_NEWLIB))
      DATA           IUNIT  / 'HSM' /
#endif
#if defined(CERNLIB_NEWLIB)
      DATA           IUNIT  / 'FAST' /
#endif
#if defined(CERNLIB_VAX)
      INTEGER*4 HMAPG,HFREEG
      DATA IGSIZE,IGOFF/0,0/
#endif
#if defined(CERNLIB_MMAP)
      INTEGER*4 HMAPM,HFREEM
	 COMMON /MMPSHR/ IGSIZE,IGOFF
#endif
*
      CALL KUBROF
      CALL KUPATL(CHPATL,NPAR)
*
      IF(CHPATL.EQ.'HRIN')THEN
         CALL PAGETI(ID)
         CALL KUGETI(ICYCLE)
         CALL KUGETI(JOFSET)
         CALL HRIN(ID,ICYCLE,JOFSET)
         JOFSET=0
         GO TO 90
      ENDIF
*
      IF(CHPATL.EQ.'HROUT')THEN
         CALL PAGETI(ID)
         CALL KUGETC(CHOPT,NCH)
         CALL HROUT(ID,ICYCLE,CHOPT)
         GO TO 90
      ENDIF
*
      IF(CHPATL.EQ.'HSCRATCH')THEN
         CALL PAGETI(ID)
         CALL HSCR(ID,99,' ')
         GO TO 90
      ENDIF
*
      IF(CHPATL.EQ.'HFETCH')THEN
         CALL PAGETI(ID)
         CALL KUGETF(CHTITL,NCH)
         LUN=97
#if !defined(CERNLIB_IBM)
*        CALL CUTOL(CHTITL)
         OPEN(UNIT=LUN,FILE=CHTITL,STATUS='UNKNOWN',
     +        FORM='UNFORMATTED')
#endif
#if (defined(CERNLIB_IBM))&&(!defined(CERNLIB_IBMMVS))
         CHTEMP='STATE '//CHTITL
         DO 5 I=7,64
            IF(CHTEMP(I:I).EQ.'.')CHTEMP(I:I)=' '
   5     CONTINUE
         CALL VMCMS(CHTEMP,ISTAT)
         IF(ISTAT.NE.0)GO TO 90
         DO 10 I=1,NCH
            IF(CHTITL(I:I).EQ.'.')CHTITL(I:I)=' '
  10     CONTINUE
         CALL VMCMS('FILEDEF 97 DISK '//CHTITL,ISTAT)
         IF(ISTAT.NE.0)GO TO 90
         OPEN(UNIT=LUN,STATUS='UNKNOWN',FORM='UNFORMATTED')
#endif
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB_IBMMVS))
*--       CONSTRUCT MVS FILE NAME
*--       DON'T ADD PREFIX IF THE FIRST CHARACTER OF FILE NAME IS A DOT
      CALL KPREFI ( PREFIX, NCHPRE )
      IF ( CHTITL(1:1) .EQ. '.' )       THEN
           NCH    = LEN(CHTITL)
           MVSFIL = CHTITL(2:NCH)
      ELSE
           MVSFIL = PREFIX(1:NCHPRE)//CHTITL
      ENDIF
#endif
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB_IBMMVS))&&(!defined(CERNLIB_NEWLIB))
      PRINT 8000,  MVSFIL(1:40), LUN
 8000 FORMAT ( ' PAHIO - OPEN FILE ', A40, ' WITH STATUS '
     +,        'UNKNOWN ON LUN ', I3 )
#endif
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB_IBMMVS))
*--       CHECK FILE EXISTENCE
      INQUIRE ( FILE='/'//MVSFIL, ERR=2, IOSTAT=ISTAT
     +,         EXIST=EXS, OPENED=OPN, NUMBER=LUNOLD )
      IF ( ISTAT .NE. 0 )               GO TO 4
      IF ( .NOT. EXS )                  THEN
*--        MVSFIL IS A NEW FILE
#endif
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB_IBMMVS))&&(!defined(CERNLIB_NEWLIB))
           PRINT *, ' PAHIO - FILE ', MVSFIL, 'DOESN''T EXIST - CREATED'
#endif
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB_IBMMVS))
           PRINT *, ' PAHIO - FILE ', MVSFIL, 'DOESN''T EXIST - CREATED'
*--        DEFINE FILE PARAMETERS
           CALL FILEINF ( ISTAT, 'DEVICE', IUNIT, ITRK, ISPACE(1)
     +,                   'SECOND', ISPACE(2), 'DIR', ISPACE(3)
     +,                   'RECFM', IRECFM, 'LRECL', IDCB(1)
     +,                   'BLKSIZE', IDCB(2) )
           IF ( ISTAT .NE. 0 )          GO TO 6
*--            OPEN FILE
           OPEN ( UNIT=LUN, ERR=8, STATUS='NEW'
     +,           FILE='/'//MVSFIL, ACCESS='SEQUENTIAL'
     +,           FORM='UNFORMATTED', IOSTAT=ISTAT
     +,           ACTION='READWRITE' )
           IF ( ISTAT .NE. 0 )          GO TO 14
      ELSE
*--        MVSFIL IS AN OLD FILE
*--        IF PREVIOUSLY OPENED, CLOSE IT
           IF ( OPN )
     +          CLOSE ( UNIT=LUNOLD, ERR=10, STATUS='KEEP', IOSTAT=IRC )
*--        OPEN FILE IN READ/WRITE MODE, READ MODE ONLY IF FILE
*--            NAME START WITH A DOT (FILE BELONGING TO AN OTHER USERID
*--            FROM AN OTHER GROUP - RACF PROTECTION)
           IF ( CHTITL(1:1) .EQ. '.'              .AND.
#endif
#if (defined(CERNLIB_IBMMVS))&&(!defined(CERNLIB_NEWLIB))
     +          CHTITL(2:5) .NE. PREFIX(1:4) )    THEN
#endif
#if defined(CERNLIB_NEWLIB)
     +          CHTITL(2:7) .NE. PREFIX(1:6) )    THEN
#endif
#if defined(CERNLIB_IBMMVS)
                IACTIO = 'READ'
           ELSE
                IACTIO = 'READWRITE'
           ENDIF
           OPEN ( UNIT=LUN, ERR=8, STATUS='OLD'
     +,           FILE='/'//MVSFIL, ACCESS='SEQUENTIAL'
     +,           FORM='UNFORMATTED', IOSTAT=ISTAT
     +,           ACTION=IACTIO )
           IF ( ISTAT .NE. 0 )          GO TO 14
      ENDIF
      GO TO 20

*--       ERROR MESSAGES

    2 PRINT *, ' PAHIO - INQUIRE ERROR'
      ISTAT = -1
      GO TO 20
    4 PRINT *, ' PAHIO - INQUIRE ERROR - IOSTAT = ', ISTAT
      GO TO 20
    6 PRINT *, ' PAHIO - FILEINF ERROR - ISTAT = ', ISTAT
      GO TO 20
    8 PRINT *, ' PAHIO - OPEN ERROR'
      ISTAT = -1
      GO TO 20
   10 PRINT *, ' PAHIO - CLOSE ERROR'
      ISTAT = -1
      GO TO 20
   14 PRINT *, ' PAHIO - OPEN ERROR - IOSTAT = ', ISTAT
   20 CONTINUE
      IF ( ISTAT .NE. 0 )               THEN
           CALL HBUG ( 'Cannot open file', 'PAHIO', 0 )
      ENDIF
#endif
         CALL HFETCH(ID,LUN)
         CALL PACLOS(LUN)
         GO TO 90
      ENDIF
*
      IF(CHPATL.EQ.'HREAD')THEN
         CALL PAGETI(ID)
         CALL KUGETF(CHTITL,NCH)
         LUN=97
         CALL KUOPEN(LUN,CHTITL,'VERYOLD',IRET)
         IF(IRET.NE.0)GO TO 90
         CALL HREAD(ID,LUN,0,0,-1,0)
         CALL PACLOS(LUN)
         GO TO 90
      ENDIF
*
      IF(CHPATL.EQ.'PRINT')THEN
         CALL KUGETC(CHID,N)
         CALL HGETID(CHID)
         IF(ID.NE.0.AND.LCID.LE.0)GO TO 90
         CALL KUGETC(CHOPT,N)
         CALL KUALFA
         IF(CHOPT(1:1).NE.'S')THEN
            CALL PAHPR(ID)
         ELSE
            CALL HPSTAT(ID)
         ENDIF
         CALL HSETCD
         GO TO 90
      ENDIF
*
      IF(CHPATL.EQ.'DUMP')THEN
         CALL KUGETC(CHID,N)
         CALL HGETID(CHID)
         IF(ID.NE.0.AND.LCID.LE.0)GO TO 90
         CALL KUALFA
         CALL HDUMP(ID)
         CALL HSETCD
         GO TO 90
      ENDIF
*
      IF(CHPATL.EQ.'OUTPUT_LP')THEN
         CALL KUGETI(LUN)
         CALL KUGETF(CHTITL,NCH)
         IF(LUN.LT.0)THEN
            LUN=-LUN
            CALL PALUNF(LUN,2,IFREE)
            IF(IFREE.EQ.9)THEN
               CALL PACLOS(LUN)
               LUNIT(LUN)=0
            ENDIF
            CALL HOUTPU(6)
            CALL HERMES(6)
         ELSEIF(NCH.EQ.0)THEN
            CALL PALUNF(LUN,1,IFREE)
            IF(IFREE.EQ.0)GO TO 90
            IF(IFREE.LT.6)GO TO 90
            CALL HOUTPU(LUN)
            CALL HERMES(LUN)
         ELSE
            CALL PALUNF(LUN,1,IFREE)
            IF(IFREE.NE.0)GO TO 90
#if !defined(CERNLIB_IBM)
            CALL KUOPEN(LUN,CHTITL,'UNKNOWN',ISTAT)
#endif
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB_IBMMVS))
            CALL KUOPEN(LUN,CHTITL,'UNKNOWN',ISTAT)
#endif
#if (defined(CERNLIB_IBM))&&(!defined(CERNLIB_IBMMVS))
            WRITE(CHLUN,'(I3)')LUN
            CHFILE=CHTITL
            IF (INDEX(CHFILE,'.').EQ.0) THEN
              CHFILE(NCH+1:NCH+4)='.DAT'
              NCH=NCH+4
            ENDIF
            DO 12 I=1,NCH
               IF(CHFILE(I:I).EQ.'.')CHFILE(I:I)=' '
12          CONTINUE
            CHFILE(NCH+1:)=' (RECFM F LRECL 133'
            CALL VMCMS('FILEDEF '//CHLUN//' DISK '//CHFILE,ISTAT)
            IF (ISTAT.EQ.0)OPEN(UNIT=LUN,STATUS='UNKNOWN',IOSTAT=ISTAT)
#endif
            IF(ISTAT.EQ.0)THEN
               LUNIT(LUN)=9
               CALL HOUTPU(LUN)
               CALL HERMES(LUN)
            ENDIF
         ENDIF
         GO TO 90
      ENDIF
*
      IF(CHPATL.EQ.'GRESET')THEN
#if defined(CERNLIB_VAX)
         JOFSET=-99999
         CALL KUGETC(CHID,N)
         CALL HGETID(CHID)
         IF(ICHTOP(ICDIR).GE.0)GO TO 90
         LOCQ=1-LOCF(IPAWC(1))-ICHTOP(ICDIR)
         CALL HRESETG(ID,IPAWC(LOCQ))
         CALL HSETCD
         JOFSET=0
#endif
#if defined(CERNLIB_MMAP)
         JOFSET=-99999
         CALL KUGETC(CHID,N)
         CALL HGETID(CHID)
         IF(IGOFF.LE.0)GO TO 90
         CALL HRESETM(ID,LQ(IGOFF+1))
         CALL HSETCD
         JOFSET=0
#endif
         GO TO 90
      ENDIF
*
      IF(CHPATL.EQ.'GLOBAL_SECT')THEN
         CALL KUGETC(CHTITL,NCH)
#if defined(CERNLIB_VAX)
         IF(IGSIZE.GT.0) THEN
            IERROR=HFREEG(IGSIZE,IPAWC,IGOFF)
            CALL HREND(CHGLOB)
         ENDIF
         IGSIZE=HMAPG(CHTITL,IPAWC,IGOFF)
         IF(IGSIZE.LE.0) THEN
            IERROR=-IGSIZE
            PRINT 105,IERROR,CHTITL(1:LENOCC(CHTITL))
105         FORMAT(' Error',I6,' mapping section ',A)
            GO TO 90
         ENDIF
*
*           Connect HRIN to Global Section.
*
         NCHT=NCHTOP
         CALL HRFILE(IPAWC(IGOFF),CHTITL,'G')
         IF(NCHTOP.NE.NCHT)THEN
            HFNAME(NCHTOP)='Global section : '//CHTITL(1:NCH)
            CHGLOB=CHTOP(NCHTOP)
         ENDIF
*
#endif
#if defined(CERNLIB_MMAP)
         CALL HLIMAP(0,CHTITL)
*
#endif
#if (!defined(CERNLIB_VAX))&&(!defined(CERNLIB_MMAP))
         CALL KUALFA
         PRINT *,' Global sections not implemented'
#endif
         GO TO 90
      ENDIF
*
  90  CALL KUBRON
*
  99  END

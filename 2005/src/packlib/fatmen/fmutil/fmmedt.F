*
* $Id: fmmedt.F,v 1.1.1.1 1996/03/07 15:18:21 mclareni Exp $
*
* $Log: fmmedt.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:21  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMMEDT(IRC)
*
*     Look for FATMEN.MEDTYPES file and read list
*     of media types and definitions
*
*     ==> N.B. these definitions OVERRIDE any previously set.
*
*     ==> N.B. the order of the definitions is SIGNIFICANT
*
*     ==> N.B. negative MEDIA-TYPE is defined but NOT SELECTED
*
*     FATMEN.MEDTYPES consists of blank separated fields.
*     All fields must be specified for a given device type,
*     otherwise that device type is skipped.
*
*     Although it is not assumed that the definitions are given
*     in the order of the media-type (integer), it is assumed
*     that there are no 'holes', i.e. if media type 4 is defined,
*     so must be media types 1-3.
*
*     The fields are as follows:
*
* MEDIA-TYPE DEVICE-TYPE GENERIC-TYPE DENSITY CAPACITY MOUNT-TYPE LABEL-TYPE
*
*     e.g.
*
* 2 3480 CT1 38K 200 M SL
* 3 3420 TAPE 6250 150 M SL
* 4 8200 8MM 43200 2300 M SL
* 5 8500 8MM 86400 5000 M SL
*
*
      CHARACTER*255 CHFILE,CHLINE
      LOGICAL       IEXIST
#include "fatmen/fatpara.inc"
#include "fatmen/fatmtp.inc"
#include "fatmen/fattyp.inc"
#include "fatmen/fatsys.inc"
#include "fatmen/fatbug.inc"
#include "fatmen/slate.inc"
 
      IRC    = 0
*
      LDEF   = LENOCC(DEFAULT)
 
#if defined(CERNLIB_CSPACK)
      IF(FATNOD.NE.' ') THEN
*
*     Open remote file. Here we are assuming that the remote
*     server is a Unix system
*
         CHFILE = DEFAULT(1:LDEF)//'/FATMEN.MEDTYPES'
         LFILE  = LENOCC(CHFILE)
         CALL CUTOL(CHFILE(1:LFILE))
*
*     Does file exist?
*
         CALL XZINQR(LUFZFA,CHFILE(1:LFILE),FATNOD,ICODE,LRECL,IRC)
         IF(ICODE.NE.0) THEN
            IF(IDEBFA.GE.2) PRINT *,'FMMEDT. FATMEN.MEDTYPES file ',
     +      'does not exist (',CHFILE(1:LFILE),')'
            GOTO 50
         ENDIF
         LRECL  = 80
         CALL XZOPEN(LUFZFA,CHFILE(1:LFILE),FATNOD,LRECL,'F',IRC)
 
      ELSE
 
#endif
#if defined(CERNLIB_UNIX)
         CHFILE = DEFAULT(1:LDEF)//'/FATMEN.MEDTYPES'
         CALL CUTOL(CHFILE)
#endif
#if defined(CERNLIB_VAXVMS)
         CHFILE = DEFAULT(1:LDEF)//'FATMEN.MEDTYPES'
#endif
#if defined(CERNLIB_IBMVM)
         CHFILE = '/FATMEN MEDTYPES '//SERMOD
#endif
#if defined(CERNLIB_IBMMVS)
         CHFILE = '/'//DEFAULT(1:LDEF)//'FATMEN.MEDTYPES'
#endif
         LFILE = LENOCC(CHFILE)
*
*     Does file exist?
*
         INQUIRE(FILE=CHFILE(1:LFILE),EXIST=IEXIST)
 
         IF(.NOT.IEXIST) THEN
            IF(IDEBFA.GE.2) PRINT *,'FMMEDT. FATMEN.MEDTYPES file ',
     +      'does not exist (',CHFILE(1:LFILE),')'
            GOTO 50
         ENDIF
*
*     Open and read the file
*
#if defined(CERNLIB_IBMVM)
         OPEN(LUFZFA,FILE=CHFILE(1:LFILE),ACTION='READ', ACCESS=
     +   'SEQUENTIAL',FORM='UNFORMATTED', STATUS='OLD',IOSTAT=IRC)
#endif
#if defined(CERNLIB_IBMMVS)
         CALL KUOPEN(LUFZFA,CHFILE(1:LFILE),'OLD',IRC)
#endif
#if defined(CERNLIB_VAXVMS)
         ISTAT = LIB$GET_LUN(LUNACC)
         IF(.NOT.ISTAT) THEN
            IRC = 42
            IF(IDEBFA.GE.-3) PRINT *,'FMMEDT. could not assign logical '
     +      , 'unit to read location codes file'
            GOTO 50
         ENDIF
 
         OPEN(LUNACC,FILE=CHFILE(1:LFILE),READONLY,ACCESS='SEQUENTIAL',
     +   STATUS='OLD',FORM='FORMATTED',IOSTAT=IRC)
 
#endif
#if defined(CERNLIB_UNIX)
         CALL CIOPEN(LUNACC,'r',CHFILE(1:LFILE),IRC)
#endif
#if defined(CERNLIB_CSPACK)
      ENDIF
#endif
      IF(IRC.NE.0) THEN
         IF(IDEBFA.GE.-3) PRINT *,'FMMEDT. error ',IRC,' opening ',
     +      CHFILE(1:LFILE)
         GOTO 50
      ENDIF
 
*
*     Save and clear number of media types
*
      NFMOLD = NFMTYP
      NFMTYP = 0
*
*     Save and clear media type selection vector
*
      NUMOLD = NUMMTP
      NUMMTP = 0
*
   10 CONTINUE
#if defined(CERNLIB_CSPACK)
      IF(FATNOD.NE.' ') THEN
         CALL XZGETL(LUFZFA,CHLINE,'(A)',' ',IRC)
         IF(IRC.NE.0) GOTO 30
         LLINE = LENOCC(CHLINE)
      ELSE
#endif
#if defined(CERNLIB_IBMVM)
         READ(LUFZFA,NUM=LLINE,END=30) CHLINE
#endif
#if defined(CERNLIB_IBMMVS)
         READ(LUFZFA,'(A)',END=30) CHLINE
         LLINE = LENOCC(CHLINE)
#endif
#if defined(CERNLIB_VAXVMS)
         READ(LUNACC,'(A)',END=30) CHLINE
         LLINE = LENOCC(CHLINE)
#endif
#if defined(CERNLIB_UNIX)
         CALL FMCFGL(LUNACC,CHLINE,LLINE,' ',ISTAT)
         IF(ISTAT.NE.0) GOTO 30
#endif
#if defined(CERNLIB_CSPACK)
      ENDIF
#endif
 
      IF(LLINE.EQ.0) GOTO 10
      IF(IDEBFA.GE.3) PRINT *,'FMMEDT. read ',CHLINE(1:LLINE)
*
*     Comments...
*
      IF(CHLINE(1:1).EQ.'!')  GOTO 10
      IF(CHLINE(1:1).EQ.'*')  GOTO 10
      IF(CHLINE(1:1).EQ.'#')  GOTO 10
*        G.Folger       "/*" is bad for cpp, so split it...
      IF(CHLINE(1:1).EQ.'/' .AND. CHLINE(2:2).EQ.'*' ) GOTO 10
 
      NFMTYP = NFMTYP + 1
      IF(NFMTYP.GT.NMTYP) THEN
         IF(IDEBFA.GE.-3) PRINT 9001,NMTYP
 9001 FORMAT(' FMMEDT. cannot process any more media definitions.',
     +      '  Maximum number = ',I10)
         GOTO 40
      ENDIF
 
      LEND = LLINE
*
*     Media-type
*
      I = ICDECI(CHLINE,1,LEND)
      J = IABS(I)
      IF(J.GT.NMTYP) THEN
         IF(IDEBFA.GE.-3) PRINT 9002,J,NMTYP
 9002 FORMAT(' FMMEDT. media type ',I10,
     +      ' outside range. Maximum allowed value = ',I10)
            GOTO 20
      ENDIF
 
      IF(J.EQ.0) THEN
         IF(IDEBFA.GE.-3) PRINT 9003,J
 9003 FORMAT(' FMMEDT. invalid media type ',I10,
     +      ' ignored. Specify a nonzero integer')
            GOTO 20
      ENDIF
 
*
*     Label-type
*
      LSTART = INDEXB(CHLINE(1:LEND),' ')
      IF(LSTART.EQ.0) GOTO 20
 
      CHMLAB(J) = CHLINE(LSTART+1:LEND)
      LEND   = LSTART - 1
*
*     Mount-type
*
      LSTART = INDEXB(CHLINE(1:LEND),' ')
      IF(LSTART.EQ.0) GOTO 20
 
      CHMMNT(J) = CHLINE(LSTART+1:LEND)
      LEND   = LSTART - 1
*
*     Capacity
*
      LSTART = INDEXB(CHLINE(1:LEND),' ')
      IF(LSTART.EQ.0) GOTO 20
 
      CHMSIZ(J) = CHLINE(LSTART+1:LEND)
      MEDSIZ(J) = ICDECI(CHLINE,LSTART+1,LEND)
      LEND   = LSTART - 1
*
*     Density
*
      LSTART = INDEXB(CHLINE(1:LEND),' ')
      IF(LSTART.EQ.0) GOTO 20
 
      CHMDEN(J) = CHLINE(LSTART+1:LEND)
      MEDDEN(J) = ICDECI(CHLINE,LSTART+1,LEND)
      IF(INDEX(CHLINE(LSTART+1:LEND),'K').NE.0) MEDDEN(J) =
     +   MEDDEN(J)*1000
      LEND   = LSTART - 1
*
*     Generic-type
*
      LSTART = INDEXB(CHLINE(1:LEND),' ')
      IF(LSTART.EQ.0) GOTO 20
 
      CHMGEN(J) = CHLINE(LSTART+1:LEND)
      LEND   = LSTART - 1
*
*     Device-type
*
      LSTART = INDEXB(CHLINE(1:LEND),' ')
      IF(LSTART.EQ.0) GOTO 20
 
      CHMTYP(J) = CHLINE(LSTART+1:LEND)
      LEND   = LSTART - 1
 
      IF(I.GT.0) THEN
*
*     Set also selection vector
*
        NUMMTP = NUMMTP + 1
        MFMMTP(NUMMTP) = J
      ELSE
        IF(IDEBFA.GE.1) PRINT 9005,J
 9005 FORMAT(' FMMEDT. media type ',I10,
     +       ' booked but not selected')
      ENDIF
 
      GOTO 10
 
   20 CONTINUE
 
      IF(IDEBFA.GE.0) WRITE(LPRTFA,9004) CHLINE(1:LLINE)
 9004 FORMAT(' FMMEDT. skipping invalid line - ',A)
      NFMTYP = NFMTYP - 1
      GOTO 10
 
   30 CONTINUE
      IF((NFMTYP.LE.0).OR.(NUMMTP.LE.0)) THEN
        NFMTYP = NFMOLD
        NUMMTP = NUMOLD
        IRC = 1
      ENDIF
 
#if defined(CERNLIB_CSPACK)
      IF(FATNOD.NE.' ') THEN
         CALL XZCLOS(LUFZFA,' ',ISTAT)
      ELSE
#endif
#if defined(CERNLIB_IBM)
         CLOSE(LUFZFA)
#endif
#if defined(CERNLIB_VAXVMS)
         CLOSE(LUNACC)
         CALL LIB$FREE_LUN(LUNACC)
#endif
#if defined(CERNLIB_UNIX)
         CALL FMCFGL(LUNACC,CHLINE,LLINE,'F',ISTAT)
         CALL CICLOS(LUNACC)
#endif
#if defined(CERNLIB_CSPACK)
      ENDIF
#endif
 
      RETURN
 
   40 CONTINUE
      IRC = 1
      GOTO 30
 
   50 CONTINUE
      IRC = 1
      END

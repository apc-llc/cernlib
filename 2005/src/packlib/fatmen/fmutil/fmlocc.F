*
* $Id: fmlocc.F,v 1.1.1.1 1996/03/07 15:18:19 mclareni Exp $
*
* $Log: fmlocc.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:19  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMLOCC(IRC)
*
*     Look for FATMEN.LOCATION file and read list
*     of location codes and definitions
*
*     Following example is for OPAL:
*
*        0=Cern Vault     CERNVM VXCERN CRAY SHIFT etc
*        1=Cern Vault
*        2=Cern Vault
*       11=VXOPON         OPAL Online Vax cluster
*       12=Online         OPAL (apollo) online facilities
*       21=VXOPOF         OPAL Offline cluster
*       31=SHIFT          SHIFT disk and archive storage
*    33101=Saclay         Active cartridges
*    33901=Saclay         'obsolete' cartridges
*    44501=UKACRL         Active cartridges
*    44901=UKACRL         'obsolete' cartridges
*
      CHARACTER*255 CHFILE,CHLINE
      LOGICAL       IEXIST
#include "fatmen/fatlcc.inc"
#include "fatmen/fatsys.inc"
#include "fatmen/fatbug.inc"
#include "fatmen/slate.inc"
 
      IRC    = 0
      NKLCFA = 0
      LDEF   = LENOCC(DEFAULT)
 
#if defined(CERNLIB_CSPACK)
      IF(FATNOD.NE.' ') THEN
*
*     Open remote file. Here we are assuming that the remote
*     server is a Unix system
*
         CHFILE = DEFAULT(1:LDEF)//'/FATMEN.LOCCODES'
         LFILE  = LENOCC(CHFILE)
         CALL CUTOL(CHFILE(1:LFILE))
*
*     Does file exist?
*
         CALL XZINQR(LUFZFA,CHFILE(1:LFILE),FATNOD,ICODE,LRECL,IRC)
         IF(ICODE.NE.0) THEN
            IF(IDEBFA.GE.2) PRINT *,'FMLOCC. FATMEN.LOCCODES file ',
     +      'does not exist (',CHFILE(1:LFILE),')'
            GOTO 30
         ENDIF
         LRECL  = 80
         CALL XZOPEN(LUFZFA,CHFILE(1:LFILE),FATNOD,LRECL,'F',IRC)
 
      ELSE
 
#endif
#if defined(CERNLIB_UNIX)
         CHFILE = DEFAULT(1:LDEF)//'/FATMEN.LOCCODES'
         CALL CUTOL(CHFILE)
#endif
#if defined(CERNLIB_VAXVMS)
         CHFILE = DEFAULT(1:LDEF)//'FATMEN.LOCCODES'
#endif
#if defined(CERNLIB_IBMVM)
         CHFILE = '/FATMEN LOCCODES '//SERMOD
#endif
#if defined(CERNLIB_IBMMVS)
         CHFILE = '/'//DEFAULT(1:LDEF)//'FATMEN.LOCCODES'
#endif
         LFILE = LENOCC(CHFILE)
*
*     Does file exist?
*
         INQUIRE(FILE=CHFILE(1:LFILE),EXIST=IEXIST)
 
         IF(.NOT.IEXIST) THEN
            IF(IDEBFA.GE.2) PRINT *,'FMLOCC. FATMEN.LOCCODES file ',
     +      'does not exist (',CHFILE(1:LFILE),')'
            GOTO 30
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
            IF(IDEBFA.GE.-3) PRINT *,'FMLOCC. could not assign logical '
     +      , 'unit to read location codes file'
            GOTO 30
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
         IF(IDEBFA.GE.-3) PRINT *,'FMLOCC. error ',IRC,' opening ',
     +      CHFILE(1:LFILE)
         GOTO 30
      ENDIF
 
   10 CONTINUE
#if defined(CERNLIB_CSPACK)
      IF(FATNOD.NE.' ') THEN
         CALL XZGETL(LUFZFA,CHLINE,'(A)',' ',IRC)
         IF(IRC.NE.0) GOTO 20
         LLINE = LENOCC(CHLINE)
      ELSE
#endif
#if defined(CERNLIB_IBMVM)
         READ(LUFZFA,NUM=LLINE,END=20) CHLINE
#endif
#if defined(CERNLIB_IBMMVS)
         READ(LUFZFA,'(A)',END=20) CHLINE
         LLINE = LENOCC(CHLINE)
#endif
#if defined(CERNLIB_VAXVMS)
         READ(LUNACC,'(A)',END=20) CHLINE
         LLINE = LENOCC(CHLINE)
#endif
#if defined(CERNLIB_UNIX)
         CALL FMCFGL(LUNACC,CHLINE,LLINE,' ',ISTAT)
         IF(ISTAT.NE.0) GOTO 20
#endif
#if defined(CERNLIB_CSPACK)
      ENDIF
#endif
 
      IF(LLINE.EQ.0) GOTO 10
      IF(IDEBFA.GE.3) PRINT *,'FMLOCC. read ',CHLINE(1:LLINE)
*
*     Comments...
*
      IF(CHLINE(1:1).EQ.'!')  GOTO 10
      IF(CHLINE(1:1).EQ.'*')  GOTO 10
      IF(CHLINE(1:1).EQ.'#')  GOTO 10
*        G.Folger       "/*" is bad for cpp, so split it...
      IF(CHLINE(1:1).EQ.'/' .AND. CHLINE(2:2).EQ.'*' ) GOTO 10
*
*     Invalid lines
*
      IEQULS = INDEX(CHLINE(1:LLINE),'=')
      IF(IEQULS.EQ.0) GOTO 10
 
      JX = ICNUM(CHLINE,1,IEQULS-1)
      IF(JX.NE.IEQULS) GOTO 10
*
*     The bizzo
*
      NKLCFA = NKLCFA + 1
      NLCCFA(NKLCFA) = ICDECI(CHLINE,1,IEQULS-1)
      CHLOCF(NKLCFA) = CHLINE(IEQULS+1:LLINE)
 
      IF(NKLCFA.LT.MXLCFA) GO TO 10
 
   20 CONTINUE
 
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
 
   30 CONTINUE
      IRC = 1
      END
 

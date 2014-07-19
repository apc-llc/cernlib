*
* $Id: fmlcod.F,v 1.1.1.1 1996/03/07 15:18:20 mclareni Exp $
*
* $Log: fmlcod.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:20  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMLCOD(LUNLOC,CHFILE,CHOPT,IRC)
*
*     Look for file CHFILE and read list
*     of location codes and definitions
*
      CHARACTER*(*) CHFILE
      CHARACTER*255 CHNAME,CHLINE
      LOGICAL       IEXIST
#include "fatmen/fatlcc.inc"
#include "fatmen/fatsys.inc"
#include "fatmen/fatbug.inc"
#include "fatmen/slate.inc"
 
      IRC    = 0
      NKLCFA = 0
 
      LFILE = LENOCC(CHFILE)
#if defined(CERNLIB_IBM)
      CHNAME = '/'//CHFILE(1:LFILE)
      LNAME  = LFILE + 1
#endif
#if !defined(CERNLIB_IBM)
      CHNAME = CHFILE(1:LFILE)
      LNAME  = LFILE
#endif
*
*     Does file exist?
*
      INQUIRE(FILE=CHNAME(1:LNAME),EXIST=IEXIST)
 
      IF(.NOT.IEXIST) THEN
         IF(IDEBFA.GE.2) PRINT 9001,CHNAME(1:LNAME)
9001  FORMAT(' FMLCOD. location codes file does not exist (',A,')')
         GOTO 30
      ENDIF
*
*     Open and read the file
*
#if defined(CERNLIB_IBMVM)
      OPEN(LUNLOC,FILE=CHFILE(1:LFILE),ACTION='READ', ACCESS= 'SEQUENTI'
     +//'AL',FORM='UNFORMATTED', STATUS='OLD',IOSTAT=IRC)
#endif
#if defined(CERNLIB_IBMMVS)
      CALL KUOPEN(LUNLOC,CHFILE(1:LFILE),'OLD',IRC)
#endif
#if defined(CERNLIB_VAXVMS)
      OPEN(LUNLOC,FILE=CHFILE(1:LFILE),READONLY,ACCESS='SEQUENTIAL',
     +STATUS='OLD',FORM='FORMATTED',IOSTAT=IRC)
#endif
#if defined(CERNLIB_UNIX)
      OPEN(LUNLOC,FILE=CHFILE(1:LFILE),ACCESS='SEQUENTIAL',
     +STATUS='OLD',FORM='FORMATTED',IOSTAT=IRC)
#endif
      IF(IRC.NE.0) THEN
         IF(IDEBFA.GE.-3) PRINT *,'FMLCOD. error ',IRC,' opening ',
     +      CHFILE(1:LFILE)
         GOTO 30
      ENDIF
 
   10 CONTINUE
#if defined(CERNLIB_IBMVM)
      READ(LUNLOC,NUM=LLINE,END=20) CHLINE
#endif
#if !defined(CERNLIB_IBMVM)
      READ(LUNLOC,'(A)',END=20) CHLINE
      LLINE = LENOCC(CHLINE)
#endif
 
      IF(LLINE.EQ.0) GOTO 10
      IF(IDEBFA.GE.3) PRINT *,'FMLCOD. read ',CHLINE(1:LLINE)
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
 
      IF(IDEBFA.GE.1) PRINT 9003,NLCCFA(NKLCFA),CHLINE(IEQULS+1:LLINE)
9003  FORMAT(' FMLCOD. code ',I8.8,' = ',A)
 
      IF(NKLCFA.LT.MXLCFA) GO TO 10
 
   20 CONTINUE
 
      CLOSE(LUNLOC)
 
      RETURN
 
   30 CONTINUE
      IRC = 1
      END

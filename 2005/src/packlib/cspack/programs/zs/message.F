*
* $Id: message.F,v 1.1.1.1 1996/03/08 15:44:21 mclareni Exp $
*
* $Log: message.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:21  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE MESSAGE(CHMAIL)
*
*          Routine called when the PAW command RSHELL is invoked
*          CHMAIL is the string following RSHELL
*
*         Use the following conventions:
*           A message must always be aknowledged with CZPUTA
*           The first character CH1 sent by CZPUTA is a control
*           character for PAW.
*           If CH1='0' PAW goes to the next command (no action)
*                 ='1' PAW prints the message in CZPUTA
*                 ='2' same as '1', then PAW waits for a new server
*                      message.
*                 ='3' same as '1', IQUEST(1) is set to 1 on client side
*                 ='9' same as '1', then PAW closes communication
*                      with the server.
*
#if defined(CERNLIB_OS9)
#include "cspack/hos9c.inc"
#endif
      CHARACTER*(*) CHMAIL
      CHARACTER*20 CHCPU,CHMOD
      CHARACTER*70 CHM
      CHARACTER*2  CHMESS
      COMMON/PAWC/NWPAW,IXPAWC,IHDIV,IXHIGZ,IXKU,FENC(5),LMAIN,HCV(9989)
      DIMENSION IQ(2),Q(2),LQ(8000)
      EQUIVALENCE (LQ(1),LMAIN),(IQ(1),LQ(9)),(Q(1),IQ(1))
      INTEGER      SYSTEMF
#include "cspack/quest.inc"
#if defined(CERNLIB_VAXVMS)||defined(CERNLIB_MMAP)
#include "cspack/czdir.inc"
      INTEGER*4 HMAPM,HFREEM
      CHARACTER*64 CHGLOB
      SAVE         CHGLOB
      DATA IGSIZE,IGOFF/0,0/
#endif
*
      CHMESS=CHMAIL(1:2)
      CALL CLTOU(CHMESS)
*
*     Locate first non-blank character
*
*     IFIRST = ICFNBL(CHMAIL,3,60)
      IFIRST = INDEX(CHMAIL(3:60),' ') + 3
   30 CHM=CHMAIL(IFIRST:)
*
*           CDIR
*
      IF(CHMESS.EQ.'CD')THEN
         CALL ZSCDIR(CHM)
         GO TO 99
      ENDIF
*
*           LIST
*
      IF(CHMESS.EQ.'LI'.OR.CHMESS.EQ.'LD')THEN
         CALL ZSLIST(IGOFF,CHM)
         GO TO 99
      ENDIF
*
*           FILE
*
      IF(CHMESS.EQ.'FI')THEN
         CALL ZSFILE(CHM)
         GO TO 99
      ENDIF
*
*           CLOSE
*
      IF(CHMESS.EQ.'CL')THEN
         CALL ZSCLOS(CHM)
         GO TO 99
      ENDIF
*
*           FILELIST
*
      IF(CHMESS.EQ.'FL')THEN
         CALL ZSLFIL
         GO TO 99
      ENDIF
#if defined(CERNLIB_MMAP)
*
*           Shared memory
*
      IF(CHMESS.EQ.'GL')THEN
         IF(IGOFF.GT.0) THEN
            IERROR=HFREEM(IGOFF)
            CALL HREND(CHGLOB)
            IGOFF=0
         ENDIF
         CALL CLTOU(CHM)
         IGSIZE=HMAPM(CHM,LQ,IGOFF)
         IF(IGSIZE.NE.0) THEN
            IGOFF = 0
            CALL CZPUTA('3 Error mapping memory section',ISTAT)
            GO TO 99
         ENDIF
*
*           Connect HRIN to Global Section.
*
         CALL HRFILE(LQ(IGOFF+1),CHM,'M')
         IF(IQUEST(1).EQ.0) THEN
            CALL CZPUTA('1 Global memory connected',ISTAT)
*
            NCHRZ=NCHRZ+1
            CHRZ(NCHRZ)=CHM
            CHGLOB=CHM
            FLNAME(NCHRZ)=CHM
            LUNRZ(NCHRZ)=-1
            IDIR=NCHRZ
         ELSE
            CALL CZPUTA('3 Error mapping memory section',ISTAT)
         ENDIF
         GO TO 99
      ENDIF
*
#endif
#if defined(CERNLIB_VAXVMS)
*
*           Global_sections
*
      IF(CHMESS.EQ.'GL')THEN
         IF(IGSIZE.GT.0) THEN
            IERROR=HFREEG(IGSIZE,IQ,IGOFF)
            CALL HREND(CHGLOB)
         ENDIF
         IGSIZE=HMAPG(CHM,IQ,IGOFF)
         IF(IGSIZE.LE.0) THEN
            CALL CZPUTA('3 Error mapping global section',ISTAT)
            GO TO 99
         ENDIF
*
*           Connect HRIN to Global Section.
*
         CALL HRFILE(IQ(IGOFF),CHM,'G')
         CALL CZPUTA('1 Global section connected',ISTAT)
*
         NCHRZ=NCHRZ+1
         CHRZ(NCHRZ)=CHM
         FLNAME(NCHRZ)=CHM
         LUNRZ(NCHRZ)=-1
         IDIR=NCHRZ
         GO TO 99
      ENDIF
*
#endif
#if defined(CERNLIB_OS9)
*
*           MODULE
*
      IF(CHMESS.EQ.'MO')THEN
         ICPU=INDEX(CHM,'/')
         IF(ICPU.NE.0)THEN
            CHCPU=CHM(1:ICPU-1)
            CHMOD=CHM(ICPU+1:)
         ELSE
            CHCPU=CPU
            CHMOD=CHM
         ENDIF
         CALL ZSMODU(CHCPU,CHMOD)
         GO TO 99
      ENDIF
*
      IUSTAT = -1
      CALL ZSOPTION(CHMAIL,CHM,IUSTAT)
      IF(IUSTAT.EQ.0) GOTO 99
*
#endif
#if defined(CERNLIB_IBMVM)
*
*           VMCMS
*
      CALL VMCMS(CHMAIL,IERR)
#endif
#if !defined(CERNLIB_IBMVM)
      IERR = SYSTEMF(CHMAIL)
#endif
      IF(IERR.NE.0)THEN
         CALL CZPUTA('3Error executing remote command',ISTAT)
      ELSE
         CALL CZPUTA('1Remote command executed',ISTAT)
      ENDIF
      GO TO 99
#if defined(CERNLIB_OLD)
*
*           Unknown message
*
   90 CALL CZPUTA('3Unknown remote command',ISTAT)
#endif
*
 8001 FORMAT('0',A4,I6)
 8002 FORMAT('3Error executing remote ',A4,
     +       ' command, return code = ',I6)
 8003 FORMAT('EEOF from ',A4,' return code = ',I6)
   99 END

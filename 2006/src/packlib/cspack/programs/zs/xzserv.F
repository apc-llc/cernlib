*
* $Id: xzserv.F,v 1.1.1.1 1996/03/08 15:44:21 mclareni Exp $
*
* $Log: xzserv.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:21  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE XZSERV(CHM)
#include "cspack/hcmail.inc"
#include "cspack/czsock.inc"
      DIMENSION ICONT(2)
*
*     Steering routine for XZ client calls
*     This routine handles the communication between the client
*     and server routines, so that the server routines need make
*     no explicit calls to CZ
*
      DIMENSION     IBUFF(8192)
      CHARACTER*(*) CHM
      CHARACTER*80  FNAME
      CHARACTER*80  CHFORM,CHLINE
      CHARACTER*64  CHPATT
      CHARACTER*26  CHOPT
      CHARACTER*20  CHTOP
      CHARACTER*80  CHSRC,CHDST
 
      LCH = LENOCC(CHM)
*
*     Handle OPEN, CLOSE, READ, WRITE, LS, CD, PWD, RM and INQUIRE
*
#if defined(CERNLIB_DEBUG)
      WRITE(LUNCZ,*) 'XZSERV. Enter for '//CHM(1:4)
#endif
      IF(CHM(1:4).EQ.'READ') THEN
         READ(CHM(5:LCH),9001) LUN,NREC,NWANT
         CHMAIL = CHM
#include "cspack/szchopt.inc"
#if defined(CERNLIB_DEBUG)
      WRITE(LUNCZ,*) 'XZSERV. call SZREAD for LUN,NREC,NWANT,CHOPT = ',
     +               LUN,NREC,NWANT,CHOPT
#endif
         CALL SZREAD(LUN,IBUFF,NREC,NWANT,NGOT,CHOPT,IRC)
9001     FORMAT(I3,I6,I6)
#include "cspack/szmess.inc"
         IF(IRC.EQ.0) THEN
*
*     Send the data
*
            ICONT(1) = 1
            LBUF     = NWANT/4
            CALL CZTCP(IBUFF,ICONT)
            ENDIF
 
      ELSEIF(CHM(1:4).EQ.'RITE') THEN
         READ(CHM(5:LCH),9001) LUN,NREC,NWRITE
         CHMAIL = CHM
#include "cspack/szchopt.inc"
*
*     Read data from network and write to file
*
         ICONT(1) = 0
         LBUF     = NWRITE/4
         CALL CZTCP(IBUFF,ICONT)
#if defined(CERNLIB_DEBUG)
      WRITE(LUNCZ,*) 'XZSERV. call SZRITE for LUN,NREC,NWRITE,CHOPT = ',
     +               LUN,NREC,NWRITE,CHOPT
#endif
         CALL SZRITE(LUN,IBUFF,NREC,NWRITE,CHOPT,IRC)
#include "cspack/szmess.inc"
 
      ELSEIF(CHM(1:4).EQ.'REDM') THEN
         READ(CHM(5:LCH),9003) LUN,NREC,NWANT,NDO
9003     FORMAT(I3,I6,I6,I6)
         CHMAIL = CHM
#include "cspack/szchopt.inc"
         IOPTD = INDEX(CHOPT,'D')
 
         DO 22 JJ=1,NDO
#if defined(CERNLIB_DEBUG)
      WRITE(LUNCZ,*) 'XZSERV. call SZREAD for LUN,NREC,NWANT,CHOPT = ',
     +               LUN,NREC,NWANT,CHOPT
#endif
         CALL SZREAD(LUN,IBUFF,NREC,NWANT,NGOT,CHOPT,IRC)
#include "cspack/szmess.inc"
         IF(IOPTD.NE.0) NREC = NREC + 1
         IF(IRC.EQ.0) THEN
*
*     Send the data
*
            ICONT(1) = 1
            LBUF     = NWANT/4
            CALL CZTCP(IBUFF,ICONT)
         ENDIF
 
22       CONTINUE
 
      ELSEIF(CHM(1:4).EQ.'RITM') THEN
         CHMAIL = CHM
         READ(CHM(5:LCH),9003) LUN,NREC,NWRITE,NDO
#include "cspack/szchopt.inc"
         IOPTD = INDEX(CHOPT,'D')
 
         DO 33 JRITE=1,NDO
*
*     Read data from network and write to file
*
         ICONT(1) = 0
         LBUF     = NWRITE/4
         CALL CZTCP(IBUFF,ICONT)
#if defined(CERNLIB_DEBUG)
      WRITE(LUNCZ,*) 'XZSERV. call SZRITE for LUN,NREC,NWRITE,CHOPT = ',
     +               LUN,NREC,NWRITE,CHOPT
#endif
         CALL SZRITE(LUN,IBUFF,NREC,NWRITE,CHOPT,IRC)
         IF(IOPTD.NE.0) NREC = NREC + 1
33       CONTINUE
#include "cspack/szmess.inc"
 
         ELSEIF(CHM(1:4).EQ.'GETL') THEN
            READ(CHM(5:LCH),'(I3)') LUN
            CHFORM = CHM(8:LCH)
            READ(LUN,FMT=CHFORM,IOSTAT=IRC) CHLINE
#include "cspack/szmess.inc"
            IF(IRC.EQ.0) CALL CZPUTA(CHLINE,ISTAT)
 
         ELSEIF(CHM(1:4).EQ.'PUTL') THEN
            READ(CHM(5:LCH),'(I3,A)') LUN
            CHFORM = CHM(8:LCH)
            CALL CZGETA(CHLINE,ISTAT)
            WRITE(LUN,FMT=CHFORM,IOSTAT=IRC) CHLINE
#include "cspack/szmess.inc"
 
         ELSEIF(CHM(1:4).EQ.'OPEN') THEN
*           READ(CHM(5:LCH),'(A)') FNAME
            JL = ICFNBL(CHM,5,LCH)
            FNAME = CHM(JL:LCH)
            LF = LENOCC(FNAME)
*
*        Handle long file names
*
         IF(FNAME(1:LF).EQ.'_') THEN
            CALL CZGETA(FNAME,ISTAT)
            LF = LENOCC(FNAME)
         ENDIF
#if defined(CERNLIB_IBM)
            CALL CTRANS('.',' ',FNAME,1,LF)
#endif
            CHMAIL = ' '
            CALL CZGETA(CHMAIL,ISTAT)
            READ(CHMAIL,9002) LUN,LRECL
9002        FORMAT(10X,I3,I6)
#include "cspack/szchopt.inc"
            CALL SZOPEN(LUN,FNAME(1:LF),LRECL,CHOPT,IRC)
            IF(IRC.EQ.28) THEN
               WRITE(CHMAIL,'(A)')
     +'2Remote file already exists - use option R to replace '
               CALL CZPUTA(CHMAIL,ISTAT)
            ENDIF
#include "cspack/szmess.inc"
 
         ELSEIF(CHM(1:4).EQ.'RZOP') THEN
            JL = ICFNBL(CHM,5,LCH)
            FNAME = CHM(JL:LCH)
            LF = LENOCC(FNAME)
            CHMAIL = ' '
            CALL CZGETA(CHMAIL,ISTAT)
            READ(CHMAIL,9002) LUN,LRECL
#include "cspack/szchopt.inc"
            CALL RZOPEN(LUN,' ',FNAME(1:LF),CHOPT,LRECL,IRC)
            WRITE(CHMAIL,9002) IRC,LRECL
            CALL CZPUTA(CHMAIL,ISTAT)
 
         ELSEIF(CHM(1:4).EQ.'HROP') THEN
            JL = ICFNBL(CHM,5,LCH)
            FNAME = CHM(JL:LCH)
            LF = LENOCC(FNAME)
            CHMAIL = ' '
            CALL CZGETA(CHMAIL,ISTAT)
            READ(CHMAIL,9002) LUN,LRECL
#include "cspack/szchopt.inc"
            CHTOP = 'LUN'
            CALL XZITOC(LUN,CHTOP(4:),LTOP)
            CALL HROPEN(LUN,CHTOP(1:LTOP+3),FNAME(1:LF),CHOPT,LRECL,IRC)
            WRITE(CHMAIL,9002) IRC,LRECL
            CALL CZPUTA(CHMAIL,ISTAT)
 
         ELSEIF(CHM(1:4).EQ.'CLOS') THEN
            READ(CHM(5:LCH),'(I3)') LUN
#include "cspack/szchopt.inc"
            CALL SZCLOS(LUN,CHOPT,IRC)
#include "cspack/szmess.inc"
 
         ELSEIF(CHM(1:4).EQ.'REWD') THEN
            READ(CHM(5:LCH),'(I3)') LUN
            CALL SZREWD(LUN,IRC)
#include "cspack/szmess.inc"
 
         ELSEIF(CHM(1:4).EQ.'INQR') THEN
*        READ(CHM(5:LCH),'(A)') FNAME
         JL = ICFNBL(CHM,5,LCH)
         FNAME = CHM(JL:LCH)
         LF = LENOCC(FNAME)
#if defined(CERNLIB_IBM)
         CALL CTRANS('.',' ',FNAME,1,LF)
#endif
         CALL SZINQR(LUN,FNAME(1:LF),IEXIST,LRECL,IRC)
#include "cspack/szmess.inc"
 
         ELSEIF(CHM(1:2).EQ.'LS')   THEN
*
*     CHM = LS [path] (options)
*
 
            LBRA = INDEX(CHM,'(')
            IF(LBRA.NE.0) THEN
               LEND = LBRA - 1
               ELSE
               LEND = LCH
               ENDIF
 
            JL = ICFNBL(CHM,5,LEND)
            IF(JL.GT.LEND) THEN
               CHPATT = ' '
               ELSE
               CHPATT = CHM(JL:LEND)
               IF(CHPATT(1:1).EQ.'(') CHPATT = ' '
               ENDIF
 
            CHMAIL = CHM
 
            IL = INDEX(CHMAIL,'(')
            IR = INDEXB(CHMAIL,')')
            IF((IL.NE.0).AND.(IR.GT.IL)) THEN
              CHOPT = CHMAIL(IL+1:IR-1)
              ELSE
              CHOPT = ' '
              ENDIF
#if defined(CERNLIB_UNIX)
            IF(INDEX(CHOPT,'C').EQ.0) CALL CUTOL(CHPATT)
#endif
#if defined(CERNLIB_DEBUG)
      WRITE(LUNCZ,*) 'XZSERV. call SZLS for CHPATT, CHOPT = ',
     +                CHPATT,' , ',CHOPT
#endif
            CALL SZLS(CHPATT,CHOPT,IRC)
#include "cspack/szmess.inc"
 
         ELSEIF(CHM(1:2).EQ.'CD')   THEN
            CHPATT = CHM(ICFNBL(CHM,5,LCH):LCH)
            LP     = LENOCC(CHPATT)
            CALL SZCD(CHPATT(1:LP),IRC)
#include "cspack/szmess.inc"
 
         ELSEIF(CHM(1:3).EQ.'PWD')  THEN
            CALL SZPWD(IRC)
#include "cspack/szmess.inc"
 
         ELSEIF(CHM(1:2).EQ.'RM')   THEN
            CHPATT = CHM(ICFNBL(CHM,5,LCH):LCH)
            LP     = LENOCC(CHPATT)
#if defined(CERNLIB_UNIX)
            CALL CUTOL(CHPATT)
#endif
#if defined(CERNLIB_IBM)
            CALL CTRANS('.',' ',CHPATT,1,LP)
            CALL SZRM('/'//CHPATT,IRC)
#endif
#if !defined(CERNLIB_IBM)
            CALL SZRM(CHPATT,IRC)
#endif
#include "cspack/szmess.inc"
 
         ELSEIF(CHM(1:3).EQ.'VER')  THEN
            CALL SZVERS(IRC)
#include "cspack/szmess.inc"
         ELSEIF(CHM(1:4).EQ.'RSYS')  THEN
#if defined(CERNLIB_IBMVM)
         CALL CZPUTA('1VM',ISTAT)
#endif
#if defined(CERNLIB_IBMMVS)
         CALL CZPUTA('1MVS',ISTAT)
#endif
#if defined(CERNLIB_VAXVMS)
         CALL CZPUTA('1VMS',ISTAT)
#endif
#if defined(CERNLIB_UNIX)
         CALL CZPUTA('1UNIX',ISTAT)
#endif
         ELSEIF(CHM(1:2).EQ.'MV')  THEN
#include "cspack/szchopt.inc"
            CALL CZGETA(CHSRC,ISTAT)
            CALL CZGETA(CHDST,ISTAT)
            CALL SZMV(CHSRC,CHDST,CHOPT,IRC)
#include "cspack/szmess.inc"
         ELSE
         CALL CZPUTA('3Unknown I/O command',ISTAT)
         ENDIF
 
8001  FORMAT('0',A4,I6)
8002  FORMAT('3Error executing remote ',A4,
     +       ' command, return code = ',I6)
8003  FORMAT('EEOF from ',A4,' return code = ',I6)
 
99    END

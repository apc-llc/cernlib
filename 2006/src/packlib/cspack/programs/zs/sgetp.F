*
* $Id: sgetp.F,v 1.5 1998/09/25 09:24:21 mclareni Exp $
*
* $Log: sgetp.F,v $
* Revision 1.5  1998/09/25 09:24:21  mclareni
* Modifications for the Mklinux port flagged by CERNLIB_PPC
*
* Revision 1.4  1997/09/02 08:46:15  mclareni
* WINNT mods, mostly cpp defines
*
* Revision 1.3  1997/01/17 08:56:12  gunter
* call vxinvb for linux too.
*
* Revision 1.2  1996/04/11 14:54:11  cernlib
* Zserv/pawserv used to be build from two patches; these were put both into this
* directory.
* The #includes in all files copied from the other directory had to updated.
*
* Revision 1.1.1.1  1996/03/08 15:44:20  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE SGETP(CHF)
*
*     Transfer a PAM file to the client
*
#include "hbook/hcmail.inc"
#include "cspack/czsock.inc"
#include "cspack/czbuff.inc"
      CHARACTER*4  CHOPT
      DIMENSION    IA(512)
      DIMENSION    IX(8)
      DIMENSION    ICONTR(2)
      DIMENSION    MPACK2(2),MPACK9(2)
      CHARACTER*12 NODE
      CHARACTER*8  DELTIM
#include "cspack/czunit.inc"
      CHARACTER*(*) CHF
      CHARACTER*80 LOCAL
      DATA MPACK2  / 2, 16 /
      DATA MPACK9  / 9, 3  /
*
      LCHF  = LENOCC(CHF)
      LBLNK = INDEX(CHF(1:LCHF),' ')
      IF(LBLNK.NE.0) THEN
         LOCAL = CHF(1:LBLNK-1)
         CHOPT = CHF(LBLNK+1:LCHF)
      ELSE
         LOCAL = CHF(1:LCHF)
         CHOPT = ' '
      ENDIF
*
*     Open local file
*
      CHMAIL='OK'
      LRECL = 2048
      IF(INDEX(CHOPT,'C').EQ.0) THEN
         CALL SZOPEN(LUNXZI,LOCAL,LRECL,'IP',ISTAT)
      ELSE
         CALL SZOPEN(LUNXZI,LOCAL,LRECL,'IPC',ISTAT)
      ENDIF
      IF(ISTAT.NE.0)GO TO 95
*
*     Inform client if file opened on local node
*
50    CALL CZPUTA(CHMAIL,ISTAT)
      IF(ISTAT.NE.0)GO TO 99
      IF(CHMAIL.EQ.'KO')GO TO 99
*
*     Start transfer
*
      NREC = 0
      NWOR = 0
1     CALL VBLANK(IA,512)
      NUM = 512
      READ(LUNXZI,END=2) NUM,(IA(I),I=1,NUM)
      IF(NUM.EQ.0) GOTO 2
      NWOR = NWOR + NUM
      NREC = NREC + 1
#if defined(CERNLIB_IBM)
*
*     Translate look-ahead name
*
      CALL XZETOA(IA(1),8)
#endif
      CALL UPKBYT(IA(3),1,IX(4),4,MPACK9)
      CALL UPKBYT(IA(3),1,IX,4,MPACK2)
*
*     Loop over body of this record, splitting into lines
*
      I = 4
      LENX = 1
      IF(IX(5).NE.0) THEN
         I = IX(7)
         LENX = IX(7) - 3
      ENDIF
#if defined(CERNLIB_VAXVMS)||defined(CERNLIB_DECS) || (defined(CERNLIB_LINUX) && !defined(CERNLIB_PPC)) || defined(CERNLIB_WINNT)
*
*     Byte swap index vector
*
      CALL VXINVB(IA(3),LENX)
#endif
*
*     Send #words to read
*
      WRITE(CHMAIL,'(I3)') NUM
      CALL CZPUTA(CHMAIL,ISTAT)
      IF(ISTAT.NE.0) GOTO 97
#if defined(CERNLIB_IBM)
*
*     Translate text
*
      CALL XZETOA(IA(I),(NUM-I+1)*4)
#endif
*
*     Send data to client
*
      LBUF      = NUM
      ICONTR(1) = 1
      ICONTR(2) = LBUF
      CALL CZTCP(IA,ICONTR)
 
      GOTO 1
*
2     CONTINUE
 
      NUM = -1
      WRITE(CHMAIL,'(I3)') NUM
      CALL CZPUTA(CHMAIL,ISTAT)
      IF(ISTAT.NE.0) GOTO 97
 
      CLOSE(LUNXZI)
      GO TO 99
*
*          Error
*
  95  CHMAIL = 'KO'
      GO TO 50
  97  CLOSE(LUNXZI)
*
  99  LBUF = 270
      END

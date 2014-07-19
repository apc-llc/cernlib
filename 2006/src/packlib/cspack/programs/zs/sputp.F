*
* $Id: sputp.F,v 1.5 1998/09/25 09:24:24 mclareni Exp $
*
* $Log: sputp.F,v $
* Revision 1.5  1998/09/25 09:24:24  mclareni
* Modifications for the Mklinux port flagged by CERNLIB_PPC
*
* Revision 1.4  1997/09/02 08:46:16  mclareni
* WINNT mods, mostly cpp defines
*
* Revision 1.3  1997/01/17 08:56:12  gunter
* call vxinvb for linux too.
*
* Revision 1.2  1996/04/11 14:54:16  cernlib
* Zserv/pawserv used to be build from two patches; these were put both into this
* directory.
* The #includes in all files copied from the other directory had to updated.
*
* Revision 1.1.1.1  1996/03/08 15:44:20  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE SPUTP(CHF)
*
*          Transfer a PAM file from client
*
#include "hbook/hcmail.inc"
#include "cspack/czsock.inc"
#include "cspack/czbuff.inc"
#include "cspack/czunit.inc"
      CHARACTER*(*) CHF
      CHARACTER*4   CHOPT
      CHARACTER*80  LOCAL
      DIMENSION     IA(512)
      DIMENSION     IB(2)
      DIMENSION     ICONTR(2)
      DIMENSION IX(8)
      DIMENSION MPACK2(2),MPACK9(2)
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
*     Create local file
*
      CHMAIL='OK'
      LRECL = 2048
      IF(INDEX(CHOPT,'C').EQ.0) THEN
         CALL SZOPEN(LUNXZO,LOCAL,LRECL,'NOP',ISTAT)
      ELSE
         CALL SZOPEN(LUNXZO,LOCAL,LRECL,'NOPC',ISTAT)
      ENDIF
      IF(ISTAT.NE.0) GOTO 95
*
*     Inform client if file opened on local node
*
  50  CALL CZPUTA(CHMAIL,ISTAT)
      IF(ISTAT.NE.0)GO TO 99
      IF(CHMAIL.EQ.'KO')GO TO 99
*
*     Transfer data
*
  20  CONTINUE
      CALL CZGETA(CHMAIL,ISTAT)
      IF(ISTAT.NE.0) GOTO 99
 
      READ(CHMAIL,'(I3)') LBUF
      IF(LBUF.LT.0) GOTO 40
 
      ICONTR(1) = 0
      ICONTR(2) = LBUF
 
      CALL CZTCP(IA,ICONTR)
#if defined(CERNLIB_IBM)
      CALL XZATOE(IA(1),8)
#endif
#if defined(CERNLIB_VAXVMS)||defined(CERNLIB_DECS) || (defined(CERNLIB_LINUX) && !defined(CERNLIB_PPC)) || defined(CERNLIB_WINNT)
      IB(1) = IA(3)
      IB(2) = IA(4)
      CALL VXINVB(IB(1),2)
      CALL UPKBYT(IB(1),1,IX(4),4,MPACK9)
      CALL UPKBYT(IB(1),1,IX,4,MPACK2)
      LENX = 1
      IF(IX(5).NE.0) LENX = IX(7) - 3
*
*     Byte swap index vector
*
      CALL VXINVB(IA(3),LENX)
#endif
      CALL UPKBYT(IA(3),1,IX(4),4,MPACK9)
      CALL UPKBYT(IA(3),1,IX,4,MPACK2)
      I    = 4
      LENX = 1
      IF(IX(5).NE.0) THEN
         I = IX(7)
         LENX = IX(7) - 3
      ENDIF
#if defined(CERNLIB_IBM)
*
*     Translate text
*
      CALL XZATOE(IA(I),(LBUF-I+1)*4)
#endif
      WRITE(LUNXZO) LBUF,(IA(I),I=1,LBUF)
 
      GOTO 20
*
  40  CONTINUE
      CLOSE(LUNXZO)
      GO TO 99
*
  95  CHMAIL='KO'
      GO TO 50
*
  99  LBUF = 270
      END
 

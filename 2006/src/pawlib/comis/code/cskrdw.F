*
* $Id: cskrdw.F,v 1.1.1.1 1996/02/26 17:16:22 mclareni Exp $
*
* $Log: cskrdw.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:22  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.16/08 01/09/93  12.36.13  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      INTEGER FUNCTION CSKRDW(JFMT,JINTBF,JCHIBF,LENIBF,CHIBUF,KINTWR)
***-------------------
*    it is interp. command READ or WRITE
***-------------------
*READ/WRITE  IOLUN,IPTF,LABEND, LABERR
      CHARACTER *(*) CHIBUF
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/comis.inc"
#include "comis/cspnts.inc"
#include "comis/cssysd.inc"
#include "comis/csfmt.inc"
      INTEGER CSIOWR,CSIORD,CSIOWI,CSIORI
      CSKRDW=1
 1501 IOLUN=KD1(IPC)
      IPTF=KD2(IPC)
      LIOEND=KD(IPC+3)
      IF(LIOEND.NE.-1)LIOEND=IBASE+LIOEND
      LIOERR=KD(IPC+4)
      IF(LIOERR.NE.-1)LIOERR=IBASE+LIOERR
      IFMTST=0
      IF(IPTF.EQ.-2)THEN
*     unformatted i/o
        IFMT=-1
      ELSEIF(IPTF.EQ.-1)THEN
*       free format i/o
        IFMT=0
      ELSE
*     formatted i/o
        I=IPTF+IBASE
        NB=KD2(I)
        IFMT=I+3+NB
        NB=NB*NBYTPW
        J=MJCHAR(KD3(I))
        FMT=' '
        CALL CCOPYS(J,JFMT,NB)
      ENDIF
      IF(IOLUN.EQ.0)THEN
        JW=IA(ITA)
        LUN=IA(JW-JTOPA)
        JSTR=0
        LENSTR=0
        IF(KEYRW.EQ.2)THEN
          I=CSIOWR(LUN)
        ELSEIF(KEYRW.EQ.1)THEN
          I=CSIORD(LUN)
        ENDIF
      ELSEIF(IOLUN.EQ.1)THEN
        JINTBF=MIWORD(IA(ITA)+1)
        L=MOD(MIWORD(IA(ITA)),KON3)
        LENIBF=MIN0(L,256)
        CALL CCOPYS(JINTBF,JCHIBF,LENIBF)
        JSTR=JCHIBF
        LENSTR=LENIBF
        IF(KEYRW.EQ.2)THEN
           KINTWR=1
           I=CSIOWI(1)
        ELSEIF(KEYRW.EQ.1)THEN
           KINTWR=0
           I=CSIORI(CHIBUF(1:LENIBF))
        ENDIF
      ENDIF
      ITA=ITA-1
      IF(I.EQ.0)THEN
        IF(JIOSTA.NE.0)IA(JIOSTA-JTOPA)=IFMTST
        IF(IFMTST.LT.0 . AND. LIOEND.NE.-1)THEN
          IPC=LIOEND
        ELSEIF(LIOERR.NE.-1)THEN
          IPC=LIOERR
        ELSE
***          GO TO 704
          CSKRDW=-4
        ENDIF
      ELSE
        IPC=IPC+5
      ENDIF
***      GO TO 999
      END

*
* $Id: csioiv.F,v 1.2 1996/03/14 13:42:56 berezhno Exp $
*
* $Log: csioiv.F,v $
* Revision 1.2  1996/03/14 13:42:56  berezhno
* mods for output logical vars for NT
*
* Revision 1.1.1.1  1996/02/26 17:16:23  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.16/23 24/10/93  12.05.22  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      INTEGER FUNCTION CSIOIV(IV)
      INTEGER CSNUMB
#include "comis/csfmt.inc"
      DOUBLE PRECISION DNUM
      CHARACTER *(*)FORM
      CHARACTER BUFFER*132
      PARAMETER (FORM='(I10)', IWD=10)
#if defined(CERNLIB_WINNT)
      LOGICAL LOGVAR
#endif
      CSIOIV=0
      IF(IFMTST.NE.0)RETURN
      IF(IFMT.EQ.0)THEN
        IF(KEYRW.EQ.2)THEN
* OUTPUT IN FREE FORMAT I10
          IBF1=IBF+IWD-1
          IF(IBF1.GT.LBUF)THEN
**was            IF(LIOERR.EQ.-1) PRINT *,' Too long output record'
**was            IFMTST=3
**was            RETURN
            WRITE(LUNIO,77,IOSTAT=ISTA, ERR=111)BUF(1:IBF-1)
  77        FORMAT(A)
            IBF=1
            IBFLST=1
            IBF1=IBF+IWD-1
          ENDIF
          WRITE(BUF(IBF:IBF1),FORM,IOSTAT=ISTA, ERR=111)IV
        ELSEIF(KEYRW.EQ.1)THEN
* READ IN FREE FORMAT
  20      CONTINUE
  2       DO 21 K=IBF,LBUF
          IF(BUF(K:K).NE.' ')GO TO 22
  21      CONTINUE
**was       IF(LIOERR.EQ.-1) PRINT *,' No more data in input record'
**was            IFMTST=1
**was            RETURN
#if !defined(CERNLIB_IBM)
          READ(LUNIO,77,IOSTAT=ISTA,ERR=111,END=111)BUF
#endif
#if defined(CERNLIB_IBM)
          BUF=' '
          READ(LUNIO,77,IOSTAT=ISTA,ERR=111,END=111)BUF(1:80)
#endif
          IBF=1
          IBFLST=1
          GO TO 20
  22      ISIGN=0
          IF(BUF(K:K).EQ.'-')THEN
             ISIGN=1
             K=K+1
          ELSEIF(BUF(K:K).EQ.'+')THEN
            K=K+1
          ENDIF
          IBF=K
          JJ=MJSCHA(BUF(IBF:))
          JS=JJ
          L=LBUF-K+1
          K=CSNUMB(JJ,L,INUM,RNUM,DNUM)
          IBF1=IBF+JJ-JS-1
          IF(K.NE.1)THEN
            IF(LIOERR.EQ.-1) PRINT *,' Expected integer number'
            IFMTST=2
            RETURN
          ELSE
            IV=INUM
            IF(ISIGN.EQ.1)IV=-IV
          ENDIF
        ENDIF
      ELSE
        CALL CSFMTC(0)
        IF(IFMTST.NE.0)RETURN
        IBF1=IBF+IWIDTH-1
        BUFFER='('//FMT(IPOS1:IPOS2)//')'
        IBUFF = IPOS2-IPOS1+3
        IF(KEYRW.EQ.2)THEN
#if defined(CERNLIB_WINNT)
          IF(ICODE.EQ.11)THEN
            LOGVAR=IV
            WRITE(BUF(IBF:IBF1),BUFFER(1:IBUFF),
     +          IOSTAT=ISTA, ERR=111)LOGVAR
          ELSE
            WRITE(BUF(IBF:IBF1),BUFFER(1:IBUFF),
     +          IOSTAT=ISTA, ERR=111)IV
          ENDIF
#else
          WRITE(BUF(IBF:IBF1),BUFFER(1:IBUFF),
     +          IOSTAT=ISTA, ERR=111)IV
#endif
        ELSE
          READ (BUF(IBF:IBF1),BUFFER(1:IBUFF),
     +          IOSTAT=ISTA, ERR=111)IV
        ENDIF
      ENDIF
      IBF=IBF1+1
      IF(IBF.GT.IBFLST)IBFLST=IBF
      CSIOIV=1
      RETURN
 111  IFMTST=ISTA
      IF(LIOERR.EQ.-1) PRINT *,' i/o error during conversion'
      END


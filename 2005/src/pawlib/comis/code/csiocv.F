*
* $Id: csiocv.F,v 1.1.1.1 1996/02/26 17:16:24 mclareni Exp $
*
* $Log: csiocv.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:24  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.16/21 21/10/93  16.11.28  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      INTEGER FUNCTION CSIOCV(CV)
      INTEGER CSICNS
      CHARACTER *(*) CV
#include "comis/csfmt.inc"
      COMMON/MDPOOL/IQ(99)
      CHARACTER *(*)FORM
      CHARACTER BUFFER*132
      PARAMETER (FORM='(A)' )
      CSIOCV=0
      IWD=LEN(CV)
      IF(IFMTST.NE.0)RETURN
      IF(IFMT.EQ.0)THEN
        IF(KEYRW.EQ.2)THEN
* OUTPUT IN FREE FORMAT A
          IF(IBF.EQ.1)THEN
            BUF(1:1)=' '
            IBF=2
          ENDIF
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
          WRITE(BUF(IBF:IBF1),FORM,
     +          IOSTAT=ISTA, ERR=111)CV
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
 22       IF(JSTR.NE.0)THEN

            IF(LIOERR.EQ.-1) THEN
              PRINT *,
     +' Read character string in free format from internal file or'
              PRINT *,' missing '' in character constant '
            ENDIF
            IFMTST=6
            RETURN
          ENDIF
          IF(BUF(K:K).EQ.'''')THEN
            K=K+1
            IBF=K
            JJ=MJSCHA(BUF(IBF:))
            JS=JJ
            L=LBUF-K+1
            IH=CSICNS(JJ,L,NSTR)
            IF(IH.EQ.0)THEN
            IF(LIOERR.EQ.-1) PRINT *,' Expected character ''string'' '
              IFMTST=2
              RETURN
            ENDIF
            IBF1=IBF+JJ-JS-1
            J=MJSCHA(CV)
            CV=' '
            L=MIN0(LEN(CV),NSTR)
            CALL CCOPYS(MJCHAR(IQ(IH)),J,L)
            CALL MHFREE(IH)
          ELSE
            LENCV=LEN(CV)
            CV=' '
            I=1
            DO 31 IBF1=K,LBUF
              IF(     BUF(IBF1:IBF1).EQ.' '
     +           .OR. BUF(IBF1:IBF1).EQ.',')GO TO 32
              IF(I.LE.LENCV)CV(I:I)=BUF(IBF1:IBF1)
              I=I+1
  31        CONTINUE
            GO TO 33
  32        IF(BUF(IBF1:IBF1).EQ.',')THEN
              IF(IBF1.LT.LBUF)IBF1=IBF1+1
            ENDIF
  33        CONTINUE
          ENDIF
        ENDIF
      ELSE
        CALL CSFMTC(0)
        IF(IFMTST.NE.0)RETURN
        IF(IWIDTH.EQ.0)IWIDTH=IWD
        IBF1=IBF+IWIDTH-1
        BUFFER='('//FMT(IPOS1:IPOS2)//')'
        IBUFF = IPOS2-IPOS1+3
        IF(KEYRW.EQ.2)THEN
          WRITE(BUF(IBF:IBF1),BUFFER(1:IBUFF),
     +          IOSTAT=ISTA, ERR=111)CV
        ELSE
          READ (BUF(IBF:IBF1),BUFFER(1:IBUFF),
     +          IOSTAT=ISTA, ERR=111)CV
        ENDIF
      ENDIF
      IBF=IBF1+1
      IF(IBF.GT.IBFLST)IBFLST=IBF
      CSIOCV=1
      RETURN
 111  IFMTST=ISTA
      IF(LIOERR.EQ.-1) PRINT *,' i/o error during conversion'
      END



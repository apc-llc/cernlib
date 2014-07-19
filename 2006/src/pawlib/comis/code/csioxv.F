*
* $Id: csioxv.F,v 1.1.1.1 1996/02/26 17:16:24 mclareni Exp $
*
* $Log: csioxv.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:24  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.16/23 24/10/93  12.06.54  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      INTEGER FUNCTION CSIOXV(CXV)
      INTEGER CSCXNU, CSIORV
      COMPLEX CXV,CX
      REAL RE,IM,REIM(2)
      EQUIVALENCE (CX,REIM), (RE,REIM(1)), (IM,REIM(2))
#include "comis/csfmt.inc"
**      DOUBLE PRECISION DNUM
*un      COMMON/MDPOOL/IQ(99)
      CHARACTER *(*)FORM
      PARAMETER (FORM='(2H ( ,G14.7,1H, ,G14.7,1H) )', IWD=32)
      CSIOXV=0
      IF(IFMTST.NE.0)RETURN
      IF(IFMT.EQ.0)THEN
        IF(KEYRW.EQ.2)THEN
* OUTPUT IN FREE FORMAT G14.7
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
     +          IOSTAT=ISTA, ERR=111)CXV
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
          K=CSCXNU(JJ,L,CXV)
          IBF1=IBF+JJ-JS-1
          IF(K.NE.7)THEN
            IF(LIOERR.EQ.-1) PRINT *,' Expected complex number'
            IFMTST=2
            RETURN
          ELSE
            IF(ISIGN.EQ.1)CXV=-CXV
          ENDIF
        ENDIF
      ELSE
        CX=CXV
        CSIOXV=CSIORV(RE)
        CSIOXV=CSIORV(IM)
        IF(KEYRW.EQ.1)CXV=CX
        RETURN
      ENDIF
      IBF=IBF1+1
      IF(IBF.GT.IBFLST)IBFLST=IBF
      CSIOXV=1
      RETURN
 111  IFMTST=ISTA
      IF(LIOERR.EQ.-1) PRINT *,' i/o error during conversion'
      END

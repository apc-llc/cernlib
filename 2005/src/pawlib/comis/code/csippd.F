*
* $Id: csippd.F,v 1.1.1.1 1996/02/26 17:16:28 mclareni Exp $
*
* $Log: csippd.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:28  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/01 23/03/94  10.13.55  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      INTEGER FUNCTION CSIPPD(J,N,MD)
*unused+SEQ,CSPAR.
*unused+SEQ,CSDPVS.
#include "comis/csichv.inc"
      INTEGER MD(11)
      INTEGER ENTRY, KI, KR, KDD, KC, KE, KSTAR
      SAVE ENTRY, KI, KR, KDD, KC, KE, KSTAR
      DATA ENTRY/0/
      IF(ENTRY.EQ.0)THEN
        KI   =ICHAR('I')
        KR   =ICHAR('R')
        KDD  =ICHAR('D')
        KC   =ICHAR('C')
        KE   =ICHAR('E')
        KSTAR=ICHAR('*')
        ENTRY=1
      ENDIF
      M=2
      K=MKBLAN(J,N)
      IF(K.NE.ICHBRA)GO TO 3
 1    J=J+1
      N=N-1
      K=MKBLAN(J,N)
      IF(N.LE.0)GO TO 3
      IF(K.EQ.KI)THEN
        MD(M)=-101
      ELSEIF(K.EQ.KR)THEN
        MD(M)=-102
      ELSEIF(K.EQ.KDD)THEN
        MD(M)=-105
      ELSEIF(K.EQ.KC)THEN
        MD(M)=-107
      ELSEIF(K.EQ.KE)THEN
        MD(M)=-7
      ELSEIF(K.EQ.KSTAR)THEN
        J=J+1
        N=N-1
        I=MLNUMB(J,N,MD(M))
        IF(I.LE.0)GO TO 3
        J=J-1
        N=N+1
      ELSE
        IF(K.EQ.ICHKET)GO TO 2
        GO TO 3
      ENDIF
      J=J+1
      N=N-1
      M=M+1
      K=MKBLAN(J,N)
      IF(K.EQ.ICHCOMM)GO TO 1
      IF(K.NE.ICHKET)GO TO 3
 2    MD(1)=M-2
      CSIPPD=M-2
      RETURN
 3    CSIPPD=-1
      END

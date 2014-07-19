*
* $Id: paxcut.F,v 1.1.1.1 1996/03/01 11:38:44 mclareni Exp $
*
* $Log: paxcut.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:44  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.03/07 28/08/93  00.18.04  by  Alfred Nathaniel
*-- Author :    Alfred Nathaniel   27/08/93
      SUBROUTINE PAXCUT(CHIN,CHOUT)
      CHARACTER*(*) CHIN,CHOUT
      CHARACTER CHUP*8
*
* expand cut identifiers in CHIN and return result in CHOUT
*

      CHOUT = ' '
      K = 1
      I = 1
      L = LENOCC(CHIN)

 1    CONTINUE
      IF(I.GT.L) RETURN

      IF(CHIN(I:I).EQ.' ') THEN
         I = I + 1
         GOTO 1
      ENDIF

      IF(CHIN(I:I).EQ.'$') THEN
         N1 = INDEX('0123456789',CHIN(I+1:I+1))
         IF(N1.NE.0) THEN
            N = N1 - 1
            I = I + 2
            N2 = INDEX('0123456789',CHIN(I:I))
            IF(N2.NE.0) THEN
               N = N * 10 + N2 - 1
               I = I + 1
            ENDIF

 2          CONTINUE
            IF(I.LE.L .AND. CHIN(I:I).EQ.' ') THEN
               I = I + 1
               GOTO 2
            ENDIF

            CALL PAQCUT(CHOUT(K:),N)
            IF(CHOUT(K:).EQ.' ') THEN
               CHOUT(K:K) = '$'
               K = K + 1
               IF(N.LT.10) THEN
                  I = I - 1
               ELSE
                  I = I - 2
               ENDIF
               GOTO 1
            ENDIF

            CALL CLTOU(CHOUT(K:))
            NEEDP = 0
            IF(K.GT.5) THEN
               CHUP = CHOUT(K-5:K-1)
               CALL CLTOU(CHUP)
*
* need () if ".NOT.$1" and $1 contains .OR. or .AND.
*      or if ".AND.$1" and $1 contains .OR.
*      or if "$1.AND." and $1 contains .OR.
*
               IF(CHUP.EQ.'.NOT.') THEN
                  IF(INDEX(CHOUT(K:),'.OR.').NE.0
     +                 .OR. INDEX(CHOUT(K:),'.AND.').NE.0) THEN
                     NEEDP = 1
                  ENDIF
               ELSEIF(CHUP.EQ.'.AND.'
     +                 .AND. INDEX(CHOUT(K:),'.OR.').NE.0) THEN
                  NEEDP = 1
               ENDIF
            ENDIF

            IF(NEEDP.EQ.0) THEN
               CHUP = CHIN(I:I+4)
               CALL CLTOU(CHUP)
               IF(CHUP.EQ.'.AND.'
     +              .AND. INDEX(CHOUT(K:),'.OR.').NE.0) THEN
                  NEEDP = 1
               ENDIF
            ENDIF

            IF(NEEDP.EQ.0) THEN
               K = LENOCC(CHOUT) + 1
            ELSE
               CHOUT(K:K) = '('
               CALL PAQCUT(CHOUT(K+1:),N)
               K = LENOCC(CHOUT) + 2
               CHOUT(K-1:K-1) = ')'
            ENDIF
            GOTO 1
         ENDIF
      ENDIF

      CHOUT(K:K) = CHIN(I:I)
      K = K + 1
      I = I + 1

      GOTO 1
      END

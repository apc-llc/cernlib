*
* $Id: xisxxtra.inc,v 1.1.1.1 1996/02/15 17:49:01 mclareni Exp $
*
* $Log: xisxxtra.inc,v $
* Revision 1.1.1.1  1996/02/15 17:49:01  mclareni
* Kernlib
*
*
*
* xisxxtra.inc
*
  50      LXI1  =  1
          LXII  =  1
          DO 56     I  =  1, M
             S51   =  ZERO
             LXIJ  =  LXI1
             DO 51  J  =  1, N
                S51   =  SQRF(X(LXIJ),S51)
                LXIJ  =  LXIJ + JX
  51            CONTINUE
             T(1)  =  S51
             IF(I .EQ. M)  GOTO 54
             LXK1  =  LXI1 + IX
             LTK  =  2
             DO 53  KDASH  =  I+1, M
                S52   =  ZERO
                LXIJ  =  LXI1
                LXKJ  =  LXK1
                DO 52  J  =  1, N
                   S52   =  DOTF(X(LXIJ),X(LXKJ),S52)
                   LXIJ  =  LXIJ + JX
                   LXKJ  =  LXKJ + JX
  52               CONTINUE
                T(LTK)  =  S52
                LXK1    =  LXK1 + IX
                LTK     =  LTK + 1
  53            CONTINUE
  54         LXIK  =  LXII
             LTK   =  1
             DO 55  KDASH  =  I, M
                X(LXIK)  =  T(LTK)
                LXIK     =  LXIK + JX
                LTK      =  LTK + 1
  55            CONTINUE
             LXI1     =  LXI1 + IX
             LXII     =  LXII + IX + JX
  56         CONTINUE
          IF(M .EQ. 1)  RETURN
          LXII  =  1
          DO 58     I  =  1, M-1
             LXIK  =  LXII + JX
             LXKI  =  LXII + IX
             DO 57  KDASH  =  I+1, M
                X(LXKI)  =  CNJF( X(LXIK) )
                LXIK     =  LXIK + JX
                LXKI     =  LXKI + IX
  57            CONTINUE
             LXII  =  LXII + IX + JX
  58         CONTINUE
          RETURN

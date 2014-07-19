*
* $Id: zisxy.inc,v 1.1.1.1 1996/02/15 17:49:01 mclareni Exp $
*
* $Log: zisxy.inc,v $
* Revision 1.1.1.1  1996/02/15 17:49:01  mclareni
* Kernlib
*
*
*
* zisxy.inc
*
          IF(LOCZ .EQ. LOCX)  GOTO 30
          IF(LOCZ .EQ. LOCY)  GOTO 40
          IF(LOCX .EQ. LOCY)  GOTO 20
  10      LY1L  =  1
          LZ1L  =  1
          DO 13     L  =  1, K
             LXI1  =  1
             LZIL  =  LZ1L
             DO 12  I  =  1, M
                S11   =  ZERO
                LXIJ  =  LXI1
                LYJL  =  LY1L
                DO 11  J  =  1, N
                   S11   =  DOTF(X(LXIJ),Y(LYJL),S11)
                   LXIJ  =  LXIJ + JX
                   LYJL  =  LYJL + JY
  11               CONTINUE
                Z(LZIL)  =  S11
                LXI1     =  LXI1 + IX
                LZIL     =  LZIL + IZ
  12            CONTINUE
             LY1L  =  LY1L + LY
             LZ1L  =  LZ1L + LZ
  13         CONTINUE
          RETURN
  20      IF(M .NE. K  .OR.  IX .NE. LY  .OR.  JX .NE. JY)  GOTO 10
          LXI1  =  1
          LZII  =  1
          DO 24     I  =  1, M
             S21   =  ZERO
             LXIJ  =  LXI1
             DO 21  J  =  1, N
                S21   =  SQRF(X(LXIJ),S21)
                LXIJ  =  LXIJ + JX
  21            CONTINUE
             Z(LZII)  =  S21
             IF(I .EQ. M)  GOTO 24
             LXK1  =  LXI1 + IX
             LZIK  =  LZII + LZ
             LZKI  =  LZII + IZ
             DO 23  KDASH  =  I+1, M
                S22   =  ZERO
                LXIJ  =  LXI1
                LXKJ  =  LXK1
                DO 22  J  =  1, N
                   S22   =  DOTF(X(LXIJ),X(LXKJ),S22)
                   LXIJ  =  LXIJ + JX
                   LXKJ  =  LXKJ + JX
  22               CONTINUE
                Z(LZIK)  =  S22
                Z(LZKI)  =  CNJF( Z(LZIK) )
                LXK1  =  LXK1 + IX
                LZIK  =  LZIK + LZ
                LZKI  =  LZKI + IZ
  23            CONTINUE
             LXI1  =  LXI1 + IX
             LZII  =  LZII + IZ + LZ
  24         CONTINUE
          RETURN
  30      IF(LOCX .EQ. LOCY)  GOTO 50
          LXI1  =  1
          DO 34     I  =  1, M
             LY1L  =  1
             LTL   =  1
             DO 32  L  =  1, K
                S31   =  ZERO
                LXIJ  =  LXI1
                LYJL  =  LY1L
                DO 31  J  =  1, N
                   S31   =  DOTF(X(LXIJ),Y(LYJL),S31)
                   LXIJ  =  LXIJ + JX
                   LYJL  =  LYJL + JY
  31               CONTINUE
                T(LTL)  =  S31
                LY1L    =  LY1L + LY
                LTL     =  LTL + 1
  32            CONTINUE
             LXIL  =  LXI1
             LTL   =  1
             DO 33  L  =  1, K
                X(LXIL)  =  T(LTL)
                LXIL     =  LXIL + JX
                LTL      =  LTL + 1
  33            CONTINUE
             LXI1  =  LXI1 + IX
  34         CONTINUE
          RETURN
  40      LY1L  =  1
          DO 44     L  =  1, K
             LXI1  =  1
             LTI   =  1
             DO 42  I  =  1, M
                S41   =  ZERO
                LXIJ  =  LXI1
                LYJL  =  LY1L
                DO 41  J  =  1, N
                   S41   =  DOTF(X(LXIJ),Y(LYJL),S41)
                   LXIJ  =  LXIJ + JX
                   LYJL  =  LYJL + JY
  41               CONTINUE
                T(LTI)  =  S41
                LXI1    =  LXI1 + IX
                LTI     =  LTI + 1
  42            CONTINUE
             LYIL  =  LY1L
             LTI   =  1
             DO 43  I  =  1, M
                Y(LYIL)  =  T(LTI)
                LYIL     =  LYIL + JY
                LTI      =  LTI + 1
  43            CONTINUE
             LY1L  =  LY1L + LY
  44         CONTINUE
          RETURN

*
* $Id: btswapcr.s,v 1.1.1.1 1996/03/08 15:21:54 mclareni Exp $
*
* $Log: btswapcr.s,v $
* Revision 1.1.1.1  1996/03/08 15:21:54  mclareni
* Epio
*
*
#if defined(CERNLIB_CRAY)
*         SUBROUTINE BTSWAP(A,N)
*
*         The subroutine expects N 8-bit bytes in array A, stored
*         contiguously from left to right, 8 bytes per 64-bit word.
*         If N<2, the subroutine returns control without action.
*         If N>1, N'=N/2 pairs of bytes will be transposed.
*         If N' is not a multiple of 4, the unused field to the
*         right of the last word will be unchanged.
*
*         H. Lipps, CERN, 12.6.1986.
*
          MACRO
          RESIDUE   N,L,W               S.L=MOD(N-1,64)+1
*
          S.W       -S.N                S.W=-N
          S.L       <6                  S.L=0...077
          S.L       #S.W&S.L            S.L=MOD(N-1,64)
          S.W       <1                  S.W=1
          S.L       S.L+S.W             S.L=MOD(N-1,64)+1
RESIDUE   ENDM
*
          IDENT     BTSWAP
ARRAY     DEFARG                        SYNONYM FOR A
BYTES     DEFARG                        SYNONYM FOR N
BTSWAP    ENTER     NP=2                (A,N)
*
          ARGADD    A1,ARRAY                      A1=(A(1))
          ARGADD    A2,BYTES                      A2=(N)
          S1        A1                            S1=(A(1))
          S2        ,A2                           S2=N
          S3        2                             S3=2
          S4        MASK,0                        S4=FF00FF00FF00FF00
          A3        64                            A3=64
          A6        3                             A6=3
          S0        S2-S3                         S0=N-2
          JSM       %3                  IF(N .LT. 2) GOTO 3
*
          S6        S2,S6>A6                      S6=MOD(N,8)*2**61+JUNK
          S2        S2>3                          S2=N/8=M (FULL WORDS)
          S6        S6>62                         S6=MOD(N,8)/2=NP
          A7        S2                            A7=M
          S0        S6                            S0=NP
          S6        S6<4                          S6=16*NP (=FIELD SIZE)
          JSZ       %1                  IF NO PARTIAL WORD GOTO 1
*
          A7        A1+A7                         A7=(A(M+1))
          S7        ,A7                           S7=A(M+1)
          A6        S6                            A6=16*NP
          A6        A3-A6                         A6=NUMBER OF BITS SAVED
          S6        S7,S6>A6                      S6=UNUSED FIELD
          S7        S0,S7>A6                      S7=ABCDEFGH, WHERE
*                                                      UPPER FIELD IS JUNK
          S5        S7&S4                         S5=A0C0E0G0
          S7        S7<8                          S7=BCDEFGH0
          S5        S5>8                          S5=0A0C0E0G
          S7        S7&S4                         S7=B0D0F0H0
          S7        S7!S5                         S7=BADCFEHG
          S7        S7,S6<A6                      RESTORE UNUSED FIELD
          ,A7       S7                            STORE PARTIAL WORD
*
%1        S0        S2                            S0=M
          JSZ       %3                  IF(M .EQ. 0) GOTO 3
*
          VL        A3                  VL=64
          S5        -1                            S5=FF...FF
          VM        S5                  VM=TRUE
          A2        8                             A2=8 (BYTE SIZE)
          V0        S4!V7&VM            V0=$(FF00FF00F...),...
          V1        V0>A2               V1=$(00FF00FF0...),...
          RESIDUE   2,3,4                         S3=MOD(M-1,64)+1=L
*
%2        A3        S3                            A3=L
          VL        A3                  VL=L
          S2        S2-S3                         S2=M-L
          A0        S1                            A0=(A(*))
          V2        ,A0,1               V2=$(ABCDEFGH),...
          V3        V2&V0               V3=$(A0C0E0G0),...
          V4        V3>A2               V4=$(0A0C0E0G),...
          V5        V2&V1               V5=$(0B0D0F0H),...
          V6        V5<A2               V6=$(B0D0F0H0),...
          V7        V4!V6               V7=$(BADCFEHG),...
          ,A0,1     V7                  STORE A(*),A(*+1),...,A(*+L-1)
          S1        S1+S3                         S1=(A(*+L))
          S0        S2                            S0=M-L
          S3        64                            S3=L=64
          JSN       %2                  IF(M .NE. 0) GOTO 2
*
%3        EXIT                          RETURN
*
MASK      CON       X'FF00FF00FF00FF00
          END
#endif

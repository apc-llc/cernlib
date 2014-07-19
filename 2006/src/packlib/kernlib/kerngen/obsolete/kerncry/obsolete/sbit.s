*
* $Id: sbit.s,v 1.1.1.1 1996/02/15 17:52:45 mclareni Exp $
*
* $Log: sbit.s,v $
* Revision 1.1.1.1  1996/02/15 17:52:45  mclareni
* Kernlib
*
*
          IDENT     SBIT
*
* CERN PROGLIB# M421    SBIT            .VERSION KERNCRY  1.00  830612
* ORIG. 12/06/83 H.LIPPS, CERN
*
SBIT      ENTER     NP=3                (A,X,LX)
          ARGADD    A5,1                A5=(A)
          ARGADD    A6,2                A6=(X)
          ARGADD    A7,3                A7=(LX)
          S7        1                   S7=1
          S5        ,A5                 S5=A
          S6        ,A6                 S6=X
          A7        ,A7                 A7=LX
          S1        S5&S7               S1=MOD(A,2)
          A7        A7-1                A7=LX-1
          S7        S7<A7               S7=1*2**(LX-1)
          S1        S1<A7               S1=MOD(A,2)*2**(LX-1)
          S6        S1!S6&S7            COPY BIT S7 OF S1 INTO S6
          ,A6       S6                  X=S6
          EXIT
          END
#ifdef CERNLIB_TCGEN_SBIT
#undef CERNLIB_TCGEN_SBIT
#endif

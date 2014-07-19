*
* $Id: jbit.s,v 1.1.1.1 1996/02/15 17:52:45 mclareni Exp $
*
* $Log: jbit.s,v $
* Revision 1.1.1.1  1996/02/15 17:52:45  mclareni
* Kernlib
*
*
          IDENT     JBIT
*
* CERN PROGLIB# M421    JBIT            .VERSION KERNCRY  1.00  830612
* ORIG. 12/06/83 H.LIPPS, CERN
*
JBIT      ENTER     NP=2                (A,LA)
          ARGADD    A6,1                A6=(A)
          ARGADD    A7,2                A7=(LA)
          S6        ,A6                 S6=A
          A7        ,A7                 A7=LA
          S7        1                   S7=1
          A7        A7-1                A7=LA-1
          S6        S6>A7               S6=A*S**(1-LA)
          S1        S6&S7               S1=MOD(S6,2)=JBIT
          EXIT
          END
#ifdef CERNLIB_TCGEN_JBIT
#undef CERNLIB_TCGEN_JBIT
#endif

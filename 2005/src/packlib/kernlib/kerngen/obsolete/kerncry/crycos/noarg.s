*
* $Id: noarg.s,v 1.1.1.1 1996/02/15 17:52:41 mclareni Exp $
*
* $Log: noarg.s,v $
* Revision 1.1.1.1  1996/02/15 17:52:41  mclareni
* Kernlib
*
*
         IDENT    NOARG
*
* CERN PROGLIB# Z029    NOARG           .VERSION KERNCRY  1.09  881102
* ORIG. 13/10/88 J-Ph Baud, CERN
*
NOARG    ENTER    NP=1,MODE=USER,TYPE=ADDRESS
         NUMARG   A0,ARGPTR=A6
         ARGADD   A5,1,ARGPTR=A6
         A2       B02
         A3       2,A2
         GETF,S2  S7,ARN,A3
         S1       S2
         $IF      AN
            0,A5  S1
         $ENDIF
         EXIT     MODE=USER
         END
#ifdef CERNLIB_TCGEN_NOARG
#undef CERNLIB_TCGEN_NOARG
#endif

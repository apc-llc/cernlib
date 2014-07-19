*
* $Id: qnexte.s,v 1.1.1.1 1996/02/15 17:52:37 mclareni Exp $
*
* $Log: qnexte.s,v $
* Revision 1.1.1.1  1996/02/15 17:52:37  mclareni
* Kernlib
*
*
         IDENT     QNEXTE
*
* CERN PROGLIB# Z041    QNEXTE          .VERSION KERNCRY  1.04  840816
* ORIG. 12/06/83 D.LELLOUCH, LPNHE
*
         EXT       QNEXT
         ENTRY QNEXTE
FIRST    BSSZ      4
QNEXTE   =         *
         A0        FIRST,
         JAN       NOTFIRST
         A1        1
         FIRST,    A1
         A2        B00
         A3        B01
         FIRST+1,  A2
         FIRST+2,  A3
         J         QNEXT
NOTFIRST A2        FIRST+1,
         A3        FIRST+2,
         B00       A2
         B01       A3
         J         QNEXT
         END
#ifdef CERNLIB_CCGEN_QNEXTE
#undef CERNLIB_CCGEN_QNEXTE
#endif
#ifdef CERNLIB_TCGEN_QNEXTE
#undef CERNLIB_TCGEN_QNEXTE
#endif

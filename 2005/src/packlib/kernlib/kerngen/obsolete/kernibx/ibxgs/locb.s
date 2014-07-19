*
* $Id: locb.s,v 1.1.1.1 1996/02/15 17:54:40 mclareni Exp $
*
* $Log: locb.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:40  mclareni
* Kernlib
*
*
LOCB     CSECT
*
* CERN PROGLIB#         LOCB            .VERSION KERNIBX  1.01  900524
*
**  This routine does nothing in AIX: the argument address is in GR0
**   on entry, and is to be returned in GR0 as the function value!
         ENTRY _locb_
_locb_   BCR   15,14
         END
#ifdef CERNLIB_CCGEN_LOCB
#undef CERNLIB_CCGEN_LOCB
#endif
#ifdef CERNLIB_TCGEN_LOCB
#undef CERNLIB_TCGEN_LOCB
#endif

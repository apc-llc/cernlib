*
* $Id: locf.s,v 1.1.1.1 1996/02/15 17:54:40 mclareni Exp $
*
* $Log: locf.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:40  mclareni
* Kernlib
*
*
LOCF     CSECT
*
* CERN PROGLIB#         LOCF            .VERSION KERNIBX  1.01  900524
*
*  AIX linkage conventions: arguemnt address is in GR0 on entry;
*    function value is to bne returned in GR0.
*
         ENTRY _locf_
_locf_   SRA   0,2           result = (arg address)/4
         BCR   15,14
         END
#ifdef CERNLIB_CCGEN_LOCF
#undef CERNLIB_CCGEN_LOCF
#endif
#ifdef CERNLIB_TCGEN_LOCF
#undef CERNLIB_TCGEN_LOCF
#endif

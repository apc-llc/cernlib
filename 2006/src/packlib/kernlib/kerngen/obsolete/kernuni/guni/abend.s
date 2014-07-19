*
* $Id: abend.s,v 1.1.1.1 1996/02/15 17:53:56 mclareni Exp $
*
* $Log: abend.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:56  mclareni
* Kernlib
*
*
 .                                      LAST MODIF.  01/09/76
          AXR$
 .    CALL ABEND             ABNORMAL JOB-STEP TERMINATION
 .                           STRASSBURG, AUG 1976
$(1)
ABEND*
          ER        EABT$
          END
#ifdef CERNLIB_TCGEN_ABEND
#undef CERNLIB_TCGEN_ABEND
#endif

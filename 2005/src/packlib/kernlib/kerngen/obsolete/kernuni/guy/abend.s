*
* $Id: abend.s,v 1.1.1.1 1996/02/15 17:53:59 mclareni Exp $
*
* $Log: abend.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:59  mclareni
* Kernlib
*
*
. *****************************         LAST MODIF.
.
 .    CALL ABEND             ABNORMAL JOB-STEP TERMINATION
 .                           STRASSBURG, AUG 1976
          AXR$
$(1)
ABEND*
          ER        EABT$
          END
#ifdef CERNLIB_TCGEN_ABEND
#undef CERNLIB_TCGEN_ABEND
#endif

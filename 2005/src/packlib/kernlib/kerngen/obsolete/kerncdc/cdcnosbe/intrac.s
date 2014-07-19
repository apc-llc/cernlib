*
* $Id: intrac.s,v 1.1.1.1 1996/02/15 17:51:28 mclareni Exp $
*
* $Log: intrac.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:28  mclareni
* Kernlib
*
*
          IDENT       INTRAC
*
* CERN PROGLIB# Z044    INTRAC          .VERSION KERNCDC  2.14  850320
* ORIG. 20/02/83  HRR FROM GENLIB
*
*     LOGICAL FUNCTION INTRAC(DUMMY)
*   RETURNS  INTRAC = .TRUE. IF CALLED IN AN INTERACTIVE JOB
*
          ENTRY       INTRAC
 INTRAC   EQ          *+400000B
          SX7         B0
          SA7         RWEP+1
          SB7         1
          SA2         RWEP
          BX7         X2
 +        SA3         B7
          NZ          X3,*
          SA7         B7
 +        SA3         RWEP+1
          ZR          X3,*
          AX6         B7,X3
          LX6         59
          AX6         58
          SA6         X1
          EQ          INTRAC
 RWEP     VFD         42/0LRWEP,18/*+1
          BSSZ        1
          END
#ifdef CERNLIB_TCGEN_INTRAC
#undef CERNLIB_TCGEN_INTRAC
#endif

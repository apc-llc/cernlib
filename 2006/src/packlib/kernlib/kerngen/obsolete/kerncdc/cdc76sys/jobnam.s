*
* $Id: jobnam.s,v 1.1.1.1 1996/02/15 17:51:30 mclareni Exp $
*
* $Log: jobnam.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:30  mclareni
* Kernlib
*
*
          IDENT JOBNAM
*
* CERN PROGLIB# Z100    JOBNAM          .VERSION KERNCDC  1.12  801111
*
          ENTRY  JOBNAM
*     JOB = JOBNAM (JOB)
 NAME     VFD 42/0HJOBNAM,18/JOBNAM
 OU.NAME  EQU    775B
 ANSWER   BSS    1
 JOBNAM   EQ     *+1BS17
          MJ     OU.NAME
 -        VFD    30/ANSWER
          SA2    ANSWER
          BX6    X2
          SA6    X1
          EQ     JOBNAM
          END
#ifdef CERNLIB_CDCSYS_JOBNAM
#undef CERNLIB_CDCSYS_JOBNAM
#endif

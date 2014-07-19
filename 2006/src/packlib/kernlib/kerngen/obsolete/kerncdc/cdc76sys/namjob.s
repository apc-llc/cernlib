*
* $Id: namjob.s,v 1.1.1.1 1996/02/15 17:51:30 mclareni Exp $
*
* $Log: namjob.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:30  mclareni
* Kernlib
*
*
          IDENT NAMJOB
*
* CERN PROGLIB#         NAMJOB          .VERSION KERNCDC  0.1   760623
*
          ENTRY  NAMJOB
*     JOB = NAMJOB (JOB)
 NAME     VFD 42/0HNAMJOB,18/NAMJOB
 OU.NAME  EQU    775B
 ANSWER   BSS    1
 NAMJOB   EQ     *+1BS17
          MJ     OU.NAME
 -        VFD    30/ANSWER
          SA2    ANSWER
          BX6    X2
          SA6    X1
          EQ     NAMJOB
          END
#ifdef CERNLIB_CDCSYS_NAMJOB
#undef CERNLIB_CDCSYS_NAMJOB
#endif

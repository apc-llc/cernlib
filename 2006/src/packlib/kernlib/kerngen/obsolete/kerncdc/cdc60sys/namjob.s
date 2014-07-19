*
* $Id: namjob.s,v 1.1.1.1 1996/02/15 17:51:32 mclareni Exp $
*
* $Log: namjob.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:32  mclareni
* Kernlib
*
*
          IDENT NAMJOB
*
* CERN PROGLIB#         NAMJOB          .VERSION KERNCDC  1.03  771219
*
*     JOB = NAMJOB (JOB)
*               GET NAME OF JOB ON CDC 6000, SYSTEM SCOPE 3.4.3
*               P. FRENKIEL, COLLEGE DE FRANCE, PARIS, 23/11/77
*
          ENTRY NAMJOB
 TRACE    VFD   36/6HNAMJOB,24/NAMJOB
 STATUSA  VFD   6/0,18/1S17+25B,12/1,12/0,12/10B
 STATUS   BSSZ  2
 RETADR   BSSZ  1
 NAMJOB   BSSZ  1
          BX6   X1
          SA6   RETADR
          SA2   STATUSA
          BX7   X2
          SA7   STATUS
          MX6   0
          SA6   STATUS+1
          SYSTEM  ACT,RCL,STATUS
          SA3   STATUS+1
          MX0   42
          SA2   RETADR
          BX6   X0*X3
          SA6   X2
          EQ    NAMJOB
          END
#ifdef CERNLIB_CDCSYS_NAMJOB
#undef CERNLIB_CDCSYS_NAMJOB
#endif

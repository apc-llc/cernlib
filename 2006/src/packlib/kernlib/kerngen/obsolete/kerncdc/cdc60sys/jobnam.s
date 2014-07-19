*
* $Id: jobnam.s,v 1.1.1.1 1996/02/15 17:51:32 mclareni Exp $
*
* $Log: jobnam.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:32  mclareni
* Kernlib
*
*
          IDENT JOBNAM
*
* CERN PROGLIB# Z100    JOBNAM          .VERSION KERNCDC  2.14  850320
*
*     JOB = JOBNAM (JOB)
*               GET NAME OF JOB ON CDC 6000, SYSTEM SCOPE 3.4.3
*               P. FRENKIEL, COLLEGE DE FRANCE, PARIS, 23/11/77
*
          ENTRY JOBNAM
 TRACE    VFD   36/6HJOBNAM,24/JOBNAM
 STATUSA  VFD   6/0,18/1S17+25B,12/1,12/0,12/10B
 STATUS   BSSZ  2
 RETADR   BSSZ  1
 JOBNAM   BSSZ  1
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
          SA1   RETADR         X1= ADR(PARAMETER)
          BX6   X0*X3              KEEP FIRST 7 CHAR.
          MX2   54             X2= 00  IN POS. 10
          SX3   55B            X3= 55  IN POS. 10
 LOOP     BX6   X6+X3              MERGE BLANK INTO POS. N
          LX2   6              X2= 00 IN POS. N = N-1
          LX3   6              X3= 55 IN POS. N
          BX4   -X2*X6         X4= CHAR. N ISOLATED
          ZR    X4,LOOP
          SA6   X1
          EQ    JOBNAM
          END
#ifdef CERNLIB_CDCSYS_JOBNAM
#undef CERNLIB_CDCSYS_JOBNAM
#endif

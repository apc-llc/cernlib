*
* $Id: epinit.F,v 1.1.1.1 1996/03/08 15:21:44 mclareni Exp $
*
* $Log: epinit.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:44  mclareni
* Epio
*
*
#include "epio/pilot.h"
      SUBROUTINE EPINIT
C. INITIALIZES EP FORMAT READ AND WRITE PACKAGE
#include "epio/epiocom.inc"
#if defined(CERNLIB_VAX)||defined(CERNLIB_STF77VX)
#include "epio/epvaxcom.inc"
#endif
#if (!defined(CERNLIB_VAX))&&(!defined(CERNLIB_STF77VX))
#include "epio/epapocom.inc"
#endif
C--- NO. OF BITS / MACHINE WORD , LOGICAL OUTPUT UNIT (PRINTER DEF)
#if (defined(CERNLIB_CDC))&&(defined(CERNLIB_F4))
      DATA NBIT/60/ ,NOUT/6LOUTPUT/
#endif
#if (defined(CERNLIB_CDC))&&(!defined(CERNLIB_F4))
      DATA NBIT/60/ ,NOUT/L"OUTPUT"/
#endif
#if (defined(CERNLIB_SINGLE))&&(!defined(CERNLIB_CDC))
      DATA NBIT/64/,NOUT/6/
#endif
#if defined(CERNLIB_ND100B32)||defined(CERNLIB_ND50)||defined(CERNLIB_ND500)||defined(CERNLIB_DOUBLE)
      DATA NBIT/32/,NOUT/6/
#endif
#if defined(CERNLIB_ND100B16)
      DATA NBIT/16/,NOUT/6/
#endif
#if defined(CERNLIB_UNIVAC)
      DATA NBIT/36/,NOUT/2/
#endif
      SAVE IFIRST
      DATA IFIRST/0/
      IF (IFIRST.NE.0) RETURN
      IFIRST=1
C--- Monitor package usage.
      CALL UMLOG('EPIO','*')
C--- MAX. NO. OF UNITS SUPPORTED SIMULTANEOUSLY
      NMUNIT=10
C--- NO. OF CONTROL WORDS / UNIT
      NWUNIT=33
C--- NO. OF OVERALL CONTROL WORDS
      NCONT=8
C--- ZERO LAST USER UNIT AND LAST INTERNAL UNIT
      LASTUT=0
      LREF=0
C--- SET DEFAULTS FOR OVERALL CONTROL WORDS
      LIST(1)=0
      LIST(2)=100
      LIST(3)=0
      LIST(4)=NBIT
      LIST(5)=180
      LIST(6)=12
      LIST(7)=9
      LIST(8)=NOUT
*
*---- BLANK DEFAULT FILE NAMES FOR VAX, APOLLO and UNIX
*
      DO 10  I=1,NMUNIT
  10  EPVXUN(I)=' '
      RETURN
      END

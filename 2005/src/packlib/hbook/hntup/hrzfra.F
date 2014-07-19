*
* $Id: hrzfra.F,v 1.2 1998/09/25 09:28:48 mclareni Exp $
*
* $Log: hrzfra.F,v $
* Revision 1.2  1998/09/25 09:28:48  mclareni
* Modifications for the Mklinux port flagged by CERNLIB_PPC
*
* Revision 1.1.1.1  1996/01/16 17:07:59  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/11 23/08/94  14.19.39  by  Rene Brun
*-- Author :    Rene Brun   17/10/92
      SUBROUTINE HRZFRA(IH,IOH,NW)
*
***********************************************************************
*
*       Converts array of NW ASCII words into  EBCDIC hollerith
*
*  Author  : R.Brun CN/AS
*  Written : 17/10/92
*  Last mod: 17/10/92
*
***********************************************************************
#if defined(CERNLIB_IBM)
      DIMENSION IH(1), IOH(1)
      PARAMETER          (NCHAR = 126)
      DIMENSION IEB(NCHAR)
      DATA IEB /8*0,5,22*0,
     + 64, 90,127,123, 91,108, 80,125, 77, 93, 92, 78,107, 96, 75, 97,
     +240,241,242,243,244,245,246,247,248,249,122, 94, 76,126,110,111,
     +124,193,194,195,196,197,198,199,200,201,209,210,211,212,213,214,
     +215,216,217,226,227,228,229,230,231,232,233,173,224,189, 95,109,
     +121,129,130,131,132,133,134,135,136,137,145,146,147,148,149,150,
     +151,152,153,162,163,164,165,166,167,168,169,192, 79,208,161/
*-----------------------------------
*
      DO 20 IW=1,NW
         DO 10 I=1,25,8
            IBYTE=JBYT(IH(IW),I,8)
            IF(IBYTE.GT.126)IBYTE=32
            CALL SBYT(IEB(IBYTE),IOH(IW),I,8)
   10    CONTINUE
   20 CONTINUE
#endif
#if defined(CERNLIB_VAX)||defined(CERNLIB_DECS)||defined(CERNLIB_MSDOS)||(defined(CERNLIB_LINUX) && !defined(CERNLIB_PPC))||defined(CERNLIB_WINNT)
      DIMENSION IH(1), IOH(1)
*
      DO 20 IW=1,NW
         IB1=JBYT(IH(IW), 1,8)
         IB2=JBYT(IH(IW), 9,8)
         IB3=JBYT(IH(IW),17,8)
         IB4=JBYT(IH(IW),25,8)
         IOH(IW)=IB4
         CALL SBYT(IB3,IOH(IW), 9,8)
         CALL SBYT(IB2,IOH(IW),17,8)
         CALL SBYT(IB1,IOH(IW),25,8)
   20 CONTINUE
#endif
      END

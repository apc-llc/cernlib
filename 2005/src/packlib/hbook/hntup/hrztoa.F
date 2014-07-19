*
* $Id: hrztoa.F,v 1.2 1998/09/25 09:28:54 mclareni Exp $
*
* $Log: hrztoa.F,v $
* Revision 1.2  1998/09/25 09:28:54  mclareni
* Modifications for the Mklinux port flagged by CERNLIB_PPC
*
* Revision 1.1.1.1  1996/01/16 17:07:59  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/11 23/08/94  14.19.39  by  Rene Brun
*-- Author :    Rene Brun   17/10/92
      SUBROUTINE HRZTOA(IH,NW)
*
***********************************************************************
*
*       Converts array of NW EBCDIC hollerith words into ASCII
*
*  Author  : R.Brun CN/AS
*  Written : 17/10/92
*  Last mod: 17/10/92
*
***********************************************************************
#if defined(CERNLIB_IBM)
      DIMENSION IH(1)
      PARAMETER          (NCHAR = 256)
      DIMENSION IASCI(NCHAR)
      DATA IASCI/
     +4*32,9,58*32,32,10*32,46,60,40,43,124,38,9*32,33,36,42,41,59,94,
     +45,47,9*32,44,37,95,62,63,9*32,96,58,35,64,39,61,34,32,97,98,99,
     +100,101,102,103,104,105,7*32,106,107,108,109,110,111,112,113,114,
     +7*32,126,115,116,117,118,119,120,121,122,3*32,91,15*32,93,32,32,
     +123,65,66,67,68,69,70,71,72,73,6*32,125,74,75,76,77,78,79,80,81,
     +82,6*32,92,32,83,84,85,86,87,88,89,90,6*32,48,49,50,51,52,53,54,
     +55,56,57,7*32/
*-----------------------------------
*
      DO 20 IW=1,NW
         DO 10 I=1,25,8
            IBYTE=JBYT(IH(IW),I,8)
            CALL SBYT(IASCI(IBYTE),IH(IW),I,8)
   10    CONTINUE
   20 CONTINUE
#endif
#if defined(CERNLIB_VAX)||defined(CERNLIB_DECS)||defined(CERNLIB_MSDOS)||(defined(CERNLIB_LINUX) && !defined(CERNLIB_PPC))||defined(CERNLIB_WINNT)
      DIMENSION IH(1)
*
      DO 20 IW=1,NW
         IB1=JBYT(IH(IW), 1,8)
         IB2=JBYT(IH(IW), 9,8)
         IB3=JBYT(IH(IW),17,8)
         IB4=JBYT(IH(IW),25,8)
         IH(IW)=IB4
         CALL SBYT(IB3,IH(IW), 9,8)
         CALL SBYT(IB2,IH(IW),17,8)
         CALL SBYT(IB1,IH(IW),25,8)
   20 CONTINUE
#endif
      END

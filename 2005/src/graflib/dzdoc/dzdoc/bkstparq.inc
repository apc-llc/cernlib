*
* $Id: bkstparq.inc,v 1.1.1.1 1996/03/04 16:12:58 mclareni Exp $
*
* $Log: bkstparq.inc,v $
* Revision 1.1.1.1  1996/03/04 16:12:58  mclareni
* Dzdoc/Zebpack
*
*
#ifndef CERNLIB_DZDOC_BKSTPARQ_INC
#define CERNLIB_DZDOC_BKSTPARQ_INC
*
*   Description of the bank state parameters bank parser
*
* bkstparq.inc
*
 
C--               General information
      PARAMETER ( ISGENQ = 1)
C--              Link description
      PARAMETER ( ISLINQ = ISGENQ+1)
C--              ref Link description
      PARAMETER ( ISRLIQ = ISLINQ+1)
C--               Status bit description
      PARAMETER ( ISBITQ = ISRLIQ+1)
C--               Data description
      PARAMETER ( ISDATQ = ISBITQ+1)
C--               Choice section
      PARAMETER ( ISCHOQ = 16)
C--               Repetition section
      PARAMETER ( ISREPQ = 32)
 

#endif

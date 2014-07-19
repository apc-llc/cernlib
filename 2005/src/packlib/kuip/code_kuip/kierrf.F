*
* $Id: kierrf.F,v 1.1.1.1 1996/03/08 15:32:55 mclareni Exp $
*
* $Log: kierrf.F,v $
* Revision 1.1.1.1  1996/03/08 15:32:55  mclareni
* Kuip
*
*
#include "kuip/pilot.h"
*CMZ :  2.01/06 30/03/93  13.08.57  by  Alfred Nathaniel
*-- Author :    Alfred Nathaniel   11/11/92
      SUBROUTINE KIERRF(CHOPT)
      CHARACTER CHOPT*(*)

*--- catch runtime errors

#if defined(CERNLIB_IBMALL)
      EXTERNAL ERRRUN

      IF(CHOPT.EQ.'TB') THEN
         INOAL  = 999
         INOMES = 999
         ITRACE = 2
      ELSEIF(CHOPT.EQ.'ON') THEN
         INOAL  = 999
         INOMES = -1
         ITRACE = 1
      ELSE
         INOAL  = 1
         INOMES = 999
         ITRACE = 2
      ENDIF
      CALL ERRSET(203,INOAL,INOMES,ITRACE,ERRRUN,204)
      CALL ERRSET(206,INOAL,INOMES,ITRACE,ERRRUN,207)
      CALL ERRSET(209,INOAL,INOMES,ITRACE,ERRRUN,229)
      CALL ERRSET(231,INOAL,INOMES,ITRACE,ERRRUN,239)
      CALL ERRSET(241,INOAL,INOMES,ITRACE,ERRRUN,301)
#endif
      END

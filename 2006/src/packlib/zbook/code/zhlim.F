*
* $Id: zhlim.F,v 1.1.1.1 1996/03/08 12:01:12 mclareni Exp $
*
* $Log: zhlim.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:12  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
      SUBROUTINE ZHLIM
C.
C.
C.    ******************************************************************
C.    *                                                                *
C.    *                                                                *
C.    *       FIX THE BOUNDARY BETWEEN ZBOOK AND HBOOK AREAS           *
C.    *       IN // IN ORDER TO PROVIDE SUFFICIENT SPACE TO HBOOK      *
C.    *       FOR FILLING AND PRINTING                                 *
C.    *                                                                *
C.    *                                                                *
C.    ******************************************************************
C.
      COMMON//IZ(1)
C
      COMMON/HFLAG /ID    ,IDBADD,LID   ,IDLAST,IDHOLD,NBIT  ,NBITCH,
     +       NCHAR ,NX0   ,NX1   ,NX2   ,INTER ,INDEX ,LAST  ,LIMIT ,
     +       LFIEL ,NEWHIS,NRLENG,NWLIB ,NWFLAG,NBFLAG,NWSTAT,NRHIST,
     +       IDISC ,LFHIST,LLHIST,NWHIST,IERR  ,NV    ,NRDIS ,IA2
C
C.
C.
C.    ------------------------------------------------------------------
C.
C.
C             NHBOOK IS THE SPACE NECESSARY FOR HBOOK
C             NX0    IS THE START ADRESS OF HBOOK IN //
C             NEWHIS HIGHEST CURRENT HBOOK LOCATION IN //
C
C             1000 WORDS ARE KEPT AS RESERVE FOR FILLING+PRINTING
C
      NHBOOK=NEWHIS-NX0+1000
C
C             NOW SET THE MAXIMUM SPACE FOR ZBOOK
C              (SEE ZHINIT DOCUMENTATION)
C
      JZ=IZ(1)
      IZ(JZ+11)=LIMIT-NHBOOK
C
      RETURN
      END

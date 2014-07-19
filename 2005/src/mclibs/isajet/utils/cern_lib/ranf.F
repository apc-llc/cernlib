*
* $Id: ranf.F,v 1.10 2001/10/08 14:03:14 mclareni Exp $
*
* $Log: ranf.F,v $
* Revision 1.10  2001/10/08 14:03:14  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
#if (defined(CERNLIB_RANFFTN))&&(defined(CERNLIB_NORANLUX))
          REAL FUNCTION RANF()
C
C         Kernlib routine G900 in CERN Program Library
C
          DOUBLE PRECISION    DRANF,    G900GT,   G900ST
          DOUBLE PRECISION    DS(2),    DM(2),    DSEED
          DOUBLE PRECISION    DX24,     DX48
          DOUBLE PRECISION    DL,       DC,       DU,       DR
          LOGICAL             SINGLE
          DATA      DS     /  1665 1885.D0, 286 8876.D0  /
          DATA      DM     /  1518 4245.D0, 265 1554.D0  /
          DATA      DX24   /  1677 7216.D0  /
          DATA      DX48   /  281 4749 7671 0656.D0  /
          SINGLE  =  .TRUE.
          GOTO 10
          ENTRY DRANF()
          SINGLE  =  .FALSE.
  10      DL  =  DS(1) * DM(1)
          DC  =  DINT(DL/DX24)
          DL  =  DL - DC*DX24
          DU  =  DS(1)*DM(2) + DS(2)*DM(1) + DC
          DS(2)  =  DU - DINT(DU/DX24)*DX24
          DS(1)  =  DL
          DR     =  (DS(2)*DX24 + DS(1)) / DX48
          IF(SINGLE)  THEN
             RANF  =  SNGL(DR)
          ELSE
             DRANF  =  DR
          ENDIF
          RETURN
          ENTRY G900GT()
          G900GT  =  DS(2)*DX24 + DS(1)
          RETURN
          ENTRY G900ST(DSEED)
          DS(2)  =  DINT(DSEED/DX24)
          DS(1)  =  DSEED - DS(2)*DX24
          G900ST =  DS(1)
          RETURN
          END
          SUBROUTINE RANGET(SEED)
          DOUBLE PRECISION    SEED,     G900GT,   G900ST,   DUMMY
          SEED  =  G900GT()
          RETURN
          ENTRY RANSET(SEED)
          DUMMY  =  G900ST(SEED)
          RETURN
          END
#endif

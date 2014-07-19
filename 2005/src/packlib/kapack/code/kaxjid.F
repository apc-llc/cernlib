*
* $Id: kaxjid.F,v 1.1.1.1 1996/03/08 11:40:53 mclareni Exp $
*
* $Log: kaxjid.F,v $
* Revision 1.1.1.1  1996/03/08 11:40:53  mclareni
* Kapack
*
*
#include "kapack/pilot.h"
      FUNCTION KAXJID()
*
*.....A FUNCTION TO RETURN A UNIQUE JOB IDENTIFIER
*
      CHARACTER DATE*8, TIME*8
#if defined(CERNLIB_NORD)||defined(CERNLIB_VAX)
*
*.....SPACE FOR COMMON SLATE IS NEEDED BY THE ASSEMBLER ROUTINE UHTOC
      COMMON /SLATE/ DUMMY(40)
      DIMENSION DATEN(2), TIMEN(2)
#endif
*
*-----------------------------------------------------------------------
*
#if (!defined(CERNLIB_NORD))&&(!defined(CERNLIB_VAX))
      CALL DATIMH(DATE,TIME)
#endif
#if defined(CERNLIB_NORD)||defined(CERNLIB_VAX)
      CALL DATIMH(DATEN,TIMEN)
      CALL UHTOC(TIMEN,4,TIME,8)
#endif
      READ(TIME,100) IHH, IMM, ISS
      KAXJID = IHH*10000 + IMM*100 + ISS
*
 100  FORMAT(I2,1X,I2,1X,I2)
*
      END

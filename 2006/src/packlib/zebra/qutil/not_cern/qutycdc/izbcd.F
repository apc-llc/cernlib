      FUNCTION IZBCD (MCHAR)

#include "zebra/zceta.inc"


      JV = SHIFT(MCHAR,6) .AND. 63
      IZBCD = IQCETA(JV+1)
      RETURN
      END

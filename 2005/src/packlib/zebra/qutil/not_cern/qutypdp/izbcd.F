      FUNCTION IZBCD (MCHAR)

#include "zebra/zceta.inc"


      JV = ISHFT (MCHAR,-29)
      IZBCD = IQCETA(JV+1)
      RETURN
      END

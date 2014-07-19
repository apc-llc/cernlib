      FUNCTION IZBCD (MCHAR)

#include "zebra/zceta.inc"


      JV = ishft (MCHAR,-56)
      IZBCD = IQCETA(JV+1)
      RETURN
      END

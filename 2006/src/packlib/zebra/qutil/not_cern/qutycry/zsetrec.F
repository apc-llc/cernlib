      SUBROUTINE ZSETREC (IPARAM)

C-    INITIALISE RECOVERY IN THE HYDRA CONTEXT

#include "zebra/qexchcry.inc"

      DATA  JCDID  /8/

      JREPCD = IPARAM
      IF (JREPCD.EQ.0)  JREPCD= 7776B

      CALL ZRECOK
      JCODE = 0
      RETURN
      END

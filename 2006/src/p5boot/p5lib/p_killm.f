CDECK  ID>, P_KILLM.
      SUBROUTINE P_KILLM (MSG)

      CHARACTER    MSG*(*)

      CALL PERRORF (' Perror has')
      CALL P_KILL (MSG)
      END

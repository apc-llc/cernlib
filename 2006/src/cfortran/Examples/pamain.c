#include <stdlib.h>
#include <cfortran.h>
#include <paw.h>
#include <kuip.h>

#define PAWC_SIZE 9000000

typedef struct { float PAW[PAWC_SIZE]; } PAWC_DEF;
#define PAWC COMMON_BLOCK(PAWC,pawc)
COMMON_BLOCK_DEF(PAWC_DEF,PAWC);

int main()
{
      int iwtyp;

      PAW(PAWC_SIZE,iwtyp);

      KUWHAG();

      PAEXIT();

      return 0;
}

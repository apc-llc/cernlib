#include <stdlib.h>
#include <stdio.h>
#include <cfortran.h>
#include <packlib.h>
 
#define PAWC_SIZE 50000
 
typedef float PAWC_DEF[PAWC_SIZE];
#define PAWC COMMON_BLOCK(PAWC,pawc)
COMMON_BLOCK_DEF(PAWC_DEF,PAWC);
 
main()
{
  const int nmax = 300;
  int record_size=1024;
  int istat, n, icycle;
 
  struct { float r; } mycwn; 

  HLIMIT(PAWC_SIZE);
 
  HROPEN(1,"MYCWN","mycwn.hbook","NX",record_size,istat);
  if (istat) {
    printf("Error in opening file ...");
    return EXIT_FAILURE;
  }
 
  HBNT(1,"MYCWN"," ");
  HBNAME(1,"BLOCK",mycwn.r,"R:R*4");
 
  for (n=0; n<=nmax; n++) {
    mycwn.r = n*n;
    HFNT(1);
  }
 
  HROUT(0,icycle," ");
  HREND("MYCWN");
  KUCLOS(1," ",1);
}

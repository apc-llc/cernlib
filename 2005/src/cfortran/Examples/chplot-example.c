/*
 * This example demonstrate how to call HPLOT from C.
 *
 * For question/problems contact: Heplib.Support@cern.ch
 *
 */

#include <stdlib.h>
#include <cfortran.h>
#include <graflib.h>
#include <packlib.h>
#include <kernlib.h>

#if defined(__hpux) || defined(_IBMR2)
# define extname
#endif

#define PAWC_SIZE 50000

typedef struct { float PAW[PAWC_SIZE]; } PAWC_DEF;
#define PAWC COMMON_BLOCK(PAWC,pawc)
COMMON_BLOCK_DEF(PAWC_DEF,PAWC);

main()
{
   int istat;
   char c;

   HLIMIT(PAWC_SIZE);
   HPLINT(4);
/*
 * Histograms booking and filling
 */
   HBOOK1(1,"x",100,-4.,4.,0.);
   HBOOK1(2,"y",100,-4.,4.,0.);
   HBOOK1(3,"(x-y)/(x+y)",100,-6.,6.,0.);
   fill();
/*
 * Histograms plotting
 */
   KUOPEN(77,"chplot-example.ps","NEW",istat);
   IGMETA(77,-111);
   HPLZON(2,2,1," ");
   HPLOT(1," "," ",0);
   HPLOT(2," "," ",0);
   HPLZON(1,2,2,"S");
   HPLOT(3," "," ",0);
   IGMETA(999,0);
   KUCLOS(77," ",1);
   c = getchar();  
   HPLEND();
}

fill()
{
   float x,y;
   int i;

   for ( i=1; i<1001; i++) {
      RANNOR(x,y);
      HF1(1,x,1.);
      HF1(2,y,1.);
      HF1(3,(x-y)/(x+y),1.);
   }
}

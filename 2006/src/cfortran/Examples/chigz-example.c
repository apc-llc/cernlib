/*
 * This example demonstrate how to call HIGZ from C.
 *
 * For question/problems contact: Heplib.Support@cern.ch
 *
 */

#include <cfortran.h>
#include <packlib.h>
#include <higz.h>

#if defined(__hpux) || defined(_IBMR2)
# define extname
#endif

#define PAWC_SIZE 50000

typedef struct { float PAW[PAWC_SIZE]; } PAWC_DEF;
#define PAWC COMMON_BLOCK(PAWC,pawc)
COMMON_BLOCK_DEF(PAWC_DEF,PAWC);

main()
{
   int itype,istat;
   char c;
/*
 * Initialisation 
 */
   MZEBRA(-3);
   MZPAW(PAWC_SIZE," ");
   IGINIT(0);
   IGWKTY(itype);
   IGSSE(6, itype);
/*
 * Some simple drawing
 */
   KUOPEN(77,"chigz-example.ps","NEW",istat);
   IGMETA(77,-111);
   IGRNG(21., 21.);
   IGBOX(1.0, 20.0, 1.0, 20.0);
   IGBOX(2.0, 19.0, 2.0, 19.0);
   IGBOX(3.0, 18.0, 3.0, 18.0);
   IGBOX(4.0, 17.0, 4.0, 17.0);
   IGSET("TXAL",23.);
   IGSET("CHHE",0.5);
   ITX (10.5,10.5,"HIGZ called from a C program");
   IGTERM();
   IGMETA(999,0);
   KUCLOS(77," ",1);
   c = getchar();  
   IGEND();
}

#define PQRY_KEYW	"PQRY"
#define INVALID_KEYW	"Invalid"
#define VERS_KEYW	"VERS"

#include "hbook/pilot.h"
#include <cfortran/cfortran.h>


/* PiafVers = 0 -> a PIAF server before new QP; >1 later versions */
static int PiafVers = 0;


/* Function to set the PIAF version */
void 
SetVers(int Version)
{
       PiafVers = Version;
}
FCALLSCSUB1(SetVers,SETPIAFVER,setpiafver,INT)


/* Function to get the PIAF version */
int
GetPiafVersion ()
{
	return PiafVers;
}
FCALLSCFUN1(INT,GetPiafVersion,IGETPIAFVER,igetpiafver,INT)


/* To allow the PAW client to query the Piaf master, the MESSAGE command   */
/* back door is used. This function checks the PIAF text output to 'grep'  */
/* the PQRY keyword; if the keyword is found some specific processing takes */
/* place, according to another keyword that identifies the name of the info */
/* actually retrieved */
void
GrepPqryStr (char *LogLine, int *retval)
{
  char s1[32],s2[32];
  int pv;
  if (strstr (LogLine,PQRY_KEYW) != NULL) {
    *retval = 1;
    /* PQRY's handler */
    if (strstr (LogLine,VERS_KEYW) != NULL) {
      /* There is a version message */
      if (strstr (LogLine,INVALID_KEYW) == NULL) {
	/* Querying the PIAF version */
	sscanf(LogLine,"%s %s %d",s1,s2,&pv);
	SetVers (pv);	
      } else {
        SetVers(0);
      }
    }
  } else {
    *retval = 0;
  }
}
FCALLSCSUB2(GrepPqryStr,GREPPQRY,greppqry,PSTRING,PINT)


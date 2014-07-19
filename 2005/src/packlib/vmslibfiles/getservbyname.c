
/*          R. Ball (University of Michigan) at the behest of           */
/*          J. Shiers (CERN/CN)                                         */
/*          Feb. 11, 1991                                               */
/*          Replace the non-supported UCX routine getservbyname.        */
/*          Input is the pseudo-Unix file SERVFILE[], a copy of         */
/*          /etc/services from a Unix machine supporting zftp/zserv.    */
/*          alias names are neither searched nor recorded.              */

#include <stdio.h>
#include <string.h>    /* strcmp */
#include <stdlib.h>    /* atoi */
#include <varargs.h>
#include <netdb.h>
#include <socket.h>
#include <in.h>

/*       From netdb.h, the servent structure definition follows:  */
/*  struct servent   {                  */
/*       char *s_name;                  */
/*       char **s_aliases;              */
/*       int s_port;                    */
/*       char *s_proto;                 */
/*       };                             */

struct servent *getservucxline (const char *name,const char *proto);

static char SERVFILE[] =   "cern:[pro.exe]etc.services";
static struct  servent  ucx_sp;
static char comp_str[80], pro_nam[80];

struct   servent  *
getservbyname ( service, protocol )
const char  *service, *protocol;
{
      struct   servent  *p;
      p = getservucxline ( service, protocol );
      return (p);
}

struct servent    *
getservucxline ( service, protocol )
const char  *service, *protocol;
{
      FILE     *inter;
      char     string[80], c, *copc;
      int      numser, max_string;
      char     num_str[80];
      char     *ipoint;
      int      i, j, k;

/* OPEN the file of services for read_only.  Loop, reading the file    */
/* until end of file or the services match.                            */
      inter = fopen( SERVFILE, "r" );
      while ( (ipoint=fgets( string, 80, inter)) != NULL)
        {
        for (i=0; ((c=string[i])!='#') && (i<80) && (c!='\n'); i++)
         max_string=i;
        if (c!='#') max_string++;
        if (string[0] != '#')
          {

/* First off, find the service name.  Be cheap, ignore the aliases.    */
          for (i=0; i<max_string; i++)
         {
         if ( ((c=string[i])!= ' ') && (c!='\t'))  comp_str[i] = c;
         else
             {
             comp_str[i] = '\0';
             break;
             }
         }
          if (strcmp(service,comp_str) == 0)
         {

/* Keep going as the service name matched.  Get the socket number.     */
         i++;
         for (j=i; j<max_string; j++)
             {
             if (((c=string[j]) != '\n') && (c != '/')) num_str[j-i] = c;
             else
            {
            num_str[j-i] = '\0';
            break;
            }
             }
         numser = htons((u_short)atoi(num_str));

/* Get the protocol name.  We may need to match it.                    */
         j++;
         for (k=j; k<=max_string; k++)
             {
             if ( ((c=string[k])!=' ') && (c!='\t') && (c!='\n'))
                 pro_nam[k-j] = c;
             else
            {
            pro_nam[k-j] = '\0';
            break;
            }
             }
         if ((protocol == NULL) || (strcmp(protocol, pro_nam) == 0))
             {

/* We have a successful match.  Fill the structure values and break.   */
             ucx_sp.s_port = numser;
             ucx_sp.s_name = comp_str;
             ucx_sp.s_proto = pro_nam;
/* Ignore the aliases.                                                 */
             break;
             }
         }
          }
        }

/* Clean up and exit.                                                  */
      fclose(inter);
      if (ipoint == NULL)  return(NULL);
      else        return(&ucx_sp);
}

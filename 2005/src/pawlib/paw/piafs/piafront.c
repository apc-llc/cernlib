
#include <fcntl.h>
#include <pwd.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <time.h>
#include <sys/stat.h>
#include <sys/param.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <stdlib.h>

#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>

#if defined(__osf__) || defined(__sgi) || defined(sun)
extern char *crypt(const char *, const char *);
#endif

#if defined(sun)
#define SHADOWPW
#endif

#ifdef SHADOWPW
#include <shadow.h>
#endif


#define MAX_SLAVES  32
#define CHECKUSERS "bin/lusers"
#define CHECKLOG   "uslog/lusers.log"
#define AKLOG      "bin/aklog"
#define SECUREFILE ".secure"

static int sockin = 0;
static int sockout = 1;
static int ismaster = 0;

#if !defined(__hpux)

static
int setresgid( gid_t r, gid_t e, gid_t s )
{
  if( setgid( r ) == -1 )
    return -1;
  return setegid( e );
}

static
int setresuid( uid_t r, uid_t e, uid_t s )
{
  if( setuid( r ) == -1 )
    return -1;
  return seteuid( e );
}

#endif

static
void czput( char *msg )
{
  char buf[80];
  int len = strlen( msg );

  if( len > 80 )
    len = 80;

  memset( buf, ' ', (sizeof buf) );
  strncpy( buf, msg, len );
  send( sockout, buf, (sizeof buf), 0 );
}


static
void send_msg( char *msg )
{
  int n = strlen( msg ) + 1;
  if( send( sockout, msg, n, 0 ) != n )
    exit( 1 );
}


static
void fatal_error( char *msg )
{
  send_msg( msg );
  exit( 1 );
}


/*
 * check the password, change user id and directory
 */
static
char *check_pass( char *conf_dir )
{
  char piafrc_name[64];
  FILE *piafrc;
  char user_pass[64];
  char new_user_pass[68];
  static char user_name[32];
  char pass_word[32];
  char *pass_crypt;
  char *passw;
  char msg[80];
  struct passwd *pw;
#ifdef SHADOWPW
  struct spwd *spw;
#endif
  int new_proto = 0;
  int n, i;
#if defined(CERNLIB_CORE)
  extern char *getactent(struct passwd *, char *, char *, int);
  char accbuf[128];
#endif

  if( (n = recv( sockin, new_user_pass, (sizeof new_user_pass), 0 )) <= 0 ) {
    fatal_error( "Cannot receive authentication" );
  }

  /* if new protocol then don't save .piafrc */
  if (!strncmp(new_user_pass, "new:", 4)) {
     for( i = 4; i < n - 1; i++ )
        user_pass[i-4] = ~new_user_pass[i];
     user_pass[i-4] = '\0';
     new_proto = 1;
  } else {
     for( i = 0; i < n - 1; i++ )
        user_pass[i] = ~new_user_pass[i];
     user_pass[i] = '\0';
  }

  if( sscanf( user_pass, "%s %s", user_name, pass_word ) != 2 ) {
    fatal_error( "Bad authentication record" );
  }

  if( (pw = getpwnam( user_name )) == NULL ) {
    sprintf( msg, "Passwd: User %s unknown", user_name );
    fatal_error( msg );
  }
#ifdef SHADOWPW
  /* System V Rel 4 style shadow passwords */
  if( (spw = getspnam( user_name )) == NULL ) {
    sprintf( msg, "Passwd: User %s password unavailable", user_name );
    fatal_error( msg );
  }
  passw = spw->sp_pwdp;
#else
  passw = pw->pw_passwd;
#endif
  pass_crypt = crypt( pass_word, passw );
  n = strlen( passw );
  if((ismaster)&&(strncmp( pass_crypt, passw, n + 1 ) != 0) ) {
     /* If CORE authentication fails try on AFS Kerberos ... */
     int status;
     FILE *klogger;
     char fname[80],cmd[128];
     sprintf (fname,"%s/%s", conf_dir, AKLOG);
     if( (klogger = fopen(fname , "r" )) != NULL ) {  
        fclose (klogger); 
        /* aklog runs klog on a remote node where AFS is mounted */
        sprintf(cmd,"%s %s %s ", fname, pw->pw_dir, user_name);
	/* The AFS password is written in a secure file in   */
	/* user's home directory                             */
	sprintf( piafrc_name, "%s/%s", pw->pw_dir, SECUREFILE);
	if( (piafrc = fopen( piafrc_name, "w" )) == NULL ) {
	  sprintf( msg, "Cannot create %s", piafrc_name );
	  fatal_error( msg );
	}
	fprintf(piafrc,"%s",pass_word);
	fclose( piafrc );
	chmod( piafrc_name, S_IRUSR|S_IWUSR );
	
        if ((status = system (cmd)) < 0) {
           fatal_error("Can't system() to check AFS authentication");
        }
        if (status  == -127) {
           fatal_error("Can't shell to check AFS authentication");
        }
        if ( WIFEXITED(status) ) {
           if (WEXITSTATUS(status) != 0) {
              sprintf( msg, 
                "Can't authenticate on AFS: %d", WEXITSTATUS(status));
              fatal_error(msg );
           }
        } else {
           sprintf( msg, "Invalid Klog authentication: %d", status );
           fatal_error(msg );
        }
     } else {
       fatal_error("Invalid login");   
     }
  }

  /* set access control list from /etc/initgroup */
  initgroups( user_name, pw->pw_gid );

  if( setresgid( pw->pw_gid, pw->pw_gid, 0 ) == -1 ) {
    sprintf( msg, "Cannot setgid for user %s", user_name );
    fatal_error( msg );
  }

  if( setresuid( pw->pw_uid, pw->pw_uid, 0 ) == -1 ) {
    sprintf( msg, "Cannot setuid for user %s", user_name );
    fatal_error( msg );
  }

#if defined(CERNLIB_CORE)
  if (!getactent(pw, NULL, accbuf, sizeof(accbuf)) ||
     /* !strstr(accbuf, "piaf")) { */
      strstr(accbuf, "coretapes")) {
    sprintf( msg, "PIAF access not enabled for user %s", user_name );
    fatal_error( msg );
  }
#endif

  if( chdir( pw->pw_dir ) == -1 ) {
    sprintf( msg, "Cannot change directory to %s", pw->pw_dir );
    fatal_error( msg );
  }

  /* if not new protocol then save .piafrc */
  if (!new_proto) {

     /* copy password info to local rc file for slave authentication */
     sprintf( piafrc_name, ".piafrc" );
     if( (piafrc = fopen( piafrc_name, "w" )) == NULL ) {
       sprintf( msg, "Cannot create %s", piafrc_name );
       fatal_error( msg );
     }
     fprintf( piafrc, "%s\n", user_pass );
     fclose( piafrc );
     chmod( piafrc_name, S_IRUSR|S_IWUSR );

  }

  return user_name;
}


/*
 * look if user should be rerouted to another server node
 */
static
char *reroute_user( char *confdir, char *user_name, char* net_interface )
{
  char conffile[256];
  FILE *piafconf;

  sprintf( conffile, "%s/etc/piaf.conf", confdir );
  if( (piafconf = fopen( conffile, "r" )) != NULL ) {
    /* read configuration file */
    static char user_on_node[32];
    struct stat statbuf;
    char line[256];
    char node_name[MAX_SLAVES][32];
    int nnodes = 0;
    int i;

    strcpy( user_on_node, "any" );

    while( fgets( line, (sizeof line), piafconf ) != NULL ) {
      char word[4][64];
      int nword = sscanf( line, " %s %s %s %s",
                         word[0], word[1], word[2], word[3] );

      /*
       * all running nodes must be configured by a line
       *     node <name>
       */
      if( nword >= 2 && strcmp( word[0], "node" ) == 0 ) {
        if( gethostbyname( word[1] ) != NULL ) {
          strcpy( node_name[nnodes], word[1] );
          nnodes++;
        }
        continue;
      }

      if( nword >= 4 ) {
        /*
         * users can be preferrably rerouted to a specific node
         *     user <name> on <node>
         */
        if ( strcmp( word[0], "user" ) == 0 &&
             strcmp( word[1], user_name ) == 0 &&
             strcmp( word[2], "on" ) == 0 ) {
          /* user <name> on <node> */
          strcpy( user_on_node, word[3] );
          continue;
        }
        /*
         * particular network interfaces can be specified to
         * require a certain node
         *     interface <name> on <node>
         */
        else if ( strcmp( word[0], "interface" ) == 0 &&
             strcmp( word[1], net_interface ) == 0 &&
             strcmp( word[2], "on" ) == 0 ) {
          /* interface <name> on <node> */
          strcpy( user_on_node, word[3] );
          continue;
        }
        /*
         * particular configuration file can be specified for a user
         *     userconf <name> on <directory>
         * If not specified default is used (argv[1])
         */
        else if ( strcmp( word[0], "userconf" ) == 0 &&
             strcmp( word[1], user_name ) == 0 &&
             strcmp( word[2], "on" ) == 0 ) {
          /* userconf <name> on <directory> */
          strcpy( confdir, word[3] );
          continue;
        }
      }
    }
    fclose( piafconf );

    /* make sure the node is running */
    for( i = 0; i < nnodes; i++ ) {
      if( strcmp( node_name[i], user_on_node ) == 0 ) {
        return user_on_node;
      }
    }

    /*
     * get the node name from next.node update by a daemon monitoring
     * the system load; make sure the file is not completely out of date
     * If it's a fifo, however, there's no point in checking the date.
     */
    sprintf( conffile, "%s/etc/next.node", confdir );
    if( stat( conffile, &statbuf ) == -1 ) {
      return NULL;
    }
    else if((S_ISFIFO(statbuf.st_mode) ||
             difftime( time( NULL ), statbuf.st_mtime ) < 600)
            && (piafconf = fopen( conffile, "r" )) != NULL ) {
      if( fgets( line, (sizeof line), piafconf ) != NULL ) {
        sscanf( line, " %s ", user_on_node );
        for( i = 0; i < nnodes; i++ ) {
          if( strcmp( node_name[i], user_on_node ) == 0 ) {
            fclose( piafconf );
            return user_on_node;
          }
        }
      }
      fclose( piafconf );
    }
  }

  return NULL;
}


/*
 * Arguments for master:  "piafserv" confdir
 * Arguments for slave :  "piafslave" confdir
 */
int main( int argc, char **argv )
{
  char *argvv[4];
  char arg0[256];
  char *user_name;
  char *node_name;
  char vtag[80];
  char msg[80];
  char user_conf[80];
  char cmd[80];

#if defined(CERNLIB_DEBUG)
  int debug = 1;
  while( debug == 1 ) {
    continue;
  }
#endif

  if( !strcmp("piafserv", argv[0]) ) 
    ismaster = 1;

  /*
   * Make this process the process group leader and disassociate from
   * control terminal - fork is executed to ensure a unique process id
   * and to make sure our process already isn't a process group leader
   * in which case the call to setsid would fail
   */
  if (fork() != 0) exit(0); /* parent exits */
  setsid();
  /* every user share the same configuration file by default */
  strcpy( user_conf, argv[1] );
  /* user authentication */
  user_name = check_pass( user_conf );
 
  /* only reroute in case of master server */
  if( ismaster ) {
    struct sockaddr_in addr;
    int len = sizeof addr;
    char host_name[MAXHOSTNAMELEN + 1];
    char net_interface[MAXHOSTNAMELEN + 1], *c;

    gethostname(host_name,sizeof host_name - 1);
    host_name[sizeof host_name - 1] = '\0';

    /*
     * Find out which network interface is used for the
     * connection (there may be several)
     */
    if (getsockname(fileno(stdin),(struct sockaddr *)&addr,&len) < 0)
      strcpy(net_interface,host_name);
    else {
      struct hostent *he =
        gethostbyaddr((char *)&addr.sin_addr,sizeof(struct in_addr),AF_INET);
      if (he && he->h_name) {
        strncpy(net_interface,he->h_name,sizeof net_interface - 1);
        net_interface[sizeof net_interface - 1] = '\0';
      }
      else
        strcpy(net_interface,host_name);
    }
    if ((c = strchr(net_interface,'.')) != NULL)
      *c = '\0';
    if ((node_name = reroute_user( user_conf, user_name, net_interface ))
        != NULL ) {
      /* send a reroute request to the client passing the IP address */

      /* make sure that we are not already on the target node */
      if( strcmp( host_name, node_name ) != 0 ) {
        struct hostent *host = gethostbyname( host_name );
        struct hostent *node; /* gethostbyname( node_name ) would overwrite */

        if( host != NULL ) {
          struct in_addr *host_addr = (struct in_addr*)(host->h_addr);
          char host_numb[32];
          strcpy( host_numb, inet_ntoa( *host_addr ) );

          if( (node = gethostbyname( node_name )) != NULL ) {
            struct in_addr *node_addr = (struct in_addr*)(node->h_addr);
            char node_numb[32];
            strcpy( node_numb, inet_ntoa( *node_addr ) );

            /*
             * compare the string representation of the IP addresses
             * to avoid possible problems with host name aliases
             */
            if( strcmp( host_numb, node_numb ) != 0 ) {
              sprintf( msg, "Reroute:%s", node_numb );
              send_msg( msg );
              exit( 0 );
            }
          }
        }
      }
    }
  }

  /* If it is a master run the connection acceptance script */

  if( !strcmp("piafserv", argv[0]) ) {
    FILE *usrcheckerf;
    char fname[80];

    sprintf (fname,"%s/%s", argv[1], CHECKUSERS);
    if( (usrcheckerf = fopen(fname , "r" )) != NULL ) {    
      int retval;
      fclose (usrcheckerf);
      sprintf ( cmd, "%s/%s %s", argv[1], CHECKUSERS, argv[1] );
      retval = system (cmd);
      switch (WEXITSTATUS(retval)) {
      case 0 : /* Connection accepted */
        break;
      case 1 : /* Too many users */
      default :
        /* When connection is refused try to retrieve the message */
        sprintf (fname,"%s/%s", argv[1], CHECKLOG);
        if ((usrcheckerf = fopen(fname , "r" )) != NULL ) {
           fgets (msg, 80, usrcheckerf);
           fclose (usrcheckerf);
           if ( strlen (msg) == 0 ) {
              strcpy  (msg, "Piaf connection denied (empty msg) \n");
           }
        } else {
           strcpy  (msg, "Piaf connection denied (unknown reason) \n");
        }
        send_msg(msg);
	return 0;
	break;
      }
    }
  }

  send_msg( "Okay" );

  /* receive version tag */
  recv( sockin, vtag, (sizeof vtag), 0 );

  /* start server version */
  sprintf( arg0, "%s/bin/piafserv.%s", argv[1], vtag );
  argvv[0] = arg0;
  argvv[1] = argv[0];
  argvv[2] = user_conf;
  argvv[3] = NULL;
  execv( arg0, argvv );

  /* tell client that exec failed */
  sprintf( msg, "\
Cannot start Piaf server version %s --- update your PAW version !", vtag );
  czput( msg );

  return 0;

}











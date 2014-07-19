/*
 * $Id: kuser.h,v 1.2 2000/01/21 13:19:53 couet Exp $
 *
 * $Log: kuser.h,v $
 * Revision 1.2  2000/01/21 13:19:53  couet
 * - C++ keywords "template" and "delete" were used for variables names
 *   in C prototypes. This prevent to use this include file with the
 *   GNU C++ compiler g++.
 *
 * Revision 1.1.1.1  1996/03/08 15:33:01  mclareni
 * Kuip
 *
 */

#define KUMAC_UNWIND -30041961 /* error status to quit macro execution */

#ifdef __cplusplus
extern "C" {
#endif

/*
 * C-interface functions
 */
extern char*  k_getar(void);
extern void   k_setar( size_t, char** );
extern char*  k_userid(void);

extern void   ku_alfa(void);
extern char*  ku_appl( int* luno, int* inmacro );

extern int    ku_bool( const char* expr );

extern int    ku_close( int luno );
extern void   ku_cmdl( const char* ctemplate );

extern int    ku_edit( const char* path, int use_server );
extern char*  ku_eval( const char* expr );
extern int    ku_exec( const char* cmd );
extern int    ku_exel( const char* cmd );
extern char*  ku_expr( const char* expr );

extern char*  ku_getc(void);
extern char*  ku_gete(void);
extern char*  ku_getf(void);
extern int    ku_geti(void);
extern char*  ku_getl(void);
extern double ku_getr(void);
extern char*  ku_gets(void);

extern char*  ku_fcase( char* path );

extern char*  ku_home( const char* fname, const char* ftype );

extern char*  ku_inps( const char* prompt );
extern int    ku_inqf( const char* path );
extern int    ku_intr( int enable );

extern void   ku_last( const char* cmd );

extern int    ku_match( const char* string, const char* pattern,
                       int ignore_case );
extern int    ku_math( const char* expr, double* result );
extern int    ku_more( const char* question, const char* line );

extern int    ku_npar(void);

extern int    ku_open( int luno, const char* path, const char* mode );

extern void   ku_pad( const char* path, int idelete );
extern char*  ku_path(void);
extern void   ku_piaf( int socket, void(*sync)() );
extern char*  ku_proc( const char* prompt, const char* dfault );
extern char*  ku_prof( const char* prompt, const char* dfault );
extern int    ku_proi( const char* prompt, int dfault );
extern char*  ku_prop( const char* prompt );
extern double ku_pror( const char* prompt, double dfault );
extern char*  ku_pros( const char* prompt, const char* dfault );

extern char** ku_qenv(void);
extern char*  ku_qexe( const char* fname );
extern int    ku_qkey(void);
extern int    ku_qmac( const char* mname );

extern int    ku_read( int luno, char* buf, size_t len );

extern int    ku_sapp( const char* path, const char* exit );
extern void   ku_shut(void);
extern void   ku_sibr(void);
extern void   ku_spy( const char* option );
extern int    ku_stop( int set );

extern void   ku_time( time_t, clock_t );
extern void   ku_trap( int enable, int traceback );

extern int    ku_vqaddr( const char* vname );
extern int    ku_vtype( const char* vname );
extern int    ku_vvalue( const char* vname, double* value );

extern void   ku_whag(void);
extern void   ku_what( void(*styleG)() );
extern void   ku_write( int, const char* line );

extern int    km_icon( const char*, const char* );

#ifdef __cplusplus
}
#endif

